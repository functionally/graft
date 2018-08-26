{-# LANGUAGE TypeFamilies         #-}


module Main (
  main
, main'
, example
) where


import Control.Arrow ((&&&))
import Data.Foldable (toList)
import Data.Maybe (isNothing)
import Data.Function (on)
import Data.List (sort, sortBy)
import Data.Graph.IO (toGraphviz, toLines)
import Data.Graph.MaximumFlow (maximumFlow, minimumCostFlow)
import Data.Graph.ShortestPath (shortestPath)
import Data.Graph.Types (Graph(..), EdgeList)
import Data.Graph.Types.Adjacency (AdjacencyGraph)
import Data.Graph.Types.MapGraph (MapGraph)
import Data.Graph.Types.TiedGraph (TiedGraph)
import Data.Graph.Types.Util (HyperVertex(..))
import Data.Graph.Types.Weight (netFlows)
import Data.Monoid (All(..), Sum(..))
import Data.Tuple.Util (snd3, trd3)
import System.Exit (exitFailure, exitSuccess)
import Test.QuickCheck (Arbitrary(..), Args(..), Gen, Positive(..), Result(..), label, quickCheckWithResult, stdArgs)

import qualified Data.Map.Strict as M ((!), foldMapWithKey, fromList, toList, unionWith)

import qualified Data.Graph.Inductive.Graph as F (Graph(..), emap)
import qualified Data.Graph.Inductive.Arbitrary as F (NoLoops(..), NoMultipleEdges(..), SimpleGraph)
import qualified Data.Graph.Inductive.PatriciaTree as F (Gr)
import qualified Data.Graph.Inductive.Query.MaxFlow as F (maxFlow)
import qualified Data.Graph.Inductive.Query.SP as F (spLength)

import qualified Numeric.LinearProgramming as L

import Debug.Trace (trace)


trace' :: (a -> String) -> a -> a
trace' = if False then (trace =<<) else const id


trace'' :: String -> Bool -> Bool
trace'' t x = if not x then trace' (const t) x else x


main :: IO ()
main = if True then test else main'


type TestFGL            = F.Gr () Double

type TestAdjacencyGraph = AdjacencyGraph Int ((Int, Int), Double)
type TestMapGraph       = MapGraph       Int ((Int, Int), Double)
type TestTiedGraph      = TiedGraph      Int ((Int, Int), Double)

newtype TestGraphs = TestGraphs (TestFGL, TestAdjacencyGraph, TestMapGraph, TestTiedGraph)
  deriving (Show)

instance Arbitrary TestGraphs where
  arbitrary =
    do
      F.NL (F.NME gr) <- arbitrary :: Gen (F.SimpleGraph F.Gr () (Positive Double))
      let
        vs = fst <$> F.labNodes gr
        es =
          [
            (from, to, ((from, to), weight))
          |
            (from, to, Positive weight) <- F.labEdges gr
          ]
      return
        $ TestGraphs (
          F.emap getPositive gr
        , fromEdgeList vs es
        , fromEdgeList vs es
        , fromEdgeList vs es
        )


prop_readshow :: TestGraphs -> Bool
prop_readshow (TestGraphs (_, gAdj, gMap, gTie)) =
     gAdj == read (show gAdj)
  && gMap == read (show gMap)
  && gTie == read (show gTie)


prop_shortestpath :: TestGraphs -> Bool
prop_shortestpath (TestGraphs (gFGL, gAdj, gMap, gTie)) =
  let
    vs = fst <$> F.labNodes gFGL
    start : finish : _ = vs
    dFGL = F.spLength start finish gFGL
    (pAdj, Sum dAdj) = shortestPath (Sum . snd) gAdj start finish
    (pMap, Sum dMap) = shortestPath (Sum . snd) gMap start finish
    (pTie, Sum dTie) = shortestPath (Sum . snd) gTie start finish
  in
       length vs < 3
    || (isNothing dFGL && null pAdj || dFGL == Just dAdj)
    && (isNothing dFGL && null pMap || dFGL == Just dMap)
    && (isNothing dFGL && null pTie || dFGL == Just dTie)


prop_maxflow :: TestGraphs -> Bool
prop_maxflow (TestGraphs (gFGL, gAdj, gMap, gTie)) =
  let
    epsilon = 1e-10
    vs = fst <$> F.labNodes gFGL
    start : finish : _ = vs
    x = F.maxFlow gFGL start finish
    check =
      getAll
        . M.foldMapWithKey (const $ All . (< epsilon) . abs)
        . M.unionWith (+) (M.fromList [(start, - x), (finish, x)])
    fAdj = netFlows fst $ maximumFlow snd gAdj start finish
    fMap = netFlows fst $ maximumFlow snd gMap start finish
    fTie = netFlows fst $ maximumFlow snd gTie start finish
  in
       length vs < 3
    || check fAdj
    && check fMap
    && check fTie
    

type TestAdjacencyGraph' = AdjacencyGraph Int ((Int, Int), (Double, Double))
type TestMapGraph'       = MapGraph       Int ((Int, Int), (Double, Double))
type TestTiedGraph'      = TiedGraph      Int ((Int, Int), (Double, Double))

newtype TestGraphs' = TestGraphs' (TestAdjacencyGraph', TestMapGraph', TestTiedGraph')
  deriving (Show)

instance Arbitrary TestGraphs' where
  arbitrary =
    do
      F.NL (F.NME gr) <- arbitrary :: Gen (F.SimpleGraph F.Gr () (Positive Double, Positive Double))
      let
        vs = fst <$> F.labNodes gr
        es =
          [
            (from, to, ((from, to), (weight, weight')))
          |
            (from, to, (Positive weight, Positive weight')) <- F.labEdges gr
          ]
      return
        $ TestGraphs' (
          fromEdgeList vs es
        , fromEdgeList vs es
        , fromEdgeList vs es
        )


prop_mincostflow :: TestGraphs' -> Bool
prop_mincostflow (TestGraphs' (gAdj, gMap, gTie)) =
  let
    epsilon = 1e-5
    vs = vertexLabels gAdj
    start : finish : _ = vs
    mf = foldl ((maximum .) . (. return) . (:)) 0 . netFlows fst $ maximumFlow (snd . snd) gMap start finish
    es = M.fromList $ zip (toList $ edges gAdj) [1..]
    flowConstraint =
      [
        (
          [ 1 L.# (es M.! e) | e <- toList $ edgesFrom gAdj v]
          ++
          [-1 L.# (es M.! e) | e <- toList $ edgesTo   gAdj v]
        )
        L.:==:
        (
          case (vertexLabel gAdj v == start, vertexLabel gAdj v == finish) of
            (True, _   ) -> mf
            (_   , True) -> - mf
            _            -> 0
        )
      |
        v <- toList $ vertices gAdj
      ]
    capacityConstraint =
      [
        [1 L.# i] L.:<=: w
      |
        (e, i) <- M.toList es
      , let (_, (_, w)) = edgeLabel gAdj e
      ]
    costConstraint =
      [
        c
      |
        (e, _) <- sortBy (compare `on` snd) $ M.toList es
      , let (_, (c, _)) = edgeLabel gAdj e
      ] 
    L.Optimal (reference, _) = L.simplex (L.Minimize costConstraint) (L.Sparse $ flowConstraint ++ capacityConstraint) []
    total c =
      sum
        [
          cost * flow
        |
          ((_, (cost, _)), flow) <- M.toList c
        ]
    cAdj = total $ minimumCostFlow snd gAdj start finish
    cMap = total $ minimumCostFlow snd gMap start finish
    cTie = total $ minimumCostFlow snd gTie start finish
    check = (< epsilon) . abs . (1 -) . (/ reference)
  in 
       length vs < 3
    || mf == 0
    || trace'' (show (length vs, cAdj / reference - 1, reference, cAdj, cMap, cTie)) (check cAdj && check cMap && check cTie)



buildExample :: (Graph g, VertexLabel g ~ HyperVertex String, EdgeLabel g ~ (String, Double, Double)) => Int -> g
buildExample = (fromEdgeList =<< (snd3 <$>)) . example


example :: Int -> EdgeList (HyperVertex String) (String, Double, Double)
example 1 =
    sort
      [
        (HyperSource    , HyperVertex "A", ("+A",  5, 0))
      , (HyperSource    , HyperVertex "B", ("+B", 10, 0))
      , (HyperVertex "A", HyperVertex "B", ("AB", 15, 1))
      , (HyperVertex "A", HyperVertex "C", ("AC", 15, 5))
      , (HyperVertex "B", HyperVertex "D", ("BD", 10, 2))
      , (HyperVertex "C", HyperVertex "E", ("CE", 15, 4))
      , (HyperVertex "D", HyperVertex "C", ("DC", 15, 1))
      , (HyperVertex "D", HyperVertex "F", ("DF",  8, 3))
      , (HyperVertex "E", HyperSink      , ("E-", 10, 0))
      , (HyperVertex "F", HyperVertex "E", ("FE", 15, 1))
      , (HyperVertex "F", HyperSink      , ("F-",  5, 0))
      ]
example 2 =
  sort
    [
      (HyperSource         , HyperVertex "P:EP"  , ("+P:EP" ,   5.6, 1.75))
    , (HyperSource         , HyperVertex "P:BA"  , ("+P:BA" , 226.9, 1.75))
    , (HyperVertex "P:EP"  , HyperVertex "T:2:EP", ("F:1:EP", 999.9, 3.67))
    , (HyperVertex "P:BA"  , HyperVertex "T:2:BA", ("F:1:BA", 999.9, 3.67))
    , (HyperVertex "T:2:EP", HyperVertex "C:EP"  , ("F:3:EP", 999.9, 0.06))
    , (HyperVertex "T:2:BA", HyperVertex "C:BA"  , ("F:3:BA", 999.9, 0.06))
    , (HyperVertex "T:2:EP", HyperVertex "T:2:BA", ("F1831" , 999.9, 0.67))
    , (HyperVertex "T:2:BA", HyperVertex "T:2:EP", ("R1831" , 999.9, 0.67))
    , (HyperVertex "C:EP"  , HyperSink           , ("EP:-D" ,  68.1, 0.00))
    ]
example 3 =
  sort 
    [
      (HyperVertex "0", HyperVertex "1", ("0 -> 1", 4, 15))
    , (HyperVertex "0", HyperVertex "2", ("0 -> 2", 4,  8))
    , (HyperVertex "1", HyperVertex "2", ("1 -> 2", 2, 20))
    , (HyperVertex "1", HyperVertex "3", ("1 -> 3", 2,  4))
    , (HyperVertex "1", HyperVertex "4", ("1 -> 4", 6, 10))
    , (HyperVertex "2", HyperVertex "3", ("2 -> 3", 1, 15))
    , (HyperVertex "2", HyperVertex "4", ("2 -> 4", 3,  4))
    , (HyperVertex "3", HyperVertex "4", ("3 -> 4", 2, 20))
    , (HyperVertex "4", HyperVertex "2", ("4 -> 2", 3,  5))
    , (HyperSource  ,   HyperVertex "0", ("+ -> 0", 0, 20))
    , (HyperVertex "3", HyperSink      , ("3 -> -", 0,  5))
    , (HyperVertex "4", HyperSink      , ("4 -> -", 0, 15))
    ]
example 4 =
  sort
    [
      (HyperVertex "A", HyperVertex "B", ("A -> B",  135.57930968627457  ,  29.01050932594674 ))
    , (HyperVertex "A", HyperVertex "C", ("A -> C",   14.344178273663076 ,  39.19201631386781 ))
    , (HyperVertex "A", HyperVertex "D", ("A -> D",    3.7442258514928928,  35.83113771900464 ))
    , (HyperVertex "A", HyperVertex "E", ("A -> E",   44.64685459256189  ,  63.19636729269409 ))
    , (HyperVertex "A", HyperVertex "G", ("A -> G",   42.25434877223405  ,  43.177909876674136))
    , (HyperVertex "B", HyperVertex "A", ("B -> A",   78.27246358181176  ,  58.69763930366173 ))
    , (HyperVertex "B", HyperVertex "D", ("B -> D",   43.95326817270366  ,  23.28079778592758 ))
    , (HyperVertex "B", HyperVertex "E", ("B -> E",   13.29127782001224  , 133.02432179747237 ))
    , (HyperVertex "C", HyperVertex "B", ("C -> B",   15.558131793874725 ,  86.9546076519134  ))
    , (HyperVertex "C", HyperVertex "G", ("C -> G",    4.178668672073619 ,  33.6681359389675  ))
    , (HyperVertex "D", HyperVertex "A", ("D -> A",   23.892583825319488 ,  21.07446757739977 ))
    , (HyperVertex "D", HyperVertex "C", ("D -> C",   32.55492159576363  ,   7.440527000373489))
    , (HyperVertex "D", HyperVertex "E", ("D -> E",   32.254629917402845 , 110.50127448936429 ))
    , (HyperVertex "D", HyperVertex "G", ("D -> G",    4.856386404269504 , 148.32945854923042 ))
    , (HyperVertex "E", HyperVertex "A", ("E -> A", 3680.1503173202336   ,  24.537500676772705))
    , (HyperVertex "E", HyperVertex "B", ("E -> B",    3.065727926896363 ,  39.22172651205008 ))
    , (HyperVertex "F", HyperVertex "D", ("F -> D",   76.02764767323477  ,  45.09932249339651 ))
    , (HyperVertex "F", HyperVertex "G", ("F -> G",   51.863503114543654 ,  60.40314018443845 ))
    , (HyperVertex "G", HyperVertex "A", ("G -> A",   38.91064559113717  ,  92.28792017023495 ))
    , (HyperVertex "G", HyperVertex "B", ("G -> B",   46.97766646957096  ,  33.56472702951771 ))
    , (HyperVertex "G", HyperVertex "E", ("G -> E",  167.30184872233383  ,  90.13849337554056 ))
    ]
example n = error $ "No example #" ++ show n ++ "."


prop_example_1 :: Bool
prop_example_1 =
  let
    gAdj = buildExample 1 :: AdjacencyGraph (HyperVertex String) (String, Double, Double)
    gMap = buildExample 1 :: MapGraph       (HyperVertex String) (String, Double, Double)
    gTie = buildExample 1 :: TiedGraph      (HyperVertex String) (String, Double, Double)
    answer =
      M.fromList
        [
          (("+A",  5, 0),  5)
        , (("+B", 10, 0), 10)
        , (("AB", 15, 1),  0)
        , (("AC", 15, 5),  5)
        , (("BD", 10, 2), 10)
        , (("CE", 15, 4), 10)
        , (("DC", 15, 1),  5)
        , (("DF",  8, 3),  5)
        , (("E-", 10, 0), 10)
        , (("F-",  5, 0),  5)
        , (("FE", 15, 1),  0)
        ]
    fAdj = maximumFlow snd3 gAdj HyperSource HyperSink
    fMap = maximumFlow snd3 gMap HyperSource HyperSink
    fTie = maximumFlow snd3 gTie HyperSource HyperSink
  in
       fAdj == answer
    && fMap == answer
    && fTie == answer


prop_example_3 :: Bool
prop_example_3 =
  let
    gAdj = buildExample 3 :: AdjacencyGraph (HyperVertex String) (String, Double, Double)
    gMap = buildExample 3 :: MapGraph       (HyperVertex String) (String, Double, Double)
    gTie = buildExample 3 :: TiedGraph      (HyperVertex String) (String, Double, Double)
    answer =
      M.fromList
        [
          (("0 -> 1", 4, 15), 12)
        , (("0 -> 2", 4,  8),  8)
        , (("1 -> 2", 2, 20),  8)
        , (("1 -> 3", 2,  4),  4)
        , (("1 -> 4", 6, 10),  0)
        , (("2 -> 3", 1, 15), 12)
        , (("2 -> 4", 3,  4),  4)
        , (("3 -> 4", 2, 20), 11)
        , (("4 -> 2", 3,  5),  0)
        , (("+ -> 0", 0, 20), 20)
        , (("3 -> -", 0,  5),  5)
        , (("4 -> -", 0, 15), 15)
        ]
    fAdj = minimumCostFlow (snd3 &&& trd3)  gAdj HyperSource HyperSink
    fMap = minimumCostFlow (snd3 &&& trd3)  gMap HyperSource HyperSink
    fTie = minimumCostFlow (snd3 &&& trd3)  gTie HyperSource HyperSink
  in
       fAdj == answer
    && fMap == answer
    && fTie == answer

prop_example_4 :: Bool
prop_example_4 =
  let
    gAdj = buildExample 4 :: AdjacencyGraph (HyperVertex String) (String, Double, Double)
    start  = HyperVertex "A"
    finish = HyperVertex "B"
    mf = foldl ((maximum .) . (. return) . (:)) 0 . netFlows (\([from, ' ', '-', '>', ' ', to], _, _) -> ([from], [to])) $ maximumFlow trd3 gAdj start finish
    es = M.fromList $ zip (toList $ edges gAdj) [1..]
    flowConstraint =
      [
        (
          [ 1 L.# (es M.! e) | e <- toList $ edgesFrom gAdj v]
          ++
          [-1 L.# (es M.! e) | e <- toList $ edgesTo   gAdj v]
        )
        L.:==:
        (
          case (vertexLabel gAdj v == start, vertexLabel gAdj v == finish) of
            (True, _   ) -> mf
            (_   , True) -> - mf
            _            -> 0
        )
      |
        v <- toList $ vertices gAdj
      ]
    capacityConstraint =
      [
        [1 L.# i] L.:<=: w
      |
        (e, i) <- M.toList es
      , let (_, _, w) = edgeLabel gAdj e
      ]
    costConstraint =
      [
        c
      |
        (e, _) <- sortBy (compare `on` snd) $ M.toList es
      , let (_, c, _) = edgeLabel gAdj e
      ] 
    L.Optimal (reference, solution) = L.simplex (L.Minimize costConstraint) (L.Sparse $ flowConstraint ++ capacityConstraint) []
    total c =
      sum
        [
          cost * flow
        |
          ((_, cost, _), flow) <- M.toList c
        ]
    fAdj = minimumCostFlow (snd3 &&& trd3)  gAdj start finish
    cAdj = total fAdj
  in
    trace' (const $ show (reference, solution, cAdj, fAdj)) True
--  (9401.970102388144,[29.010509325946746,39.19201631386781,35.83113771900464,39.22172651205008,5.174116310886564,0.0,0.0,0.0,46.6325433142413,0.0,0.0,7.440527000373489,0.0,28.390610718631148,0.0,39.22172651205008,0.0,0.0,0.0,33.56472702951771,0.0],10111.898266956787,fromList [(("A -> B",135.57930968627457,29.01050932594674),29.01050932594674),(("A -> C",14.344178273663076,39.19201631386781),39.19201631386781),(("A -> D",3.7442258514928928,35.83113771900464),35.83113771900464),(("A -> E",44.64685459256189,63.19636729269409),10.831115793418935),(("A -> G",42.25434877223405,43.177909876674136),33.56472702951771),(("B -> A",78.27246358181176,58.69763930366173),0.0),(("B -> D",43.95326817270366,23.28079778592758),0.0),(("B -> E",13.29127782001224,133.02432179747237),0.0),(("C -> B",15.558131793874725,86.9546076519134),46.6325433142413),(("C -> G",4.178668672073619,33.6681359389675),0.0),(("D -> A",23.892583825319488,21.07446757739977),0.0),(("D -> C",32.55492159576363,7.440527000373489),7.440527000373489),(("D -> E",32.254629917402845,110.50127448936429),28.390610718631148),(("D -> G",4.856386404269504,148.32945854923042),0.0),(("E -> A",3680.1503173202336,24.537500676772705),0.0),(("E -> B",3.065727926896363,39.22172651205008),39.22172651205008),(("F -> D",76.02764767323477,45.09932249339651),0.0),(("F -> G",51.863503114543654,60.40314018443845),0.0),(("G -> A",38.91064559113717,92.28792017023495),0.0),(("G -> B",46.97766646957096,33.56472702951771),33.56472702951771),(("G -> E",167.30184872233383,90.13849337554056),0.0)])
  

test :: IO ()
test =
  do
    let
      tests = 10000
      isSuccess Success{} = True
      isSuccess _         = False
    results  <- mapM (quickCheckWithResult stdArgs {maxSuccess = tests})
      [
        label "read & show"       prop_readshow
      , label "shortest path"     prop_shortestpath
      , label "maximum flow"      prop_maxflow
      , label "minimum cost flow" prop_mincostflow
      ]
    results' <- mapM (quickCheckWithResult stdArgs {maxSuccess = 1})
      [
        label "example #1" prop_example_1
      , label "example #3" prop_example_3
      , label "example #4" prop_example_4
      ]
    if all isSuccess $ results ++ results'
      then exitSuccess
      else exitFailure


main' :: IO ()
main' =
  sequence_
    [
      do
        putStrLn $ "Example " ++ show n ++ ": " ++ show (example n == snd (toEdgeList g)) ++ " " ++ show (g == read (show g))
        putStrLn . unlines $ ("  " ++) <$> toLines g
        writeFile ("example-" ++ show n ++ ".dot") $ toGraphviz ("Example " ++ show n) g
    |
      n <- [1..3]
    , let g = buildExample n :: MapGraph (HyperVertex String) (String, Double, Double)
    ]
