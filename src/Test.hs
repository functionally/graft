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
trace' = if True then (trace =<<) else const id


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


prop_shortestpath :: Bool -> TestGraphs -> Bool
prop_shortestpath negativeWeights (TestGraphs (gFGL, gAdj, gMap, gTie)) =
  let
    vs = fst <$> F.labNodes gFGL
    start : finish : _ = vs
    dFGL = F.spLength start finish gFGL
    (pAdj, Sum dAdj) = shortestPath negativeWeights (Sum . snd) gAdj start finish
    (pMap, Sum dMap) = shortestPath negativeWeights (Sum . snd) gMap start finish
    (pTie, Sum dTie) = shortestPath negativeWeights (Sum . snd) gTie start finish
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
    L.Optimal (reference, _) = trace' (const $ show (vertexLabels gAdj, edgeLabels gAdj)) $ L.simplex (L.Minimize costConstraint) (L.Sparse $ flowConstraint ++ capacityConstraint) []
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
      (HyperVertex "A", HyperVertex "B", ("A -> B",  135,  29))
    , (HyperVertex "A", HyperVertex "C", ("A -> C",   14,  39))
    , (HyperVertex "A", HyperVertex "D", ("A -> D",    3,  35))
    , (HyperVertex "A", HyperVertex "E", ("A -> E",   44,  63))
    , (HyperVertex "A", HyperVertex "G", ("A -> G",   42,  43))
    , (HyperVertex "C", HyperVertex "B", ("C -> B",   15,  86))
    , (HyperVertex "D", HyperVertex "C", ("D -> C",   32,   7))
    , (HyperVertex "D", HyperVertex "E", ("D -> E",   32, 110))
    , (HyperVertex "D", HyperVertex "G", ("D -> G",    4, 148))
    , (HyperVertex "E", HyperVertex "B", ("E -> B",    3,  39))
    , (HyperVertex "G", HyperVertex "B", ("G -> B",   46,  33))
    ]
example 5 =
  sort
    [
      (HyperVertex "-51", HyperVertex "  3", ("-51 ->  3",  163.2759627122456  ,   72.2769546463097   ))
    , (HyperVertex "-51", HyperVertex " 47", ("-51 -> 47",   94.36360894100373 ,   89.7782144337968   ))
    , (HyperVertex "-18", HyperVertex " -8", ("-18 -> -8",  254.8914612649748  ,   76.9006139281888   ))
    , (HyperVertex "-18", HyperVertex " 32", ("-18 -> 32",   56.14298988425839 , 1090.39209707493     ))
    , (HyperVertex " -8", HyperVertex "-18", (" -8 ->-18",    2.767169816649054,   61.29921020779062  ))
    , (HyperVertex " -8", HyperVertex " 47", (" -8 -> 47",   19.008449671407494,   74.37446366854016  ))
    , (HyperVertex " -8", HyperVertex " 58", (" -8 -> 58",  193.3707193876969  ,   23.597985048028526 ))
    , (HyperVertex "  3", HyperVertex "-51", ("  3 ->-51",   12.298407769517826,  123.79581142962333  ))
    , (HyperVertex "  3", HyperVertex " 47", ("  3 -> 47",  114.19378674414946 ,    2.5913858580612135))
    , (HyperVertex "  5", HyperVertex "-51", ("  5 ->-51",  201.963304240616   ,    4.1514298465543416))
    , (HyperVertex "  5", HyperVertex "-18", ("  5 ->-18", 1000.7751883985791  ,   92.09805736236967  ))
    , (HyperVertex "  5", HyperVertex " -8", ("  5 -> -8",   73.1183209863981  ,   55.61306179329624  ))
    , (HyperVertex "  5", HyperVertex "  3", ("  5 ->  3",    7.695308892502776,   80.59123248350916  ))
    , (HyperVertex "  5", HyperVertex "  8", ("  5 ->  8",    9.453040138945308,  416.75725622872284  ))
    , (HyperVertex "  5", HyperVertex " 21", ("  5 -> 21",  131.51060513931805 ,  104.83763927808187  ))
    , (HyperVertex "  5", HyperVertex " 32", ("  5 -> 32",   70.51540389131019 ,  248.43584611663215  ))
    , (HyperVertex "  8", HyperVertex " -8", ("  8 -> -8",   64.67487081331834 ,   67.68831013625415  ))
    , (HyperVertex "  8", HyperVertex "  5", ("  8 ->  5",    8.639843283558937,  351.66287562290995  ))
    , (HyperVertex "  8", HyperVertex " 58", ("  8 -> 58",   33.53306080912911 ,   32.021741770072886 ))
    , (HyperVertex " 21", HyperVertex "-18", (" 21 ->-18",   16.825879479072192,   29.83081634181621  ))
    , (HyperVertex " 21", HyperVertex " -8", (" 21 -> -8",   66.82441106114995 ,   57.445664835932575 ))
    , (HyperVertex " 21", HyperVertex "  8", (" 21 ->  8",    6.089828719548931,   88.29165073403323  ))
    , (HyperVertex " 21", HyperVertex " 32", (" 21 -> 32",    4.175662305604122,   93.0372870656308   ))
    , (HyperVertex " 32", HyperVertex "-51", (" 32 ->-51",  482.04946669417023 ,  303.6907381601643   ))
    , (HyperVertex " 32", HyperVertex "  8", (" 32 ->  8",   11.96244776672608 ,   26.715334723821716 ))
    , (HyperVertex " 32", HyperVertex " 21", (" 32 -> 21",   48.992587906652744,   76.0396092545033   ))
    , (HyperVertex " 32", HyperVertex " 58", (" 32 -> 58",   58.495717441731934,    1.5470546781805221))
    , (HyperVertex " 47", HyperVertex "  3", (" 47 ->  3",  167.26463062138774 ,   28.096549845584622 ))
    , (HyperVertex " 47", HyperVertex "  5", (" 47 ->  5",   64.96995653348121 ,   21.114800020496922 ))
    , (HyperVertex " 47", HyperVertex " 32", (" 47 -> 32",   89.62500242004171 ,  290.62323460036095  ))
    , (HyperVertex " 58", HyperVertex "-18", (" 58 ->-18",  114.86782481137261 ,    0.2201591442574662))
    , (HyperVertex " 58", HyperVertex "  5", (" 58 ->  5",   63.089796817212985,   44.52416436842465  ))
    , (HyperVertex " 58", HyperVertex " 32", (" 58 -> 32",   10.02733871920842 ,   44.85496855889023  ))
    , (HyperVertex " 58", HyperVertex " 47", (" 58 -> 47",   48.587972528464064,   14.429136244864297 ))
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
          (("+ -> 0", 0, 20), 20)
        , (("0 -> 1", 4, 15), 12)
        , (("0 -> 2", 4,  8),  8)
        , (("1 -> 2", 2, 20),  8)
        , (("1 -> 3", 2,  4),  4)
        , (("1 -> 4", 6, 10),  0)
        , (("2 -> 3", 1, 15), 13)
        , (("2 -> 4", 3,  4),  3)
        , (("3 -> -", 0,  5),  5)
        , (("3 -> 4", 2, 20), 12)
        , (("4 -> -", 0, 15), 15)
        , (("4 -> 2", 3,  5),  0)
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
    L.Optimal (reference, _solution) = L.simplex (L.Minimize costConstraint) (L.Sparse $ flowConstraint ++ capacityConstraint) []
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
    reference == cAdj
  

prop_example_5 :: Bool
prop_example_5 =
  let
    gAdj = buildExample 5 :: AdjacencyGraph (HyperVertex String) (String, Double, Double)
    start  = HyperVertex "-51"
    finish = HyperVertex "-18"
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
    L.Optimal (reference, _solution) = L.simplex (L.Minimize costConstraint) (L.Sparse $ flowConstraint ++ capacityConstraint) []
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
    reference == cAdj
  

test :: IO ()
test =
  do
    let
      tests = 100
      isSuccess Success{} = True
      isSuccess _         = False
    results  <- mapM (quickCheckWithResult stdArgs {maxSuccess = tests})
      [
        label "read & show"                    prop_readshow
      , label "shortest path [Dijkstra]"     $ prop_shortestpath False
      , label "shortest path [Bellman-Ford]" $ prop_shortestpath True
      , label "maximum flow"                   prop_maxflow
--    , label "minimum cost flow"              prop_mincostflow
      ]
    results' <- mapM (quickCheckWithResult stdArgs {maxSuccess = 1})
      [
        label "example #1" prop_example_1
      , label "example #3" prop_example_3
      , label "example #4" prop_example_4
      , label "example #5" prop_example_5
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
      n <- [1..5]
    , let g = buildExample n :: MapGraph (HyperVertex String) (String, Double, Double)
    ]
