{-# LANGUAGE TypeFamilies         #-}


module Main (
  main
, main'
, example
) where


import Data.Maybe (isNothing)
import Data.List (sort)
import Data.Graph.IO (toGraphviz, toLines)
import Data.Graph.MaximumFlow (maximumFlow)
import Data.Graph.ShortestPath (shortestPath)
import Data.Graph.Types (Graph(..), EdgeList)
import Data.Graph.Types.Adjacency (AdjacencyGraph)
import Data.Graph.Types.MapGraph (MapGraph)
import Data.Graph.Types.TiedGraph (TiedGraph)
import Data.Graph.Types.Util (HyperVertex(..))
import Data.Graph.Types.Weight (netFlows)
import Data.Monoid (All(..), Sum(..))
import Data.Tuple.Util (snd3)
import System.Exit (exitFailure, exitSuccess)
import Test.QuickCheck (Arbitrary(..), Args(..), Gen, Positive(..), Result(..), label, quickCheckWithResult, stdArgs)

import qualified Data.Map.Strict as M (foldMapWithKey, fromList, toList, unionWith)

import qualified Data.Graph.Inductive.Graph as F (Graph(..), emap)
import qualified Data.Graph.Inductive.Arbitrary as F (NoLoops(..), NoMultipleEdges(..), SimpleGraph)
import qualified Data.Graph.Inductive.PatriciaTree as F (Gr)
import qualified Data.Graph.Inductive.Query.MaxFlow as F (maxFlow)
import qualified Data.Graph.Inductive.Query.SP as F (spLength)

{-
import Debug.Trace (trace)


trace' :: (a -> String) -> a -> a
trace' = if True then (trace =<<) else const id


trace'' :: String -> Bool -> Bool
trace'' t x = if True && not x then trace' (const t) x else x
-}


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


main' :: IO ()
main' =
  do

    putStrLn ""

    sequence_
      [
        do
          putStrLn $ "Example " ++ show n ++ ": " ++ show (example n == snd (toEdgeList g)) ++ show (g == read (show g))
          putStrLn . unlines $ ("  " ++) <$> toLines g
          writeFile ("example-" ++ show n ++ ".dot") $ toGraphviz ("Example " ++ show n) g
      |
        n <- [1..2]
      , let g = buildExample n :: MapGraph (HyperVertex String) (String, Double, Double)
      ]

    putStrLn ""

    let
      g = buildExample 1 :: MapGraph (HyperVertex String) (String, Double, Double)
      p = shortestPath (Sum . snd3) g HyperSource HyperSink
    mapM_ print $ fst p
    print $ snd p

    putStrLn ""

    let
      fs = maximumFlow snd3 g HyperSource HyperSink
    sequence_
      [
        print (e, f)
      |
        (e, f) <- M.toList fs
      , f /= 0
      ]


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
example n = error $ "No example #" ++ show n ++ "."


main :: IO ()
main =
  do
    let
      tests = 10000
      isSuccess Success{} = True
      isSuccess _         = False
    results  <- mapM (quickCheckWithResult stdArgs {maxSuccess = tests})
      [
        label "read & show"    prop_readshow
      , label "shortest path"  prop_shortestpath
      , label "maximum flow"   prop_maxflow
      ]
    results' <- mapM (quickCheckWithResult stdArgs {maxSuccess = tests})
      [
        label "example #1" prop_example_1
      ]
    if all isSuccess $ results ++ results'
      then exitSuccess
      else exitFailure
