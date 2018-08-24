{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}


module Main (
  main
, example
) where


import Data.List (sort)
import Data.Graph.IO (toGraphviz, toLines)
import Data.Graph.MaximumFlow (maximumFlow)
import Data.Graph.ShortestPath (shortestPath)
import Data.Graph.Types (Graph(..), EdgeList)
import Data.Graph.Types.MapGraph (MapGraph)
import Data.Graph.Types.Util (HyperVertex(..))
import Data.Monoid (Sum(..))
import Data.Tuple.Util (snd3)

import qualified Data.Map.Strict as M (toList)

import Test.QuickCheck
import qualified Data.Graph.Inductive.Graph as F
import qualified Data.Graph.Inductive.Arbitrary as F
import qualified Data.Graph.Inductive.PatriciaTree as F (Gr)
import qualified Data.Graph.Inductive.Query.SP as F

import Debug.Trace (trace)


trace' :: (a -> String) -> a -> a
trace' = if True then (trace =<<) else const id


prop_sample :: F.SimpleGraph F.Gr () Double -> Bool
prop_sample g =
  let
    vs = fst <$> F.labNodes g
    g' :: MapGraph Int (Int, Double)
    g' =
      fromEdgeList
        vs
        [
          (from, to, (i, weight))
        |
          ((from, to, weight), i) <- zip (F.labEdges g) [1..]
        ]
    start : finish : _ = vs
    p = F.sp start finish g
    p' = shortestPath (Sum . snd) g' start finish
  in
       length vs < 3
    || p == Nothing
    || (trace' (const $ show (p, p')) $ g' == read (show g'))


main :: IO ()
main =
  do

    let
      buildExample = (fromEdgeList =<< (snd3 <$>)) . example

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

    quickCheck prop_sample


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
