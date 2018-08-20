{-# LANGUAGE RecordWildCards #-}


module Data.Graph.ShortestPath
where


import Control.Arrow (first)
import Control.Monad (foldM, guard)
import Data.Function (on)
import Data.Graph.Legacy.Types (Flows, Graph(..), Measure, MeasureCapacity, MeasureCost, Capacity(..), Path, SetFlow, TaggedGraph, TaggedItem(..), Valid, addEdge, getCapacity)
import Data.List (minimumBy)
import Data.Maybe (catMaybes, fromJust)
import Data.Monoid (Sum(..), (<>))
import Data.Heap (Heap)
import Data.Tuple.Util (trd4)

import qualified Data.Heap as H (Entry(..), insert, null, singleton, uncons)
import qualified Data.Map.Strict as M ((!), empty, insert, lookup, mapWithKey)
import qualified Data.Set as S (findMin, lookupLE, member, notMember, toList)
import qualified Debug.Trace as T (trace)


shortestPath :: (Show w, Show v, Show e)
             => (Monoid w, Ord w, Ord v, Ord e)
             => Valid w
             -> Measure c e w
             -> Graph v e
             -> c
             -> v
             -> v
             -> (Path v e, c)
shortestPath validate measure graph context start finish =
  let
    Graph{..} = shortestPathTree validate measure (const $ const . (== finish)) graph context start
    f [] = []
    f path@((to, _, _) : _) =
      let
        (TaggedItem from _, edge) = S.findMin $ incomingEdges M.! TaggedItem to undefined
        path' = (from, to, edge) : path
      in
        if from == start
          then path'
          else f path'
    finish' = TaggedItem finish undefined
  in
    if finish' `S.member` allVertices
      then (init $ f [(finish, undefined, undefined)], snd . tag . fromJust $ finish' `S.lookupLE` allVertices)
      else ([], context)


shortestPathTree :: (Show w, Show v, Show e)
                 => (Monoid w, Ord w, Ord v, Ord e)
                 => Valid w
                 -> Measure c e w
                 -> (c -> v -> w -> Bool)
                 -> Graph v e
                 -> c
                 -> v
                 -> TaggedGraph v e (w, c)
shortestPathTree validate measure halt graph context start =
  let
    tree = mempty
    fringe = H.singleton . H.Entry mempty $ TaggedItem start (mempty, context, id)
  in
    shortestPathTree' validate measure halt graph fringe tree


shortestPathTree' :: (Monoid w, Ord w, Ord v, Ord e)
                  => Measure c e w
                  -> (c -> v -> w -> Bool)
                  -> Graph v e
                  -> Heap (H.Entry w (TaggedItem v (w, c, TaggedGraph v e (w, c) -> TaggedGraph v e (w, c))))
                  -> TaggedGraph v e (w, c)
                  -> TaggedGraph v e (w, c)
shortestPathTree' validate measure halt graph fringe tree
  | H.null fringe = tree
  | otherwise     =
      let
        Just (H.Entry distance (TaggedItem from (_, context, append)), fringe'') = H.uncons fringe
        tree' = append tree
        fringe' = 
          foldr H.insert
            fringe''
            $ catMaybes
            [
              do
                (distance', context') <- first (distance <>) <$> measure context edge
                guard
                  $ validate distance' 
                return
                  . H.Entry distance'
                  $ TaggedItem to
                  (
                    distance'
                  , context'
                  , (`addEdge` (TaggedItem from (distance, context), TaggedItem to (distance', context'), edge))
                  )
            |
              (to, edge) <- maybe [] S.toList $ from `M.lookup` outgoingEdges graph
            , TaggedItem to undefined `S.notMember` allVertices tree
            ]
      in
        case (TaggedItem from undefined `S.member` allVertices tree, halt context from distance) of
          (True, _   ) -> shortestPathTree' validate measure halt graph fringe'' tree 
          (_   , True) -> tree'
          _            -> shortestPathTree' validate measure halt graph fringe'  tree'
