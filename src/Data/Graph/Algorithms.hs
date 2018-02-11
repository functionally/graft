{-# LANGUAGE RecordWildCards #-}


module Data.Graph.Algorithms (
  bareMinimumCostFlow
, minimumCostFlow
, shortestPath
, shortestPathTree
, measurePath
) where


import Control.Arrow (first)
import Control.Monad (foldM, guard)
import Data.Graph.Types (Flows, Graph(..), Measurer, Minimum(..), Path, TaggedGraph, TaggedItem(..), addEdge, getMinimum)
import Data.Maybe (catMaybes, fromJust)
import Data.Monoid (Sum(..), (<>))
import Data.Heap (Heap)

import qualified Data.Heap as H (Entry(..), filter, insert, minimum, null, singleton)
import qualified Data.Map.Strict as M ((!), empty, insert, lookup, mapWithKey)
import qualified Data.Set as S (findMin, lookupLE, member, notMember, toList)



bareMinimumCostFlow :: (Ord v, Ord e, Num w, Ord w, Num w', Ord w', Show e, Show w, Show w', Show v)
                    => Measurer () e w
                    -> Measurer () e w'
                    -> Graph v e
                    -> v
                    -> v
                    -> Flows e w'
bareMinimumCostFlow cost capacity graph start finish =
  let
    cost' context edge =
      do
        (weight, ()) <- cost () edge
        guard
          $ context M.! edge > 0
        return (Sum weight, context)
    capacity' weight context edge =
      let
        remaining = context M.! edge
      in
        case weight of
          Nothing       -> Just (Minimum remaining, context)
          Just weight'' -> let
                               remaining' = remaining - getMinimum weight''
                             in
                               if remaining <= 0
                                 then Nothing
                                 else Just (Minimum remaining', M.insert edge remaining' context)
  in
    M.mapWithKey (\edge weight -> fst (fromJust $ capacity () edge) - weight)
      $ minimumCostFlow cost' capacity' graph
        (foldr (\edge -> M.insert edge . fst . fromJust $ capacity () edge) M.empty $ allEdges graph)
        start
        finish


minimumCostFlow :: (Ord v, Ord e, Ord w, Monoid w, Monoid w', Show v, Show e, Show w, Show w', Show c)
                => Measurer c e w
                -> (Maybe w' -> Measurer c e w')
                -> Graph v e
                -> c
                -> v
                -> v
                -> c
minimumCostFlow cost capacity graph context start finish =
  let
    (path, _) = shortestPath cost graph context start finish
    context' = remeasurePath capacity context path
  in
    if null path
      then context
      else minimumCostFlow cost capacity graph context' start finish


remeasurePath :: Monoid w => Show w
              => (Maybe w -> Measurer c e w)
              -> c
              -> Path v e
              -> c
remeasurePath measure context path =
  maybe context snd
    $ do
      (weight, _) <- measurePath (measure Nothing) context path
      measurePath (measure $ Just weight)  context path


measurePath :: Monoid w
            => Measurer c e w
            -> c
            -> Path v e
            -> Maybe (w, c)
measurePath measure context =
  foldM
    (\(weight', context') (_, _, edge) -> first (weight' <>) <$> measure context' edge)
    (mempty, context)


shortestPath :: (Monoid w, Ord w, Ord v, Ord e)
             => Measurer c e w
             -> Graph v e
             -> c
             -> v
             -> v
             -> (Path v e, c)
shortestPath measure graph context start finish =
  let
    Graph{..} = shortestPathTree measure (const $ const . (== finish)) graph context start
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


shortestPathTree :: (Monoid w, Ord w, Ord v, Ord e)
                 => Measurer c e w
                 -> (c -> v -> w -> Bool)
                 -> Graph v e
                 -> c
                 -> v
                 -> TaggedGraph v e (w, c)
shortestPathTree measure halt graph context start =
  let
    tree = mempty
    fringe = H.singleton . H.Entry mempty $ TaggedItem start (mempty, context, id)
  in
    shortestPathTree' measure halt graph fringe tree


shortestPathTree' :: (Monoid w, Ord w, Ord v, Ord e)
                  => Measurer c e w
                  -> (c -> v -> w -> Bool)
                  -> Graph v e
                  -> Heap (H.Entry w (TaggedItem v (w, c, TaggedGraph v e (w, c) -> TaggedGraph v e (w, c))))
                  -> TaggedGraph v e (w, c)
                  -> TaggedGraph v e (w, c)
shortestPathTree' measure halt graph fringe tree
  | H.null fringe = tree
  | otherwise     =
      let
        H.Entry distance from'@(TaggedItem from (_, context, append)) = H.minimum fringe
        tree' = append tree
        fringe' =
          foldr H.insert
            (H.filter ((/= from') . H.payload) fringe)
            $ catMaybes
            [
              do
                (distance', context') <- first (distance <>) <$> measure context edge
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
        if halt context from distance
          then tree'
          else shortestPathTree' measure halt graph fringe' tree'
