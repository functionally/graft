{-# LANGUAGE RecordWildCards #-}


module Data.Graph.Algorithms (
  bareCapacityCostFlow
, minimumCostFlow
, shortestPath
, shortestPathTree
, measurePath
) where


import Control.Arrow (first)
import Control.Monad (foldM, guard)
import Data.Function (on)
import Data.Graph.Types (Flows, Graph(..), Measure, MeasureCapacity, MeasureCost, Capacity(..), Path, SetFlow, TaggedGraph, TaggedItem(..), Valid, addEdge, getCapacity)
import Data.List (minimumBy)
import Data.Maybe (catMaybes, fromJust)
import Data.Monoid (Sum(..), (<>))
import Data.Heap (Heap)
import Data.Tuple.Util (trd4)

import qualified Data.Heap as H (Entry(..), insert, null, singleton, uncons)
import qualified Data.Map.Strict as M ((!), empty, insert, lookup, mapWithKey)
import qualified Data.Set as S (findMin, lookupLE, member, notMember, toList)
import qualified Debug.Trace as T (trace)


trace :: String -> a -> a
trace = if True then T.trace else const id


bareCapacityCostFlow :: (Show v, Show e, Show w, Show w')
                     => (Ord v, Ord e, Num w, Ord w, Num w', Ord w')
                     => (e -> w)
                     -> (e -> w')
                     -> Graph v e
                     -> v
                     -> v
                     -> Flows e w'
bareCapacityCostFlow cost capacity graph start finish =
  let
    cost' _ context edge =
      do
        let
          weight = cost edge
        guard
          $ context M.! edge > 0
        return (Sum weight, context)
    capacity' context edge =
      do
        guard
          $ context M.! edge > 0
        return (Capacity $ context M.! edge, context)
    set' weight context edge =
      let
        remaining = context M.! edge
        remaining' = remaining - getCapacity weight
      in
        M.insert edge remaining' context
  in
    M.mapWithKey (\edge weight -> capacity edge - weight)
      $ minimumCostFlow (const True) cost' (< Capacity 0) capacity' set' graph
        (foldr (\edge -> M.insert edge $ capacity edge) M.empty $ allEdges graph)
        start
        finish


minimumCostFlow :: (Show v, Show e, Show cost, Show flow)
                => (Ord v, Ord e, Ord cost, Monoid cost, Ord flow, Monoid flow)
                => Valid cost
                -> MeasureCost c e flow cost
                -> Valid flow
                -> MeasureCapacity c e flow
                -> SetFlow c e flow
                -> Graph v e
                -> c
                -> v
                -> v
                -> c
minimumCostFlow validateCost cost validateCapacity capacity set graph context start finish =
  let
    
    next v c (_, f, _, x) = 
      let
        (p', x')     = (\z -> trace ("GRAFT\tminimumCostFlow\tSHORTEST\t" ++ show (fst     z)) z) $ shortestPath v (c f) graph x start finish
        Just (f', _) = (\z -> trace ("GRAFT\tminimumCostFlow\tFLOW\t"     ++ show (fst <$> z)) z) $ measurePath capacity x' p'
        Just (c', _) = (\z -> trace ("GRAFT\tminimumCostFlow\tCOST\t"     ++ show (fst <$> z)) z) $ measurePath (cost f') x' p'
      in
        (p', f', c', x')

    pfc@(path, _, _, _) = trace "GRAFT\tminimumCostFlow\tNEXT" $ next validateCapacity (const capacity) (undefined, undefined, undefined, trace "GRAFT\tminimumCostFlow\tFIRST" context)
    pfcs = iterate (next validateCost cost) pfc
    (path', flow', _, _) = (\z@(z0, z1, _, _) -> trace ("GRAFT\tminimumCostFlow\tFLOWPATH\t" ++ show z1 ++ "\t" ++ show z0) z) $
      case (True, 1) of
        (True , n) -> pfcs !! n
        (False, n) -> minimumBy (compare `on` trd4) . tail $ take n pfcs

    context' = setFlow set flow' context path'

  in
    if null path
      then context
      else minimumCostFlow validateCost cost validateCapacity capacity set graph context' start finish


setFlow :: Monoid w
        => SetFlow c e w
        -> w
        -> c
        -> Path v e
        -> c
setFlow measure flow = foldl (\context' (_, _, edge) -> measure flow context' edge)


measurePath :: Monoid w
            => Measure c e w
            -> c
            -> Path v e
            -> Maybe (w, c)
measurePath measure context =
  foldM
    (\(weight', context') (_, _, edge) -> first (weight' <>) <$> measure context' edge)
    (mempty, context)


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


shortestPathTree' :: (Show w, Show v, Show e)
                  => (Monoid w, Ord w, Ord v, Ord e)
                  => Valid w
                  -> Measure c e w
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
                trace ("GRAFT\tshortestPathTree'\t" ++ show edge ++ "\t" ++ show to ++ "\t" ++ show distance')
                  . guard
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
