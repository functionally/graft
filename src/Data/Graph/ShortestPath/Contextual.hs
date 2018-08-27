{-# LANGUAGE TypeFamilies #-}


module Data.Graph.ShortestPath.Contextual (
  shortestPathTreeDijkstra
, shortestPath
, shortestPathBellmanFord
, shortestPathDijkstra
, measurePath
) where


import Control.Arrow (first)
import Control.Monad (foldM)
import Data.Foldable (toList)
import Data.Function (on)
import Data.List (minimumBy)
import Data.Graph.Types (Graph(..), MutableGraph(..), Path)
import Data.Graph.Types.Util (Tagged(..), TaggedGraph)
import Data.Graph.Types.Weight (HaltC, MeasureC)
import Data.Maybe (catMaybes, fromJust)
import Data.Monoid ((<>))
import Data.Heap (Heap)
import Debug.Trace (trace)

import qualified Data.Heap as H (Entry(..), insert, null, singleton, uncons)
import qualified Data.Map as M -- FIXME
import qualified Data.Set as S (findMin, lookupLE, member, notMember)


trace' :: (a -> String) -> a -> a
trace' = if True then (trace =<<) else const id


measurePath :: Monoid w
            => MeasureC c e w
            -> c
            -> Path v e
            -> Maybe (w, c)
measurePath measure context =
  foldM
    (\(weight', context') (_, _, edge) -> first (weight' <>) <$> measure context' edge)
    (mempty, context)


shortestPath :: (Show v, Show e, Show w)
             => (Graph g, v ~ VertexLabel g, e ~ EdgeLabel g)
             => (Ord v, Ord e, Ord w, Monoid w)
             => Bool
             -> MeasureC c e w
             -> g
             -> c
             -> v
             -> v
             -> (Path v e, w, c)
shortestPath True  = shortestPathBellmanFord
shortestPath False = shortestPathDijkstra


shortestPathDijkstra :: (Show v, Show e, Show w)
                     => (Graph g, v ~ VertexLabel g, e ~ EdgeLabel g)
                     => (Ord v, Ord e, Ord w, Monoid w)
                     => MeasureC c e w
                     -> g
                     -> c
                     -> v
                     -> v
                     -> (Path v e, w, c)
shortestPathDijkstra measure graph context start finish =
  let
    tree = shortestPathTreeDijkstra measure (const $ const . (== finish)) graph context start
    makePath [] = []
    makePath path@((to, _, _) : _) =
      let
        ((Tagged from _, _), edge) = S.findMin . edgesTo tree $ Tagged to undefined
        path' = (from, to, edge) : path
      in
        if from == start
          then path'
          else makePath path'
    finish' = Tagged finish undefined
  in
    if finish' `S.member` vertices tree
      then (\(x, (y, z)) -> (x, y, z)) (init $ makePath [(finish, undefined, undefined)], tag . fromJust $ finish' `S.lookupLE` vertices tree)
      else ([], mempty, context)


shortestPathTreeDijkstra :: (Show v, Show e, Show w)
                 => (Graph g, v ~ VertexLabel g, e ~ EdgeLabel g)
                 => (Ord v, Ord e, Ord w, Monoid w)
                 => MeasureC c e w
                 -> HaltC c v w
                 -> g
                 -> c
                 -> v
                 -> TaggedGraph v e (w, c)
shortestPathTreeDijkstra measure halt graph context start =
  let
    tree = mempty
    fringe =
      H.singleton
        . H.Entry mempty
        $ Tagged (fromJust $ vertexLabeled graph start) (mempty, context, id)
  in
    shortestPathTreeDijkstra' measure halt graph fringe tree


shortestPathTreeDijkstra' :: (Show v, Show e, Show w)
                          => (Graph g, v' ~ Vertex g, v ~ VertexLabel g, e ~ EdgeLabel g)
                          => (Ord v, Ord e, Ord w, Monoid w)
                          => MeasureC c e w
                          -> HaltC c v w
                          -> g
                          -> Heap (H.Entry w (Tagged v' (w, c, TaggedGraph v e (w, c) -> TaggedGraph v e (w, c))))
                          -> TaggedGraph v e (w, c)
                          -> TaggedGraph v e (w, c)
shortestPathTreeDijkstra' measure halt graph fringe tree
  | H.null fringe = tree
  | otherwise     =
      let
        Just (H.Entry distance (Tagged from (_, context, append)), fringe'') = H.uncons fringe
        from' = vertexLabel graph from
        tree' = append tree
        fringe' = 
          foldr H.insert
            fringe''
            $ catMaybes
            [
              do
                (distance', context') <- first (distance <>) <$> measure context edge'
                return
                  . H.Entry distance'
                  $ Tagged to
                  (
                    distance'
                  , context'
                  , addEdge (Tagged from' (distance, context)) (Tagged to' (distance', context')) edge'
                  )
            |
              edge <- toList $ edgesFrom graph from
            , let edge' = edgeLabel graph edge
                  to = vertexTo graph edge
                  to' = vertexLabel graph to
            , Tagged to' undefined `S.notMember` vertices tree
            ]
      in
        case (Tagged from' undefined `S.member` vertices tree, halt context from' distance) of
          (True, _   ) -> shortestPathTreeDijkstra' measure halt graph fringe'' tree 
          (_   , True) -> tree'
          _            -> shortestPathTreeDijkstra' measure halt graph fringe'  tree'


shortestPathBellmanFord :: (Show v, Show e, Show w)
                        => (Graph g, v ~ VertexLabel g, e ~ EdgeLabel g)
                        => (Ord v, Ord e, Ord w, Monoid w)
                        => MeasureC c e w
                        -> g
                        -> c
                        -> v
                        -> v
                        -> (Path v e, w, c)
shortestPathBellmanFord measure graph context start finish =
  let
    Just start' = vertexLabeled graph start
    initial = M.singleton start (mempty, ((undefined, start, undefined), start'))
    relax provisional =
      M.unionWith ((minimumBy (compare `on` fst) .) . (. return) . (:)) provisional
        . M.fromListWith ((minimumBy (compare `on` fst) .) . (. return) . (:))
        $ catMaybes
        [
          do
            distance' <- (distance <>) . fst <$> measure context edge
            return (to, (distance', ((from, to, edge), to')))
        |
          (from, (distance, (_, from'))) <- M.toList provisional
        , edge' <- toList $ edgesFrom graph from'
        , let edge = edgeLabel graph edge'
              to' = vertexTo graph edge'
              to = vertexLabel graph to'
        ]
    final = foldl (\previous i -> trace' (const $ "RELAX\t" ++ show i) $ relax previous) initial [1..length(toList $ vertices graph)]
    makePath path' (_, (segment@(from, _, _), _))
      | from == start = segment : path'
      | otherwise     = makePath (segment : path') (final M.! from)
    path = makePath [] $ final M.! finish
    Just (length', context') = measurePath measure context path
  in
    if M.member finish final
      then (path, length', context')
      else ([], mempty, context)
