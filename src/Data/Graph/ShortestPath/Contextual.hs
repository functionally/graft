{-# LANGUAGE TypeFamilies     #-}


module Data.Graph.ShortestPath.Contextual (
  shortestPathTree
, shortestPath
, measurePath
) where


import Control.Arrow (first)
import Control.Monad (foldM)
import Data.Foldable (toList)
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


trace' = (trace =<<)


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
shortestPath negativeWeights measure graph context start finish =
  let
    tree = shortestPathTree negativeWeights measure (const $ const . (== finish)) graph context start
    f [] = []
    f path@((to, _, _) : _) =
      let
        ((Tagged from _, _), edge) = S.findMin . edgesTo tree $ Tagged to undefined
        path' = (from, to, edge) : path
      in
        if from == start
          then path'
          else f path'
    finish' = Tagged finish undefined
  in
    if finish' `S.member` vertices tree
      then (\(x, (y, z)) -> (x, y, z)) (init $ f [(finish, undefined, undefined)], tag . fromJust $ finish' `S.lookupLE` vertices tree)
      else ([], mempty, context)


shortestPathTree :: (Show v, Show e, Show w)
                 => (Graph g, v ~ VertexLabel g, e ~ EdgeLabel g)
                 => (Ord v, Ord e, Ord w, Monoid w)
                 => Bool
                 -> MeasureC c e w
                 -> HaltC c v w
                 -> g
                 -> c
                 -> v
                 -> TaggedGraph v e (w, c)
shortestPathTree False = shortestPathTreeDijkstra
shortestPathTree True  = shortestPathTreeBellmanFord


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
        from' = trace' (\x -> "VISIT\t" ++ show x ++ "\t" ++ show distance) $ vertexLabel graph from
        tree' = append tree
        fringe' = 
          foldr H.insert
            fringe''
            $ catMaybes
            [
              do
                (distance', context') <- first (distance <>) <$> measure context edge'
                return
                  . H.Entry (trace' (const $ "PUSH\t" ++ show to' ++ "\t" ++ show distance') distance')
                  $ Tagged to
                  (
                    distance'
                  , context'
                  , trace' (const $ "ADD\t" ++ show from' ++ "\t" ++ show to' ++ "\t" ++ show distance ++ "\t" ++ show distance') $ addEdge (Tagged from' (distance, context)) (Tagged to' (distance', context')) edge'
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


shortestPathTreeBellmanFord :: (Show v, Show e, Show w)
                            => (Graph g, v ~ VertexLabel g, e ~ EdgeLabel g)
                            => (Ord v, Ord e, Ord w, Monoid w)
                            => MeasureC c e w
                            -> HaltC c v w
                            -> g
                            -> c
                            -> v
                            -> TaggedGraph v e (w, c)
shortestPathTreeBellmanFord measure _ graph context start =
  let
    initial = M.singleton start Nothing
    relax provisional =
      foldr
        (M.insertWith $ \old 
    revise provisional =
      M.alter
        
      foldr H.insert
        fringe'
        $ catMaybes
        [
          do
            M.lookup
            (distance', _) <- first (distance <>) <$> measure context edge'
            return
              . H.Entry distance'
              $ Tagged to
              (
                distance'
              , context'
              , addEdge (Tagged from' (distance, context)) (Tagged to' (distance', context')) edge'
              )
        |
          edge <- toList $ edges graph
        , let edge' = edgeLabel graph edge
              from = vertexFrom graph edge
              from' = vertexLabel graph from
              to = vertexTo graph edge
              to' = vertexLabel graph to
        ]

    initial = M.singleton start $ Just (mempty, undefined)
      M.insert start (Just (mempty, undefined))
        $ foldMap (`M.singleton` Nothing)
        $ vertexLabels graph
    relax provisional =
      let
M.adjust (\old ->
  if
  in
    undefined
