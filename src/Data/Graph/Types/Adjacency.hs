{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}


module Data.Graph.Types.Adjacency (
  AdjacencyGraph
) where


import Data.Graph.Types (Adjacencies, Graph(..), MutableGraph(..))
import Data.Maybe (catMaybes)
import Data.Set (Set)

import qualified Data.Map.Strict as M ((!), foldMapWithKey, insert, keys, keysSet)
import qualified Data.Set as S (filter, map, toList)


newtype AdjacencyGraph v e = Adjacencies {unadjacencies :: Adjacencies v e}


instance (Ord v, Ord e) => Graph (AdjacencyGraph v e) where
  type VertexLabel (AdjacencyGraph v e) = v
  type Vertex (AdjacencyGraph v e) = v
  type Vertices (AdjacencyGraph v e) = Set
  type EdgeLabel (AdjacencyGraph v e) = e
  type Edge (AdjacencyGraph v e) = e
  type Edges (AdjacencyGraph v e) = Set
  vertices = M.keysSet . unadjacencies
  edges = foldMap (S.map snd) . unadjacencies
  vertexLabel = id
  vertexLabels = M.keys . unadjacencies
  edgesFrom = (S.map snd .) . (M.!) . unadjacencies
  edgesTo graph to = foldMap (S.map snd . S.filter ((== to) . fst)) $ unadjacencies graph
  edgeLabel = id
  edgeLabels = foldMap (fmap snd . S.toList) . unadjacencies
  vertexFrom = findVertex const
  vertexTo = findVertex $ const id
  fromAdjacencies = Adjacencies
  toAdjacencies = unadjacencies


findVertex :: Eq e => (v -> v -> v) -> AdjacencyGraph v e -> e -> v
findVertex f graph edge =
  head
    . catMaybes
    . M.foldMapWithKey (\from evs -> foldMap (\(to, edge') -> [if edge == edge' then Just $ f from to else Nothing]) evs)
    $ unadjacencies graph


instance (Ord v, Ord e) => MutableGraph (AdjacencyGraph v e) where
  addVertex graph vertex = Adjacencies $ M.insertWith vertex mempty $ unadjacencies graph 
  addEdge graph from to edge = 
