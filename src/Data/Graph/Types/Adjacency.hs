{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}


module Data.Graph.Types.Adjacency (
  AdjacencyGraph
) where


import Data.Function (on)
import Data.Graph.Types (Adjacencies, Graph(..), MutableGraph(..))
import Data.Maybe (catMaybes)
import Data.Set (Set)

import qualified Data.Map.Strict as M ((!), adjust, delete, foldMapWithKey, insertWith, keys, keysSet, map, unionWith)
import qualified Data.Set as S (delete, filter, map, singleton, toList)


newtype AdjacencyGraph v e = Adjacencies {unadjacencies :: Adjacencies v e}
  deriving (Eq, Ord, Read, Show)

instance (Ord v, Ord e) => Monoid (AdjacencyGraph v e) where
  mempty = Adjacencies mempty
  mappend x y =
    Adjacencies
      $ on (M.unionWith mappend) unadjacencies x y

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

instance (Ord v, Ord e) => MutableGraph (AdjacencyGraph v e) where
  addVertex vertex =
    Adjacencies
      . M.insertWith mappend vertex mempty
      . unadjacencies
  addEdge from to edge =
    Adjacencies
      . M.insertWith mappend from (S.singleton (to, edge))
      . unadjacencies
  removeVertex vertex =
    Adjacencies
      . M.map (S.filter $ (/= vertex) . fst)
      . M.delete vertex
      . unadjacencies
  removeEdge from to edge =
    Adjacencies
      . M.adjust (S.delete (to, edge)) from
      . unadjacencies


findVertex :: Eq e => (v -> v -> v) -> AdjacencyGraph v e -> e -> v
findVertex f graph edge =
  head
    . catMaybes
    . M.foldMapWithKey (\from evs -> foldMap (\(to, edge') -> [if edge == edge' then Just $ f from to else Nothing]) evs)
    $ unadjacencies graph
