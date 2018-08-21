{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeFamilies     #-}


module Data.Graph.Types (
  Graph(..)
, MutableGraph(..)
, Adjacencies
, Path
, makeGraph
) where


import Data.Foldable (toList)
import Data.List (find)
import Data.Map.Strict (Map)
import Data.Set (Set)

import qualified Data.Map.Strict as M (fromListWith)
import qualified Data.Set as S (singleton)


class (Foldable (Vertices graph), Foldable (Edges graph)) => Graph graph where

  type VertexLabel graph :: *

  type Vertex graph :: *

  type Vertices graph :: * -> *

  type EdgeLabel graph :: *

  type Edge graph :: *

  type Edges graph :: * -> *

  vertices :: graph
           -> Vertices graph (Vertex graph)

  vertexLabel :: graph
              -> Vertex graph
              -> VertexLabel graph

  vertexLabels :: graph
               -> [VertexLabel graph]
  vertexLabels graph = vertexLabel graph <$> toList (vertices graph)

  vertexLabeled :: Eq (VertexLabel graph)
                => graph
                -> VertexLabel graph
                -> Maybe (Vertex graph)
  vertexLabeled graph label = find ((== label) . vertexLabel graph) $ vertices graph

  vertexFrom :: graph
             -> Edge graph
             -> Vertex graph

  vertexTo :: graph
           -> Edge graph
           -> Vertex graph

  edges :: graph
        -> Edges graph (Edge graph)

  edgeLabel :: graph
            -> Edge graph
            -> EdgeLabel graph

  edgeLabels :: graph
             -> [EdgeLabel graph]
  edgeLabels graph = edgeLabel graph <$> toList (edges graph)

  edgeLabeled :: Eq (EdgeLabel graph)
              => graph
              -> EdgeLabel graph
              -> Maybe (Edge graph)
  edgeLabeled graph label = find ((== label) . edgeLabel graph) $ edges graph

  edgesFrom :: graph
            -> Vertex graph
            -> Edges graph (Edge graph)

  edgesTo :: graph
          -> Vertex graph
          -> Edges graph (Edge graph)

  fromAdjacencies :: (Ord (VertexLabel graph), Ord (EdgeLabel graph))
                  => Adjacencies (VertexLabel graph) (EdgeLabel graph)
                  -> graph

  toAdjacencies :: (Ord (VertexLabel graph), Ord (EdgeLabel graph))
                => graph
                -> Adjacencies (VertexLabel graph) (EdgeLabel graph)


class Graph graph => MutableGraph graph where

  addVertex :: VertexLabel graph -> graph -> graph

  addEdge :: VertexLabel graph -> VertexLabel graph -> EdgeLabel graph -> graph -> graph

  removeVertex :: VertexLabel graph -> graph -> graph

  removeEdge :: VertexLabel graph -> VertexLabel graph -> EdgeLabel graph -> graph -> graph


makeGraph :: (Ord (VertexLabel graph), Ord (EdgeLabel graph), Graph graph) => [VertexLabel graph] -> [(VertexLabel graph, VertexLabel graph, EdgeLabel graph)] -> graph
makeGraph vs es =
  fromAdjacencies
    .  M.fromListWith mappend
    $  fmap (, mempty) vs
    ++ fmap (\(from, to, edge) -> (from, S.singleton (to, edge))) es


type Adjacencies v e = Map v (Set (v, e))


type Path g = [(VertexLabel g, VertexLabel g, EdgeLabel g)]
