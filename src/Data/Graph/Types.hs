{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}


module Data.Graph.Types (
  Graph(..)
, MutableGraph(..)
, Adjacencies
, Path
) where


import Data.Map.Strict (Map)
import Data.Set (Set)


class (Foldable (Vertices graph), Foldable (Edges graph)) => Graph graph where

  type VertexLabel graph :: *

  type Vertex graph :: *

  type Vertices graph :: * -> *

  type EdgeLabel graph :: *

  type Edge graph :: *

  type Edges graph :: * -> *

  vertices :: graph
           -> Vertices graph (Vertex graph)

  edges :: graph
        -> Edges graph (Edge graph)

  vertexLabel :: Vertex graph
              -> VertexLabel graph

  vertexLabels :: graph
               -> [VertexLabel graph]

  edgesFrom :: graph
            -> Vertex graph
            -> Edges graph (Edge graph)

  edgesTo :: graph
          -> Vertex graph
          -> Edges graph (Edge graph)

  edgeLabel :: Edge graph
            -> EdgeLabel graph

  edgeLabels :: graph
             -> [EdgeLabel graph]

  vertexFrom :: graph
             -> Edge graph
             -> Vertex graph

  vertexTo :: graph
           -> Edge graph
           -> Vertex graph

  fromAdjacencies :: (Ord (VertexLabel graph), Ord (EdgeLabel graph))
                  => Adjacencies (VertexLabel graph) (EdgeLabel graph)
                  -> graph

  toAdjacencies :: (Ord (VertexLabel graph), Ord (EdgeLabel graph))
                => graph
                -> Adjacencies (VertexLabel graph) (EdgeLabel graph)


class Graph graph => MutableGraph graph where

  addVertex :: graph -> Vertex graph -> graph

  addEdge :: graph -> Vertex graph -> Vertex graph -> Edge graph -> graph

  removeVertex :: graph -> Vertex graph -> graph

  removeEdge :: graph -> Vertex graph -> Vertex graph -> Edge graph -> graph


type Adjacencies v e = Map v (Set (v, e))


type Path graph = [Edge graph]
