{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}


module Data.Graph (
  Graph(..)
, Path
, LabeledPayload(..)
) where


import Data.Function (on)
import Data.Map.Strict (Map)
import Data.Set (Set)


class Graph graph where

  type VertexLabel graph :: *

  type Vertex graph :: *

  type EdgeLabel graph :: *

  type Edge graph :: *

  vertices :: graph
           -> [Vertex graph]

  edges ::graph
        -> [Edge graph]

  vertexLabel :: Vertex graph
              -> VertexLabel graph

  edgesFrom :: graph
            -> Vertex graph
            -> [Edge graph]

  edgesTo :: graph
          -> Vertex graph
          -> [Edge graph]

  edgeLabel :: Edge graph
            -> EdgeLabel graph

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


type Path graph = [Edge graph]


type Adjacencies v e = Map v (Set (v, e))


data LabeledPayload l p =
  LabeledPayload
  {
    label   :: l
  , payload :: p
  }
    deriving (Read, Show)

instance Eq l => Eq (LabeledPayload l p) where
  (==) = (==) `on` label

instance Ord l => Ord (LabeledPayload l p) where
  compare = compare `on` label
