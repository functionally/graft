{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}


module Data.Graph.Types.TiedGraph (
  TiedGraph
, TiedVertex
, TiedEdge

) where


import Control.Arrow ((&&&))
import Data.Function (on)
import Data.Function.MapReduce (mapReduce)
import Data.Graph.Types (Adjacencies, Graph(..))
import Data.Map.Strict (Map)
import Data.Set (Set)

import qualified Data.Map.Strict as M ((!), fromList, fromSet, mapKeys, mapWithKey, toList, unionWith)
import qualified Data.Set as S (fromList, insert, map, toList)


data TiedGraph v e =
  TiedGraph
  {
    tiedVertices :: Set (TiedVertex v e)
  , tiedEdges    :: Set (TiedEdge v e)
  }
    deriving (Eq, Ord)

instance (Ord v, Ord e) => Monoid (TiedGraph v e) where
  mempty = TiedGraph mempty mempty
  mappend = (tieGraph .) . (. untieGraph) . M.unionWith mappend . untieGraph

instance Graph (TiedGraph v e) where
  type VertexLabel (TiedGraph v e) = v
  type Vertex (TiedGraph v e) = TiedVertex v e
  type Vertices (TiedGraph v e) = []
  type EdgeLabel (TiedGraph v e) = e
  type Edge (TiedGraph v e) = TiedEdge v e
  type Edges (TiedGraph v e) = []
  vertices = S.toList . tiedVertices
  edges = S.toList . tiedEdges
  vertexLabel = tiedVertexLabel
  vertexLabels = fmap tiedVertexLabel . vertices
  edgesFrom = const tiedEdgesFrom
  edgesTo = const tiedEdgesTo
  edgeLabel = tiedEdgeLabel
  edgeLabels = fmap tiedEdgeLabel . edges
  vertexFrom = const tiedVertexFrom
  vertexTo = const tiedVertexTo
  fromAdjacencies = tieGraph
  toAdjacencies = untieGraph


data TiedVertex v e =
  TiedVertex
  {
    tiedVertexLabel :: v
  , tiedEdgesFrom   :: [TiedEdge v e]
  , tiedEdgesTo     :: [TiedEdge v e]
  }

instance Eq v => Eq (TiedVertex v e) where
  (==) = (==) `on` tiedVertexLabel

instance Ord v => Ord (TiedVertex v e) where
  compare = compare `on` tiedVertexLabel


data TiedEdge v e =
  TiedEdge
  {
    tiedEdgeLabel  :: e
  , tiedVertexFrom :: TiedVertex v e
  , tiedVertexTo   :: TiedVertex v e
  }

instance Eq e => Eq (TiedEdge v e) where
  (==) = (==) `on` tiedEdgeLabel

instance Ord e => Ord (TiedEdge v e) where
  compare = compare `on` tiedEdgeLabel


tieGraph :: forall v e. (Ord v, Ord e) => Adjacencies v e -> TiedGraph v e
tieGraph adjacencies =
  let
    vertexTies :: Map v (TiedVertex v e)
    vertexTies =
      M.mapWithKey
        (
          \tiedVertexLabel _ ->
            let
              tiedEdgesFrom = S.toList . S.map snd $ outgoingEdgeTies M.! tiedVertexLabel
              tiedEdgesTo   = S.toList . S.map snd $ incomingEdgeTies M.! tiedVertexLabel
            in
              TiedVertex{..}
        )
          adjacencies
    outgoingEdgeTies :: Map v (Set (v, TiedEdge v e))
    outgoingEdgeTies =
      M.mapWithKey
        (
          \from ->
            S.map
              (
                \(to, tiedEdgeLabel) ->
                  let
                    tiedVertexFrom = vertexTies M.! from
                    tiedVertexTo   = vertexTies M.! to
                  in
                    (to, TiedEdge{..})
              )
        )
        adjacencies
    rev (x, (y, z)) = (y, (x, z))
    incomingEdgeTies :: Map v (Set (v, TiedEdge v e))
    incomingEdgeTies =
      M.fromList
        . mapReduce
          id
          ((. S.fromList) . (,))
        . fmap rev
        . concatMap (uncurry $ (. S.toList) . fmap . (,))
        $ M.toList outgoingEdgeTies
  in
    TiedGraph
    {
      tiedVertices = foldr S.insert              mempty vertexTies
    , tiedEdges    = foldr (mappend . S.map snd) mempty outgoingEdgeTies
    }


untieGraph :: forall v e. (Ord v, Ord e) => TiedGraph v e -> Map v (Set (v, e))
untieGraph =
  M.mapKeys
    tiedVertexLabel
  . M.fromSet
    (
      S.fromList
        . fmap ((tiedVertexLabel . tiedVertexFrom) &&& tiedEdgeLabel)
        . tiedEdgesFrom
    )
    . tiedVertices
