{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}


module Data.Graph.Types.MapGraph (
  MapGraph
, makeMapGraph
, mapGraph
, mapVertices
, mapEdges
) where


import Control.Arrow ((***), first)
import Control.Monad (ap)
import Data.Function (on)
import Data.Graph.Types (Graph(..), MutableGraph(..))
import Data.Map.Strict (Map)
import Data.Set (Set)

import qualified Data.Map.Strict as M ((!), adjust, delete, foldMapWithKey, insert, insertWith, map, mapKeys, unionWith)
import qualified Data.Set as S (delete, filter, insert, map, singleton, toList)


data MapGraph v e =
  MapGraph
  {
    allVertices   :: Set v
  , allEdges      :: Set ((v, v), e)
  , incomingEdges :: Map v (Set ((v, v), e))
  , outgoingEdges :: Map v (Set ((v, v), e))
  }
    deriving (Eq, Ord, Read, Show)

instance (Ord v, Ord e) => Monoid (MapGraph v e) where
  mempty =
    MapGraph
    {
      allVertices   = mempty
    , allEdges      = mempty
    , incomingEdges = mempty
    , outgoingEdges = mempty
    }
  mappend x y =
    MapGraph
    {
      allVertices   = on mappend allVertices x y
    , allEdges      = on mappend allEdges x y
    , incomingEdges = on (M.unionWith mappend) incomingEdges x y
    , outgoingEdges = on (M.unionWith mappend) outgoingEdges x y
    }

instance (Ord v, Ord e) => Graph (MapGraph v e) where
  type Vertex (MapGraph v e) = v
  type VertexLabel (MapGraph v e) = v
  type Vertices (MapGraph v e) = Set
  type Edge (MapGraph v e) = ((v, v), e)
  type EdgeLabel (MapGraph v e) = e
  type Edges (MapGraph v e) = Set
  vertices = allVertices
  edges = allEdges
  vertexLabel = id
  vertexLabels = S.toList . allVertices
  edgesFrom MapGraph{..} = (outgoingEdges M.!)
  edgesTo MapGraph{..} = (incomingEdges M.!)
  edgeLabel = snd
  edgeLabels = fmap snd . S.toList . allEdges
  vertexFrom = const $ fst . fst
  vertexTo = const $ snd . fst
  fromAdjacencies = M.foldMapWithKey $ \from -> foldl (\graph (to, label) -> addEdge' graph ((from, to), label)) mempty
  toAdjacencies MapGraph{..} = M.map (S.map $ first fst) outgoingEdges

instance (Ord v, Ord e) => MutableGraph (MapGraph v e) where
  addVertex vertex MapGraph{..} =
    MapGraph
      (S.insert vertex allVertices)
      allEdges
      incomingEdges
      outgoingEdges
  addEdge from to edge MapGraph{..} =
    MapGraph
      (S.insert from $ S.insert to allVertices)
      (S.insert ((from, to), undefined) allEdges)
      (M.insertWith mappend to   (S.singleton ((from, to), edge)) incomingEdges)
      (M.insertWith mappend from (S.singleton ((from, to), edge)) outgoingEdges)
  removeVertex vertex MapGraph{..} =
    MapGraph
      (S.delete vertex allVertices)
      (S.filter match allEdges)
      (clean incomingEdges)
      (clean outgoingEdges)
      where
        clean = M.map (S.filter match) . M.delete vertex
        match ((from, to), _) = vertex /= from && vertex /= to
  removeEdge from to edge MapGraph{..} =
    MapGraph
      allVertices
      (S.delete edge' allEdges)
      (M.adjust (S.delete edge') to   incomingEdges)
      (M.adjust (S.delete edge') from outgoingEdges)
      where
        edge' = ((from, to), edge)


addEdge' :: (Ord v, Ord e) => MapGraph v e -> ((v, v), e) -> MapGraph v e
addEdge' MapGraph{..} edge@((from, to), _) =
  MapGraph
  {
    allVertices   = S.insert from $ S.insert to allVertices
  , allEdges      = S.insert edge allEdges
  , incomingEdges = M.insertWith mappend to   (S.singleton edge) incomingEdges
  , outgoingEdges = M.insertWith mappend from (S.singleton edge) outgoingEdges
  }


makeMapGraph :: (Ord v, Ord e) => [((v, v), e)] -> MapGraph v e
makeMapGraph = foldl addEdge' mempty


mapGraph :: (Ord v, Ord v', Ord e, Ord e') => (v -> v') -> (e -> e') -> MapGraph v e -> MapGraph v' e'
mapGraph f g MapGraph{..} =
  let
    vertexMap = foldr (ap M.insert f) mempty allVertices
    edgeMap   = foldr (ap M.insert $ (f *** f) *** g) mempty allEdges
    mapVertex = (M.!) vertexMap
    mapEdge   = (M.!) edgeMap
  in
    MapGraph
    {
      allVertices   = foldr S.insert mempty vertexMap
    , allEdges      = foldr S.insert mempty edgeMap
    , incomingEdges = M.map (S.map mapEdge) $ M.mapKeys mapVertex incomingEdges
    , outgoingEdges = M.map (S.map mapEdge) $ M.mapKeys mapVertex outgoingEdges
    }


mapVertices :: (Ord v, Ord v', Ord e) => (v -> v') -> MapGraph v e -> MapGraph v' e
mapVertices = flip mapGraph id


mapEdges :: (Ord v, Ord e, Ord e') => (e -> e') -> MapGraph v e -> MapGraph v e'
mapEdges = mapGraph id
