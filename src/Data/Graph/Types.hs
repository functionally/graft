{-# LANGUAGE RecordWildCards #-}


module Data.Graph.Types (
  Graph(..)
, bimap
, mapVertices
, mapEdges
, TaggedItem(..)
, TaggedGraph
, TaggedEdge
, Vertex(..)
, Edge(..)
, addEdge
, makeGraph
, Measure
, MeasureCost
, MeasureCapacity
, SetFlow
, Path
, Capacity(..)
, hasCapacity
, getCapacity
, Flows
) where


import Control.Arrow ((***))
import Control.Monad (ap)
import Data.Function (on)
import Data.Map.Strict (Map)
import Data.Set (Set)

import qualified Data.Map.Strict as M ((!), empty, insert, insertWith, map, mapKeys, unionWith)
import qualified Data.Set as S (empty, insert, map, singleton, union)


data Graph v e =
  Graph
  {
    allVertices   :: Set v
  , allEdges      :: Set e
  , incomingEdges :: Map v (Set (v, e))
  , outgoingEdges :: Map v (Set (v, e))
  }
    deriving (Eq, Ord, Read, Show)

instance (Ord v, Ord e) => Monoid (Graph v e) where
  mempty =
    Graph
    {
      allVertices   = S.empty
    , allEdges      = S.empty
    , incomingEdges = M.empty
    , outgoingEdges = M.empty
    }
  mappend x y =
    Graph
    {
      allVertices   = on S.union allVertices x y
    , allEdges      = on S.union allEdges x y
    , incomingEdges = on (M.unionWith S.union) incomingEdges x y
    , outgoingEdges = on (M.unionWith S.union) outgoingEdges x y
    }


bimap :: (Ord v, Ord v', Ord e, Ord e') => (v -> v') -> (e -> e') -> Graph v e -> Graph v' e'
bimap f g Graph{..} =
  let
    vertexMap = foldr (ap M.insert f) M.empty allVertices
    edgeMap   = foldr (ap M.insert g) M.empty allEdges
    mapVertex = (M.!) vertexMap
    mapEdge   = (M.!) edgeMap
  in
    Graph
    {
      allVertices   = foldr S.insert S.empty vertexMap
    , allEdges      = foldr S.insert S.empty edgeMap
    , incomingEdges = M.map (S.map $ mapVertex *** mapEdge) $ M.mapKeys mapVertex incomingEdges
    , outgoingEdges = M.map (S.map $ mapVertex *** mapEdge) $ M.mapKeys mapVertex outgoingEdges
    }


mapVertices :: (Ord v, Ord v', Ord e) => (v -> v') -> Graph v e -> Graph v' e
mapVertices = flip bimap id


mapEdges :: (Ord v, Ord e, Ord e') => (e -> e') -> Graph v e -> Graph v e'
mapEdges = bimap id


data TaggedItem a b =
  TaggedItem
  {
    item :: a
  , tag  :: b
  }
    deriving (Read, Show)

instance Eq a => Eq (TaggedItem a b) where
  TaggedItem x _ == TaggedItem y _ = x == y

instance Ord a => Ord (TaggedItem a b) where
  TaggedItem x _ `compare` TaggedItem y _ = x `compare` y


type TaggedGraph v e t = Graph (TaggedItem v t) e


type TaggedEdge v e t = (TaggedItem v t, TaggedItem v t, e)


data Vertex a =
    SuperSource
  | SuperSink
  | Vertex a
    deriving (Eq, Ord, Read, Show)


data Edge a =
    Edge a
  | ReversedEdge a
    deriving (Eq, Ord, Read, Show)


addEdge :: (Ord v, Ord e) => Graph v e -> (v, v, e) -> Graph v e
addEdge Graph{..} (from, to, edge) =
  Graph
  {
    allVertices   = S.insert from $ S.insert to allVertices
  , allEdges      = S.insert edge allEdges
  , incomingEdges = M.insertWith S.union to   (S.singleton (from, edge)) incomingEdges
  , outgoingEdges = M.insertWith S.union from (S.singleton (to  , edge)) outgoingEdges
  }


makeGraph :: (Ord v, Ord e) => [(v, v, e)] -> Graph v e
makeGraph = foldl addEdge mempty


type Measure context edge weight = context -> edge -> Maybe (weight, context)


type MeasureCost context edge flow cost = flow -> context -> edge -> Maybe (cost, context)


type MeasureCapacity context edge flow = context -> edge -> Maybe (flow, context)


type SetFlow context edge flow = flow -> context -> edge -> Maybe context


type Path v e = [(v, v, e)]



data Capacity a = Capacity a | NoCapacity
  deriving (Eq, Read, Show)

instance Ord a => Ord (Capacity a) where
  compare NoCapacity   NoCapacity   = EQ
  compare NoCapacity   _            = GT
  compare _            NoCapacity   = LT
  compare (Capacity x) (Capacity y) = compare x y

instance Ord a => Monoid (Capacity a) where
  mempty = NoCapacity
  mappend NoCapacity   x            = x
  mappend x            NoCapacity   = x
  mappend (Capacity x) (Capacity y) = Capacity $ minimum [x, y]


hasCapacity :: Capacity a -> Bool
hasCapacity NoCapacity = False
hasCapacity _         = True


getCapacity :: Capacity a -> a
getCapacity NoCapacity   = error "getCapacity: no value."
getCapacity (Capacity x) = x


type Flows e w = Map e w
