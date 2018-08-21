module Data.Graph.Types.Util (
  Tagged(..)
, TaggedGraph
, HyperVertex(..)
, HyperEdge(..)
, Halt
, HaltC
, Measure
, MeasureC
, MeasureM
, SetFlowC
, Flows
) where


import Control.Monad.State (State)
import Data.Graph.Types.MapGraph (MapGraph)
import Data.Map.Strict (Map)


data Tagged a b =
  Tagged
  {
    item :: a
  , tag  :: b
  }
    deriving (Read, Show)

instance Eq a => Eq (Tagged a b) where
  Tagged x _ == Tagged y _ = x == y

instance Ord a => Ord (Tagged a b) where
  Tagged x _ `compare` Tagged y _ = x `compare` y


type TaggedGraph v e t = MapGraph (Tagged v t) e


data HyperVertex a =
    HyperSource
  | HyperSink
  | HyperVertex a
    deriving (Eq, Ord, Read, Show)


data HyperEdge a =
    ForwardEdge a
  | ReversedEdge a
    deriving (Eq, Ord, Read, Show)


type Halt vertex weight = vertex -> weight -> Bool


type HaltC context vertex weight = context -> vertex -> weight -> Bool


type Measure edge weight = edge -> Maybe weight


type MeasureC context edge weight = context -> edge -> Maybe (weight, context)


type MeasureM context edge weight = edge -> State context (Maybe weight)


type SetFlowC context edge flow = context -> edge -> flow -> context


type Flows e w = Map e w
