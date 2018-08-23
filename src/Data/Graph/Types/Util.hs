module Data.Graph.Types.Util (
  Tagged(..)
, TaggedGraph
, HyperVertex(..)
, HyperEdge(..)
) where


import Data.Graph.Types.MapGraph (MapGraph)


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
    ForwardEdge {self :: a, opposite :: HyperEdge a}
  | ReverseEdge {self :: a, opposite :: HyperEdge a}
    deriving (Eq, Ord, Read, Show)
