module Data.Graph.Types.Weight (
  Halt
, HaltC
, Measure
, MeasureC
, MeasureM
, SetFlowC
, Flows
, Flow(..)
, Capacity(..)
, hasCapacity
, getCapacity
) where


import Control.Monad.State (State)
import Data.Default.Util (inf)
import Data.Map.Strict (Map)
import Numeric.NonNegative.Class (C(..), splitDefault)


type Halt vertex weight = vertex -> weight -> Bool


type HaltC context vertex weight = context -> vertex -> weight -> Bool


type Measure edge weight = edge -> weight


type MeasureC context edge weight = context -> edge -> Maybe (weight, context)


type MeasureM context edge weight = edge -> State context (Maybe weight)


type SetFlowC context edge flow = flow -> context -> edge -> context


type Flows e w = Map e w


newtype Flow a = Flow {unflow :: a}
  deriving (Eq, Read, Show)

instance Ord a => Ord (Flow a) where
  compare = flip compare

instance RealFloat a => Monoid (Flow a) where
  mempty = Flow inf
  Flow x `mappend` Flow y = Flow $ minimum [x, y]

instance (RealFloat a, Num a) => C (Flow a) where
  split = splitDefault unflow Flow


data Capacity a = Capacity a | Unlimited
  deriving (Eq, Read, Show)

instance Ord a => Ord (Capacity a) where
  compare Unlimited    Unlimited   = EQ
  compare Unlimited    _           = LT
  compare _            Unlimited   = GT
  compare (Capacity x) (Capacity y) = compare y x

instance Ord a => Monoid (Capacity a) where
  mempty = Unlimited
  mappend Unlimited   x            = x
  mappend x            Unlimited   = x
  mappend (Capacity x) (Capacity y) = Capacity $ minimum [x, y]


hasCapacity :: Capacity a -> Bool
hasCapacity Unlimited = False
hasCapacity _         = True


getCapacity :: Capacity a -> a
getCapacity Unlimited   = error "getCapacity: no value."
getCapacity (Capacity x) = x
