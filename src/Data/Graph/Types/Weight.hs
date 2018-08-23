module Data.Graph.Types.Weight (
  Halt
, HaltC
, Measure
, MeasureC
, MeasureM
, SetFlowC
, GetFlowC
, Flows
, Flow(..)
) where


import Control.Monad.State (State)
import Data.Default.Util (inf)
import Data.Map.Strict (Map)
import Data.Monoid.Zero (MonoidZero(..))
import Numeric.NonNegative.Class (C(..), splitDefault)


type Halt vertex weight = vertex -> weight -> Bool


type HaltC context vertex weight = context -> vertex -> weight -> Bool


type Measure edge weight = edge -> weight


type MeasureC context edge weight = context -> edge -> Maybe (weight, context)


type MeasureM context edge weight = edge -> State context (Maybe weight)


type GetFlowC context edge flow = context -> edge -> ((flow, flow), context)


type SetFlowC context edge flow = Bool -> flow -> context -> edge -> context


type Flows e w = Map e w


newtype Flow a = Flow {unFlow :: a}
  deriving (Eq, Read, Show)

instance Ord a => Ord (Flow a) where
  Flow x `compare` Flow y = y `compare` x

instance RealFloat a => Monoid (Flow a) where
  mempty = Flow inf
  Flow x `mappend` Flow y = Flow $ minimum [x, y]

instance (RealFloat a, Num a) => MonoidZero (Flow a) where
  zero = Flow 0

instance (RealFloat a, Num a) => C (Flow a) where
  split = splitDefault unFlow Flow
