{-# LANGUAGE TypeFamilies #-}


module Data.Graph.Types.Weight (
  Halt
, HaltC
, Measure
, MeasureC
, MeasureM
, SetFlowC
, GetFlowC
, Flows
, netFlows
, Flow(..)
, CostFlow(..)
) where


import Control.Monad.State (State)
import Data.Default.Util (inf)
import Data.Map.Strict (Map)
import Data.Monoid.Zero (MonoidZero(..))
import Numeric.NonNegative.Class (C(..), splitDefault)

import qualified Data.Map.Strict as M (filter, foldMapWithKey, fromListWith)


type Halt vertex weight = vertex -> weight -> Bool


type HaltC context vertex weight = context -> vertex -> weight -> Bool


type Measure edge weight = edge -> weight


type MeasureC context edge weight = context -> edge -> Maybe (weight, context)


type MeasureM context edge weight = edge -> State context (Maybe weight)


type GetFlowC context edge flow = context -> edge -> ((flow, flow), context)


type SetFlowC context edge flow = Bool -> flow -> context -> edge -> context


type Flows e w = Map e w


netFlows :: (Ord v, Eq w, Num w) -- FIXME
         => (e -> (v, v))
         -> Flows e w
         -> Map v w
netFlows vertices =
  M.filter (/= 0)
    . M.fromListWith (+)
    . M.foldMapWithKey
      (
        \edge flow' ->
          let
            (from, to) = vertices edge
          in
            [
              (from,   flow')
            , (to  , - flow')
            ]
      )


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


data CostFlow a b =
  CostFlow
  {
    cost :: a
  , flow :: b
  }
    deriving (Eq, Read, Show)

instance (Ord a, Ord b) => Ord (CostFlow a b) where
  CostFlow cost' flow' `compare` CostFlow cost'' flow'' =
    case cost' `compare` cost'' of
      EQ -> flow'' `compare` flow'
      x   -> x

instance (Num a, RealFloat b, Num b) => Monoid (CostFlow a b) where
  mempty = CostFlow 0 inf
  CostFlow cost' flow' `mappend` CostFlow cost'' flow'' =
    CostFlow
      (cost' + cost'')
      $ minimum [flow', flow'']

instance (RealFloat a, Num a, RealFloat b, Num b) => MonoidZero (CostFlow a b) where
  zero = CostFlow inf 0
