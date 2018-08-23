{-# LANGUAGE TypeFamilies     #-}


module Data.Graph.MaximumFlow (
  maximumFlow
) where


import Control.Monad (ap, guard)
import Data.Graph.Types (Graph(..))
import Data.Graph.Types.Weight (Capacity(..), Flows, Measure, getCapacity)

import qualified Data.Graph.MaximumFlow.Contextual as C (maximumFlow')
import qualified Data.Map.Strict as M ((!), empty, insert, mapWithKey)


maximumFlow :: (Show v, Show e, Show w)
            => (Graph g, v ~ VertexLabel g, e ~ EdgeLabel g)
            => (Ord v, Ord e, Ord w, {- Numeric.NonNegative.Class w, -}RealFloat w, Num w)
            => Measure e w
            -> g
            -> v
            -> v
            -> Flows e w
maximumFlow measure graph start finish =
  let
    measure' context edge =
      do
        guard
          $ context M.! edge > 0
        return (Capacity $ context M.! edge, context)
    set' weight context edge =
      let
        remaining = context M.! edge
        remaining' = remaining - getCapacity weight
      in
        M.insert edge remaining' context
  in
    M.mapWithKey (\edge weight -> measure edge - weight)
      $ C.maximumFlow'
        measure'
        set'
        graph
        (foldr (ap M.insert measure) M.empty $ edgeLabels graph)
        start
        finish
