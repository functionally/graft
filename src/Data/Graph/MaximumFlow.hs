{-# LANGUAGE TypeFamilies     #-}


module Data.Graph.MaximumFlow (
  maximumFlow
) where


import Data.Graph.Types (Graph(..))
import Data.Graph.Types.Util (Flows, Measure)

import qualified Data.Graph.MaximumFlow.Contextual as C (maximumFlow)


maximumFlow :: (Graph g, v ~ VertexLabel g, e ~ EdgeLabel g)
            => (Ord v, Ord e, Ord w, Monoid w)
            => Measure e w
            -> g
            -> v
            -> v
            -> Flows e w
maximumFlow measure graph start finish =
  let
    cost' _ context edge =
      do
        let
          weight = cost edge
        guard
          $ context M.! edge > 0
        return (Sum weight, context)
    capacity' context edge =
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
    M.mapWithKey (\edge weight -> flow edge - weight)
      $ C.maximumFlow measure'
        graph
        (foldr (\edge -> M.insert edge $ capacity edge) M.empty $ allEdges graph)
        start
        finish
