{-# LANGUAGE TypeFamilies     #-}


module Data.Graph.MaximumFlow (
  maximumFlow
) where


import Data.Graph.Types (Graph(..))
import Data.Graph.Types.Weight (Flow(..), Flows, Measure)

import qualified Data.Graph.MaximumFlow.Contextual as C (maximumFlow)
import qualified Data.Map.Strict as M ((!), empty, insert)


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
      let
         flow = context M.! edge
      in
        (
          (
            Flow $ measure edge - flow
          , Flow flow
          )
        , context
        )
    set' forward flow context edge =
      let
        previous = context M.! edge
      in
        M.insert
          edge
          (
            if forward
              then previous + unFlow flow
              else previous - unFlow flow
          )
          context
  in
    C.maximumFlow
      measure'
      set'
      graph
      (foldr (`M.insert` 0) M.empty $ edgeLabels graph)
      start
      finish
