{-# LANGUAGE TypeFamilies        #-}


module Data.Graph.MaximumFlow.Contextual (
  maximumFlow
, maximumFlow'
) where


import Data.Graph.ShortestPath.Contextual (shortestPath)
import Data.Graph.Types (Graph(..), Path)
import Data.Graph.Types.MapGraph (makeMapGraph)
import Data.Graph.Types.Util (HyperEdge(..))
import Data.Graph.Types.Weight (MeasureC, SetFlowC)
import Numeric.NonNegative.Class (C)


setFlow :: Monoid w
        => SetFlowC c e w
        -> w
        -> c
        -> Path v e
        -> c
setFlow set flow = foldl (\context' (_, _, edge) -> set flow context' edge)


maximumFlow :: (Show v, Show e, Show w)
            => (Graph g, v ~ VertexLabel g, e ~ EdgeLabel g)
            => (Ord v, Ord e, C w)
            => MeasureC c e w
            -> SetFlowC c e w
            -> g
            -> c
            -> v
            -> v
            -> c
maximumFlow measure set graph context start finish =
  let
    (originalVertices, originalEdges) = toEdgeList graph
    hyper =
      makeMapGraph originalVertices
        $ concat
        [
          [
            (from, to  , f)
          , (to  , from, r)
          ]
        |
          (from, to, edge) <- originalEdges
        , let f = ForwardEdge edge r
              r = ReverseEdge edge f
        ]
    measure' context' (ForwardEdge edge _) = measure context' edge
    measure' _        (ReverseEdge _    _) = Nothing
    set' flow context' (ForwardEdge edge _) = set flow context' edge
    set' _    context' (ReverseEdge _    _) = context'
  in
    maximumFlow' measure' set' hyper context start finish
   

maximumFlow' :: (Show v, Show e, Show w)
             => (Graph g, v ~ VertexLabel g, e ~ EdgeLabel g)
             => (Ord v, Ord e, Ord w, Monoid w)
             => MeasureC c e w
             -> SetFlowC c e w
             -> g
             -> c
             -> v
             -> v
             -> c
maximumFlow' measure set graph context start finish =
  let
    -- Find a shortest path and its flow.
    (path, flow, _) = shortestPath measure graph context start finish
    -- Record the flow along the path.
    context' = setFlow set flow context path
  in
    if null path
      then context
      else maximumFlow' measure set graph context' start finish
