{-# LANGUAGE TypeFamilies        #-}


module Data.Graph.MaximumFlow.Contextual (
  maximumFlow
, maximumFlow'
) where


import Control.Arrow (first)
import Data.Graph.ShortestPath.Contextual (shortestPath)
import Data.Graph.Types (Graph(..), Path)
import Data.Graph.Types.MapGraph (makeMapGraph)
import Data.Graph.Types.Util (HyperEdge(..))
import Data.Graph.Types.Weight (GetFlowC, MeasureC, SetFlowC)
import Data.Monoid.Zero (MonoidZero(..))
import Numeric.NonNegative.Class (C)


setFlow :: Monoid w
        => SetFlowC c e w
        -> w
        -> c
        -> Path v e
        -> c
setFlow set flow = foldl (\context' (_, _, edge) -> set True flow context' edge)


maximumFlow :: (Show v, Show e, Show w)
            => (Graph g, v ~ VertexLabel g, e ~ EdgeLabel g)
            => (Ord v, Ord e, MonoidZero w, C w)
            => GetFlowC c e w
            -> SetFlowC c e w
            -> g
            -> c
            -> v
            -> v
            -> c
maximumFlow measure set graph context start finish =
  let
    nonzero x = if fst x == zero then Nothing else Just x
    (originalVertices, originalEdges) = toEdgeList graph
    hyper =
      makeMapGraph originalVertices
        $ concat
        [
          [
            (from, to  , ForwardEdge edge)
          , (to  , from, ReverseEdge edge)
          ]
        |
          (from, to, edge) <- originalEdges
        ]
    measure'  context' (ForwardEdge edge) = nonzero . first fst $ measure context' edge
    measure'  context' (ReverseEdge edge) = nonzero . first snd $ measure context' edge
    set' _ flow context' (ForwardEdge edge) = set True  flow context' edge
    set' _ flow context' (ReverseEdge edge) = set False flow context' edge
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
