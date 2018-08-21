{-# LANGUAGE TypeFamilies     #-}


module Data.Graph.MaximumFlow.Contextual (
  maximumFlow
) where


import Data.Graph.ShortestPath.Contextual (measurePath, shortestPath)
import Data.Graph.Types (Graph(..), Path)
import Data.Graph.Types.Util (MeasureC, SetFlowC)


setFlow :: Monoid w
        => SetFlowC c e w
        -> w
        -> c
        -> Path v e
        -> c
setFlow set flow = foldl (\context' (_, _, edge) -> set context' edge flow)


maximumFlow :: (Graph g, v ~ VertexLabel g, e ~ EdgeLabel g)
            => (Ord v, Ord e, Ord w, Monoid w)
            => MeasureC c e w
            -> MeasureC c e w
            -> g
            -> c
            -> v
            -> v
            -> c
maximumFlow measure set graph context start finish =
  let
    -- Find a shortest path.
    (path, _) = shortestPath measure graph context start finish
    -- Record the flow along the path.
    Just (_, context') = measurePath set context' path
  in
    if null path
      then context
      else maximumFlow measure set graph context' start finish
