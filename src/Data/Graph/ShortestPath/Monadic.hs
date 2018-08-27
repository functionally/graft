{-# LANGUAGE TypeFamilies     #-}


module Data.Graph.ShortestPath.Monadic (
  shortestPath
, measurePath
) where


import Control.Monad.State (MonadState(..), State, runState)
import Data.Graph.Types (Graph(..), Path)
import Data.Graph.Types.Weight (MeasureM)

import qualified Data.Graph.ShortestPath.Contextual as C (measurePath, shortestPath)


outside :: MonadState c m => Maybe (a, c) -> m (Maybe a)
outside Nothing             = return Nothing
outside (Just (w, context)) = put context >> return (Just w)


inside :: c -> State c (Maybe a) -> Maybe (a, c)
inside context action =
  case runState action context of
    (Nothing, _       ) -> Nothing
    (Just w , context') -> Just (w, context')


measurePath :: MonadState c m
            => Monoid w
            => MeasureM c e w
            -> Path v e
            -> m (Maybe w)
measurePath measure path =
  do
    context <- get
    outside $ C.measurePath ((. measure) . inside) context path


shortestPath :: (Show v, Show e, Show w)
             => MonadState c m
             => (Graph g, v ~ VertexLabel g, e ~ EdgeLabel g)
             => (Ord v, Ord e, Ord w, Monoid w)
             => Bool
             -> MeasureM c e w
             -> g
             -> v
             -> v
             -> m (Path v e, w)
shortestPath negativeWeights measure graph start finish =
  do
    context <- get
    let
      (path, weight, context') = C.shortestPath negativeWeights ((. measure) . inside) graph context start finish
    put context'
    return (path, weight)
