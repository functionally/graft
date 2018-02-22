{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExplicitForAll #-}


module Data.Graph.Tied
where


import Data.Function (on)
import Data.Function.MapReduce (mapReduce)
import Data.Map.Strict (Map)
import Data.Set (Set)

import qualified Data.Map.Strict as M
import qualified Data.Set as S


data TiedGraph v e =
  TiedGraph
  {
    vertices :: Set (Vertex v e)
  , edges    :: Set (Edge v e)
  }
    deriving (Eq, Ord)


data Vertex v e =
  Vertex
  {
    vertexLabel :: v
  , edgesFrom   :: [Edge v e]
  , edgesTo     :: [Edge v e]
  }

instance Eq v => Eq (Vertex v e) where
  (==) = (==) `on` vertexLabel

instance Ord v => Ord (Vertex v e) where
  compare = compare `on` vertexLabel


data Edge v e =
  Edge
  {
    edgeLabel :: e
  , vertexFrom :: Vertex v e
  , vertexTo   :: Vertex v e
  }

instance Eq e => Eq (Edge v e) where
  (==) = (==) `on` edgeLabel

instance Ord e => Ord (Edge v e) where
  compare = compare `on` edgeLabel


tieGraph :: forall v e. (Ord v, Ord e) => Map v (Set (v, e)) -> TiedGraph v e
tieGraph adjacencies =
  let
    vertexTies :: Map v (Vertex v e)
    vertexTies =
      M.mapWithKey
        (
          \vertexLabel _ ->
            let
              edgesFrom = S.toList . S.map snd $ outgoingEdgeTies M.! vertexLabel
              edgesTo   = S.toList . S.map snd $ incomingEdgeTies M.! vertexLabel
            in
              Vertex{..}
        )
          adjacencies
    outgoingEdgeTies :: Map v (Set (v, Edge v e))
    outgoingEdgeTies =
      M.mapWithKey
        (
          \from ->
            S.map
              (
                \(to, edgeLabel) ->
                  let
                    vertexFrom = vertexTies M.! from
                    vertexTo   = vertexTies M.! to
                  in
                    (to, Edge{..})
              )
        )
        adjacencies
    rev (x, (y, z)) = (y, (x, z))
    incomingEdgeTies :: Map v (Set (v, Edge v e))
    incomingEdgeTies =
      M.fromList
        . mapReduce
          id
          ((. S.fromList) . (,))
        . fmap rev
        . concatMap (uncurry $ (. S.toList) . fmap . (,))
        $ M.toList outgoingEdgeTies
  in
    TiedGraph
    {
      vertices = M.foldr S.insert              S.empty vertexTies
    , edges    = M.foldr (S.union . S.map snd) S.empty outgoingEdgeTies
    }


example1 ::  TiedGraph Char String
example1 =
  tieGraph
    $ M.fromList
      [
        ('+', S.fromList [('A', "+A"), ('B', "+B")])
      , ('A', S.fromList [('B', "AB"), ('C', "AC")])
      , ('B', S.fromList [('D', "BD")             ])
      , ('C', S.fromList [('E', "CE")             ])
      , ('D', S.fromList [('C', "DC"), ('F', "DF")])
      , ('E', S.fromList [('-', "E-")             ])
      , ('F', S.fromList [('E', "FE"), ('-', "F-")])
      , ('-', S.fromList [                        ])
      ]
