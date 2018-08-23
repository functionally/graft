{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}


module Data.Graph.IO (
  toLines
, toGraphviz
) where


import Data.Graph.Types (Graph(..))
import Data.String.Builder (build, literal)

import qualified Data.Map.Strict as M (keys, toList)
import qualified Data.Set as S (toList)


toLines :: (Graph g, Ord (VertexLabel g), Show (VertexLabel g), Ord (EdgeLabel g), Show (EdgeLabel g))
        => g
        -> [String]
toLines graph =
  concat
    [
      show from
    : [
        "  " ++ show to ++ " --> " ++ show edge
      |
        (to, edge) <- S.toList outgoings
      ]
    |
      (from, outgoings) <- M.toList $ toAdjacencyMatrix graph
    ]
    

toGraphviz :: (Graph g, Ord (VertexLabel g), Show (VertexLabel g), Ord (EdgeLabel g), Show (EdgeLabel g))
           => String
           -> g
           -> String
toGraphviz name graph =
  build
    $ do
        let
          matrix = toAdjacencyMatrix graph
        literal $ "digraph " ++ show name ++ " {\n"
        sequence_
          [
            literal $ "  " ++ show2 vertex ++ " [label=" ++ show2 vertex ++ "]\n"
          |
            vertex <- M.keys matrix
          ]
        sequence_
          [
            literal $ "  " ++ show2 from ++ " -> " ++ show2 to ++ " [label=" ++ show2 edge ++ "]\n"
          |
            (from, outgoings) <- M.toList matrix
          , (to, edge) <- S.toList outgoings
          ]
        "}"
    where
      show2 :: Show a => a -> String
      show2 = show . show

