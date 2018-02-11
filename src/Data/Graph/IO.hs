{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGe RecordWildCards   #-}


module Data.Graph.IO (
  toGraphviz
) where


import Data.Graph.Types (Graph(..))
import Data.String.Builder (build, literal)

import qualified Data.Map.Strict as M (toList)
import qualified Data.Set as S (toList)


show2 :: Show a => a -> String
show2 = show . show


toGraphviz :: (Show v, Show e) => String -> Graph v e -> String
toGraphviz name Graph{..} =
  build
    $ do
        literal $ "digraph " ++ show name ++ " {\n"
        sequence_
          [
            literal $ "  " ++ show2 from ++ " -> " ++ show2 to ++ " [label=" ++ show2 edge ++ "]\n"
          |
            (from, edges) <- M.toList outgoingEdges
          , (to, edge) <- S.toList edges
          ]
        "}"
