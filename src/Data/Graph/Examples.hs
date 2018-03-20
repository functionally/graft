module Data.Graph.Examples
where


import Data.Graph.Algorithms
import Data.Graph.Types
import Data.Monoid (Sum(..))


example1 ::  Graph (Vertex String) (String, Double, Double)
example1 =
  makeGraph
    [
      (SuperSource, Vertex "A", ("+A",  5, 0))
    , (SuperSource, Vertex "B", ("+B", 10, 0))
    , (Vertex "A" , Vertex "B", ("AB", 15, 1))
    , (Vertex "A" , Vertex "C", ("AC", 15, 5))
    , (Vertex "B" , Vertex "D", ("BD", 10, 2))
    , (Vertex "C" , Vertex "E", ("CE", 15, 4))
    , (Vertex "D" , Vertex "C", ("DC", 15, 1))
    , (Vertex "D" , Vertex "F", ("DF",  8, 3))
    , (Vertex "E" , SuperSink , ("E-", 10, 0))
    , (Vertex "F" , Vertex "E", ("FE", 15, 1))
    , (Vertex "F" , SuperSink , ("F-",  5, 0))
    ]


test1 :: a -> TaggedGraph (Vertex String) (String, Double, Double) (Sum Double, a)
test1 c0 =
  shortestPathTree
    (\c (_, x, _) -> Just (Sum x, c))
    (\_ v _ -> v == SuperSink)
    example1
    c0
    SuperSource


newtype String' = String' String
  deriving (Eq, Ord)

instance Show String' where
  show (String' x) = x


test2 :: Graph String' String'
test2 =
  bimap
    (\(TaggedItem v (Sum w, ())) -> String' $ show v ++ " = " ++ show w)
    (\(l, x, _) -> String' $ l ++ " <= " ++ show x)
    (test1 ())


test3 :: (Path (Vertex String) (String, Double, Double), Int)
test3 =
  shortestPath
    (\c (_, x, _) -> Just (Sum x, c + 1))
    example1
    0
    SuperSource
    SuperSink


test4 :: Maybe (Capacity Double, Int)
test4 =
  measurePath
    (\c (_, x, _) -> Just (Capacity x, c + 1))
    0
    $ fst test3


test5 :: Flows (String, Double, Double) Double
test5 =
  bareCapacityCostFlow
    (\(_, _, x) -> x)
    (\(_, x, _) -> x)
    example1
    SuperSource
    SuperSink


example2 ::  Graph (Vertex String) (String, Double, Double)
example2 =
  makeGraph
    [
      (SuperSource    , Vertex "P:EP"  , ("+P:EP" ,   5.6, 1.75))
    , (SuperSource    , Vertex "P:BA"  , ("+P:BA" , 226.9, 1.75))
    , (Vertex "P:EP"  , Vertex "T:2:EP", ("F:1:EP", 999.9, 3.67))
    , (Vertex "P:BA"  , Vertex "T:2:BA", ("F:1:BA", 999.9, 3.67))
    , (Vertex "T:2:EP", Vertex "C:EP"  , ("F:3:EP", 999.9, 0.06))
    , (Vertex "T:2:BA", Vertex "C:BA"  , ("F:3:BA", 999.9, 0.06))
    , (Vertex "T:2:EP", Vertex "T:2:BA", ("F1831" , 999.9, 0.67))
    , (Vertex "T:2:BA", Vertex "T:2:EP", ("R1831" , 999.9, 0.67))
    , (Vertex "C:EP"  , SuperSink      , ("-D:EP" ,  68.1, 0.00))
    ]
