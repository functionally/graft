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


test :: a -> TaggedGraph (Vertex String) (String, Double, Double) (Sum Double, a)
test c0 =
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
    (test ())


test3 :: (Path (Vertex String) (String, Double, Double), Int)
test3 =
  shortestPath
    (\c (_, x, _) -> Just (Sum x, c + 1))
    example1
    0
    SuperSource
    SuperSink


test4 :: Maybe (Minimum Double, Int)
test4 =
  measurePath
    (\c (_, x, _) -> Just (Minimum x, c + 1))
    0
    $ fst test3


test5 :: Flows (String, Double, Double) Double
test5 =
  bareMinimumCostFlow
    (\() (_, _, x) -> Just (x, ()))
    (\() (_, x, _) -> Just (x, ()))
    example1
    SuperSource
    SuperSink


