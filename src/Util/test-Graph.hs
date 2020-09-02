module Main where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Test.HUnit hiding (Testable)
import Test.QuickCheck
  ( quickCheck
  , quickCheckWithResult
  , stdArgs
  , isSuccess
  , within
  , Args(..)
  , Testable
  , Result(..)
  , Property )

import Util.Graph


main = runTestTT tests

tests :: Test
tests =
  TestList
  [ TestLabel "example graph" $ TestCase testExample
  , TestLabel "simple cycle" $ TestCase testSimpleCycle
  , TestLabel "longer cycle" $ TestCase testLongerCycle
  , TestLabel "test cycle 3" $ TestCase testCycle3
  , TestLabel "path exists" $ testPathExists
  , TestLabel "path exists2" $ testPathExists2
  , checkProperty "nodes in cycle can reach themselves" propReachableCycle
  , checkProperty "no empty groups" propNoEmptyGroups
  , checkProperty "same cardnality" propSameCardnality
  , checkProperty "disjoint" propDisjoint
  , checkProperty "all reachable in groups" propAllReachableInGroups
  , checkProperty "topological" propTopological
  ]

checkProperty :: (Testable prop) => String -> prop -> Test
checkProperty name property = TestLabel name $ TestCase $ do
  let args = stdArgs { maxSuccess = 10000, maxShrinks = 10, maxSize = 20, chatty = False }
  result <- quickCheckWithResult args property
  assertBool "" (isSuccess result)


propReachableCycle :: Graph Int -> Property -- Graph Char -> Bool
propReachableCycle graph =
  let inCycle = nodesInCycle graph
      nodeReachesSelf node = pathExists graph node node
  in within 1000000 $ all (\node -> nodeReachesSelf node == (node `elem` inCycle)) (nodes graph)

propNoEmptyGroups :: Graph Char -> Bool
propNoEmptyGroups graph =
  let cmps = components $ fixupGraph graph
  in all (not . null) cmps

propSameCardnality :: Graph Char -> Bool
propSameCardnality graph =
  let g = fixupGraph graph
      cmps = components g
  in length g == sum (map length cmps)

propDisjoint :: Graph Char -> Bool
propDisjoint graph =
  let ns = concat $ components $ fixupGraph graph
  in length ns == length (Set.toList $ Set.fromList ns)

propAllReachableInGroups :: Graph Char -> Bool
propAllReachableInGroups graph =
  let g = fixupGraph graph
      cmps = components g
      allReachable ns node = let seen = reachable node g in all (`Set.member` seen) ns
      mutuallyReachable ns = all (allReachable ns) ns
  in all mutuallyReachable cmps

propTopological :: Graph Char -> Bool
propTopological graph =
  let g = fixupGraph graph
      cmps = components g
      noBackreferences (previous, group) =
        let prevSet = Set.fromList $ concat previous
            groupReachable = foldl Set.union Set.empty $ map (`reachable` g) group
        in all (\n -> not $ Set.member n prevSet) $ Set.toList groupReachable
  in all noBackreferences (prefixes cmps)


prefixes :: [a] -> [([a], a)]
prefixes items = prefixes' items []
  where prefixes' []     _     = []
        prefixes' (x:xs) trail = (reverse trail, x) : prefixes' xs (x : trail)

-- fixupGraph adds missing nodes
fixupGraph :: (Ord a) => Graph a -> Graph a
fixupGraph graph = Map.union graph empties
  where chs = Set.toList $ foldl Set.union Set.empty $ map (Set.fromList . snd) $ Map.toList graph
        empties = Map.fromList $ zip chs (repeat [])

testExample :: Assertion
testExample =
  let graph = Map.fromList [('a', "b"), ('b', "ecf"), ('c', "dg"), ('d', "ch"), ('e', "af"), ('f', "g"), ('g', "f"), ('h', "gd")]
      expected = reverse $ map reverse ["fg", "cdh", "abe"]
  in assertEqual "" expected (components graph)

testSimpleCycle :: Assertion
testSimpleCycle =
  let graph = Map.fromList [('a', "b"), ('b', "bc")]
      expected = "b"
  in assertEqual "" expected (nodesInCycle graph)

testLongerCycle :: Assertion
testLongerCycle =
  let graph = Map.fromList [('a', "bx"), ('b', "yc"), ('c', "az")]
      expected = "abc"
  in assertEqual "" expected (nodesInCycle graph)

testCycle3 :: Assertion
testCycle3 =
  let graph = Map.fromList [('b',"ac"),('a',"b"),('c',"a")]
      expected = "abc"
  in assertEqual "" expected (nodesInCycle graph)

testPathExists :: Test
testPathExists =
  let graph = Map.fromList [('a', "b"), ('b', "bc"), ('x', "y"), ('y', "x")]
      check label expected from to =
        TestLabel label $ TestCase $ assertEqual "" expected $ pathExists graph from to
  in TestList
     [ check "a>a" False 'a' 'a'
     , check "a>b" True  'a' 'b'
     , check "b>a" False 'b' 'a'
     , check "b>b" True  'b' 'b'
     , check "a>c" True  'a' 'c'
     , check "c>c" False 'c' 'c'
     , check "x>y" True  'x' 'y'
     ]

testPathExists2 :: Test
testPathExists2 =
  let graph = Map.fromList [('a', "b"), ('b', "c"), ('c', "a")]
      check label expected from to =
        TestLabel label $ TestCase $ assertEqual "" expected $ pathExists graph from to
  in TestList
     [ check "a>a" True 'a' 'a'
     , check "b>b" True 'b' 'b'
     , check "c>c" True 'c' 'c'
     ]
