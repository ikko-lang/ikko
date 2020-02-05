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
  , Args(..)
  , Testable
  , Result(..) )

import Util.Graph


main = runTestTT tests

tests :: Test
tests =
  TestList
  [ TestLabel "example graph" $ TestCase testExample
  , checkProperty "no empty groups" propNoEmptyGroups
  , checkProperty "same cardnality" propSameCardnality
  , checkProperty "disjoint" propDisjoint
  , checkProperty "all reachable in groups" propAllReachableInGroups
  , checkProperty "topological" propTopological
  ]

checkProperty :: (Testable prop) => String -> prop -> Test
checkProperty name property = TestLabel name $ TestCase $ do
  let args = stdArgs { maxSuccess = 100, chatty = False }
  result <- quickCheckWithResult args property
  assertBool "" (isSuccess result)

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
