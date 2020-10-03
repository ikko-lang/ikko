module Util.Functions where

import Data.Set (Set)
import qualified Data.Set as Set

import Data.List (intercalate)

mapFst :: (a -> b) -> [(a, c)] ->  [(b, c)]
mapFst f = map (applyFst f)

mapSnd :: (a -> b) -> [(c, a)] -> [(c, b)]
mapSnd f = map (applySnd f)

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft _ (Right c) = Right c
mapLeft f (Left a)  = Left (f a)

applyFst :: (a -> b) -> (a, c) -> (b, c)
applyFst f (a, c) = (f a, c)

applySnd :: (a -> b) -> (c, a) -> (c, b)
applySnd f (c, a) = (c, f a)

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

commaSep :: [String] -> String
commaSep = intercalate ", "

duplicates :: (Ord a) => [a] -> [a]
duplicates = dups Set.empty Set.empty
  where dups _    ds []     = Set.toList ds
        dups seen ds (x:xs) =
          if Set.member x seen
          then dups seen                (Set.insert x ds) xs
          else dups (Set.insert x seen) ds                xs

unions :: (Ord a) => [Set a] -> Set a
unions = foldl Set.union Set.empty
