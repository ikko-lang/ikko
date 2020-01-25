module Util.Functions where

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
