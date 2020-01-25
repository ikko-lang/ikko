module Util.PrettyPrint where

import Data.List (intercalate)

prettyPrint :: (PrettyPrint a) => a -> String
prettyPrint x = printer 0 False x

prettyPrintVerbose :: (PrettyPrint a) => a -> String
prettyPrintVerbose x = printer 0 True x

class PrettyPrint a where
  printer :: Int -> Bool -> a -> String

instance (PrettyPrint a) => PrettyPrint [a] where
  printer indent verbose items =
    let printed = map (printer indent verbose) items
        filtered = [s | s <- printed, not $ null s]
    in intercalate ", " filtered
