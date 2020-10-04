module Util.PrettyPrint
  ( prettyPrint
  , increaseIndent
  , decreaseIndent
  , writeLine
  , writeComment
  , debug
  , debugPairs
  , render
  , printLines
  , Debug
  , PrettyPrint
  , PrettyPrinter
  , Render )
where

import Control.Monad.State.Lazy (gets, evalState, modify, State)

import Util.Functions (commaSep)

class Debug a where
  debug :: a -> String


instance (Debug a) => Debug [a] where
  debug items =
    commaSep $ filter (not . null) $ map debug items

debugPairs :: (Debug a) => [(String, a)] -> String
debugPairs pairs =
  commaSep [k ++ "=" ++ debug v | (k, v) <- pairs]

class Render a where
  render :: a -> String

instance (Render a) => Render [a] where
  render items =
    let printed = map render items
        filtered = [s | s <- printed, not $ null s]
    in commaSep filtered


prettyPrint :: (PrettyPrint a) => a -> String
prettyPrint x = evalState (runPrint x) startingState

runPrint :: (PrettyPrint a) => a -> PrettyPrinter String
runPrint x = do
  leading <- printLines x
  printed <- getPrinted
  return (leading ++ printed)

class PrettyPrint a where
  -- Finished lines are emitted with writeLine,
  -- in-progress lines are returned as the string
  printLines :: a -> PrettyPrinter String


data PrintState
  = PrintState
  { indent :: Int
  , linesBackwards :: [String] }

type PrettyPrinter a = State PrintState a

startingState :: PrintState
startingState =
  PrintState { indent=0, linesBackwards=[] }

getPrinted :: PrettyPrinter String
getPrinted =
  unlines . reverse <$> gets linesBackwards

increaseIndent :: PrettyPrinter ()
increaseIndent =
  modify (\ps -> ps { indent=indent ps + 1 })

decreaseIndent :: PrettyPrinter ()
decreaseIndent =
  modify (\ps -> ps { indent=indent ps - 1 })

writeComment :: String -> PrettyPrinter ()
writeComment comment = writeLine $ "// " ++ comment

writeLine :: String -> PrettyPrinter ()
writeLine s = do
  indented <- gets indent
  writeRawLine (addIndent indented s)

-- writeRawLine is writeLine minus indentation handling
writeRawLine :: String -> PrettyPrinter ()
writeRawLine s = modify addLine
  where addLine ps = ps { linesBackwards=s : linesBackwards ps }

addIndent :: Int -> String -> String
addIndent n s = replicate n ' ' ++ s
