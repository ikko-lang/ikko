module Util.PrettyPrint
  ( prettyPrint
  , increaseIndent
  , decreaseIndent
  , writeLine
  , writeComment
  , render
  , printLines
  , PrettyPrint
  , PrettyPrinter
  , Render )
where

import Control.Monad.State.Lazy (gets, evalState, modify, State)

import Util.Functions (commaSep)


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
  _ <- printLines x
  getPrinted

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
