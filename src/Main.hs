module Main where

import Control.Monad.Except
import System.Environment ( getArgs )
import System.Exit ( exitWith, exitSuccess, ExitCode(..) )
import System.IO (stderr, hPutStrLn, hFlush)

import Compiler ( compile )
import Errors (renderError)
import qualified Interpreter

type ExitCodeResult = ExceptT String IO ExitCode


main :: IO ()
main = do
  args <- getArgs
  case args of
   [fileName] -> do
     content <- readFile fileName
     interpret fileName content

   _ ->
     exitError "usage: ikko <file.at>"

interpret :: String -> String -> IO ()
interpret fileName content =
  case compile fileName content of
   Left err ->
     exitError $ renderError err content
   Right result -> do
     --putStrLn $ show result
     Interpreter.interpret result
     exitSuccess


either2Except :: Monad m => Either e a -> ExceptT e m a
either2Except = ExceptT . return

printErrLn :: String -> IO ()
printErrLn s = do
  hPutStrLn stderr s
  hFlush stderr

exitError :: String -> IO ()
exitError err = do
  printErrLn err
  exitWith (ExitFailure 1)
