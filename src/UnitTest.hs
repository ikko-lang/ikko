module UnitTest where

import Control.Monad (when)
import System.Exit
  ( exitFailure )


type Test = IO Bool
type Assertion = Either String ()

runTests :: String -> [Test] -> IO ()
runTests name ts = do
  putStrLn $ "\nTesting " ++ name
  (passes, failures) <- getResults ts 0 0
  renderResults passes failures

renderResults :: Int -> Int -> IO ()
renderResults passes failures = do
  let total = passes + failures
  putStrLn $ show passes ++ "/" ++ show total ++ " passed"
  when (failures > 0) $ do
    putStrLn $ show failures ++ " failed"
    exitFailure

getResults :: [Test] -> Int -> Int -> IO (Int, Int)
getResults []     passes failures =
  return (passes, failures)
getResults (t:ts) passes failures = do
  result <- t
  if result
     then getResults ts (1 + passes) failures
    else getResults ts passes (1 + failures)


test :: String -> Assertion -> Test
test name assertion =
  case assertion of
   Right _  -> return True
   Left err -> do
     putStrLn $ name ++ " failed:\n" ++ indent "    " err ++ "\n"
     return False


indent :: String -> String -> String
indent indentation text =
  unlines $ map (indentation ++) $ lines text

assert :: Bool -> String -> Assertion
assert test message =
  if test then ok else err message

assertTrue :: Bool -> Assertion
assertTrue b = assert b "expected True, got False"

assertFalse :: Bool -> Assertion
assertFalse b = assert (not b) "expected False, got True"


assertEq :: (Show a, Eq a) => a -> a -> Assertion
assertEq x y
  | x == y    = ok
  | otherwise = err $ "not equal:\n  " ++ show x ++ "\nand\n  " ++ show y

assertLeft :: (Show a) => Either l a -> Assertion
assertLeft (Left _)  = ok
assertLeft (Right x) =
  err $ "expected Left, got Right " ++ show x

assertRight :: (Show a) => Either a r -> Assertion
assertRight (Right _) = ok
assertRight (Left x)  =
  err $ "expected Right, got Left " ++ show x

err :: String -> Assertion
err = Left

ok :: Assertion
ok = return ()
