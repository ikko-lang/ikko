module Compiler where

import Util.Functions
import Errors ( Error(..), Result )
import FirstPass ( firstPass )
import Inference ( inferModule, InferResult )
import Parser ( parseFile )
--import Types ( Scheme )

compile :: String -> String -> Result InferResult
compile fileName text = do
  file <- mapLeft ParseError $ parseFile fileName text
  m <- firstPass file
  inferModule m
