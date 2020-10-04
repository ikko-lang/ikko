module Main where

import Test.HUnit

import qualified Data.Map as Map

import FirstPass
  ( checkReturns
  , makeConstructors
  , Constructor(..)
  )
import qualified AST
import Types

main = runTestTT tests

tests =
  TestList
  [ ts "empty fn body" testCheckReturns1
  , ts "unreachable statement" testCheckReturns2
  , ts "building struct constructor" testBuildStructConstructor
  , ts "building enum constructor" testBuildEnumConstructor
  ]

ts name assertion = TestLabel name $ TestCase assertion

assertRight :: Either a b -> Assertion
assertRight (Left _) = assertFailure "expected Right, got Left"
assertRight _        = return ()

assertLeft :: Either a b -> Assertion
assertLeft (Right _) = assertFailure "expected Left, got Right"
assertLeft _         = return ()

testCheckReturns1 :: Assertion
testCheckReturns1 = do
  let fn = AST.DFunction [] "foo" Nothing [] (AST.Block [] [])
  assertRight $ checkReturns fn

testCheckReturns2 :: Assertion
testCheckReturns2 = do
  let returnStmt = AST.Return [] Nothing
  let stmts = [printStmt, returnStmt, printStmt]
  let fn = AST.DFunction [] "foo" Nothing [] (AST.Block [] stmts)
  assertLeft $ checkReturns fn

tgenN :: Int -> Type
tgenN n = TGen n Star

testBuildStructConstructor :: Assertion
testBuildStructConstructor = do
  let tDef = AST.TypeDef { AST.defAnn=[], AST.defName="Pair", AST.defGenerics=["A", "B"] }
  let tDecl = AST.TStruct [] [("first", AST.TName [] "A"), ("second", AST.TName [] "B")]
  let result = makeConstructors [(tDef, tDecl)] Map.empty

  let firstSch = Scheme [Star, Star] $ Qual [] $ makeFuncType [tcon "Pair" [tgenN 0, tgenN 1]] (tgenN 0)
  let secondSch = Scheme [Star, Star] $ Qual [] $ makeFuncType [tcon "Pair" [tgenN 0, tgenN 1]] (tgenN 1)
  let fields = [("first", firstSch), ("second", secondSch)]
  let sch = Scheme [Star, Star] $ Qual [] (makeFuncType [tgenN 0, tgenN 1] $ tcon "Pair" [tgenN 0, tgenN 1])
  let ctor = Constructor { ctorFields=fields, ctorType=sch }
  let expected = Map.fromList [("Pair", ctor)]
  assertEqual "" (Right expected) result


testBuildEnumConstructor :: Assertion
testBuildEnumConstructor = do
  let tDef = AST.TypeDef { AST.defAnn=[], AST.defName="Maybe", AST.defGenerics=["X"] }
  let optJust = [("val", AST.TName [] "X")]
  let optNothing = []
  let tDecl = AST.TEnum [] [("Just", optJust), ("Nothing", optNothing)]
  let result = makeConstructors [(tDef, tDecl)] Map.empty


  let schNothing = Scheme [Star] $ Qual [] (makeFuncType [] $ tcon "Maybe" [tgenN 0])
  let ctorNothing = Constructor { ctorFields=[], ctorType=schNothing }

  let schVal = Scheme [Star] $ Qual [] (makeFuncType [tcon "Maybe" [tgenN 0]] (tgenN 0))
  let fieldsJust = [("val", schVal)]
  let schJust = Scheme [Star] $ Qual [] (makeFuncType [tgenN 0] $ tcon "Maybe" [tgenN 0])
  let ctorJust = Constructor { ctorFields=fieldsJust, ctorType=schJust }

  let schMaybe = Scheme [Star] $ Qual [] (makeFuncType [] $ tcon "Maybe" [tgenN 0])
  let ctorMaybe = Constructor { ctorFields=[], ctorType=schMaybe }
  let expected = Map.fromList [("Maybe", ctorMaybe), ("Just", ctorJust), ("Nothing", ctorNothing)]

  assertEqual "" (Right expected) result


printStmt =
  AST.Expr [] $ AST.Call [] (AST.Var [] "print") [AST.Val [] (AST.StrVal [] "hello world")]

tcon :: String -> [Type] -> Type
tcon name types =
  applyTypes (TCon name $ kindN $ length types) types
