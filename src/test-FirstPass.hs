module Main where

import Test.HUnit

import qualified Data.Map as Map

import FirstPass
  ( checkReturns
  , makeConstructors
  , Constructor(..) )
import qualified AST.Expression as E
import qualified AST.Declaration as D
import qualified AST.Statement as S
import qualified AST.Type as T
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
  let fn = D.Function [] "foo" Nothing [] (S.Block [] [])
  assertRight $ checkReturns fn

testCheckReturns2 :: Assertion
testCheckReturns2 = do
  let returnStmt = S.Return [] Nothing
  let stmts = [printStmt, returnStmt, printStmt]
  let fn = D.Function [] "foo" Nothing [] (S.Block [] stmts)
  assertLeft $ checkReturns fn

tgenN :: Int -> Type
tgenN n = TGen n Star

testBuildStructConstructor :: Assertion
testBuildStructConstructor = do
  let tDef = T.TypeDef { T.defAnn=[], T.defName="Pair", T.defGenerics=["A", "B"] }
  let tDecl = T.Struct [] [("first", T.TypeName [] "A"), ("second", T.TypeName [] "B")]
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
  let tDef = T.TypeDef { T.defAnn=[], T.defName="Maybe", T.defGenerics=["X"] }
  let optJust = [("val", T.TypeName [] "X")]
  let optNothing = []
  let tDecl = T.Enum [] [("Just", optJust), ("Nothing", optNothing)]
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
  S.Expr [] $ E.Call [] (E.Var [] "print") [E.Val [] (E.StrVal [] "hello world")]

tcon :: String -> [Type] -> Type
tcon name types =
  applyTypes (TCon name $ kindN $ length types) types
