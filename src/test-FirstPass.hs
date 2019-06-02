module Main where

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
import UnitTest
  ( Assertion
  , assertEq
  , assertRight
  , assertLeft
  , runTests
  , test )

main = runTests "FirstPass" tests

tests =
  [ test "empty fn body" testCheckReturns1
  , test "unreachable statement" testCheckReturns2
  , test "building struct constructor" testBuildStructConstructor
  , test "building enum constructor" testBuildEnumConstructor
  ]


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

  let firstSch = Scheme [Star, Star] $ makeFuncType [tcon "Pair" [tgenN 1, tgenN 2]] (tgenN 1)
  let secondSch = Scheme [Star, Star] $ makeFuncType [tcon "Pair" [tgenN 1, tgenN 2]] (tgenN 2)
  let fields = [("first", firstSch), ("second", secondSch)]
  let sch = Scheme [Star, Star] (makeFuncType [tgenN 1, tgenN 2] $ tcon "Pair" [tgenN 1, tgenN 2])
  let ctor = Constructor { ctorFields=fields, ctorType=sch }
  let expected = Map.fromList [("Pair", ctor)]
  assertEq (Right expected) result


testBuildEnumConstructor :: Assertion
testBuildEnumConstructor = do
  let tDef = T.TypeDef { T.defAnn=[], T.defName="Maybe", T.defGenerics=["X"] }
  let optJust = [("val", T.TypeName [] "X")]
  let optNothing = []
  let tDecl = T.Enum [] [("Just", optJust), ("Nothing", optNothing)]
  let result = makeConstructors [(tDef, tDecl)] Map.empty


  let schNothing = Scheme [Star] (makeFuncType [] $ tcon "Maybe" [tgenN 1])
  let ctorNothing = Constructor { ctorFields=[], ctorType=schNothing }

  let schVal = Scheme [Star] (makeFuncType [tcon "Maybe" [tgenN 1]] (tgenN 1))
  let fieldsJust = [("val", schVal)]
  let schJust = Scheme [Star] (makeFuncType [tgenN 1] $ tcon "Maybe" [tgenN 1])
  let ctorJust = Constructor { ctorFields=fieldsJust, ctorType=schJust }

  let schMaybe = Scheme [Star] (makeFuncType [] $ tcon "Maybe" [tgenN 1])
  let ctorMaybe = Constructor { ctorFields=[], ctorType=schMaybe }
  let expected = Map.fromList [("Maybe", ctorMaybe), ("Just", ctorJust), ("Nothing", ctorNothing)]

  assertEq (Right expected) result


printStmt =
  S.Expr [] $ E.Call [] (E.Var [] "print") [E.Val [] (E.StrVal [] "hello world")]

tcon :: String -> [Type] -> Type
tcon name types =
  applyTypes (TCon name $ kindN $ length types) types
