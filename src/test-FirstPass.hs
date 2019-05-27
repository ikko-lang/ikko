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

testBuildStructConstructor :: Assertion
testBuildStructConstructor = do
  let tDef = T.TypeDef { T.defAnn=[], T.defName="Pair", T.defGenerics=["A", "B"] }
  let tDecl = T.Struct [] [("first", T.TypeName [] "A"), ("second", T.TypeName [] "B")]
  let result = makeConstructors [(tDef, tDecl)] Map.empty

  let firstSch = Scheme [Star, Star] $ TFunc [TCon "Pair" [TGen 1, TGen 2] Star] (TGen 1) Star
  let secondSch = Scheme [Star, Star] $ TFunc [TCon "Pair" [TGen 1, TGen 2] Star] (TGen 2) Star
  let fields = [("first", firstSch), ("second", secondSch)]
  let sch = Scheme [Star, Star] (TFunc [TGen 1, TGen 2] (TCon "Pair" [TGen 1, TGen 2] Star) Star)
  let k = kindN 2
  let ctor = Constructor { ctorFields=fields, ctorType=sch, ctorKind=k }
  let expected = Map.fromList [("Pair", ctor)]
  assertEq (Right expected) result


testBuildEnumConstructor :: Assertion
testBuildEnumConstructor = do
  let tDef = T.TypeDef { T.defAnn=[], T.defName="Maybe", T.defGenerics=["X"] }
  let optJust = [("val", T.TypeName [] "X")]
  let optNothing = []
  let tDecl = T.Enum [] [("Just", optJust), ("Nothing", optNothing)]
  let result = makeConstructors [(tDef, tDecl)] Map.empty

  let k = kindN 1

  let schNothing = Scheme [Star] (TFunc [] (TCon "Maybe" [TGen 1] Star) Star)
  let ctorNothing = Constructor { ctorFields=[], ctorType=schNothing, ctorKind=k }

  let schVal = Scheme [Star] (TFunc [TCon "Maybe" [TGen 1] Star] (TGen 1) Star)
  let fieldsJust = [("val", schVal)]
  let schJust = Scheme [Star] (TFunc [TGen 1] (TCon "Maybe" [TGen 1] Star) Star)
  let ctorJust = Constructor { ctorFields=fieldsJust, ctorType=schJust, ctorKind=k }

  let schMaybe = Scheme [Star] (TFunc [] (TCon "Maybe" [TGen 1] Star) Star)
  let ctorMaybe = Constructor { ctorFields=[], ctorType=schMaybe, ctorKind=k }
  let expected = Map.fromList [("Maybe", ctorMaybe), ("Just", ctorJust), ("Nothing", ctorNothing)]

  assertEq (Right expected) result


printStmt =
  S.Expr [] $ E.Call [] (E.Var [] "print") [E.Val [] (E.StrVal [] "hello world")]
