module Main where

import Test.HUnit

import qualified Data.Map as Map
import Debug.Trace (trace)

import AST.Annotation (Annotated, Annotation, getType)
import qualified AST.Declaration as D
import qualified AST.Expression as E
import qualified AST.Statement as S
import qualified AST.Type as T

import FirstPass
  ( Module(..)
  , makeClassEnv )

import Types
  ( Substitution
  , Kind(..)
  , Type(..)
  , TyVar(..)
  , Scheme(..)
  , Predicate(..)
  , Qualified(..)
  , QualType
  , applyTypes
  , asScheme
  , composeSubs
  , apply
  , kindN
  , tUnit
  , tInt
  , tFloat
  , tBool
  , tString
  , tUnit
  , makeFuncType
  , makeSub
  , emptySubstitution )

import Inference
  ( mgu
  , startingEnv
  , runInfer
  , inferExpr
  , inferDecl
  , unifies
  , alphaSubstitues
  , makeBindGroup
  , implicitBindings
  , explicitBindings
  , inferModule
  , freshInst
  , splitExplicit
  , getExplicitType
  , InferResult(..)
  , BindGroup(..) )

import Errors
  ( Error(..)
  , Result )


type DeclarationT     = D.Declaration     Annotation
type ExpressionT      = E.Expression      Annotation
type ValueT           = E.Value           Annotation
type MatchCaseT       = S.MatchCase       Annotation
type MatchExpressionT = S.MatchExpression Annotation
type StatementT       = S.Statement       Annotation
type TypeDeclT        = T.TypeDecl        Annotation

assertRight :: Either a b -> Assertion
assertRight (Left _) = assertFailure "expected Right, got Left"
assertRight _        = return ()

assertLeft :: Either a b -> Assertion
assertLeft (Right _) = assertFailure "expected Left, got Right"
assertLeft _         = return ()

assertFalse = assertEqual "" False
assertTrue = assertEqual "" True


main = runTestTT tests

tests :: Test
tests =
  TestList
  [ ts "comparing types" comparingTypes
  , ts "composing substitutions" composingSubs
  , ts "basic unification" basicUnification
  , ts "recursive unification" recursiveUnification
  , ts "getting explicit type" gettingExplicitType
  , ts "instantiation" instantiation
  , ts "expression inference" simpleInference
  , ts "simple functions" functionInference
  , ts "while loop" whileLoop
  , ts "if-else return" ifElseReturn
  , ts "if-then return" ifThenReturn
  , ts "return a-b-c" returnABC
  , ts "return a-b-c 2" returnABC2
  , ts "return a-b" returnAB
  , ts "return a-b end" returnABEnd
  , ts "missing return" missingReturn
  , ts "first class function" firstClassFunction
  , ts "no higher order polymorphism" noHigherOrderPolymorphism
  , ts "infinite type" infiniteType
  , ts "finding dependencies" findDependencies
  , ts "simple module" simpleModule
  , ts "explicit let binding" explicitLetBinding
  , ts "explicitly typed function" explicitFunctionBinding
  ]

ts name assertion = TestLabel name $ TestCase assertion


-- Make should ghat the isAlphaSub function works properly,
-- which the other tests rely on to tell if the thing they test is working.
comparingTypes :: Assertion
comparingTypes = do
  let varA = tvar "a"
  let varB = tvar "b"
  let varX = tvar "x"
  let varY = tvar "y"
  assertTrue $ alphaSubstitues varX varX
  assertTrue $ alphaSubstitues varX varY
  assertTrue $ alphaSubstitues tInt tInt
  -- allows repeated vars
  assertTrue $ alphaSubstitues (makeFuncType [varX, varX] varY) (makeFuncType [varA, varA] varY)
  assertTrue $ alphaSubstitues (makeFuncType [varX, varX] varY) (makeFuncType [varA, varA] varB)
  assertTrue $ alphaSubstitues (tcon "L" [varX]) (tcon "L" [varB])

  -- doesn't allow making types more or less general
  assertFalse $ alphaSubstitues (makeFuncType [varX, varY] varY) (makeFuncType [varA, varA] varA)
  assertFalse $ alphaSubstitues (makeFuncType [varX, varX] varX) (makeFuncType [varA, varB] varB)

  assertFalse $ alphaSubstitues (makeFuncType [varX] varY) (makeFuncType [varA, varA] varY)
  assertFalse $ alphaSubstitues (tcon "L" [varX]) (tcon "L" [varB, varB])
  assertFalse $ alphaSubstitues (tgenN 0) (tgenN 0)
  assertFalse $ alphaSubstitues tInt tBool
  assertFalse $ alphaSubstitues (tvar "x") tInt
  assertFalse $ alphaSubstitues tInt (tvar "x")

composingSubs :: Assertion
composingSubs = do
  -- Try the trivial cases
  assertEqual "" emptySubstitution (composeSubs emptySubstitution emptySubstitution)

  let subAB = makeSub [(tvar "a", tvar "b")]
  assertEqual "" subAB (composeSubs emptySubstitution subAB)
  assertEqual "" subAB (composeSubs subAB emptySubstitution)
  assertEqual "" subAB (composeSubs subAB subAB)

  -- Test updating elements of the other substitution
  let subBC = makeSub [(tvar "b", tvar "c")]
  let subABtoC = makeSub [(tvar "a", tvar "c"), (tvar "b", tvar "c")]
  assertEqual "" subABtoC $ composeSubs subAB subBC

basicUnification :: Assertion
basicUnification = do
  let result1 = mgu tUnit tUnit
  assertEqual "" (Right emptySubstitution) result1

  let result2 = mgu tUnit tInt
  assertLeft result2

  let result3 = mgu (tvar "a") tInt
  assertEqual "" (Right $ makeSub [(tvar "a", tInt)]) result3

  let result4 = mgu (tvar "a") (tvar "a")
  assertEqual "" (Right emptySubstitution) result4

  let result5 = mgu tInt (tvar "x")
  assertEqual "" (Right $ makeSub [(tvar "x", tInt)]) result5

  let result6 = mgu (tvar "a") (tvar "b")
  assertEqual "" (Right $ makeSub [(tvar "a", tvar "b")]) result6

  let result7 = mgu (tvar "a") (makeFuncType [tvar "a"] tInt)
  assertEqual "" (Left $ InfiniteType $ TyVar "a" Star) result7


recursiveUnification :: Assertion
recursiveUnification = do
  let result1 = mgu (makeFuncType [tvar "a"] (tvar "a")) (makeFuncType [tvar "b"] tInt)
  let expected1 = makeSub [(tvar "a", tInt), (tvar "b", tInt)]
  assertEqual "" (Right expected1) result1

  let result2 = mgu (makeFuncType [tvar "a"] (tvar "b")) (makeFuncType [tvar "b"] (tvar "a"))
  let expected2 = makeSub [(tvar "a", tvar "b")]
  assertEqual "" (Right expected2) result2

  let result3 = mgu (makeFuncType [tInt] (tvar "a")) (makeFuncType [tvar "a"] tUnit)
  assertLeft result3


gettingExplicitType :: Assertion
gettingExplicitType = do
  let tdecl = (["A"], T.Function [] [T.TypeName [] "A"] (T.TypeName [] "A"))
  let body = S.Block [] [S.Return [] $ Just $ E.Var [] "a"] 
  let decl = D.Function [] "identity" (Just tdecl) ["a"] body
  let result = inferEmpty $ getExplicitType ("identity", decl)
  let sch = Scheme [Star] (Qual [] (TAp (TAp (TFunc 1 (kindN 2)) (TGen 0 Star)) (TGen 0 Star)))
  let expected = ("identity", sch)
  assertEqual "" (Right expected) result


instantiation :: Assertion
instantiation = do
  assertInstantiates (Scheme [] $ Qual [] tInt) tInt
  assertInstantiates (Scheme [Star] $ Qual [] tInt) tInt
  assertInstantiates (Scheme [Star] $ Qual [] $ tgenN 0) (tvar "a")
  let sch2 = Scheme [Star, Star] (Qual [] $ makeFuncType [tgenN 0, tgenN 1] (tgenN 1))
  let t2 = makeFuncType [tvar "a", tvar "b"] (tvar "b")
  assertInstantiates sch2 t2


simpleInference :: Assertion
simpleInference = do
  let floatExpr = floatVal 123.0
  assertExprTypes tFloat floatExpr

  let intExpr = intVal 123
  assertExprTypesP intExpr (TVar (TyVar "_v0" Star)) [Pred "Num" $ TVar (TyVar "_v0" Star)]

  let lessExpr = E.Binary [] E.Less (intVal 5) (intVal 6)
  assertExprTypes tBool lessExpr

  let notExpr = E.Unary [] E.BoolNot (boolVal True)
  assertExprTypes tBool notExpr

  let parenExpr = E.Paren [] $ strVal "foo"
  assertExprTypes tString parenExpr

  let undefinedVar = E.Var [] "bad var"
  assertExprFails undefinedVar

  let badComparison = E.Binary [] E.Less (floatVal 5) (strVal "bar")
  assertExprFails badComparison

  -- 3()
  let badCall = E.Call [] (floatVal 3) []
  assertExprFails badCall

  -- 3(5)
  let badCall2 = E.Call [] (floatVal 3) [intVal 5]
  assertExprFails badCall2


functionInference :: Assertion
functionInference = do
  -- shared definitions
  let varX = E.Var [] "x"

  -- f() { }
  let func0 = func "f" [] []
  let type0 = Qual [] $ makeFuncType [] tUnit
  assertDeclTypes type0 func0

  -- f() { return 1.0; }
  let func1 = func "f" [] [returnJust $ floatVal 1]
  let type1 = Qual [] $ makeFuncType [] tFloat
  assertDeclTypes type1 func1

  -- f(x) { return 1.0; }
  let func2 = func "f" ["x"] [returnJust $ floatVal 1]
  let type2 = Qual [] $ makeFuncType [tvar "a"] tFloat
  assertDeclTypes type2 func2

  -- f(x) { return x; }
  let func3 = func "f" ["x"] [returnJust varX]
  let type3 = Qual [] $ makeFuncType [tvar "a"] (tvar "a")
  assertDeclTypes type3 func3

  -- f(x) { return x + 1.0; }
  let func4 = func "f" ["x"] [returnJust $ E.Binary [] E.Plus varX (floatVal 1)]
  let type4 = Qual [] $ makeFuncType [tFloat] tFloat
  assertDeclTypes type4 func4

  -- f(x) { return x > 123.0; }
  let func5 = func "f" ["x"] [returnJust $ E.Binary [] E.Less varX (floatVal 123.0)]
  let type5 = Qual [] $ makeFuncType [tFloat] tBool
  assertDeclTypes type5 func5

  -- f(x) { return x && True; }
  let funcBool = func "f" ["x"] [returnJust $ E.Binary [] E.BoolAnd varX (boolVal True)]
  let typeBool = Qual [] $ makeFuncType [tBool] tBool
  assertDeclTypes typeBool funcBool

  -- f(x) { let y = x; return y; }
  let letStmt = S.Let [] "y" Nothing (E.Var [] "x")
  let returnStmt = returnJust (E.Var [] "y")
  let funcLet = func "f" ["x"] [letStmt, returnStmt]
  let idType = Qual [] $ makeFuncType [tvar "a"] (tvar "a")
  assertDeclTypes idType funcLet

  -- TODO: Test assignment


whileLoop :: Assertion
whileLoop = do
  -- f(y) = let a = 1.0; while a < y { a = a * 2.0 }; return a
  let aTo1 = S.Let [] "a" Nothing (floatVal 1)
  let aLessY = E.Binary [] E.Less (E.Var [] "a") (E.Var [] "y")
  let aTimes2 = E.Binary [] E.Times (E.Var [] "a") (floatVal 2)
  let whileBody = S.Assign [] ["a"] aTimes2
  let while = S.While [] aLessY [whileBody]
  let returnA = returnJust $ E.Var [] "a"
  let func6 = func "f" ["y"] [aTo1, while, returnA]
  let type6 = Qual [] $ makeFuncType [tFloat] tFloat
  assertDeclTypes type6 func6


ifElseReturn :: Assertion
ifElseReturn = do
  -- f(x, y) = if x > y { return x; } else { return y; }
  let test = E.Binary [] E.Greater (E.Var [] "x") (E.Var [] "y")
  let returnX = returnJust $ E.Var [] "x"
  let returnY = returnJust $ E.Var [] "y"
  let ifStmt = S.If [] test [returnX] (Just returnY)
  let func7 = func "f" ["x", "y"] [ifStmt]
  let tvar = TVar (TyVar "_v4" Star)
  let type7 = Qual [Pred "Ord" tvar] $ makeFuncType [tvar, tvar] tvar
  assertDeclTypes type7 func7


ifThenReturn :: Assertion
ifThenReturn = do
  -- f(x, y) = if x > y { return x; }; return y;
  let test = E.Binary [] E.Greater (E.Var [] "x") (E.Var [] "y")
  let returnX = returnJust $ E.Var [] "x"
  let ifStmt = S.If [] test [returnX] Nothing
  let returnY = returnJust $ E.Var [] "y"
  let func8 = func "f" ["x", "y"] [ifStmt, returnY]
  let tvar = TVar (TyVar "_v4" Star)
  let type8 = Qual [Pred "Ord" tvar] $ makeFuncType [tvar, tvar] tvar
  assertDeclTypes type8 func8


returnABC :: Assertion
returnABC = do
  -- f(a, b, c) = if a { return b; } else { return c; }
  let returnB = returnJust $ E.Var [] "b"
  let returnC = returnJust $ E.Var [] "c"
  let ifStmt = S.If [] (E.Var [] "a") [returnB] (Just returnC)
  let func9 = func "f" ["a", "b", "c"] [ifStmt]
  let type9 = Qual [] $ makeFuncType [tBool, tvar "a", tvar "a"] (tvar "a")
  assertDeclTypes type9 func9


returnABC2 :: Assertion
returnABC2 = do
  -- f(a, b, c) = if a { return b; } return c;
  let returnB = returnJust $ E.Var [] "b"
  let returnC = returnJust $ E.Var [] "c"
  let ifStmt = S.If [] (E.Var [] "a") [returnB] Nothing
  let func9 = func "f" ["a", "b", "c"] [ifStmt, returnC]
  let type9 = Qual [] $ makeFuncType [tBool, tvar "a", tvar "a"] (tvar "a")
  assertDeclTypes type9 func9


returnAB :: Assertion
returnAB = do
  -- fn(a, b) = if a { return b; } else { return b; }
  let returnB = returnJust $ E.Var [] "b"
  let ifStmt = S.If [] (E.Var [] "a") [returnB] (Just returnB)
  let funcAB = func "f" ["a", "b"] [ifStmt]
  let typeAB = Qual [] $ makeFuncType [tBool, tvar "a"] (tvar "a")
  assertDeclTypes typeAB funcAB

returnABEnd :: Assertion
returnABEnd = do
  -- fn(a, b) = if a { return b; } return b;
  let returnB = returnJust $ E.Var [] "b"
  let ifStmt = S.If [] (E.Var [] "a") [returnB] Nothing
  let funcAB = func "f" ["a", "b"] [ifStmt, returnB]
  let typeAB = Qual [] $ makeFuncType [tBool, tvar "a"] (tvar "a")
  assertDeclTypes typeAB funcAB


missingReturn :: Assertion
missingReturn = do
  -- f(x, y) = if x > y { return x; }
  let returnX = returnJust $ E.Var [] "x"
  let test = E.Binary [] E.Greater (E.Var [] "x") (E.Var [] "y")
  let ifStmt = S.If [] test [returnX] Nothing
  assertDeclFails $ func "f" ["x", "y"] [ifStmt]


firstClassFunction :: Assertion
firstClassFunction = do
  -- f(x, y) = { return x(y, y); }
  let varX = E.Var [] "x"
  let varY = E.Var [] "y"
  let call = E.Call [] varX [varY, varY]
  let f = func "f" ["x", "y"] [returnJust call]
  -- (a -> a -> b)
  let xType = makeFuncType [tvar "a", tvar "a"] (tvar "b")
  -- (a -> a -> b) -> a -> b
  let t = Qual [] $ makeFuncType [xType, tvar "a"] (tvar "b")
  assertDeclTypes t f


noHigherOrderPolymorphism :: Assertion
noHigherOrderPolymorphism = do
  -- f(x) { return x(x(1) > 2); }
  let varX = E.Var [] "x"
  let innerCall = E.Call [] varX [intVal 1]
  let comparison = E.Binary [] E.Greater innerCall (intVal 2)
  let call = E.Call [] varX [comparison]
  let f = func "f" ["x"] [returnJust call]
  assertDeclFails f


infiniteType :: Assertion
infiniteType = do
  -- f(x) { return x(x); }
  let varX = E.Var [] "x"
  let call = E.Call [] varX [varX]
  let f = func "f" ["x"] [returnJust call]
  assertDeclFails f


findDependencies :: Assertion
findDependencies = do
  let varX = E.Var [] "x"
  let varF = E.Var [] "f"
  let varG = E.Var [] "g"

  -- f(x) { return g(x); }
  -- g(x) { return x; }
  let fCallsG = func "f" ["x"] [returnJust $ E.Call [] varG [varX]]
  let g = func "g" ["x"] [returnJust varX]
  assertEqual "" [["g"], ["f"]] (findGroups [("f", fCallsG), ("g", g)])

  -- f(x) { return g(x); }
  -- g(x) { return f(x); }
  -- h() { return g; }
  let gCallsF = func "g" ["x"] [returnJust $ E.Call [] varF [varX]]
  let hReturnsG = func "h" [] [returnJust varG]
  let bindings2 = [("f", fCallsG), ("g", gCallsF), ("h", hReturnsG)]
  assertEqual "" [["g", "f"], ["h"]] (findGroups bindings2)

  -- f(x Int) Int { return g(x); }
  -- g(x) { return f(x); }
  -- h() { return g; }
  let intName = T.TypeName [] "Int"
  let typeAnnotation = Just ([],T.Function [] [intName] intName)
  let fExpl = D.Function [] "f" typeAnnotation ["x"] (returnJust $ E.Call [] varG [varX])
  let bindings3 = [("f", fExpl), ("g", gCallsF), ("h", hReturnsG)]
  let (di, de) = splitExplicit $ Map.fromList bindings3
  assertEqual "" ["g", "h"] $ Map.keys di
  assertEqual "" ["f"] $ Map.keys de
  assertEqual "" [["g"], ["h"]] (findGroups bindings3)
  assertEqual "" ["f"] $ map fst $ explicitBindings $ makeBindGroup $ makeModule bindings3

  -- TODO: Test more deeply nested AST
  -- TODO: Test larger call graphs w/ longer cycles
  -- TODO: Test shadowing via arg names
  -- TODO: Test shadowing via let statements


simpleModule :: Assertion
simpleModule = do
  let varF = E.Var [] "f"
  let varN = E.Var [] "n"
  let varX = E.Var [] "x"
  let varID = E.Var [] "id"

  -- Test a super basic module
  -- f(n) { return n + 1; }
  let nPlus1 = func "f" ["n"] [returnJust $ E.Binary [] E.Plus varN (intVal 1)]
  let result1 = inferModule $ makeModule [("f", nPlus1)]
  let intFn = Scheme [Star] $ Qual [Pred "Num" $ tgenN 0] $ makeFuncType [tgenN 0] (tgenN 0)
  assertModuleTypes "f" intFn result1

  -- Test basic let-polymorphism
  -- id(x) { return x; }
  let identity = func "id" ["x"] [returnJust varX]
  let result2 = inferModule $ makeModule [("id", identity)]
  let idType = Scheme [Star] $ Qual [] $ makeFuncType [tgenN 0] (tgenN 0)
  -- assertModuleTypes "id" idType result2

  -- Test usage of let-polymorphism
  -- id(x) { return x; }
  -- f(n) { return id(n > id(3)); }
  let id3 = E.Call [] varID [intVal 3]
  let idExpr = E.Call [] varID [E.Binary [] E.Greater varN id3]
  let fN = func "f" ["n"] [returnJust idExpr]
  let result3 = inferModule $ makeModule [("f", fN), ("id", identity)]
  let fNType = Scheme [] $ Qual []  $ makeFuncType [tInt] tBool
  -- assertModuleTypes "f" fNType result3
  -- assertModuleTypes "id" idType result3

  -- Test the fact that mutually-recursive functions
  -- are sometimes less general than you'd expect
  -- id(x) { f(1); return x; }
  -- f(x) { return id(x) > 2; }
  let callF = S.Expr [] $ E.Call [] varF [intVal 1]
  let identityCallingF = func "id" ["x"] [callF, returnJust varX]
  let idOfX = E.Call [] varID [varX]
  let fCallsID = func "f" ["x"] [returnJust $ E.Binary [] E.Greater idOfX (intVal 2)]
  let result4 = inferModule $ makeModule [("f", fCallsID), ("id", identityCallingF)]
  let lessGeneralIDType = Scheme [] $ Qual []  $ makeFuncType [tInt] tInt
  let fCallsIDType = Scheme [] $ Qual []  $ makeFuncType [tInt] tBool
  -- assertModuleTypes "f" fCallsIDType result4
  -- assertModuleTypes "id" lessGeneralIDType result4
  return ()


explicitLetBinding :: Assertion
explicitLetBinding = do
  -- func(x) { let y Int = x; return y; }
  -- should type as Int -> Int
  let typeAnnotation = Just $ T.TypeName [] "Int"
  let letStmt = S.Let [] "y" typeAnnotation (E.Var [] "x")
  let returnStmt = returnJust (E.Var [] "y")
  let funcLet = func "f" ["x"] [letStmt, returnStmt]
  let fnType = Qual [] $ makeFuncType [tInt] tInt
  assertDeclTypes fnType funcLet


explicitFunctionBinding :: Assertion
explicitFunctionBinding = do
  -- func(x Int) Int { return x }
  let intName = T.TypeName [] "Int"
  let typeAnnotation = Just ([], T.Function [] [intName] intName)
  let returnStmt = returnJust (E.Var [] "x")
  let funcInts = D.Function [] "f" typeAnnotation ["x"] returnStmt
  let fnType = asScheme $ makeFuncType [tInt] tInt
  let result = inferModule $ makeModule  [("f", funcInts)]
  assertModuleTypes "f" fnType result

  -- This should be rejected because the return type is actually Int
  -- func(x Int) Bool { return x }
  let boolName = T.TypeName [] "Bool"
  let type2 = Just ([], T.Function [] [intName] boolName)
  let funcWrongType = D.Function [] "f" type2 ["x"] returnStmt
  assertLeft $ inferModule $ makeModule  [("f", funcWrongType)]

-- TODO: Test that explicitly typed bindings break cycles

assertModuleTypes :: String -> Scheme -> Result InferResult -> Assertion
assertModuleTypes name sch result = case result of
  Left msg          -> assertFailure $ "failed to infer type for module: " ++ show msg
  Right inferResult ->
    case Map.lookup name $ topLevelEnv inferResult of
     Nothing        -> assertFailure $ "can't find " ++ name
     Just resultSch -> assertSchemeUnifies sch resultSch


makeModule :: [(String, DeclarationT)] -> Module
makeModule bindings =
  let bindMap = Map.fromList bindings
  in Module
     { bindings=bindMap
     , constructors=Map.empty
     , classEnv=makeClassEnv }


findGroups :: [(String, DeclarationT)] -> [[String]]
findGroups bindings =
  getGroupNames $ makeBindGroup $ makeModule bindings


getGroupNames :: BindGroup -> [[String]]
getGroupNames bg = map (map fst) (implicitBindings bg)


returnJust expr = S.Return [] (Just expr)


assertExprTypes :: Type -> ExpressionT -> Assertion
assertExprTypes t expr = assertTypes3 t expr inferExpr

assertExprTypesP :: ExpressionT -> Type -> [Predicate] -> Assertion
assertExprTypesP expr t ps = assertTypesP t ps expr inferExpr


assertExprFails :: ExpressionT -> Assertion
assertExprFails expr = assertFails expr inferExpr


assertDeclTypes :: QualType -> DeclarationT -> Assertion
assertDeclTypes qt decl = assertTypes2 qt decl inferDecl


assertDeclFails :: DeclarationT -> Assertion
assertDeclFails decl = assertFails decl inferDecl

assertTypesP t ps ast inferFn = do
  let result = inferEmpty $ inferFn startingEnv ast
  assertRight result
  let (Right (_, resultT, resultPS)) = result
  assertMatches t resultT
  assertEqual "" ps resultPS


assertTypes3 t ast inferFn = do
  let result = inferEmpty $ inferFn startingEnv ast
  assertRight result
  let (Right (typed, _, _)) = result
  let (Just resultType) = getType typed
  assertMatches t resultType

assertTypes2 (Qual ps t) ast inferFn = do
  let result = inferEmpty $ inferFn startingEnv ast
  assertRight result
  let (Right (typed, resultPS)) = result
  let (Just resultType) = getType typed
  assertMatches t resultType
  assertEqual "" ps resultPS


assertTypes t ast inferFn = do
  let result = inferEmpty $ inferFn startingEnv ast
  assertRight result
  let (Right typed) = result
  let (Just resultType) = getType typed
  assertMatches t resultType


assertFails ast inferFn = do
  let result = inferEmpty $ inferFn startingEnv ast
  assertLeft result


-- TODO: This should really be "assert matches" not "assert unifies"
-- so that it doesn't allow narrower types than it should.
assertMatches :: Type -> Type -> Assertion
assertMatches expected result = do
  assertNoGenerics expected
  assertNoGenerics result
  let message = "expected\n  " ++ show result ++
                "\nto be equivalent to\n  " ++ show expected ++ "\n"
  assertEqual message True (alphaSubstitues expected result)


assertInstantiates :: Scheme -> Type -> Assertion
assertInstantiates sch = assertMatches (runInstantiate sch)


runInstantiate :: Scheme -> Type
runInstantiate sch =
  let (Right (t, _)) = inferEmpty (freshInst sch)
  in t

inferEmpty = runInfer Map.empty makeClassEnv


assertNoGenerics :: Type -> Assertion
assertNoGenerics t =
  let message = "expected the type (" ++ show t ++ ") to not contain generics"
  in assertEqual message True (not $ containsGenerics t)


containsGenerics :: Type -> Bool
containsGenerics t = case t of
  TCon _ _  -> False
  TFunc _ _ -> False
  TAp a b   -> any containsGenerics [a, b]
  TVar _    -> False
  TGen _ _  -> True


-- expected, result
assertSchemeUnifies :: Scheme -> Scheme -> Assertion
assertSchemeUnifies s1@(Scheme n1 _) s2@(Scheme n2 _) = do
  assertEqual "" n1 n2
  let (Qual ps1 t1) = testInstantiate s1
  let (Qual ps2 t2) = testInstantiate s2
  assertEqual "" ps1 ps2
  assertMatches t1 t2


-- testInstantiate instantiates without the InferM monad available
testInstantiate :: Scheme -> QualType
testInstantiate (Scheme kinds qt) =
  let n = length kinds
      range = [0..n-1]
      newVars = map TVar $ zipWith TyVar ["-t" ++ show i | i <- range] kinds
      genVars = zipWith TGen range kinds
      sub = Map.fromList $ zip genVars newVars
  in apply sub qt


floatVal :: Float -> ExpressionT
floatVal n = E.Val [] $ E.FloatVal [] n


intVal :: Int -> ExpressionT
intVal n = E.Val [] $ E.IntVal [] n


strVal :: String -> ExpressionT
strVal s = E.Val [] $ E.StrVal [] s


boolVal :: Bool -> ExpressionT
boolVal b = E.Val [] $ E.BoolVal [] b


func :: String -> [String] -> [StatementT] -> DeclarationT
func name args stmts =
  let fnbody = S.Block [] stmts
  in D.Function [] name Nothing args fnbody


tcon :: String -> [Type] -> Type
tcon name types =
  applyTypes (TCon name $ kindN $ length types) types


tgenN :: Int -> Type
tgenN n = TGen n Star

tvar :: String -> Type
tvar name = TVar $ TyVar name Star
-- TODO
-- Test inference for DAGs of functions
---- let id x = x in (id id) 123
---- let f y = y + 1 in g x = f x
-- Test inference for cyclic functions
-- Test all pairwise combinations of syntax (e.g. trying to call 3 as a function)
-- Test structures (including field access and field update)
-- Test enums and match statements
