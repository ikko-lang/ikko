module Main where


import Data.Functor.Identity (runIdentity)

import Text.Parsec (eof, ParseError)
import Text.Parsec.Indent

import AST.Annotation (Annotated, Annotation, removeAnnotations)
import qualified AST
import Parser

import Test.HUnit


type DeclarationT     = AST.Declaration     Annotation
type ExpressionT      = AST.Expression      Annotation
type ValueT           = AST.Value           Annotation
type MatchCaseT       = AST.MatchCase       Annotation
type MatchExpressionT = AST.MatchExpression Annotation
type StatementT       = AST.Statement       Annotation
type TypeDeclT        = AST.TypeDecl        Annotation


main = runTestTT tests

boolT :: TypeDeclT
boolT = AST.TName [] "Bool"
intT :: TypeDeclT
intT = AST.TName [] "Int"
stringT :: TypeDeclT
stringT = AST.TName [] "String"
nilT :: TypeDeclT
nilT = AST.TName [] "()"

type Val = AST.Value Annotation
type Expr = AST.Expression Annotation
type Stmt = AST.Statement Annotation

floatVal :: Float -> Val
floatVal = AST.FloatVal []

intVal :: Int -> Val
intVal = AST.IntVal []

boolVal :: Bool -> Val
boolVal = AST.BoolVal []

strVal :: String -> Val
strVal = AST.StrVal []

eVal :: Val -> Expr
eVal = AST.Val []

eVar :: String -> Expr
eVar = AST.Var []

eCall :: Expr -> [Expr] -> Expr
eCall = AST.Call []

eBinary :: AST.BinOp -> Expr -> Expr -> Expr
eBinary = AST.Binary []

sLet :: String -> Expr -> Stmt
sLet name = AST.Let [] name Nothing

sLetT :: String -> TypeDeclT -> Expr -> Stmt
sLetT name t = AST.Let [] name (Just t)

sBlock :: [Stmt] -> Stmt
sBlock = AST.Block []

sReturn :: Maybe Expr -> Stmt
sReturn = AST.Return []

sAssign :: [String] -> Expr -> Stmt
sAssign = AST.Assign []

tests :: Test
tests =
  -- expressions
  TestList
  [ expectParsesA numberParser "123.345" (floatVal 123.345)
  , expectParsesA valueParser "123.345" (floatVal 123.345)
  , expectParsesA expressionParser "123.345" (eVal (floatVal 123.345))
  , expectParsesA expressionParser "-10" (eVal (intVal (-10)))
  , expectParsesA expressionParser "a123" (eVar "a123")
  , expectParsesA expressionParser "f(a)" (eCall (eVar "f") [eVar "a"])
  , expectParsesA expressionParser "f(a, b , c )"
    (eCall (eVar "f") [eVar "a", eVar "b", eVar "c"])
  , expectParsesA expressionParser "Bool(a)" (AST.Cast [] "Bool" (eVar "a"))
  , expectParsesA expressionParser "2 + 3"
    (eBinary AST.Plus (eVal (intVal 2)) (eVal (intVal 3)))
  , expectParsesA expressionParser "(2 + 3)"
    (AST.Paren [] (eBinary AST.Plus (eVal (intVal 2)) (eVal (intVal 3))))
  , expectParsesA expressionParser "a || b"
    (eBinary AST.BoolOr (eVar "a") (eVar "b"))
  , expectParsesA expressionParser "Point{\nx: 123,\ny: 45, \n}"
    (eVal $ AST.StructVal [] "Point"
     [("x", eVal $ intVal 123), ("y", eVal $ intVal 45)])
  , expectParsesA expressionParser "int ** 3"
    (eBinary AST.Power (eVar "int") (eVal (intVal 3)))
  , expectParsesA expressionParser "\"a quoted \\\"string\\\" \""
    (eVal (strVal "a quoted \"string\" "))
  , expectParsesA expressionParser "!False"
    (AST.Unary [] AST.BoolNot (eVal (boolVal False)))
  , expectParsesA expressionParser "foo.bar"
    (AST.Access [] (eVar "foo") "bar")
  , expectParsesA expressionParser "foo.bar.baz"
    (AST.Access [] (AST.Access [] (eVar "foo") "bar") "baz")
  , expectParsesA expressionParser "1 + 2 * 3 + 4"
    -- evaluation order: 2*3, then 1+(2*3), then (1+(2*3))+4
    (eBinary AST.Plus
      (eBinary AST.Plus
       (eVal $ intVal 1)
       (eBinary AST.Times (eVal $ intVal 2) (eVal $ intVal 3)))
      (eVal $ intVal 4))
  , expectParsesA expressionParser "10 + -5"
    (eBinary AST.Plus (eVal $ intVal 10) (eVal $ intVal (-5)))
  , expectParsesA expressionParser "10 - 3 - 4"
    (eBinary AST.Minus
     (eBinary AST.Minus (eVal $ intVal 10) (eVal $ intVal 3))
     (eVal $ intVal 4))
  , expectParsesA expressionParser "String(10 + -5)"
    (AST.Cast [] "String" (eBinary AST.Plus (eVal $ intVal 10) (eVal $ intVal (-5))))
  , expectParsesA expressionParser "1 == 1 && 2 < 3"
    (eBinary AST.BoolAnd
     (eBinary AST.Eq (eVal (intVal 1)) (eVal (intVal 1)))
     (eBinary AST.Less (eVal (intVal 2)) (eVal (intVal 3))))
  , expectParses typeNameParser "Int" "Int"
  , expectParsesA (withPos typeDefParser) "struct:\n  a Int\n  b String\n"
    (AST.TStruct [] [("a", intT), ("b", stringT)])
  , testEnumType
  , testEnumType2

    -- statements
  , expectParsesA statementParser "return \"foo\""
    (sReturn $ Just $ eVal $ strVal "foo")
  , expectParsesA statementParser "print(String(10 + -5))"
    (AST.Expr [] $ AST.Call []
     (AST.Var [] "print") [AST.Cast [] "String"
                         (eBinary AST.Plus
                          (eVal $ intVal 10)
                          (eVal $ intVal (-5)))])
  , expectParses statementSep "\n" ()
  , expectParses statementSep "  \n  " ()
  , expectParses statementSep "  \n\n  \n  " ()
  , expectParsesA statementParser "pass" (AST.Pass [])
  , expectParsesA statementParser "return 1\n"
    (sReturn $ Just $ eVal $ intVal 1)
  , expectParsesA statementParser "let a123 = True"
    (sLet "a123" (eVal $ boolVal True))
  , expectParsesA typeDefParser "Bool"
    (AST.TName [] "Bool")
  , expectParsesA typeDefParser "Pair<Int, Int>"
    (AST.TGeneric [] "Pair" [AST.TName [] "Int" | _ <- [1,2]])
  , expectParsesA statementParser "let a123 Bool = True"
    (sLetT "a123" (AST.TName [] "Bool") (eVal $ boolVal True))
  , expectParsesA statementParser "let x_y Pair<Int, Int> = foo()"
    (sLetT "x_y"
     (AST.TGeneric [] "Pair" [AST.TName [] "Int" | _ <- [1,2]])
     (AST.Call [] (AST.Var [] "foo") []))
  , expectParsesA statementParser "a.b.c = True"
    (sAssign ["a", "b", "c"] (eVal $ boolVal True))
  , expectParsesA statementParser "print(c)"
    (AST.Expr [] $ eCall (eVar "print") [eVar "c"])

  , expectParsesA letStatement "let int = 5 + (2 * 10) / 3 % 4"
    (sLet "int"
     (eBinary AST.Plus
       (eVal $ intVal 5)
       (eBinary AST.Mod
         (eBinary AST.Divide
           (AST.Paren []
            (eBinary AST.Times
             (eVal (intVal 2))
             (eVal (intVal 10))))
           (eVal $ intVal 3))
         (eVal $ intVal 4))))

  , expectParsesA assignStatement "int = 3"
    (sAssign ["int"] $ eVal $ intVal 3)
  , expectParsesA assignStatement "int = int ** 3"
    (sAssign ["int"] $ eBinary AST.Power (eVar "int") (eVal $ intVal 3))

  -- blocks and larger
  , testParsingBlock
  , testParsingIf
  , testParsingMatch
  , testParsingFunc
  , testParsingFunc2
  , testParsingTypedFunction
  , testParsingTypeDecl
  , testParsingLambda
  , testParsingLambdaStatement
  , testParsingLambdaInFunction
  ]

testEnumType :: Test
testEnumType =
  let text = "enum:\n Cons:\n  item Int\n  next List\n End\n"
      expected = AST.TEnum [] [ ("Cons", [("item", intT), ("next", AST.TName [] "List")])
                           , ("End", []) ]
  in expectParsesA typeDefParser text expected

testEnumType2 :: Test
testEnumType2 =
  let text = "enum:\n IntT\n FloatT\n"
      expected = AST.TEnum [] [ ("IntT", [])
                              , ("FloatT", []) ]
  in expectParsesA typeDefParser text expected

testParsingBlock :: Test
testParsingBlock =
  let text = "\n  let a1 = True\n  return a1  \n"
      expected = sBlock [ sLet "a1" (eVal $ boolVal True)
                         , sReturn (Just $ eVar "a1")
                         ]
  in expectParsesA blockStatement text expected

testParsingIf :: Test
testParsingIf =
  let text = "if a == 1:\n    return a"
      test = eBinary AST.Eq (eVar "a") (eVal $ intVal 1)
      body = [sReturn $ Just $ eVar "a"]
      expected = AST.If [] test body Nothing
  in expectParsesA ifStatement text expected

testParsingMatch :: Test
testParsingMatch =
  let text = "match x:\n  _:\n    return 1\n  Link(_, next):\n    return 2"
      ret n = sBlock [sReturn $ Just $ eVal $ intVal n]
      case1 = AST.MatchCase (AST.MatchAnything []) (ret 1)
      case2 = AST.MatchCase (AST.MatchStructure [] "Link" [AST.MatchAnything [], AST.MatchVariable [] "next"]) (ret 2)
      expected = AST.Match [] (eVar "x") [case1, case2]
  in expectParsesA statementParser text expected

testParsingFunc :: Test
testParsingFunc =
  let text = "fn main():\n  pass"
      expected = AST.DFunction [] "main" Nothing [] (sBlock [AST.Pass []])
  in expectParsesA declarationParser text expected

testParsingFunc2 :: Test
testParsingFunc2 =
  let text = "fn main(a, b):\n  //a comment\n  pass"
      expected = AST.DFunction [] "main" Nothing ["a", "b"] (sBlock [AST.Pass []])
  in expectParsesA declarationParser text expected

testParsingTypedFunction :: Test
testParsingTypedFunction =
  let text = "fn main(a Int, b Bool) Bool:\n//a comment\n  pass"
      fnType = Just $ AST.TFunction [] [] [intT, boolT] boolT
      expected = AST.DFunction [] "main" fnType ["a", "b"] (sBlock [AST.Pass []])
  in expectParsesA declarationParser text expected

testParsingTypeDecl :: Test
testParsingTypeDecl =
  let text = "type Foo struct:\n  asdf Int\n  xyz Foo\n"
      declaredType = AST.TStruct [] [("asdf", intT), ("xyz", AST.TName [] "Foo")]
      def = AST.TypeDef [] "Foo" []
      expected = AST.DTypeDef [] def declaredType
  in expectParsesA declarationParser text expected

testParsingLambda :: Test
testParsingLambda =
  let text = "fn(a):\n  return a\n"
      returnStmt = sReturn $ Just $ eVar "a"
      body = AST.Block [] [returnStmt]
      expected = AST.Lambda [] ["a"] body
  in expectParsesA expr text expected

testParsingLambdaStatement :: Test
testParsingLambdaStatement =
  let text = "let foo = fn(a):\n  return a\n"
      returnStmt = sReturn $ Just $ eVar "a"
      body = AST.Block [] [returnStmt]
      lambda = AST.Lambda [] ["a"] body
      expected = AST.Let [] "foo" Nothing lambda
  in expectParsesA (withPos statementParser) text expected

testParsingLambdaInFunction :: Test
testParsingLambdaInFunction =
  let text = "fn foo():\n  let f = fn(a):\n    return a\n  return f\n"
      body = AST.Block [] [sReturn $ Just $ eVar "a"]
      lambda = AST.Lambda [] ["a"] body
      letStmt = AST.Let [] "f" Nothing lambda
      returnStmt = sReturn $ Just $ eVar "f"
      expected = AST.DFunction [] "foo" Nothing [] (sBlock [letStmt, returnStmt])
  in expectParsesA declarationParser text expected

---- Utilities ----

expectParsesA ::
  (Eq (a Annotation), Show (a Annotation), Annotated a) =>
  Parser (a Annotation) ->
  String ->
  a Annotation ->
  Test
expectParsesA = expectParses' removeAnnotations

expectParses :: (Eq a, Show a) => Parser a -> String -> a -> Test
expectParses = expectParses' id

expectParses' :: (Eq a, Show a) => (a -> a) -> Parser a -> String -> a -> Test
expectParses' postprocess parser text expected =
  TestCase $
  case parse (parser <* anyWhitespace <* eof) text of
   (Left err) ->
     let quoted = if length (lines text) > 1
                  then "''\n" ++ text ++ "''"
                  else "''" ++ text ++ "''"
     in assertFailure $  "failed parsing " ++ quoted ++ ", error parsing (" ++ show err ++ ")"
   (Right result) ->
     let message =
           concat 
             [ "failed parsing ''"
             , text
             , "''\n=== expected: ===\n  "
             , show expected
             , "\n=== got ===\n  "
             , show result ]
     in assertEqual message expected (postprocess result)

parse :: Parser a -> String -> Either ParseError a
parse parser text = runIdentity $ runIndentParserT parser () "<test>" text
