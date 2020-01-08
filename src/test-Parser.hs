module Main where


import Data.Functor.Identity (runIdentity)

import Text.Parsec (eof, ParseError)
import Text.Parsec.Indent

import AST.Annotation (Annotated, Annotation, removeAnnotations)
import qualified AST.Declaration as D
import qualified AST.Expression as E
import qualified AST.Statement as S
import qualified AST.Type as T
import Parser

import Test.HUnit


type DeclarationT     = D.Declaration     Annotation
type ExpressionT      = E.Expression      Annotation
type ValueT           = E.Value           Annotation
type MatchCaseT       = S.MatchCase       Annotation
type MatchExpressionT = S.MatchExpression Annotation
type StatementT       = S.Statement       Annotation
type TypeDeclT        = T.TypeDecl        Annotation


main = runTestTT tests

boolT :: TypeDeclT
boolT = T.TypeName [] "Bool"
intT :: TypeDeclT
intT = T.TypeName [] "Int"
stringT :: TypeDeclT
stringT = T.TypeName [] "String"
nilT :: TypeDeclT
nilT = T.TypeName [] "()"

type Val = E.Value Annotation
type Expr = E.Expression Annotation
type Stmt = S.Statement Annotation

floatVal :: Float -> Val
floatVal = E.FloatVal []

intVal :: Int -> Val
intVal = E.IntVal []

boolVal :: Bool -> Val
boolVal = E.BoolVal []

strVal :: String -> Val
strVal = E.StrVal []

eVal :: Val -> Expr
eVal = E.Val []

eVar :: String -> Expr
eVar = E.Var []

eCall :: Expr -> [Expr] -> Expr
eCall = E.Call []

eBinary :: E.BinOp -> Expr -> Expr -> Expr
eBinary = E.Binary []

sLet :: String -> Expr -> Stmt
sLet name = S.Let [] name Nothing

sLetT :: String -> TypeDeclT -> Expr -> Stmt
sLetT name t = S.Let [] name (Just t)

sBlock :: [Stmt] -> Stmt
sBlock = S.Block []

sReturn :: Maybe Expr -> Stmt
sReturn = S.Return []

sAssign :: [String] -> Expr -> Stmt
sAssign = S.Assign []

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
  , expectParsesA expressionParser "Bool(a)" (E.Cast [] "Bool" (eVar "a"))
  , expectParsesA expressionParser "2 + 3"
    (eBinary E.Plus (eVal (intVal 2)) (eVal (intVal 3)))
  , expectParsesA expressionParser "(2 + 3)"
    (E.Paren [] (eBinary E.Plus (eVal (intVal 2)) (eVal (intVal 3))))
  , expectParsesA expressionParser "a || b"
    (eBinary E.BoolOr (eVar "a") (eVar "b"))
  , expectParsesA expressionParser "Point{\nx: 123,\ny: 45, \n}"
    (eVal $ E.StructVal [] "Point"
     [("x", eVal $ intVal 123), ("y", eVal $ intVal 45)])
  , expectParsesA expressionParser "int ** 3"
    (eBinary E.Power (eVar "int") (eVal (intVal 3)))
  , expectParsesA expressionParser "\"a quoted \\\"string\\\" \""
    (eVal (strVal "a quoted \"string\" "))
  , expectParsesA expressionParser "!False"
    (E.Unary [] E.BoolNot (eVal (boolVal False)))
  , expectParsesA expressionParser "foo.bar"
    (E.Access [] (eVar "foo") "bar")
  , expectParsesA expressionParser "foo.bar.baz"
    (E.Access [] (E.Access [] (eVar "foo") "bar") "baz")
  , expectParsesA expressionParser "1 + 2 * 3 + 4"
    -- evaluation order: 2*3, then 1+(2*3), then (1+(2*3))+4
    (eBinary E.Plus
      (eBinary E.Plus
       (eVal $ intVal 1)
       (eBinary E.Times (eVal $ intVal 2) (eVal $ intVal 3)))
      (eVal $ intVal 4))
  , expectParsesA expressionParser "10 + -5"
    (eBinary E.Plus (eVal $ intVal 10) (eVal $ intVal (-5)))
  , expectParsesA expressionParser "10 - 3 - 4"
    (eBinary E.Minus
     (eBinary E.Minus (eVal $ intVal 10) (eVal $ intVal 3))
     (eVal $ intVal 4))
  , expectParsesA expressionParser "String(10 + -5)"
    (E.Cast [] "String" (eBinary E.Plus (eVal $ intVal 10) (eVal $ intVal (-5))))
  , expectParsesA expressionParser "1 == 1 && 2 < 3"
    (eBinary E.BoolAnd
     (eBinary E.Eq (eVal (intVal 1)) (eVal (intVal 1)))
     (eBinary E.Less (eVal (intVal 2)) (eVal (intVal 3))))
  , expectParses typeParser "Int" "Int"
  , expectParsesA typeDefParser "struct:\n  a Int\n  b String\n"
    (T.Struct [] [("a", intT), ("b", stringT)])
  , testEnumType
  , testEnumType2

    -- statements
  , expectParsesA statementParser "return \"foo\""
    (sReturn $ Just $ eVal $ strVal "foo")
  , expectParsesA statementParser "print(String(10 + -5))"
    (S.Expr [] $ E.Call []
     (E.Var [] "print") [E.Cast [] "String"
                         (eBinary E.Plus
                          (eVal $ intVal 10)
                          (eVal $ intVal (-5)))])
  , expectParses statementSep "\n" ()
  , expectParses statementSep "  \n  " ()
  , expectParses statementSep "  \n\n  \n  " ()
  , expectParsesA statementParser "pass" (S.Pass [])
  , expectParsesA statementParser "return 1\n"
    (sReturn $ Just $ eVal $ intVal 1)
  , expectParsesA statementParser "let a123 = True"
    (sLet "a123" (eVal $ boolVal True))
  , expectParsesA typeDefParser "Bool"
    (T.TypeName [] "Bool")
  , expectParsesA typeDefParser "Pair<Int, Int>"
    (T.Generic [] "Pair" [T.TypeName [] "Int" | _ <- [1,2]])
  , expectParsesA statementParser "let a123 Bool = True"
    (sLetT "a123" (T.TypeName [] "Bool") (eVal $ boolVal True))
  , expectParsesA statementParser "let x_y Pair<Int, Int> = foo()"
    (sLetT "x_y"
     (T.Generic [] "Pair" [T.TypeName [] "Int" | _ <- [1,2]])
     (E.Call [] (E.Var [] "foo") []))
  , expectParsesA statementParser "a.b.c = True"
    (sAssign ["a", "b", "c"] (eVal $ boolVal True))
  , expectParsesA statementParser "print(c)"
    (S.Expr [] $ eCall (eVar "print") [eVar "c"])

  , expectParsesA letStatement "let int = 5 + (2 * 10) / 3 % 4"
    (sLet "int"
     (eBinary E.Plus
       (eVal $ intVal 5)
       (eBinary E.Mod
         (eBinary E.Divide
           (E.Paren []
            (eBinary E.Times
             (eVal (intVal 2))
             (eVal (intVal 10))))
           (eVal $ intVal 3))
         (eVal $ intVal 4))))

  , expectParsesA assignStatement "int = 3"
    (sAssign ["int"] $ eVal $ intVal 3)
  , expectParsesA assignStatement "int = int ** 3"
    (sAssign ["int"] $ eBinary E.Power (eVar "int") (eVal $ intVal 3))

  -- blocks and larger
  , testParsingBlock
  , testParsingIf
  , testParsingMatch
  , testParsingFunc
  , testParsingFunc2
  , testParsingTypedFunction
  , testParsingTypeDecl
  ]

testEnumType :: Test
testEnumType =
  let text = "enum:\n Cons:\n  item Int\n  next List\n End\n"
      expected = T.Enum [] [ ("Cons", [("item", intT), ("next", T.TypeName [] "List")])
                           , ("End", []) ]
  in expectParsesA typeDefParser text expected

testEnumType2 :: Test
testEnumType2 =
  let text = "enum:\n TInt\n TFloat\n"
      expected = T.Enum [] [ ("TInt", [])
                           , ("TFloat", []) ]
  in expectParsesA typeDefParser text expected

testParsingBlock :: Test
testParsingBlock =
  let text = "let a1 = True\nreturn a1  \n"
      expected = sBlock [ sLet "a1" (eVal $ boolVal True)
                         , sReturn (Just $ eVar "a1")
                         ]
  in expectParsesA blockStatement' text expected

testParsingIf :: Test
testParsingIf =
  let text = "if a == 1:\n    return a"
      test = eBinary E.Eq (eVar "a") (eVal $ intVal 1)
      body = [sReturn $ Just $ eVar "a"]
      expected = S.If [] test body Nothing
  in expectParsesA ifStatement text expected

testParsingMatch :: Test
testParsingMatch =
  let text = "match x:\n  _:\n    return 1\n  Link(_, next):\n    return 2"
      ret n = sBlock [sReturn $ Just $ eVal $ intVal n]
      case1 = S.MatchCase (S.MatchAnything []) (ret 1)
      case2 = S.MatchCase (S.MatchStructure [] "Link" [S.MatchAnything [], S.MatchVariable [] "next"]) (ret 2)
      expected = S.Match [] (eVar "x") [case1, case2]
  in expectParsesA statementParser text expected

testParsingFunc :: Test
testParsingFunc =
  let text = "fn main():\n  pass"
      expected = D.Function [] "main" Nothing [] (sBlock [S.Pass []])
  in expectParsesA declarationParser text expected

testParsingFunc2 :: Test
testParsingFunc2 =
  let text = "fn main(a, b):\n  //a comment\n  pass"
      expected = D.Function [] "main" Nothing ["a", "b"] (sBlock [S.Pass []])
  in expectParsesA declarationParser text expected

testParsingTypedFunction :: Test
testParsingTypedFunction =
  let text = "fn main(a Int, b Bool) Bool:\n//a comment\n  pass"
      fnType = Just ([], T.Function [] [intT, boolT] boolT)
      expected = D.Function [] "main" fnType ["a", "b"] (sBlock [S.Pass []])
  in expectParsesA declarationParser text expected

testParsingTypeDecl :: Test
testParsingTypeDecl =
  let text = "type Foo struct:\n  asdf Int\n  xyz Foo\n"
      declaredType = T.Struct [] [("asdf", intT), ("xyz", T.TypeName [] "Foo")]
      def = T.TypeDef [] "Foo" []
      expected = D.TypeDef [] def declaredType
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
  case parse (parser <* eof) text of
   (Left err) ->
     assertFailure $  "failed parsing ''" ++ text ++ "'', error parsing (" ++ show err ++ ")"
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
