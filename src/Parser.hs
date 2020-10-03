{-# LANGUAGE FlexibleContexts #-}

module Parser where

import Control.Monad (when)
import Data.Functor (($>))
import Data.Functor.Identity (runIdentity)
import Data.Maybe (isNothing, fromMaybe)

import Text.Parsec
import Text.Parsec.Pos (sourceLine, sourceColumn, sourceName)
import Text.Parsec.Indent

import AST.Annotation (Annotated, Annotation)
import qualified AST.Annotation as Ann
import qualified AST
import AST (BinOp, UnaryOp)
import Region (Position(..), Region(..))

type File            = AST.File Annotation
type Declaration     = AST.Declaration Annotation
type Statement       = AST.Statement Annotation
type MatchCase       = AST.MatchCase Annotation
type MatchExpression = AST.MatchExpression Annotation
type Expression      = AST.Expression Annotation
type Value           = AST.Value Annotation
type Type            = AST.Type
type TypeDecl        = AST.TypeDecl Annotation
type TypeDef         = AST.TypeDef Annotation
type EnumOption      = AST.EnumOption Annotation
type Predicate       = AST.Predicate Annotation
type ClassMethod     = AST.ClassMethod Annotation

type Parser a = IndentParser String () a

keywords :: [String]
keywords =
  [ "package", "import", "let", "fn", "type", "struct", "enum",
    "if", "else", "while", "for", "match", "with"
  ]

parseFile :: String -> String -> Either String File
parseFile fileName content =
  applyLeft show $ runIdentity $ runIndentParserT fileParser () fileName content

---- AST.Declaration parsers ----

fileParser :: Parser File
fileParser = do
  anyWhitespace
  decls <- many (topLevel *> declarationParser)
  anyWhitespace
  _ <- eof
  return decls

declarationParser :: Parser Declaration
declarationParser =
  withPos $ addLocation $ choice [letDeclaration, funcDeclaration, typeDeclaration, instanceDeclaration]

letDeclaration :: Parser Declaration
letDeclaration = do
  (name, mtype) <- letName
  result <- AST.DLet [] name mtype <$> expressionParser
  statementSep
  return result

letName :: Parser (String, Maybe TypeDecl)
letName = do
  string_ "let"
  any1Whitespace
  name <- valueName
  any1Whitespace
  mtype <- optionMaybe $ try $ do
    typ <- simpleTypeDefParser
    any1Whitespace
    return typ
  char_ '='
  any1Whitespace
  return (name, mtype)

equalsWhitespace :: Parser ()
equalsWhitespace = do
  any1Whitespace
  char_ '='
  any1Whitespace
  return ()

funcDeclaration :: Parser Declaration
funcDeclaration = do
  string_ "fn"
  any1LinearWhitespace
  name <- valueName
  char_ '('
  anyLinearWhitespace
  argsAndTypes <- funcArgDecl
  let (args, argTypes) = unzip argsAndTypes
  retType <- optionMaybe $ try $ do
    any1LinearWhitespace
    simpleTypeDefParser
  mpredicates <- optionMaybe $ try $ do
    any1Whitespace
    whereClauseParser
  mtype <- assembleFunctionType argTypes retType mpredicates
  char_ ':'
  statementSep
  AST.DFunction [] name mtype args <$> blockStatement


whereClauseParser :: Parser [Predicate]
whereClauseParser = do
  string_ "where"
  any1Whitespace
  sepBy (addLocation predParser) commaSep

-- e.g. `t: Eq`
predParser :: Parser Predicate
predParser = do
  tv <- valueName
  char_ ':'
  any1LinearWhitespace
  cls <- typeNameParser
  return $ AST.Predicate { AST.predAnn=[], AST.predClass=cls, AST.predType=tv }

assembleFunctionType
  :: [Maybe TypeDecl]
  -> Maybe TypeDecl
  -> Maybe [Predicate]
  -> Parser (Maybe TypeDecl)
assembleFunctionType argTypes retType predicates =
  if allNothings argTypes && isNothing retType && isNothing predicates
  then return Nothing
  else do
    argTs <- requireJusts argTypes
    let retT = unwrapOr retType nilType
    let preds = unwrapOr predicates []
    let typ = AST.TFunction [] preds argTs retT
    return $ Just typ


genericList :: Parser [Type]
genericList = do
  string_ "<"
  gens <- sepBy valueName commaSep
  string_ ">"
  return gens


allNothings :: [Maybe a] -> Bool
allNothings = all isNothing

requireJusts :: [Maybe a] -> Parser [a]
requireJusts [] = return []
requireJusts (Nothing:_) =
  unexpected "When a function is typed, all arguments must be typed"
requireJusts (Just t:ts) = do
  rest <- requireJusts ts
  return (t:rest)

-- Type declarations cover structs, enums, and class definitions
typeDeclaration :: Parser Declaration
typeDeclaration = do
  string_ "type"
  any1LinearWhitespace
  declared <- simpleTypeDefParser
  tdef <- mustBeTDef declared
  any1LinearWhitespace
  AST.DTypeDef [] tdef <$> typeDefParser


mustBeTDef :: TypeDecl -> Parser TypeDef
mustBeTDef decl = case decl of
  AST.TName a name ->
    return $ AST.TypeDef a name []
  AST.TGeneric a name ts -> do
    ts' <- mapM mustBeTypeName ts
    return $ AST.TypeDef a name ts'
  _ ->
    fail "invalid type to declare"


mustBeTypeName :: TypeDecl -> Parser Type
mustBeTypeName decl = case decl of
  AST.TName _ name -> return name
  _ -> fail "invalid generic parameter in type to declare"


instanceDeclaration :: Parser Declaration
instanceDeclaration = do
  string_ "instance"
  any1LinearWhitespace
  cls <- typeName
  any1LinearWhitespace
  declared <- simpleTypeDefParser
  tdef <- mustBeTDef declared

  mpredicates <- optionMaybe $ try $ do
    any1Whitespace
    whereClauseParser
  let predicates = fromMaybe [] mpredicates

  char_ ':'
  statementSep

  AST.DInstance [] cls tdef predicates <$> block declarationParser


type ArgDecls = [(String, Maybe TypeDecl)]

funcArgDecl :: Parser ArgDecls
funcArgDecl = argDeclEnd <|> argDecl

argDeclEnd :: Parser ArgDecls
argDeclEnd = do
  char_ ')'
  return []

argDecl :: Parser ArgDecls
argDecl = do
  name <- valueName
  mtype <- optionMaybe $ try $ do
   any1LinearWhitespace
   simpleTypeDefParser
  anyLinearWhitespace
  rest <- argDeclEnd <|> nextArgDecl
  return ((name, mtype) : rest)

nextArgDecl :: Parser ArgDecls
nextArgDecl = do
  char_ ','
  anyLinearWhitespace
  argDecl

---- AST.Statement parsers ----

statementParser :: Parser Statement
statementParser = do
  let stmtTypes = [
        passStatement,
        returnStatement, letStatement, ifStatement, whileStatement,
        matchStatement, assignStatement, exprStatement]
  addLocation $ withPos $ choice $ map try stmtTypes

passStatement :: Parser Statement
passStatement = do
  string_ "pass"
  statementSep
  return $ AST.Pass []

returnStatement :: Parser Statement
returnStatement = do
  string_ "return"
  e <- optionMaybe $ do
    any1LinearWhitespace
    expressionParser
  statementSep
  return $ AST.Return [] e

letStatement :: Parser Statement
letStatement = do
  (name, mtype) <- letName
  l <- AST.Let [] name mtype <$> expressionParser
  statementSep
  return l

assignStatement :: Parser Statement
assignStatement = do
  name <- valueName
  names <- try (assignFields [name]) <|> return [name]
  equalsWhitespace
  a <- AST.Assign [] names <$> expressionParser
  statementSep
  return a

assignFields :: [String] -> Parser [String]
assignFields lefts = do
  char_ '.'
  right <- valueName
  let names = right : lefts
  try (assignFields names) <|> return (reverse names)

blockStatement :: Parser Statement
blockStatement =
  indented >> blockStatement'

blockStatement' :: Parser Statement
blockStatement' =
  fmap (AST.Block []) (block statementParser)

statementSep :: Parser ()
statementSep = eof <|> do
  anyLinearWhitespace
  char_ '\n'
  anyWhitespace
  return ()

exprStatement :: Parser Statement
exprStatement = do
  e <- AST.Expr [] <$> expressionParser
  statementSep
  return e

ifStatement :: Parser Statement
ifStatement = do
  string_ "if"
  (test, body) <- testedBlock
  elsePart <- optionMaybe $ try elseBlock
  return $ let (AST.Block [] stmts) = body
           in AST.If [] test stmts elsePart

elseBlock :: Parser Statement
elseBlock = do
  _ <- checkIndent *> string "else"
  (any1Whitespace *> ifStatement) <|> (char ':' *> statementSep *> blockStatement)

whileStatement :: Parser Statement
whileStatement = do
  string_ "while"
  (test, body) <- testedBlock
  return $ let (AST.Block [] stmts) = body
           in AST.While [] test stmts

testedBlock :: Parser (Expression, Statement)
testedBlock = do
  any1Whitespace
  test <- expressionParser
  char_ ':'
  statementSep
  body <- blockStatement
  return (test, body)

matchStatement :: Parser Statement
matchStatement = do
  string_ "match"
  any1Whitespace
  value <- expressionParser
  char_ ':'
  statementSep
  AST.Match [] value <$> block matchCaseParser

matchCaseParser :: Parser MatchCase
matchCaseParser = do
  e <- matchExpression
  char_ ':'
  statementSep
  AST.MatchCase e <$> blockStatement

matchExpression :: Parser MatchExpression
matchExpression = addLocation (matchAnything <|> matchVariable <|> matchStructure)

matchAnything :: Parser MatchExpression
matchAnything = do
  string_ "_"
  return $ AST.MatchAnything []

matchVariable :: Parser MatchExpression
matchVariable = AST.MatchVariable [] <$> valueName

matchStructure :: Parser MatchExpression
matchStructure = do
  structType <- typeNameParser
  minner <- optionMaybe $ try $ do
    char_ '('
    anyWhitespace
    choice [matchExpressions, argsEnd]
  return $ AST.MatchStructure [] structType (fromMaybe [] minner)

matchExpressions :: Parser [MatchExpression]
matchExpressions = do
  e <- matchExpression
  anyWhitespace
  rest <- choice [matchExpressionsNext, argsEnd]
  return $ e : rest

matchExpressionsNext :: Parser [MatchExpression]
matchExpressionsNext = do
  char_ ','
  anyWhitespace
  matchExpressions

---- AST.Expression parsers ----

--- parse expressions

-- Binary expressions are handled at this level
expressionParser :: Parser Expression
expressionParser = unfoldParts <$> readBinExprParts

readBinExprParts :: Parser ([Expression], [BinOp])
readBinExprParts = do
  e <- expr
  anyLinearWhitespace
  parts <- many $ try $ do
    op <- opParser
    anyWhitespace
    e' <- expr
    anyLinearWhitespace
    return (op, e')
  let (ops, es) = unzip parts
  return (e : es, ops)


unfoldParts :: ([Expression], [BinOp]) -> Expression
unfoldParts bin =
  case foldl unfoldOps bin precOrder of
   ([e], []) -> e
   _         -> error "compiler bug in unfoldParts"

unfoldOps :: ([Expression], [BinOp]) -> [BinOp] -> ([Expression], [BinOp])
unfoldOps ([e], [])    _     = ([e], [])
unfoldOps ([],  [])    _     = ([], [])
unfoldOps (l:r:es, o:os) opset =
  if o `elem` opset
  then unfoldOps (AST.Binary [] o l r : es, os) opset
  else let (restE, restO) = unfoldOps (r:es, os) opset
       in (l:restE, o:restO)
unfoldOps _ _ = error "invalid call to unfoldOps"

precOrder :: [[BinOp]]
precOrder =
  [ [AST.Times, AST.Divide, AST.Mod]
  , [AST.Plus, AST.Minus]
  , [AST.LShift, AST.RShift]
  , [AST.Power]
  , [AST.Less, AST.LessEq, AST.Greater, AST.GreaterEq]
  , [AST.Eq, AST.NotEq]
  , [AST.BitAnd]
  , [AST.BitXor]
  , [AST.BitOr]
  , [AST.BoolAnd]
  , [AST.BoolOr]
  ]

expr :: Parser Expression
expr = do
  let options = [parenExpr, castExpr, valueExpr, unaryExpr, callExpr, varExpr]
  e <- addLocation $ choice $ map try options
  try (addLocation $ accessExpr e) <|> return e

accessExpr :: Expression -> Parser Expression
accessExpr left = do
  char_ '.'
  right <- valueName
  let e = AST.Access [] left right
  try (accessExpr e) <|> return e

parenExpr :: Parser Expression
parenExpr = AST.Paren [] <$> parenExpr'

valueExpr :: Parser Expression
valueExpr = AST.Val [] <$> valueParser

unaryExpr :: Parser Expression
unaryExpr = do
  op <- unaryOpParser
  anyWhitespace
  AST.Unary [] op <$> expressionParser

callExpr :: Parser Expression
callExpr = do
  fn <- varExpr
  char_ '('
  anyWhitespace
  args <- choice [fnCallArg, argsEnd]
  return $ AST.Call [] fn args

fnCallArg :: Parser [Expression]
fnCallArg = do
  arg <- expressionParser
  anyWhitespace
  rest <- choice [fnCallNextArg, argsEnd]
  return $ arg : rest

fnCallNextArg :: Parser [Expression]
fnCallNextArg = do
  char_ ','
  anyWhitespace
  fnCallArg

argsEnd :: Parser [a]
argsEnd = do
  char_ ')'
  return []

castExpr :: Parser Expression
castExpr = do
  typ <- typeNameParser
  AST.Cast [] typ <$> parenExpr'

parenExpr' :: Parser Expression
parenExpr' = do
  char_ '('
  anyWhitespace
  ex <- expressionParser
  anyWhitespace
  char_ ')'
  return ex

varExpr :: Parser Expression
varExpr = AST.Var [] <$> valueName

--- parse values

valueParser :: Parser Value
valueParser = addLocation $ choice $ map try [boolParser, structValueParser, stringParser, numberParser]

structValueParser :: Parser Value
structValueParser = do
  typ <- typeName
  mfields <- optionMaybe $ try $ do
    string_ "{"
    anyWhitespace
    fields <- sepEndBy structFieldValue any1Whitespace
    string_ "}"
    return fields
  return $ AST.StructVal [] typ (fromMaybe [] mfields)

structFieldValue :: Parser (String, Expression)
structFieldValue = do
  field <- valueName
  string_ ":"
  anyLinearWhitespace
  value <- expressionParser
  string_ ","
  return (field, value)

stringParser :: Parser Value
stringParser = AST.StrVal [] <$> doubleQuotedString

boolParser :: Parser Value
boolParser = do
  b <- choices [("False", False), ("True", True)]
  return $ AST.BoolVal [] b

--- parse floating and integer numbers

numberParser :: Parser Value
numberParser = do
  sign <- optionMaybe $ string "-"
  dgts <- digits
  let start =  fromMaybe "" sign ++ dgts
  choice [float start, integer start]

float :: String -> Parser Value
float start = do
  fp <- floatingPart
  return $ AST.FloatVal [] (read (start ++ fp))

floatingPart :: Parser String
floatingPart = do
  char_ '.'
  ds <- optionMaybe digits
  exPart <- optionMaybe exponentPart
  return ('.' : unwrapOr ds "0" ++ maybeEmpty exPart)

exponentPart :: Parser String
exponentPart = do
  char_ 'e'
  sign <- choice [string "+", string "-", string ""]
  ds <- many digit
  return ('e' : sign ++ ds)

digits :: Parser String
digits = do
  d <- digit
  ds <- many _digit
  return (d : ds)

_digit :: Parser Char
_digit = do
  _ <- optional underscore
  digit

integer :: String -> Parser Value
integer start = return $ AST.IntVal [] (read start)

--- parse operators

opParser :: Parser BinOp
opParser = choices opChoices

opChoices :: [(String, BinOp)]
opChoices =
      [ ("+",  AST.Plus)
      , ("-",  AST.Minus)
      , ("**", AST.Power)
      , ("*",  AST.Times)
      , ("/",  AST.Divide)
      , ("%",  AST.Mod)
      , ("^",  AST.BitXor)
      , ("&&", AST.BoolAnd)
      , ("&",  AST.BitAnd)
      , ("||", AST.BoolOr)
      , ("|",  AST.BitOr)
      , ("<<", AST.LShift)
      , (">>", AST.RShift)
      , ("==", AST.Eq)
      , ("!=", AST.NotEq)
      , ("<=", AST.LessEq)
      , ("<",  AST.Less)
      , (">=", AST.GreaterEq)
      , (">",  AST.Greater)
      ]

unaryOpParser :: Parser UnaryOp
unaryOpParser =
  choices [ ("~", AST.BitInvert)
          , ("!", AST.BoolNot) ]

---- AST.Type parsers ----

nilType :: TypeDecl
nilType = AST.TName [] "()"

typeNameParser :: Parser Type
typeNameParser = string "()" <|> typeName

typeDefParser :: Parser TypeDecl
typeDefParser = addLocation $ tryAll parsers
  where parsers =
          [ enumTypeParser
          , structTypeParser
          , funcTypeParser
          , classDefParser
          , genericType
          , namedType
          , typeVariable
          ]

simpleTypeDefParser :: Parser TypeDecl
simpleTypeDefParser = addLocation $ tryAll parsers
  where parsers = [funcTypeParser, genericType, namedType, typeVariable]

typeNameOrVar :: Parser TypeDecl
typeNameOrVar = addLocation $ tryAll [namedType, typeVariable]

enumTypeParser :: Parser TypeDecl
enumTypeParser = do
  string_ "enum:"
  statementSep
  options <- block enumField
  return $ AST.TEnum [] options

enumHeader :: Parser String
enumHeader = string "enum:" <* statementSep

enumField :: Parser (String, EnumOption)
enumField = withPos $ do
  name <- typeName
  fields <- enumOptionFields <|> (statementSep $> [])
  return (name, fields)

enumOptionFields :: Parser [(String, TypeDecl)]
enumOptionFields = do
  char_ ':'
  statementSep
  block structField

structTypeParser :: Parser TypeDecl
structTypeParser = do
  string_ "struct:"
  statementSep
  AST.TStruct [] <$> block structField

structField :: Parser (String, TypeDecl)
structField = do
  name <- valueName
  any1LinearWhitespace
  typ <- addLocation simpleTypeDefParser
  statementSep
  return (name, typ)


funcTypeParser :: Parser TypeDecl
funcTypeParser = do
  -- TODO: Does this need to accept generics here?
  string_ "fn("
  argDecls <- sepBy (indented *> simpleTypeDefParser) commaSep
  -- TODO: Accept trailing commas
  _ <- atOrBelow *> string ")"
  any1LinearWhitespace
  AST.TFunction [] [] argDecls <$> simpleTypeDefParser


classDefParser :: Parser TypeDecl
classDefParser = do
  string_ "class"

  msupers <- optionMaybe $ try $ do
    any1LinearWhitespace
    string_ "extends"
    any1Whitespace
    sepBy typeName commaSep
  let superclasses = fromMaybe [] msupers

  char_ ':'
  statementSep
  AST.TClassDecl [] superclasses <$> block (addLocation classMethod)

classMethod :: Parser ClassMethod
classMethod = do
  string_ "fn"
  any1LinearWhitespace
  name <- valueName

  string_ "("
  -- TODO: Allow line wrapping with indentation (like funcTypeParser)
  argTypes <- sepBy simpleTypeDefParser commaSep
  string_ ")"

  any1LinearWhitespace
  retType <- simpleTypeDefParser

  mpredicates <- optionMaybe $ try $ do
    any1Whitespace
    whereClauseParser
  let predicates = fromMaybe [] mpredicates

  statementSep

  let typ = AST.TFunction [] predicates argTypes retType
  return $ AST.ClassMethod [] name typ

genericType :: Parser TypeDecl
genericType = do
  name <- typeNameParser <|> valueName
  string_ "<"
  parts <- sepBy simpleTypeDefParser commaSep
  string_ ">"
  return $ AST.TGeneric [] name parts

namedType :: Parser TypeDecl
namedType = AST.TName [] <$> typeNameParser

typeVariable :: Parser TypeDecl
typeVariable = AST.TName [] <$> valueName

---- Helper functions ----

string_ :: String -> Parser ()
string_ s = do
  _ <- string s
  return ()

char_ :: Char -> Parser ()
char_ c = do
  _  <- char c
  return ()

tryAll :: [Parser a] -> Parser a
tryAll = choice . map try

choices :: [(String, a)] -> Parser a
choices = choice . map (try . pair2parser)

pair2parser :: (String, a) -> Parser a
pair2parser (str, result) = do
  string_ str
  return result

doubleQuotedString :: Parser String
doubleQuotedString = do
  char_ '"'
  stringContents <- many $ choice [escapedChar, many1 $ noneOf "\\\""]
  char_ '"'
  return $ concat stringContents

escapedChar :: Parser String
escapedChar = do
  char_ '\\'
  c <- anyChar
  case c of
   'n'  -> return "\n"
   't'  -> return "\t"
   '\\' -> return "\\"
   '"'  -> return "\""
   _    -> return $ '\\' : c : ""

typeName :: Parser String
typeName = do
  first <- upper
  rest <- many $ choice [letter, underscore]
  return $ first : rest

valueName :: Parser String
valueName = do
  first <- lower
  rest <- many $ choice [letter, digit, underscore]
  let name = first : rest
  when (name `elem` keywords) (fail $ "Cannot use a keyword as a variable name: " ++ name)
  return name

ignore :: Parser a -> Parser ()
ignore parser = do
  _ <- parser
  return ()

anyWhitespaceS :: Parser ()
-- `try` is needed here so that it can back out of parsing a division operator
anyWhitespaceS =
  ignore (many1 anyWhitespaceCh <|> try parseComment)

anyWhitespace :: Parser ()
anyWhitespace =
  ignore $ many anyWhitespaceS

any1Whitespace :: Parser ()
any1Whitespace =
  ignore $ many1 anyWhitespaceS

whitespaceChs :: String
whitespaceChs = " \t\r\n"

anyWhitespaceCh :: Parser Char
anyWhitespaceCh = oneOf whitespaceChs

linearWhitespaceCh :: Parser Char
linearWhitespaceCh = oneOf " \t"

anyLinearWhitespace :: Parser ()
anyLinearWhitespace = ignore $ many linearWhitespaceCh

any1LinearWhitespace :: Parser ()
any1LinearWhitespace = ignore $ many1 linearWhitespaceCh

-- Indentation parser utility
atOrBelow :: (Monad m, Stream s (IndentT m) z) => IndentParserT s u m ()
atOrBelow = indented <|> checkIndent

commaSep :: Parser ()
commaSep = do
  string_ ","
  any1LinearWhitespace
  return ()

parseComment :: Parser String
parseComment = try parseLineComment <|> parseBlockComment <?> "Comment"

parseLineComment :: Parser String
parseLineComment = do
  start <- string "//"
  comment <- many $ noneOf "\r\n"
  return $ start ++ comment

parseBlockComment :: Parser String
parseBlockComment = do
  start <- string "/*"
  contentList <- blockCommentContents
  return $ concat $ start : contentList

blockCommentContents :: Parser [String]
blockCommentContents = starContent <|> nonStarConents

nonStarConents :: Parser [String]
nonStarConents = do
  content <- many1 $ noneOf "*"
  rest <- starContent
  return $ content : rest

starContent :: Parser [String]
starContent = try blockCommentEnd <|> starRest

starRest :: Parser [String]
starRest = do
  star <- string "*"
  rest <- blockCommentContents
  return $ star : rest

blockCommentEnd :: Parser [String]
blockCommentEnd = do
  end <- string "*/"
  return [end]

underscore :: Parser Char
underscore = char '_'

unwrapOr :: Maybe a -> a -> a
unwrapOr (Just a) _ = a
unwrapOr Nothing  b = b

maybeEmpty :: Maybe String -> String
maybeEmpty m = unwrapOr m ""

applyLeft :: (a -> b) -> Either a r -> Either b r
applyLeft fn (Left  a) = Left (fn a)
applyLeft _  (Right r) = Right r


---- Position utilities ----

position :: Parser Position
position = convertPosition <$> getPosition

convertPosition :: SourcePos -> Position
convertPosition pos =
  Position { pLine=sourceLine pos, pColumn=sourceColumn pos }

getRegion :: Position -> Parser Region
getRegion start = do
  end <- position
  name <- sourceName <$> getPosition
  return Region { pFileName=name, pStart=start, pEnd=end }

addLocation :: (Annotated a) => Parser (a Annotation) -> Parser (a Annotation)
addLocation inner = do
  start <- position
  result <- inner
  region <- getRegion start
  return $ Ann.addLocation region result
