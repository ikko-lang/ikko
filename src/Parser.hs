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
import qualified AST.Annotation as An
import qualified AST.Declaration as D
import AST.Expression (BinOp, UnaryOp)
import qualified AST.Expression as E
import qualified AST.Statement as S
import qualified AST.Type as T
import Region (Position(..), Region(..))

type File            = D.File Annotation
type Declaration     = D.Declaration Annotation
type FuncType        = D.FuncType Annotation
type Statement       = S.Statement Annotation
type MatchCase       = S.MatchCase Annotation
type MatchExpression = S.MatchExpression Annotation
type Expression      = E.Expression Annotation
type Value           = E.Value Annotation
type Type            = T.Type
type TypeDecl        = T.TypeDecl Annotation
type TypeDef         = T.TypeDef Annotation
type EnumOption      = T.EnumOption Annotation

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
  _ <- anyWhitespace
  decls <- many (topLevel *> declarationParser)
  _ <- anyWhitespace
  _ <- eof
  return decls

declarationParser :: Parser Declaration
declarationParser =
  withPos $ addLocation $ choice [letDeclaration, funcDeclaration, typeDeclaration]

letDeclaration :: Parser Declaration
letDeclaration = do
  (name, mtype) <- letName
  result <- D.Let [] name mtype <$> expressionParser
  _ <- statementSep
  return result

letName :: Parser (String, Maybe TypeDecl)
letName = do
  _ <- string "let"
  _ <- any1Whitespace
  name <- valueName
  _ <- any1Whitespace
  mtype <- optionMaybe $ try $ do
    typ <- simpleTypeDefParser
    _ <- any1Whitespace
    return typ
  _ <- char '='
  _ <- any1Whitespace
  return (name, mtype)

equalsWhitespace :: Parser ()
equalsWhitespace = do
  _ <- any1Whitespace
  _ <- char '='
  _ <- any1Whitespace
  return ()

funcDeclaration :: Parser Declaration
funcDeclaration = do
  _ <- string "fn"
  _ <- any1LinearWhitespace
  name <- valueName
  gens <- optionMaybe $ try genericList
  _ <- char '('
  _ <- anyLinearWhitespace
  argsAndTypes <- funcArgDecl
  let (args, argTypes) = unzip argsAndTypes
  retType <- optionMaybe $ try $ do
    _ <- any1LinearWhitespace
    simpleTypeDefParser
  mtype <- assembleFunctionType (fromMaybe [] gens) argTypes retType
  _ <- char ':'
  _ <- statementSep
  D.Function [] name mtype args <$> blockStatement

assembleFunctionType :: [Type] -> [Maybe TypeDecl] -> Maybe TypeDecl -> Parser (Maybe FuncType)
assembleFunctionType gens argTypes retType =
  if allNothings argTypes && isNothing retType
  then return Nothing
  else do
    argTs <- requireJusts argTypes
    let retT = unwrapOr retType nilType
    let typ = T.Function [] argTs retT
    return $ Just (gens, typ)


genericList :: Parser [Type]
genericList = do
  _ <- string "<"
  gens <- sepBy typeParser commaSep
  _ <- string ">"
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

typeDeclaration :: Parser Declaration
typeDeclaration = do
  _ <- string "type"
  _ <- any1LinearWhitespace
  declared <- simpleTypeDefParser
  tdef <- mustBeTDef declared
  _ <- any1LinearWhitespace
  D.TypeDef [] tdef <$> typeDefParser


mustBeTDef :: TypeDecl -> Parser TypeDef
mustBeTDef decl = case decl of
  T.TypeName a name ->
    return $ T.TypeDef a name []
  T.Generic a name ts -> do
    ts' <- mapM mustBeTypeName ts
    return $ T.TypeDef a name ts'
  _ ->
    fail "invalid type to declare"


mustBeTypeName :: TypeDecl -> Parser Type
mustBeTypeName decl = case decl of
  T.TypeName _ name -> return name
  _ -> fail "invalid generic parameter in type to declare"

type ArgDecls = [(String, Maybe TypeDecl)]

funcArgDecl :: Parser ArgDecls
funcArgDecl = argDeclEnd <|> argDecl

argDeclEnd :: Parser ArgDecls
argDeclEnd = do
  _ <- char ')'
  return []

argDecl :: Parser ArgDecls
argDecl = do
  name <- valueName
  mtype <- optionMaybe $ try $ do
   _ <- any1LinearWhitespace
   simpleTypeDefParser
  _ <- anyLinearWhitespace
  rest <- argDeclEnd <|> nextArgDecl
  return ((name, mtype) : rest)

nextArgDecl :: Parser ArgDecls
nextArgDecl = do
  _ <- char ','
  _ <- anyLinearWhitespace
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
  _ <- string "pass"
  _ <- statementSep
  return $ S.Pass []

returnStatement :: Parser Statement
returnStatement = do
  _ <- string "return"
  e <- optionMaybe $ do
    _ <- any1LinearWhitespace
    expressionParser
  _ <- statementSep
  return $ S.Return [] e

letStatement :: Parser Statement
letStatement = do
  (name, mtype) <- letName
  l <- S.Let [] name mtype <$> expressionParser
  _ <- statementSep
  return l

assignStatement :: Parser Statement
assignStatement = do
  name <- valueName
  names <- try (assignFields [name]) <|> return [name]
  equalsWhitespace
  a <- S.Assign [] names <$> expressionParser
  _ <- statementSep
  return a

assignFields :: [String] -> Parser [String]
assignFields lefts = do
  _ <- char '.'
  right <- valueName
  let names = right : lefts
  try (assignFields names) <|> return (reverse names)

blockStatement :: Parser Statement
blockStatement =
  indented >> blockStatement'

blockStatement' :: Parser Statement
blockStatement' =
  fmap (S.Block []) (block statementParser)

statementSep :: Parser ()
statementSep = eof <|> do
  _ <- anyLinearWhitespace
  _ <- char '\n'
  _ <- anyWhitespace
  return ()

exprStatement :: Parser Statement
exprStatement = do
  e <- S.Expr [] <$> expressionParser
  _ <- statementSep
  return e

ifStatement :: Parser Statement
ifStatement = do
  _ <- string "if"
  (test, body) <- testedBlock
  elsePart <- optionMaybe $ try elseBlock
  return $ let (S.Block [] stmts) = body
           in S.If [] test stmts elsePart

elseBlock :: Parser Statement
elseBlock = do
  _ <- checkIndent *> string "else"
  (any1Whitespace *> ifStatement) <|> (char ':' *> statementSep *> blockStatement)

whileStatement :: Parser Statement
whileStatement = do
  _ <- string "while"
  (test, body) <- testedBlock
  return $ let (S.Block [] stmts) = body
           in S.While [] test stmts

testedBlock :: Parser (Expression, Statement)
testedBlock = do
  _ <- any1Whitespace
  test <- expressionParser
  _ <- char ':'
  _ <- statementSep
  body <- blockStatement
  return (test, body)

matchStatement :: Parser Statement
matchStatement = do
  _ <- string "match"
  _ <- any1Whitespace
  value <- expressionParser
  _ <- char ':'
  _ <- statementSep
  S.Match [] value <$> block matchCaseParser

matchCaseParser :: Parser MatchCase
matchCaseParser = do
  e <- matchExpression
  _ <- char ':'
  _ <- statementSep
  S.MatchCase e <$> blockStatement

matchExpression :: Parser MatchExpression
matchExpression = addLocation (matchAnything <|> matchVariable <|> matchStructure)

matchAnything :: Parser MatchExpression
matchAnything = do
  _ <- string "_"
  return $ S.MatchAnything []

matchVariable :: Parser MatchExpression
matchVariable = S.MatchVariable [] <$> valueName

matchStructure :: Parser MatchExpression
matchStructure = do
  structType <- typeParser
  minner <- optionMaybe $ try $ do
    _ <- char '('
    _ <- anyWhitespace
    choice [matchExpressions, argsEnd]
  return $ S.MatchStructure [] structType (fromMaybe [] minner)

matchExpressions :: Parser [MatchExpression]
matchExpressions = do
  e <- matchExpression
  _ <- anyWhitespace
  rest <- choice [matchExpressionsNext, argsEnd]
  return $ e : rest

matchExpressionsNext :: Parser [MatchExpression]
matchExpressionsNext = do
  _ <- char ','
  _ <- anyWhitespace
  matchExpressions

---- AST.Expression parsers ----

--- parse expressions

-- Binary expressions are handled at this level
expressionParser :: Parser Expression
expressionParser = unfoldParts <$> readBinExprParts

readBinExprParts :: Parser ([Expression], [BinOp])
readBinExprParts = do
  e <- expr
  _ <- anyLinearWhitespace
  parts <- many $ try $ do
    op <- opParser
    _ <- anyWhitespace
    e' <- expr
    _ <- anyLinearWhitespace
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
  then unfoldOps (E.Binary [] o l r : es, os) opset
  else let (restE, restO) = unfoldOps (r:es, os) opset
       in (l:restE, o:restO)
unfoldOps _ _ = error "invalid call to unfoldOps"

precOrder :: [[BinOp]]
precOrder =
  [ [E.Times, E.Divide, E.Mod]
  , [E.Plus, E.Minus]
  , [E.LShift, E.RShift]
  , [E.Power]
  , [E.Less, E.LessEq, E.Greater, E.GreaterEq]
  , [E.Eq, E.NotEq]
  , [E.BitAnd]
  , [E.BitXor]
  , [E.BitOr]
  , [E.BoolAnd]
  , [E.BoolOr]
  ]

expr :: Parser Expression
expr = do
  e <- addLocation $ choice $ map try [parenExpr, castExpr, valueExpr, unaryExpr, callExpr, varExpr]
  try (addLocation $ accessExpr e) <|> return e

accessExpr :: Expression -> Parser Expression
accessExpr left = do
  _ <- char '.'
  right <- valueName
  let e = E.Access [] left right
  try (accessExpr e) <|> return e

parenExpr :: Parser Expression
parenExpr = E.Paren [] <$> parenExpr'

valueExpr :: Parser Expression
valueExpr = E.Val [] <$> valueParser

unaryExpr :: Parser Expression
unaryExpr = do
  op <- unaryOpParser
  _ <- anyWhitespace
  E.Unary [] op <$> expressionParser

callExpr :: Parser Expression
callExpr = do
  fn <- varExpr
  _ <- char '('
  _ <- anyWhitespace
  args <- choice [fnCallArg, argsEnd]
  return $ E.Call [] fn args

fnCallArg :: Parser [Expression]
fnCallArg = do
  arg <- expressionParser
  _ <- anyWhitespace
  rest <- choice [fnCallNextArg, argsEnd]
  return $ arg : rest

fnCallNextArg :: Parser [Expression]
fnCallNextArg = do
  _ <- char ','
  _ <- anyWhitespace
  fnCallArg

argsEnd :: Parser [a]
argsEnd = do
  _ <- char ')'
  return []

castExpr :: Parser Expression
castExpr = do
  typ <- typeParser
  E.Cast [] typ <$> parenExpr'

parenExpr' :: Parser Expression
parenExpr' = do
  _ <- char '('
  _ <- anyWhitespace
  ex <- expressionParser
  _ <- anyWhitespace
  _ <- char ')'
  return ex

varExpr :: Parser Expression
varExpr = E.Var [] <$> valueName

--- parse values

valueParser :: Parser Value
valueParser = addLocation $ choice $ map try [boolParser, structValueParser, stringParser, numberParser]

structValueParser :: Parser Value
structValueParser = do
  typ <- typeName
  mfields <- optionMaybe $ try $ do
    _ <- string "{"
    _ <- anyWhitespace
    fields <- sepEndBy structFieldValue any1Whitespace
    _ <- string "}"
    return fields
  return $ E.StructVal [] typ (fromMaybe [] mfields)

structFieldValue :: Parser (String, Expression)
structFieldValue = do
  field <- valueName
  _ <- string ":"
  _ <- anyLinearWhitespace
  value <- expressionParser
  _ <- string ","
  return (field, value)

stringParser :: Parser Value
stringParser = E.StrVal [] <$> doubleQuotedString

boolParser :: Parser Value
boolParser = do
  b <- choices [("False", False), ("True", True)]
  return $ E.BoolVal [] b

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
  return $ E.FloatVal [] (read (start ++ fp))

floatingPart :: Parser String
floatingPart = do
  _ <- char '.'
  ds <- optionMaybe digits
  exPart <- optionMaybe exponentPart
  return ('.' : unwrapOr ds "0" ++ maybeEmpty exPart)

exponentPart :: Parser String
exponentPart = do
  _ <- char 'e'
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
integer start = return $ E.IntVal [] (read start)

--- parse operators

opParser :: Parser BinOp
opParser = choices opChoices

opChoices :: [(String, BinOp)]
opChoices =
      [ ("+",  E.Plus)
      , ("-",  E.Minus)
      , ("**", E.Power)
      , ("*",  E.Times)
      , ("/",  E.Divide)
      , ("%",  E.Mod)
      , ("^",  E.BitXor)
      , ("&&", E.BoolAnd)
      , ("&",  E.BitAnd)
      , ("||", E.BoolOr)
      , ("|",  E.BitOr)
      , ("<<",  E.LShift)
      , (">>",  E.RShift)
      , ("==", E.Eq)
      , ("!=", E.NotEq)
      , ("<=", E.LessEq)
      , ("<",  E.Less)
      , (">=", E.GreaterEq)
      , (">",  E.Greater)
      ]

unaryOpParser :: Parser UnaryOp
unaryOpParser =
  choices [ ("~", E.BitInvert)
          , ("!", E.BoolNot) ]

---- AST.Type parsers ----

nilType :: TypeDecl
nilType = T.TypeName [] "()"

typeParser :: Parser Type
typeParser = string "()" <|> typeName

typeDefParser :: Parser TypeDecl
typeDefParser = addLocation $ tryAll parsers
  where parsers = [enumTypeParser, structTypeParser, funcTypeParser, genericType, namedType]

simpleTypeDefParser :: Parser TypeDecl
simpleTypeDefParser = addLocation $ tryAll parsers
  where parsers = [funcTypeParser, genericType, namedType]

enumTypeParser :: Parser TypeDecl
enumTypeParser = do
  _ <- string "enum:"
  _ <- statementSep
  options <- block enumField
  return $ T.Enum [] options

enumHeader :: Parser String
enumHeader = string "enum:" <* statementSep

enumField :: Parser (String, EnumOption)
enumField = withPos $ do
  name <- typeName
  fields <- enumOptionFields <|> (statementSep $> [])
  return (name, fields)

enumOptionFields :: Parser [(String, TypeDecl)]
enumOptionFields = do
  _ <- char ':'
  _ <- statementSep
  block structField

structTypeParser :: Parser TypeDecl
structTypeParser = do
  _ <- string "struct:"
  _ <- statementSep
  T.Struct [] <$> block structField

structField :: Parser (String, TypeDecl)
structField = do
  name <- valueName
  _ <- any1LinearWhitespace
  typ <- addLocation simpleTypeDefParser
  _ <- statementSep
  return (name, typ)


funcTypeParser :: Parser TypeDecl
funcTypeParser = do
  _ <- string "fn("
  argDecls <- sepBy (indented *> simpleTypeDefParser) commaSep
  -- TODO: Accept trailing commas
  _ <- atOrBelow *> string ")"
  _ <- any1LinearWhitespace
  T.Function [] argDecls <$> simpleTypeDefParser


genericType :: Parser TypeDecl
genericType = do
  name <- typeParser
  _ <- string "<"
  parts <- sepBy simpleTypeDefParser commaSep
  _ <- string ">"
  return $ T.Generic [] name parts

namedType :: Parser TypeDecl
namedType = T.TypeName [] <$> typeParser

---- Helper functions ----

tryAll :: [Parser a] -> Parser a
tryAll = choice . map try

choices :: [(String, a)] -> Parser a
choices = choice . map (try . pair2parser)

pair2parser :: (String, a) -> Parser a
pair2parser (str, result) = do
  _ <- string str
  return result

doubleQuotedString :: Parser String
doubleQuotedString = do
  _ <- char '"'
  stringContents <- many $ choice [escapedChar, many1 $ noneOf "\\\""]
  _ <- char '"'
  return $ concat stringContents

escapedChar :: Parser String
escapedChar = do
  _ <- char '\\'
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

anyWhitespaceS :: Parser String
-- `try` is needed here so that it can back out of parsing a division operator
anyWhitespaceS = many1 anyWhitespaceCh <|> try parseComment

anyWhitespace :: Parser String
anyWhitespace = do
  whitespaces <- many anyWhitespaceS
  return $ concat whitespaces

any1Whitespace :: Parser String
any1Whitespace = do
  whitespaces <- many1 anyWhitespaceS
  return $ concat whitespaces

whitespaceChs :: String
whitespaceChs = " \t\r\n"

anyWhitespaceCh :: Parser Char
anyWhitespaceCh = oneOf whitespaceChs

linearWhitespaceCh :: Parser Char
linearWhitespaceCh = oneOf " \t"

anyLinearWhitespace :: Parser String
anyLinearWhitespace = many linearWhitespaceCh

any1LinearWhitespace :: Parser String
any1LinearWhitespace = many1 linearWhitespaceCh

-- Indentation parser utility
atOrBelow :: (Monad m, Stream s (IndentT m) z) => IndentParserT s u m ()
atOrBelow = indented <|> checkIndent

commaSep :: Parser ()
commaSep = do
  _ <- string ","
  _ <- any1LinearWhitespace
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
  return $ An.addLocation region result
