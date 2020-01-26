{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module AST.Statement where

import Data.List (intercalate)

import Util.PrettyPrint (
  Render, PrettyPrint, PrettyPrinter,
  printLines, writeLine, increaseIndent, decreaseIndent)
import Util.Functions (commaSep)

import AST.Annotation (Annotated, emitAnnotation)
import AST.Expression (Expression)
import AST.Type (TypeDecl)

data Statement a
  = Return  a (Maybe (Expression a))
  | Let     a String (Maybe (TypeDecl a)) (Expression a)
  | Assign  a [String] (Expression a)
  | Block   a [Statement a]
  | Expr    a (Expression a) -- e.g. just calling a function
  | If      a (Expression a) [Statement a] (Maybe (Statement a))
  | While   a (Expression a) [Statement a]
  | Match   a (Expression a) [MatchCase a]
  | Pass    a
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Annotated Statement where
  --  use all default methods

instance (Render a) => PrettyPrint (Statement a) where
  printLines stmt = do
    prettyPrintStatement stmt
    return ""

prettyPrintStatement :: (Render a) => Statement a -> PrettyPrinter ()
prettyPrintStatement stmt = case stmt of
  Return _ Nothing ->
    writeLine "return"
  Return _ (Just expr) -> do
    exprS <- printLines expr
    writeLine $ "return " ++ exprS
  Let _ name _ expr -> do
    exprS <- printLines expr
    writeLine $ "let " ++ name ++ " = " ++ exprS
  Assign _ names expr -> do
    exprS <- printLines expr
    writeLine $ (intercalate "." names) ++ " = " ++ exprS
  Block _ stmts -> do
    increaseIndent
    mapM_ printLines stmts
    decreaseIndent
  Expr _ expr -> do
    exprS <- printLines expr
    writeLine exprS
  If _ test t e -> do
    testS <- printLines test
    writeLine $ "if " ++ testS ++ ":"
    increaseIndent
    mapM_ printLines t
    decreaseIndent
    case e of
      Nothing -> return ()
      Just st -> do
        printLines st
        return ()
  While _ test stmts -> do
    testS <- printLines test
    writeLine $ "while " ++ testS ++ ":"
    increaseIndent
    mapM_ printLines stmts
    decreaseIndent
  Match _ expr cases -> do
    exprS <- printLines expr
    writeLine $ "match " ++ exprS ++ ":"
    increaseIndent
    mapM_ printLines cases
    decreaseIndent
  Pass _ ->
    writeLine "pass"
  

data MatchCase a
  = MatchCase (MatchExpression a) (Statement a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Annotated MatchCase where
  --  use all default methods

instance (Render a) => PrettyPrint (MatchCase a) where
  printLines (MatchCase matchExpr stmt) = do
    exprS <- printLines matchExpr
    writeLine $ exprS ++ ":"
    increaseIndent
    printLines stmt
    decreaseIndent
    return ""

-- TODO: add support for matching literal values (int, string)
data MatchExpression a
  = MatchAnything  a
  | MatchVariable  a String
  | MatchStructure a String [MatchExpression a]
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Annotated MatchExpression where
  --  use all default methods

instance (Render a) => PrettyPrint (MatchExpression a) where
  printLines matchExpr = do
    rendered <- prettyPrintMatchExpr matchExpr
    emitAnnotation matchExpr rendered
    return rendered

prettyPrintMatchExpr :: (Render a) => MatchExpression a -> PrettyPrinter String
prettyPrintMatchExpr matchExpr = case matchExpr of
  MatchAnything _ ->
    return "_"
  MatchVariable _ var ->
    return var
  MatchStructure _ const exprs -> do
    exprsS <- mapM printLines exprs
    return $ const ++ "(" ++ commaSep exprsS ++ ")"
