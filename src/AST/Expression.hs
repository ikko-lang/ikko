{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module AST.Expression where

import AST.Annotation (Annotated, emitAnnotation)
import AST.Type (Type)
import AST.Statement (Statement)

import Util.PrettyPrint (Render, PrettyPrint, PrettyPrinter, printLines)
import Util.Functions (commaSep)

data Value a
  = StrVal     a String
  | BoolVal    a Bool
  | IntVal     a Int
  | FloatVal   a Float
  | StructVal  a String [(String, Expression a)]
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Annotated Value where
  --  use all default methods

instance (Render a) => PrettyPrint (Value a) where
  printLines val = do
    rendered <- case  val of
                 StrVal    _ s ->
                   return $ "\"" ++ show s ++ "\""
                 BoolVal   _ b ->
                   return $ show b
                 IntVal    _ i ->
                   return $ show i
                 FloatVal  _ f ->
                   return $ show f
                 StructVal _ name fields -> do
                   inner <- showFields fields
                   return $ name ++ "{" ++ inner ++ "}"
    emitAnnotation val rendered
    return rendered

showFields :: (Render a) => [(String, Expression a)] -> PrettyPrinter String
showFields fields = do
  fieldStrings <- mapM showField fields
  return $ commaSep fieldStrings

showField :: (PrettyPrint a) => (String, a) -> PrettyPrinter String
showField (name, expr) = do
  exprString <- printLines expr
  return $ name ++ ": " ++ exprString

data Expression a
  = Paren   a (Expression a)
  | Val     a (Value a)
  | Unary   a UnaryOp (Expression a)
  | Binary  a BinOp (Expression a) (Expression a)
  | Call    a (Expression a) [Expression a]
  | Cast    a Type (Expression a)
  | Var     a String
  | Access  a (Expression a) String
  -- TODO: Think about allowing a name and type
  | Lambda  a [String] (Statement a) -- args, body
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Annotated Expression where
  --  use all default methods

instance (Render a) => PrettyPrint (Expression a) where
  printLines expr = do
    rendered <- case expr of
          Paren  _ e1        -> do
            inner <- printLines e1
            return $ "(" ++ inner ++ ")"
          Val    _ v         ->
            printLines v
          Unary  _ uop e1    -> do
            uopS <- printLines uop
            e1S <- printLines e1
            return $ uopS ++ " " ++ e1S
          Binary _ bop e1 e2 -> do
            e1S  <- printLines e1
            bopS <- printLines bop
            e2S  <- printLines e2
            return $ e1S ++ " " ++ bopS ++ " " ++ e2S
          Call   _ fn args   -> do
            fnS   <- printLines fn
            argsS <- mapM printLines args
            return $ fnS ++ "(" ++ commaSep argsS ++ ")"
          Cast   _ t e1      -> do
            e1S <- printLines e1
            return $ t ++ "(" ++ e1S ++ ")"
          Var    _ name      ->
            return name
          Access _ e1 name    -> do
            e1S <- printLines e1
            return $ e1S ++ "." ++ name

    emitAnnotation expr rendered
    return rendered

data UnaryOp
  = BitInvert
  | BoolNot
  deriving (Eq, Show)

instance PrettyPrint UnaryOp where
  printLines uop = return $ case uop of
    BitInvert -> "~"
    BoolNot   -> "!"

data BinOp
  = Plus
  | Minus
  | Times
  | Divide
  | Mod
  | Power
  | BitAnd
  | BitOr
  | BitXor
  | BoolAnd
  | BoolOr
  | Eq
  | NotEq
  | Less
  | LessEq
  | Greater
  | GreaterEq
  | LShift
  | RShift
  deriving (Eq, Show)

instance PrettyPrint BinOp where
  printLines bop = return $ case bop of
                              Plus      -> "+"
                              Minus     -> "-"
                              Times     -> "*"
                              Divide    -> "/"
                              Mod       -> "%"
                              Power     -> "**"
                              BitAnd    -> "&"
                              BitOr     -> "|"
                              BitXor    -> "^"
                              BoolAnd   -> "&&"
                              BoolOr    -> "||"
                              Eq        -> "=="
                              NotEq     -> "!="
                              Less      -> "<"
                              LessEq    -> "<="
                              Greater   -> ">"
                              GreaterEq -> ">="
                              LShift    -> "<<"
                              RShift    -> ">>"
