{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module AST.Expression where

import AST.Annotation (Annotated)
import AST.Type (Type)

data Value a
  = StrVal     a String
  | BoolVal    a Bool
  | IntVal     a Int
  | FloatVal   a Float
  | StructVal  a String [(String, Expression a)]
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Annotated Value where
  --  use all default methods

data Expression a
  = Paren   a (Expression a)
  | Val     a (Value a)
  | Unary   a UnaryOp (Expression a)
  | Binary  a BinOp (Expression a) (Expression a)
  | Call    a (Expression a) [Expression a]
  | Cast    a Type (Expression a)
  | Var     a String
  | Access  a (Expression a) String
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Annotated Expression where
  --  use all default methods

data UnaryOp
  = BitInvert
  | BoolNot
  deriving (Eq, Show)

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
