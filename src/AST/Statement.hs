{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module AST.Statement where

import AST.Annotation (Annotated)
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

data MatchCase a
  = MatchCase (MatchExpression a) (Statement a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Annotated MatchCase where
  --  use all default methods

-- TODO: add support for matching literal values (int, string)
data MatchExpression a
  = MatchAnything  a
  | MatchVariable  a String
  | MatchStructure a String [MatchExpression a]
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Annotated MatchExpression where
  --  use all default methods
