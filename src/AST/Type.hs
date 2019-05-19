{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module AST.Type where

import AST.Annotation (Annotated)

type Type = String

data TypeDecl a
  = TypeName a Type
  | Generic a Type [TypeDecl a]
  | Function a [TypeDecl a] (TypeDecl a)
  | Struct a [(String, TypeDecl a)]
  | Enum a [(String, EnumOption a)]
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Annotated TypeDecl where
  --  use all default methods

data TypeDef a
  = TypeDef
    { defAnn :: a
    , defName :: Type
    , defGenerics :: [Type] }
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Annotated TypeDef where
  --  use all default methods

type EnumOption a = [(String, TypeDecl a)]
