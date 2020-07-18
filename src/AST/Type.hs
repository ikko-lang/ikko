{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module AST.Type where

import AST.Annotation (Annotated)

type Type = String


data TypeDecl a
  = TypeName a Type
  | Generic a Type [TypeDecl a]
  | Function a [Predicate a] [TypeDecl a] (TypeDecl a)
  | Struct a [(String, TypeDecl a)]
  | Enum a [(String, EnumOption a)]
  | ClassDecl a [Type] [ClassMethod a] -- superclasses, methods
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Annotated TypeDecl where
  --  use all default methods


data Predicate a
  = Predicate
    { predAnn   :: a
    , predClass :: String
    , predType  :: Type
    }
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Annotated Predicate where
  -- use all default methods


data TypeDef a
  = TypeDef
    { defAnn :: a
    , defName :: Type
    , defGenerics :: [Type] }
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Annotated TypeDef where
  --  use all default methods


data ClassMethod a
  = ClassMethod a String (FuncType a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Annotated ClassMethod where
  -- use all default methods


-- This is a bit gross, but whatever
-- (generics, TFunc _ _)
type FuncType a = ([Type], TypeDecl a)


type EnumOption a = [(String, TypeDecl a)]
