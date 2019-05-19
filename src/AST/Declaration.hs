{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module AST.Declaration where

import AST.Annotation (Annotated)
import AST.Expression (Expression)
import AST.Statement (Statement)
import AST.Type (Type, TypeDecl, TypeDef, defName)

type File a = [Declaration a]

data Declaration a
  = Let a String (Maybe (TypeDecl a)) (Expression a)
  | Function a String (Maybe (FuncType a)) [String] (Statement a)
  | TypeDef a (TypeDef a) (TypeDecl a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Annotated Declaration where
  --  use all default methods

-- This is a bit gross, but whatever
-- (generics, TFunc _ _)
type FuncType a = ([Type], TypeDecl a)

getDeclaredName :: Declaration a -> String
getDeclaredName (Let      _ name _ _)   = name
getDeclaredName (Function _ name _ _ _) = name
getDeclaredName (TypeDef  _ tdef _)     = defName tdef
