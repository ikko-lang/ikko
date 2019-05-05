{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module AST.Declaration where

import AST.Annotation (Annotated)
import AST.Expression (Expression)
import AST.Statement (Statement)
import AST.Type (TypeDecl, TypeDef, Predicate, defName)


type File a = [Declaration a]

data Declaration a
  = Let a String (Maybe (TypeDecl a)) (Expression a)
  | Function a String (Maybe (TypeDecl a)) [String] (Statement a)
  | TypeDef a (TypeDef a) (TypeDecl a)
  deriving (Eq, Show, Functor, Foldable, Traversable)


instance Annotated Declaration where
  --  use all default methods


data InstanceDecl a
  = InstanceDecl
    { idAnn :: a
    , idType :: Predicate a -- e.g. `Show [a]`
    , idPredicates :: [Predicate a] -- e.g. `(Show a) =>`
    , idMethods :: [InstanceMethod a]
    }
    deriving (Eq, Show, Functor, Foldable, Traversable)

instance Annotated InstanceDecl where
  -- use all default methods


data InstanceMethod a
  = InstanceMethod
    { imAnn :: a
    , imName :: String
    , imArgs :: [String]
    , imBody :: Statement a
    }
    deriving (Eq, Show, Functor, Foldable, Traversable)

instance Annotated InstanceMethod where
  -- use all default methods


getDeclaredName :: Declaration a -> String
getDeclaredName (Let      _ name _ _)   = name
getDeclaredName (Function _ name _ _ _) = name
getDeclaredName (TypeDef  _ tdef _)     = defName tdef
