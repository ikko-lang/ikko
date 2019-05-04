{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module AST.Declaration where


import AST.Annotation (Annotated, WithAnnotation, unannotate)
import AST.Expression (Expression)
import AST.Statement (Statement)
import AST.Type (TypeDecl, TypeDef, Predicate, defName)


type File a = [Declaration a]

data Declaration a
  = Let a String (Maybe (TypeDecl a)) (Expression a)
  | Function a String (Maybe (TypeDecl a)) [String] (Statement a)
  | TypeDef a (TypeDef a) (TypeDecl a)
  | TraitDecl
    { tdAnn :: a
    , tdName :: WithAnnotation String a -- Name of trait declared
    , tdExtends :: [WithAnnotation String a] -- Parent trait names
    , tdMethods :: [TraitMethod a]
    }
  | InstanceDecl
    { idAnn :: a
    , idName :: WithAnnotation String a -- Name of trait implemented, e.g. `Show`
    , idType :: TypeDecl a -- Type the trait is implemented for, e.g. `[a]`
    , idPredicates :: [Predicate a] -- e.g. Show a
    , idMethods :: [InstanceMethod a]
    }
  deriving (Eq, Show, Functor, Foldable, Traversable)


instance Annotated Declaration where
  --  use all default methods


data TraitMethod a
  = TraitMethod
    { tmAnn :: a
    , tmName :: String
    , tmType :: TypeDecl a
    }
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Annotated TraitMethod where
  --  use all default methods

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
getDeclaredName td@TraitDecl{}          = unannotate $ tdName td
getDeclaredName is@InstanceDecl{}       = unannotate $ idName is
