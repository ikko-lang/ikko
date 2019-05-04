{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module AST.Type where

import Data.Char (isLower)

import AST.Annotation (Annotated)


type Type = String

{-
TypeDecl serves dual purposes.

It is used when declaring a type, like `type X struct {...}`. In this case,
the TypeName and Generic variants are presently not allowed at the top level.

It is also used to record types for let declarations and function declarations,
when those are given an explicit type like `let x Int = ...`. In this case,
the Struct and Enum variants are not allowed.
-}
data TypeDecl a
  = TypeName a Type
  | Generic a Type [TypeDecl a] -- e.g. Pair<Int, b>
  | Function a [TypeDecl a] (TypeDecl a) [Predicate a]
  | Struct a [(String, TypeDecl a)]
  | Enum a [(String, EnumOption a)]
  | Predicated a [Predicate a] (TypeDecl a)
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


-- E.g. (Show [a]) becomes (Predicate "Show" (Generic ...))
data Predicate a
  = Predicate a String (TypeDecl a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Annotated Predicate where
  --  use all default methods

predType :: Predicate a -> TypeDecl a
predType (Predicate _ _ td) = td


data ClassDecl a
  = ClassDecl
    { cdAnn :: a
    , cdName :: Type
    , cdTypVar :: Type
    , cdSuperClasses :: [Type]
    , cdMethods :: [ClassMethod a]
    }
    deriving (Eq, Show, Functor, Foldable, Traversable)

instance Annotated ClassDecl where
  --  use all default methods


data ClassMethod a
  = ClassMethod
    { cmAnn :: a
    , cmName :: String
    , cmType :: TypeDecl a
    }
    deriving (Eq, Show, Functor, Foldable, Traversable)

instance Annotated ClassMethod where
  --  use all default methods


class GetGenerics a where
  getGenerics :: a -> [Type]

instance GetGenerics (TypeDecl a) where
  getGenerics t = case t of
    TypeName _ name       ->
      [name | isTypeVar name]
    Generic  _ name ts    ->
      [name | isTypeVar name] ++ getGenerics ts
    Function _ tArgs tRet preds ->
      getGenerics tArgs ++ getGenerics tRet ++ getGenerics preds
    Struct _ fields       ->
      getGenerics fields
    Enum _ options        ->
      getGenerics options
    Predicated _ preds td ->
      getGenerics td ++ getGenerics preds

instance (GetGenerics a) => GetGenerics [a] where
  getGenerics = concatMap getGenerics

instance (GetGenerics a) => GetGenerics (b, a) where
  getGenerics (_, t) = getGenerics t

instance GetGenerics (Predicate a) where
  getGenerics (Predicate _ _ pt) = getGenerics pt


isTypeVar :: Type -> Bool
isTypeVar (c:_) = isLower c
isTypeVar _     = False
