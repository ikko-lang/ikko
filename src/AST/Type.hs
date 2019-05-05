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
  | Function a [TypeDecl a] (TypeDecl a)
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


getGenerics :: TypeDecl a -> [Type]
getGenerics t = case t of
  TypeName _ name       ->
    [name | isTypeVar name]
  Generic  _ name ts    ->
    [name | isTypeVar name] ++ concatMap getGenerics ts
  Function _ tArgs tRet ->
    concatMap getGenerics tArgs ++ getGenerics tRet
  Struct _ fields       ->
    concatMap (getGenerics . snd) fields
  Enum _ options        ->
    concatMap (concatMap (getGenerics . snd) . snd) options
  Predicated _ preds td ->
    getGenerics td ++ concatMap getGenerics [pt | Predicate _ _ pt <- preds]

isTypeVar :: Type -> Bool
isTypeVar (c:_) = isLower c
isTypeVar _     = False
