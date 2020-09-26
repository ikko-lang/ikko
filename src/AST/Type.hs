{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module AST.Type where


import AST.Annotation (Annotated, Annotation, emptyAnnotation)

import Data.Char (isLower)
import qualified Data.Set as Set
import Data.Set (Set)


type Type = String

-- TODO Refactor: This structure is a mess
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

isTypeVar :: Type -> Bool
isTypeVar = isLower . head

gatherTypeVars :: TypeDecl a -> Set Type
gatherTypeVars td = case td of
  (TypeName _ t) ->
    if isTypeVar t then Set.singleton t else Set.empty
  (Generic _ t ts) ->
    let tvs = gatherTVList ts
    in if isTypeVar t then Set.insert t tvs else tvs
  (Function _ ps ats rt) ->
    let predTypes = map predType ps
        predTVs = Set.fromList (filter isTypeVar predTypes)
        tvs = gatherTVList (rt : ats)
    in Set.union predTVs tvs
  (Struct _ fields) ->
    let types = map snd fields
    in gatherTVList types
  (Enum _ branches) ->
    let options = map snd branches
        types = concatMap (map snd) options
    in gatherTVList types
  (ClassDecl _ _ methods) ->
    let methodTypes = [t | ClassMethod _ _ (_, t) <- methods]
    in gatherTVList methodTypes


gatherTVList :: [TypeDecl a] -> Set Type
gatherTVList = unions . map gatherTypeVars

unions :: (Ord a) => [Set a] -> Set a
unions = foldl Set.union Set.empty

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

typeDefToDecl :: TypeDef Annotation -> TypeDecl Annotation
typeDefToDecl def = Generic (defAnn def) (defName def) generics
  where generics = [TypeName emptyAnnotation t | t <- defGenerics def]


data ClassMethod a
  = ClassMethod a String (FuncType a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Annotated ClassMethod where
  -- use all default methods


-- This is a bit gross, but whatever
-- (generics, TFunc _ _)
type FuncType a = ([Type], TypeDecl a)


type EnumOption a = [(String, TypeDecl a)]
