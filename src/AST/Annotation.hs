{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module AST.Annotation where

import Region (Region)
import Types (Type)
import Util.DoOnce (evalDoOnce, isFirst)

class (Functor f, Foldable f, Traversable f) => Annotated f where
  getAnnotation :: f a -> a
  getAnnotation ast = head $ foldr (:) [] ast

  removeAnnotations :: f a -> f Annotation
  removeAnnotations = fmap (const emptyAnnotation)

  -- setAnnotation has a truely horrifying implementation,
  -- but I can't think of a better way right now
  setAnnotation :: Annotation -> f Annotation -> f Annotation
  setAnnotation ann ast =
    let replaceFirst val = do
          replace <- isFirst
          return $ if replace then ann else val
    in evalDoOnce $ traverse replaceFirst ast


type Annotation = [Metadata]

emptyAnnotation :: Annotation
emptyAnnotation = []


data Metadata
  = Location Region
  | Typed Type
  deriving (Eq, Show)


addAnnotation :: (Annotated a) => Metadata -> a Annotation -> a Annotation
addAnnotation metadata a =
  setAnnotation (metadata : getAnnotation a) a


data WithAnnotation b a =
  WithAnnotation a b
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Annotated (WithAnnotation b) where
  --  use all default methods


unannotate :: WithAnnotation b a -> b
unannotate (WithAnnotation _ x) = x


addType :: (Annotated a) => Type -> a Annotation -> a Annotation
addType t =
  addAnnotation (Typed t)

addLocation :: (Annotated a) => Region -> a Annotation -> a Annotation
addLocation r =
  addAnnotation (Location r)


getType :: (Annotated a) => a Annotation -> Maybe Type
getType node =
  listToMaybe [t | Typed t <- getAnnotation node]

getLocation :: (Annotated a) => a Annotation -> Maybe Region
getLocation node =
  listToMaybe [r | Location r <- getAnnotation node]


listToMaybe :: [a] -> Maybe a
listToMaybe []    = Nothing
listToMaybe (a:_) = Just a
