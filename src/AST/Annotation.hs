module AST.Annotation where

import Region (Region)
import Types (Type)


class Annotated a where
  getAnnotation :: a -> Annotation
  setAnnotation :: Annotation -> a -> a
  -- unlike the other functions, removeAnnotations is recurisve
  removeAnnotations :: a -> a


type Annotation = [Metadata]

emptyAnnotation :: Annotation
emptyAnnotation = []

data Metadata
  = Location Region
  | Typed Type
  deriving (Eq, Show)


addAnnotation :: (Annotated a) => Metadata -> a -> a
addAnnotation metadata a = setAnnotation (metadata : getAnnotation a) a

addType :: (Annotated a) => Type -> a -> a
addType t = addAnnotation (Typed t)

addLocation :: (Annotated a) => Region -> a -> a
addLocation r = addAnnotation (Location r)


getType :: (Annotated a) => a -> Maybe Type
getType node = listToMaybe [t | Typed t <- getAnnotation node]

getLocation :: (Annotated a) => a -> Maybe Region
getLocation node = listToMaybe [r | Location r <- getAnnotation node]


listToMaybe :: [a] -> Maybe a
listToMaybe []    = Nothing
listToMaybe (a:_) = Just a
