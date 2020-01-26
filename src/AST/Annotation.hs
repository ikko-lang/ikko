module AST.Annotation where

import Control.Monad (unless)

import Region (Region)
import Types (Type)
import Util.DoOnce (evalDoOnce, isFirst)
import Util.PrettyPrint (Render, PrettyPrinter, render, writeComment)

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

instance Render Metadata where
  render (Location _) = ""
  render (Typed t)    = render t

emptyAnnotation :: Annotation
emptyAnnotation = []

data Metadata
  = Location Region
  | Typed Type
  deriving (Eq, Show)

addAnnotation :: (Annotated a) => Metadata -> a Annotation -> a Annotation
addAnnotation metadata a =
  setAnnotation (metadata : getAnnotation a) a

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

-- Adds the annotated type as a comment, assuming there is an annotated type.
-- The comment also contains the rendered partial expression, so you can tell
-- multiple comments apart.
emitAnnotation :: (Annotated f, Render a) => f a -> String -> PrettyPrinter ()
emitAnnotation ast rendered = do
  let annotation = render $ getAnnotation ast
  unless (null annotation) $
    writeComment ("`" ++ rendered ++ "` has type: " ++ annotation)

listToMaybe :: [a] -> Maybe a
listToMaybe []    = Nothing
listToMaybe (a:_) = Just a
