module AST.Declaration where

import AST.Annotation
    ( Annotation
    , Annotated
    , getAnnotation
    , setAnnotation
    , removeAnnotations
    )

import AST.Expression (Expression)
import AST.Statement (Statement)
import AST.Type (Type, TypeDecl, TypeDef, defName)
import Util.Functions (applySnd)

type File = [Declaration]

data Declaration
  = Let Annotation String (Maybe TypeDecl) Expression
  | Function Annotation String (Maybe FuncType) [String] Statement
  | TypeDef Annotation TypeDef TypeDecl
  deriving (Eq, Show)

-- This is a bit gross, but whatever
-- (generics, TFunc _ _)
type FuncType = ([Type], TypeDecl)

getDeclaredName :: Declaration -> String
getDeclaredName (Let      _ name _ _)   = name
getDeclaredName (Function _ name _ _ _) = name
getDeclaredName (TypeDef  _ tdef _)     = defName tdef

instance Annotated Declaration where
  getAnnotation decl = case decl of
    Let      a _ _ _   -> a
    Function a _ _ _ _ -> a
    TypeDef  a _ _     -> a

  setAnnotation ann decl = case decl of
    Let      _ n t e   -> Let      ann n t e
    Function _ n t a s -> Function ann n t a s
    TypeDef  _ s t     -> TypeDef  ann s t

  removeAnnotations decl = case decl of
    Let      _ n t e   -> Let      [] n (fmap removeAnnotations t) (removeAnnotations e)
    Function _ n t a s -> Function [] n (fmap (applySnd removeAnnotations) t) a (removeAnnotations s)
    TypeDef  _ s t     -> TypeDef  [] (removeAnnotations s) (removeAnnotations t)
