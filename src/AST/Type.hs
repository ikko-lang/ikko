module AST.Type where

import Util.Functions
import AST.Annotation
    ( Annotation
    , Annotated
    , getAnnotation
    , setAnnotation
    , removeAnnotations
    )

type Type = String

data TypeDecl
  = TypeName Annotation Type
  | Generic Annotation Type [TypeDecl]
  | Function Annotation [TypeDecl] TypeDecl
  | Struct Annotation [(String, TypeDecl)]
  | Enum Annotation [(String, EnumOption)]
  deriving (Eq, Show)


data TypeDef = TypeDef
               { defAnn :: Annotation
               , defName :: Type
               , defGenerics :: [Type] }
             deriving (Eq, Show)

type EnumOption = [(String, TypeDecl)]

instance Annotated TypeDecl where
  getAnnotation tdecl = case tdecl of
    TypeName  a _   -> a
    Generic   a _ _ -> a
    Function  a _ _ -> a
    Struct    a _   -> a
    Enum      a _   -> a

  setAnnotation ann tdecl = case tdecl of
    TypeName  _ t   -> TypeName ann t
    Generic   _ t g -> Generic  ann t g
    Function  _ a r -> Function ann a r
    Struct    _ f   -> Struct   ann f
    Enum      _ o   -> Enum     ann o

  removeAnnotations tdecl = case tdecl of
    TypeName  _ t   -> TypeName [] t
    Generic   _ t g -> Generic  [] t (map removeAnnotations g)
    Function  _ a r -> Function [] (map removeAnnotations a) (removeAnnotations r)
    Struct    _ f   -> Struct   [] (mapSnd removeAnnotations f)
    Enum      _ o   -> Enum     [] (mapSnd (mapSnd removeAnnotations) o)

instance Annotated TypeDef where
  getAnnotation = defAnn
  setAnnotation ann tdef = tdef { defAnn=ann }
  removeAnnotations = setAnnotation []
