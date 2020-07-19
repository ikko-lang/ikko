{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module AST.Declaration where

import AST.Annotation (Annotated)
import AST.Expression (Expression)
import AST.Statement (Statement)
import AST.Type (TypeDecl, TypeDef, FuncType, defName)

import Util.PrettyPrint (Render, PrettyPrint, render, printLines, writeLine, writeComment)
import Util.Functions (commaSep)


type File a = [Declaration a]


data Declaration a
  = Let a String (Maybe (TypeDecl a)) (Expression a)
  | Function a String (Maybe (FuncType a)) [String] (Statement a)
  | TypeDef a (TypeDef a) (TypeDecl a)
  | Instance a String (TypeDef a) [Declaration a] -- class, type, methods
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Annotated Declaration where
  --  use all default methods

instance (Render a) => PrettyPrint (Declaration a) where
  printLines decl = do
    writeLine "" -- blank separator

    case decl of
      Let      a name _ expr      -> do
        exprS <- printLines expr
        writeComment $ "type of " ++ name ++ ": " ++ render a
        writeLine $ "let " ++ name ++ " = " ++ exprS
      Function a name _ args stmt -> do
        writeComment $ "type of " ++ name ++ ": " ++ render a
        writeLine $ "function " ++ name ++ "(" ++ commaSep args ++ "):"
        _ <- printLines stmt
        return ()
      TypeDef{}                   ->
        return () -- ignore these
      Instance{}                  ->
        return () -- ignore these for now

    return ""

getDeclaredName :: Declaration a -> String
getDeclaredName (Let      _ name _ _)   = name
getDeclaredName (Function _ name _ _ _) = name
getDeclaredName (TypeDef  _ tdef _)     = defName tdef
getDeclaredName (Instance _ _ tdef _)   = defName tdef
