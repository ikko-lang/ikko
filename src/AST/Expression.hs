{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module AST.Expression where

import AST.Annotation (Annotated, getAnnotation)
import AST.Type (Type)

import Util.PrettyPrint
import Util.Functions (commaSep)

data Value a
  = StrVal     a String
  | BoolVal    a Bool
  | IntVal     a Int
  | FloatVal   a Float
  | StructVal  a String [(String, Expression a)]
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Annotated Value where
  --  use all default methods

instance (PrettyPrint a) => PrettyPrint (Value a) where
  printer _ verbose val =
    let printedVal = case val of
          StrVal _ s -> "\"" ++ show s ++ "\""
          BoolVal _ b -> show b
          IntVal _ i -> show i
          FloatVal _ f -> show f
          StructVal _ name fields -> name ++ "{" ++ showFields verbose fields ++ "}"
    in printedVal ++ annotationSuffix verbose val

annotationSuffix verbose a =
  if verbose
  then let annotation = prettyPrint $ getAnnotation a
       in if not (null annotation)
          then " /* type: " ++ annotation ++ " */"
          else ""
  else ""

showFields :: (PrettyPrint a) => Bool -> [(String, Expression a)] -> String
showFields verbose fields =
  let showField (name, expr) = name ++ ": " ++ (printer 0 verbose expr)
  in commaSep $ map showField fields

data Expression a
  = Paren   a (Expression a)
  | Val     a (Value a)
  | Unary   a UnaryOp (Expression a)
  | Binary  a BinOp (Expression a) (Expression a)
  | Call    a (Expression a) [Expression a]
  | Cast    a Type (Expression a)
  | Var     a String
  | Access  a (Expression a) String
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Annotated Expression where
  --  use all default methods

instance (PrettyPrint a) => PrettyPrint (Expression a) where
  printer _ verbose expr =
    let printed = case expr of
          Paren _ e1 -> "(" ++ printer 0 verbose e1 ++ ")"
          Val _ v -> printer 0 verbose v
          Unary _ uop e1 -> prettyPrint uop ++ " " ++ printer 0 verbose e1
          Binary _ bop e1 e2 ->
            printer 0 verbose e1 ++ " " ++
            prettyPrint bop ++ " " ++
            printer 0 verbose e2
          Call _ fn args -> undefined
          Cast _ t e1 -> undefined
          Var _ name -> name
          Access _ e name -> undefined
    in printed ++ annotationSuffix verbose expr

data UnaryOp
  = BitInvert
  | BoolNot
  deriving (Eq, Show)

instance PrettyPrint UnaryOp where
  printer _ _ BitInvert = "~"
  printer _ _ BoolNot   = "!"

data BinOp
  = Plus
  | Minus
  | Times
  | Divide
  | Mod
  | Power
  | BitAnd
  | BitOr
  | BitXor
  | BoolAnd
  | BoolOr
  | Eq
  | NotEq
  | Less
  | LessEq
  | Greater
  | GreaterEq
  | LShift
  | RShift
  deriving (Eq, Show)

instance PrettyPrint BinOp where
  printer _ _ bop = undefined -- TODO
