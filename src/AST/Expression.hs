module AST.Expression where

import AST.Annotation (Annotation, Annotated, getAnnotation, setAnnotation, removeAnnotations)
import AST.Type (Type)
import Util.Functions (mapSnd)

data Value
  = StrVal     Annotation String
  | BoolVal    Annotation Bool
  | IntVal     Annotation Int
  | FloatVal   Annotation Float
  | StructVal  Annotation String [(String, Expression)]
  deriving (Eq, Show)

data Expression
  = Paren   Annotation Expression
  | Val     Annotation Value
  | Unary   Annotation UnaryOp Expression
  | Binary  Annotation BinOp Expression Expression
  | Call    Annotation Expression [Expression]
  | Cast    Annotation Type Expression
  | Var     Annotation String
  | Access  Annotation Expression String
  deriving (Eq, Show)

data UnaryOp
  = BitInvert
  | BoolNot
  deriving (Eq, Show)

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


instance Annotated Value where
  getAnnotation val = case val of
    StrVal    a _   -> a
    BoolVal   a _   -> a
    IntVal    a _   -> a
    FloatVal  a _   -> a
    StructVal a _ _ -> a

  setAnnotation ann val = case val of
    StrVal    _ s   -> StrVal    ann s
    BoolVal   _ b   -> BoolVal   ann b
    IntVal    _ i   -> IntVal    ann i
    FloatVal  _ f   -> FloatVal  ann f
    StructVal _ t f -> StructVal ann t f

  removeAnnotations val = case val of
    StrVal    _ s   -> StrVal    [] s
    BoolVal   _ b   -> BoolVal   [] b
    IntVal    _ i   -> IntVal    [] i
    FloatVal  _ f   -> FloatVal  [] f
    StructVal _ t f -> StructVal [] t (mapSnd removeAnnotations f)


instance Annotated Expression where
  getAnnotation expr = case expr of
    Paren   a _     -> a
    Val     a _     -> a
    Unary   a _ _   -> a
    Binary  a _ _ _ -> a
    Call    a _ _   -> a
    Cast    a _ _   -> a
    Var     a _     -> a
    Access  a _ _   -> a

  setAnnotation ann expr = case expr of
    Paren   _ e     -> Paren   ann e
    Val     _ v     -> Val     ann v
    Unary   _ o e   -> Unary   ann o e
    Binary  _ o l r -> Binary  ann o l r
    Call    _ f a   -> Call    ann f a
    Cast    _ t e   -> Cast    ann t e
    Var     _ v     -> Var     ann v
    Access  _ e f   -> Access  ann e f

  removeAnnotations expr = case expr of
    Paren   _ e     -> Paren   [] (removeAnnotations e)
    Val     _ v     -> Val     [] (removeAnnotations v)
    Unary   _ o e   -> Unary   [] o (removeAnnotations e)
    Binary  _ o l r -> Binary  [] o (removeAnnotations l) (removeAnnotations r)
    Call    _ f a   -> Call    [] (removeAnnotations f) (map removeAnnotations a)
    Cast    _ t e   -> Cast    [] t (removeAnnotations e)
    Var     _ v     -> Var     [] v
    Access  _ e f   -> Access  [] (removeAnnotations e) f
