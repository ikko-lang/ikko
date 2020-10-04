{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

-- AST is one giant module due to circular dependencies
-- and to avoid dangling instances
module AST where

import Data.Char
  ( isLower
  )

import Data.List
  ( intercalate
  )

import Data.Set (Set)
import qualified Data.Set as Set

import Util.PrettyPrint
  ( PrettyPrint
  , PrettyPrinter
  , Render
  , decreaseIndent
  , increaseIndent
  , printLines
  , prettyPrint
  , render
  , writeComment
  , writeLine
  )

import Util.Functions
  ( commaSep
  , unions
  )

import AST.Annotation
  ( Annotated
  , Annotation
  , emitAnnotation
  , emptyAnnotation
  )


---------------------------
---- Utility functions ----
---------------------------

getDeclaredName :: Declaration a -> String
getDeclaredName (DLet      _ name _ _)   = name
getDeclaredName (DFunction _ name _ _ _) = name
getDeclaredName (DTypeDef  _ tdef _)     = defName tdef
getDeclaredName (DInstance _ _ tdef _ _) = defName tdef

isTypeVar :: Type -> Bool
isTypeVar = isLower . head

gatherTypeVars :: TypeDecl a -> Set Type
gatherTypeVars td = case td of
  (TName _ t) ->
    if isTypeVar t then Set.singleton t else Set.empty
  (TGeneric _ t ts) ->
    let tvs = gatherTVList ts
    in if isTypeVar t then Set.insert t tvs else tvs
  (TFunction _ ps ats rt) ->
    let predTypes = map predType ps
        predTVs = Set.fromList (filter isTypeVar predTypes)
        tvs = gatherTVList (rt : ats)
    in Set.union predTVs tvs
  (TStruct _ fields) ->
    let types = map snd fields
    in gatherTVList types
  (TEnum _ branches) ->
    let options = map snd branches
        types = concatMap (map snd) options
    in gatherTVList types
  (TClassDecl _ _ methods) ->
    let methodTypes = [t | ClassMethod _ _ t <- methods]
    in gatherTVList methodTypes

gatherTVList :: [TypeDecl a] -> Set Type
gatherTVList = unions . map gatherTypeVars

typeDefToDecl :: TypeDef Annotation -> TypeDecl Annotation
typeDefToDecl def = TGeneric (defAnn def) (defName def) generics
  where generics = [TName emptyAnnotation t | t <- defGenerics def]

---------------------------
---- AST structures ----
---------------------------

-- A simple type, like 'Int'
type Type = String

-- A definition of a type
data TypeDecl a
  = TName a Type
  | TGeneric a Type [TypeDecl a]
  | TFunction a [Predicate a] [TypeDecl a] (TypeDecl a)
  | TStruct a [(String, TypeDecl a)]
  | TEnum a [(String, EnumOption a)]
  | TClassDecl a [Type] [ClassMethod a] -- superclasses, methods
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- One variant of an enum
type EnumOption a = [(String, TypeDecl a)]

-- A single method in a class definition
data ClassMethod a
  = ClassMethod a String (TypeDecl a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- A predicate asserting that a type have a class
data Predicate a
  = Predicate
    { predAnn   :: a
    , predClass :: String
    , predType  :: Type
    }
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- A structure for types used on the left-hand side when declaring types
data TypeDef a
  = TypeDef
    { defAnn :: a
    , defName :: Type
    , defGenerics :: [Type] }
  deriving (Eq, Show, Functor, Foldable, Traversable)


-- The contents of a file
type File a = [Declaration a]

-- A single declaration
data Declaration a
  = DLet a String (Maybe (TypeDecl a)) (Expression a)
  | DFunction a String (Maybe (TypeDecl a)) [String] (Statement a)
  | DTypeDef a (TypeDef a) (TypeDecl a)
  | DInstance a String (TypeDef a) [Predicate a] [Declaration a] -- class, type, predicates, methods
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- A single statement within a function
data Statement a
  = Return  a (Maybe (Expression a))
  | Let     a String (Maybe (TypeDecl a)) (Expression a)
  | Assign  a [String] (Expression a)
  | Block   a [Statement a]
  | Expr    a (Expression a) -- e.g. just calling a function
  | If      a (Expression a) [Statement a] (Maybe (Statement a))
  | While   a (Expression a) [Statement a]
  | Match   a (Expression a) [MatchCase a]
  | Pass    a
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- An arm of a match statement
data MatchCase a
  = MatchCase (MatchExpression a) (Statement a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- A pattern in an arm of a match statement
data MatchExpression a
  = MatchAnything  a
  | MatchVariable  a String
  | MatchStructure a String [MatchExpression a]
  deriving (Eq, Show, Functor, Foldable, Traversable)


data Expression a
  = Paren   a (Expression a)
  | Val     a (Value a)
  | Unary   a UnaryOp (Expression a)
  | Binary  a BinOp (Expression a) (Expression a)
  | Call    a (Expression a) [Expression a]
  | Cast    a Type (Expression a)
  | Var     a String
  | Access  a (Expression a) String
  -- TODO: Think about allowing a name and type
  | Lambda  a [String] (Statement a) -- args, body
  deriving (Eq, Show, Functor, Foldable, Traversable)

data Value a
  = StrVal     a String
  | BoolVal    a Bool
  | IntVal     a Int
  | FloatVal   a Float
  | StructVal  a String [(String, Expression a)]
  deriving (Eq, Show, Functor, Foldable, Traversable)

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

-----------------------------
---- Annotated instances ----
-----------------------------

instance Annotated TypeDecl where
  --  use all default methods

instance Annotated Predicate where
  --  use all default methods

instance Annotated TypeDef where
  --  use all default methods

instance Annotated ClassMethod where
  --  use all default methods

instance Annotated Declaration where
  --  use all default methods

instance Annotated Statement where
  --  use all default methods

instance Annotated MatchCase where
  --  use all default methods

instance Annotated MatchExpression where
  --  use all default methods

instance Annotated Expression where
  --  use all default methods

instance Annotated Value where
  --  use all default methods


-------------------------------
---- PrettyPrint instances ----
-------------------------------

instance (Render a) => PrettyPrint (Declaration a) where
  printLines decl = do
    writeLine "" -- blank separator

    case decl of
      DLet      a name _ expr      -> do
        exprS <- printLines expr
        writeComment $ "type of " ++ name ++ ": " ++ render a
        writeLine $ "let " ++ name ++ " = " ++ exprS
      DFunction a name _ args stmt -> do
        writeComment $ "type of " ++ name ++ ": " ++ render a
        writeLine $ "fn " ++ name ++ "(" ++ commaSep args ++ "):"
        _ <- printLines stmt
        return ()
      DTypeDef{}                   ->
        return () -- ignore these
      DInstance{}                  ->
        return () -- ignore these for now

    return ""

instance (Render a) => PrettyPrint (Statement a) where
  printLines stmt = do
    prettyPrintStatement stmt
    return ""

prettyPrintStatement :: (Render a) => Statement a -> PrettyPrinter ()
prettyPrintStatement stmt = case stmt of
  Return _ Nothing ->
    writeLine "return"
  Return _ (Just expr) -> do
    exprS <- printLines expr
    writeLine $ "return " ++ exprS
  Let _ name _ expr -> do
    exprS <- printLines expr
    writeLine $ "let " ++ name ++ " = " ++ exprS
  Assign _ names expr -> do
    exprS <- printLines expr
    writeLine $ intercalate "." names ++ " = " ++ exprS
  Block _ stmts -> do
    increaseIndent
    mapM_ printLines stmts
    decreaseIndent
  Expr _ expr -> do
    exprS <- printLines expr
    writeLine exprS
  If _ test t e -> do
    testS <- printLines test
    writeLine $ "if " ++ testS ++ ":"
    increaseIndent
    mapM_ printLines t
    decreaseIndent
    case e of
      Nothing -> return ()
      Just st -> do
        _ <- printLines st
        return ()
  While _ test stmts -> do
    testS <- printLines test
    writeLine $ "while " ++ testS ++ ":"
    increaseIndent
    mapM_ printLines stmts
    decreaseIndent
  Match _ expr cases -> do
    exprS <- printLines expr
    writeLine $ "match " ++ exprS ++ ":"
    increaseIndent
    mapM_ printLines cases
    decreaseIndent
  Pass _ ->
    writeLine "pass"

instance (Render a) => PrettyPrint (MatchCase a) where
  printLines (MatchCase matchExpr stmt) = do
    exprS <- printLines matchExpr
    writeLine $ exprS ++ ":"
    increaseIndent
    _ <- printLines stmt
    decreaseIndent
    return ""

instance (Render a) => PrettyPrint (MatchExpression a) where
  printLines matchExpr = do
    rendered <- prettyPrintMatchExpr matchExpr
    emitAnnotation matchExpr rendered
    return rendered

prettyPrintMatchExpr :: (Render a) => MatchExpression a -> PrettyPrinter String
prettyPrintMatchExpr matchExpr = case matchExpr of
  MatchAnything _ ->
    return "_"
  MatchVariable _ var ->
    return var
  MatchStructure _ cnst exprs -> do
    exprsS <- mapM printLines exprs
    return $ cnst ++ "(" ++ commaSep exprsS ++ ")"

instance (Render a) => PrettyPrint (Expression a) where
  printLines expr = do
    rendered <- case expr of
          Paren  _ e1        -> do
            inner <- printLines e1
            return $ "(" ++ inner ++ ")"
          Val    _ v         ->
            printLines v
          Unary  _ uop e1    -> do
            uopS <- printLines uop
            e1S <- printLines e1
            return $ uopS ++ " " ++ e1S
          Binary _ bop e1 e2 -> do
            e1S  <- printLines e1
            bopS <- printLines bop
            e2S  <- printLines e2
            return $ e1S ++ " " ++ bopS ++ " " ++ e2S
          Call   _ fn args   -> do
            fnS   <- printLines fn
            argsS <- mapM printLines args
            return $ fnS ++ "(" ++ commaSep argsS ++ ")"
          Cast   _ t e1      -> do
            e1S <- printLines e1
            return $ t ++ "(" ++ e1S ++ ")"
          Var    _ name      ->
            return name
          Access _ e1 name    -> do
            e1S <- printLines e1
            return $ e1S ++ "." ++ name
          Lambda _ args body  -> do
            return $ "fn(" ++ commaSep args ++ "):\n" ++ prettyPrint body

    emitAnnotation expr rendered
    return rendered


instance (Render a) => PrettyPrint (Value a) where
  printLines val = do
    rendered <- case  val of
                 StrVal    _ s ->
                   return $ show s
                 BoolVal   _ b ->
                   return $ show b
                 IntVal    _ i ->
                   return $ show i
                 FloatVal  _ f ->
                   return $ show f
                 StructVal _ name fields -> do
                   inner <- showFields fields
                   return $ name ++ "{" ++ inner ++ "}"
    emitAnnotation val rendered
    return rendered

showFields :: (Render a) => [(String, Expression a)] -> PrettyPrinter String
showFields fields = do
  fieldStrings <- mapM showField fields
  return $ commaSep fieldStrings

showField :: (PrettyPrint a) => (String, a) -> PrettyPrinter String
showField (name, expr) = do
  exprString <- printLines expr
  return $ name ++ ": " ++ exprString

instance PrettyPrint UnaryOp where
  printLines uop = return $ case uop of
    BitInvert -> "~"
    BoolNot   -> "!"

instance PrettyPrint BinOp where
  printLines bop = return $ case bop of
    Plus      -> "+"
    Minus     -> "-"
    Times     -> "*"
    Divide    -> "/"
    Mod       -> "%"
    Power     -> "**"
    BitAnd    -> "&"
    BitOr     -> "|"
    BitXor    -> "^"
    BoolAnd   -> "&&"
    BoolOr    -> "||"
    Eq        -> "=="
    NotEq     -> "!="
    Less      -> "<"
    LessEq    -> "<="
    Greater   -> ">"
    GreaterEq -> ">="
    LShift    -> "<<"
    RShift    -> ">>"
