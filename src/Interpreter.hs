module Interpreter (interpret) where

import Data.Bits (complement, (.&.), (.|.), xor, shiftL, shiftR)
import Data.Map (Map)
import Data.List (intercalate)
import System.IO (hFlush, stdout)
import qualified Data.Map as Map

import Control.Applicative ((<$>))
import Control.Monad (zipWithM)

import Data.IORef

import Inference
  ( InferResult(..)
  , topLevelBindings )
import AST.Annotation (Annotation, addType)
import qualified AST
import Types (tUnit, makeFuncType, qualify)


type ExpressionT      = AST.Expression      Annotation
type ValueT           = AST.Value           Annotation
type MatchCaseT       = AST.MatchCase       Annotation
type MatchExpressionT = AST.MatchExpression Annotation
type StatementT       = AST.Statement       Annotation


interpret :: InferResult -> IO ()
interpret body =
  let mainT = makeFuncType [] tUnit
      callMain = AST.Call [] (addType (qualify mainT) $ AST.Var [] "main") []
  in do
    scope <- startingState body
    _ <- interpretExpr scope callMain
    return ()


startingState :: InferResult -> IO Scope
startingState body = do
  rootScope <- newIORef builtIns
  let scope = [rootScope]
  let binds = topLevelBindings body
  -- TODO: Also evaluate constant expression
  let functions =
        [ (name, toClosure scope args stmt)
        | (name, AST.DFunction _ _ _ args stmt) <- binds ]
  insertAll functions scope

builtIns :: Map String Value
builtIns =
  Map.fromList
  [ ("print", VBuiltIn "print") ]

insertAll :: [(String, Value)] -> Scope -> IO Scope
insertAll [] scope = return scope
insertAll ((name, val):rest) (sc:ss) = do
  bottomScope <- readIORef sc
  let bottomScope' = Map.insert name val bottomScope
  writeIORef sc bottomScope'
  insertAll rest (sc:ss)
insertAll _ [] = error "How did insertAll get a scope with no parts?"


interpretExpr :: Scope -> ExpressionT -> IO Value
interpretExpr scope expr = case expr of
  AST.Paren _ ex ->
    interpretExpr scope ex
  AST.Val _ val ->
    interpretVal scope val
  AST.Unary _ uop ex -> do
    val <- interpretExpr scope ex
    applyUOp uop val
  AST.Binary _ bop l r -> do
    lVal <- interpretExpr scope l
    rVal <- interpretExpr scope r
    applyBOp bop lVal rVal
  AST.Call _ fnEx argExs -> do
    fnVal <- interpretExpr scope fnEx
    argVals <- mapM (interpretExpr scope) argExs
    callFunction fnVal argVals
  AST.Cast _ t ex -> do
    val <- interpretExpr scope ex
    castVal t val
  AST.Var _ name ->
    lookupVar scope name
  AST.Access _ ex field -> do
    val <- interpretExpr scope ex
    accessField val field
  AST.Lambda _ _args _ret ->
    error "TODO: lambda"

data StmtResult
  = Returned Value
  | FellThrough

getReturnValue :: StmtResult -> Value
getReturnValue (Returned val) = val
getReturnValue FellThrough = VVoid

interpretStmt :: Scope -> StatementT -> IO StmtResult
interpretStmt scope stmt = case stmt of
  AST.Return _ Nothing ->
    return $ Returned VVoid
  AST.Return _ (Just expr) -> do
    val <- interpretExpr scope expr
    return $ Returned val

  AST.Let _ name _ expr -> do
    val <- interpretExpr scope expr
    let (s0:_) = scope
    ss <- readIORef s0
    writeIORef s0 (Map.insert name val ss)
    return FellThrough

  AST.Assign _ names expr -> do
    val <- interpretExpr scope expr
    assign scope names val
    return FellThrough

  AST.Block _ stmts ->
    interpretBlock scope stmts

  AST.Expr _ expr -> do
    _ <- interpretExpr scope expr
    return FellThrough

  AST.If _ tst thn els -> do
    testVal <- interpretExpr scope tst
    b <- requireBool testVal
    if b
       then interpretBlock scope thn
      else case els of
            Nothing -> return FellThrough
            Just st -> interpretStmt scope st

  AST.While _ tst blk -> do
    testVal <- interpretExpr scope tst
    b <- requireBool testVal
    if b
       then do
         blkResult <- interpretBlock scope blk
         case blkResult of
          FellThrough ->
            -- try the next iteration of the loop
            interpretStmt scope stmt
          Returned val ->
            return $ Returned val
      else
        return FellThrough

  AST.Match _ expr cases -> do
    val <- interpretExpr scope expr
    runMatchingCase scope val cases

  AST.Pass _ ->
    return FellThrough

runMatchingCase :: Scope -> Value -> [MatchCaseT] -> IO StmtResult
runMatchingCase _ _ [] =
  error "no cases matched"
runMatchingCase scope val (AST.MatchCase me ms:cs) = do
  matched <- checkMatch val me
  case matched of
    Nothing ->
      runMatchingCase scope val cs
    Just newBindings -> do
      caseScope <- newIORef $ Map.fromList newBindings
      interpretStmt (caseScope:scope) ms

checkMatch :: Value -> MatchExpressionT -> IO (Maybe [(String, Value)])
checkMatch value matchExpr = case matchExpr of
  AST.MatchAnything _ ->
    return (Just [])
  AST.MatchVariable _ name ->
    return (Just [(name, value)])
  AST.MatchStructure _ name fields ->
    case value of
      VStruct sname valRefs ->
        if name == sname && length valRefs == length fields
        then do
          vals <- mapM (readIORef . snd) valRefs
          innerMatches <- zipWithM checkMatch vals fields
          return $ concat <$> sequence innerMatches
        else
          return Nothing
      _ ->
        return Nothing

interpretBlock :: Scope -> [StatementT] -> IO StmtResult
interpretBlock scope stmts = do
  blockScope <- newIORef Map.empty
  interpretBlockScoped (blockScope : scope) stmts

interpretBlockScoped :: Scope -> [StatementT] -> IO StmtResult
interpretBlockScoped _ [] =
  return FellThrough
interpretBlockScoped scope (s:stmts) = do
  result <- interpretStmt scope s
  case result of
   FellThrough -> interpretBlockScoped scope stmts
   Returned _  -> return result

interpretVal :: Scope -> ValueT -> IO Value
interpretVal scope val = case val of
  AST.StrVal    _ s           -> return $ VString s
  AST.BoolVal   _ b           -> return $ VBool b
  AST.IntVal    _ i           -> return $ VInt i
  AST.FloatVal  _ f           -> return $ VFloat f
  AST.StructVal _ name fields -> do
    let mapField (fname, fexpr) = do
          fval <- interpretExpr scope fexpr
          ref <- newIORef fval
          return (fname, ref)
    vals <- mapM mapField fields
    return $ VStruct name vals


assign :: Scope -> [String] -> Value -> IO ()
assign (_:_)  []             _     =
  error "Compiler bug: empty set of names to assign"
assign []     _              _     =
  error "Empty scope given to assign"
assign (s:ss) names@(name:_) value = do
  m <- readIORef s
  case Map.lookup name m of
   Nothing ->
     assign ss names value
   Just existing ->
     updateExisting s names existing value


updateExisting :: IORef (Map String Value) -> [String] -> Value -> Value -> IO ()
updateExisting ref [name] _ newVal =
  modifyIORef ref (Map.insert name newVal)
updateExisting _ (_:names) existing newVal =
  updateStruct names existing newVal
updateExisting _ [] _ _ = undefined

updateStruct :: [String] -> Value -> Value -> IO ()
updateStruct (n:names) struct newVal = case struct of
  VStruct _ fields -> case lookup n fields of
    Just ref ->
      updateRef ref names newVal
    Nothing ->
      error $ "can't find struct field " ++ n
  _ ->
    error "can't update field in non structure"
updateStruct [] _ _ = undefined

updateRef :: IORef Value -> [String] -> Value -> IO ()
updateRef ref [] newVal =
  writeIORef ref newVal
updateRef ref names newVal = do
  existing <- readIORef ref
  updateStruct names existing newVal



applyUOp :: AST.UnaryOp -> Value -> IO Value
applyUOp op val = case op of
  AST.BitInvert -> do
    i <- requireInt val
    return $ VInt $ complement i
  AST.BoolNot -> do
    b <- requireBool val
    return $ VBool $ not b

applyBOp :: AST.BinOp -> Value -> Value -> IO Value
applyBOp op l r = case op of
  AST.Less      -> numOp (\a b -> VBool $ a <  b) (\a b -> VBool $ a <  b) l r
  AST.LessEq    -> numOp (\a b -> VBool $ a <= b) (\a b -> VBool $ a <= b) l r
  AST.Greater   -> numOp (\a b -> VBool $ a >  b) (\a b -> VBool $ a >  b) l r
  AST.GreaterEq -> numOp (\a b -> VBool $ a >= b) (\a b -> VBool $ a >= b) l r
  AST.Eq        -> return $ VBool $ l == r
  AST.NotEq     -> return $ VBool $ l /= r

  AST.BoolAnd   -> VBool <$> boolOp (&&)   l r
  AST.BoolOr    -> VBool <$> boolOp (||)   l r

  AST.Plus      -> numOp (\a b -> VInt $ a + b) (\a b -> VFloat $ a + b) l r
  AST.Minus     -> numOp (\a b -> VInt $ a - b) (\a b -> VFloat $ a - b) l r
  AST.Times     -> numOp (\a b -> VInt $ a * b) (\a b -> VFloat $ a * b) l r
  AST.Divide    -> numOp (\a b -> VInt $ a `div` b) (\a b -> VFloat $ a / b) l r
  AST.Mod       -> VInt  <$> intOp  mod    l r
  AST.Power     -> numOp (\a b -> VInt $ a ^ b) (\a b -> VFloat $ a ** b) l r

  AST.BitAnd    -> VInt  <$> intOp  (.&.)  l r
  AST.BitOr     -> VInt  <$> intOp  (.|.)  l r
  AST.BitXor    -> VInt  <$> intOp  xor    l r

  AST.LShift    -> VInt  <$> intOp  shiftL l r
  AST.RShift    -> VInt  <$> intOp  shiftR l r

numOp :: (Int -> Int -> Value) -> (Float -> Float -> Value) -> Value -> Value -> IO Value
numOp ifn ffn l r = do
  pair <- requireNum l r
  case pair of
    Left  (li, ri) -> return $ ifn li ri
    Right (lf, rf) -> return $ ffn lf rf

requireNum :: Value -> Value -> IO (Either (Int, Int) (Float, Float))
requireNum (VInt i1) (VInt i2) = return $ Left (i1, i2)
requireNum (VFloat f1) (VFloat f2) = return $ Right (f1, f2)
requireNum _ _ = error "Not a number"

intOp :: (Int -> Int -> a) -> Value -> Value -> IO a
intOp fn l r = do
  li <- requireInt l
  ri <- requireInt r
  return $ fn li ri


boolOp :: (Bool -> Bool -> a) -> Value -> Value -> IO a
boolOp fn l r = do
  lb <- requireBool l
  rb <- requireBool r
  return $ fn lb rb

requireInt :: Value -> IO Int
requireInt (VInt i) = return i
requireInt _ = error "Not an integer"

requireBool :: Value -> IO Bool
requireBool (VBool b) = return b
requireBool _ = error "Not a boolean"

callFunction :: Value -> [Value] -> IO Value
callFunction (VClosure scope (Function names body)) args = do
  innerScope <- newIORef $ Map.fromList $ zip names args
  let fnScope = innerScope : scope
  result <- interpretStmt fnScope body
  return $ getReturnValue result
callFunction (VBuiltIn name) args = case name of
  "print" -> builtinPrint args
  _ -> error $ "Unknown built-in " ++ name
callFunction _ _ =
  error "calling a non-function"

builtinPrint :: [Value] -> IO Value
builtinPrint args = case args of
  [VString s] -> do
    putStr s
    hFlush stdout
    return VVoid
  _ -> error "non-string value to print"


-- Note: Keep this in sync with the types allowed by Inference.canCast.
castVal :: String -> Value -> IO Value
castVal t value = case t of
  "String" -> do
    s <- render value
    return $ VString s

  "Int" -> case value of
    VFloat f ->
      return $ VInt (round f)
    _ ->
      error "Cannot cast that to an int"

  "Float" -> case value of
    VInt i ->
      return $ VFloat $ fromIntegral i
    _ ->
      error "Cannot cast that to an float"

  _ ->
    error $ "Cannot cast to " ++ t


lookupVar :: Scope -> String -> IO Value
lookupVar [] name = error $ "name undefined: " ++ name
lookupVar (s:ss) name = do
  mapping <- readIORef s
  case Map.lookup name mapping of
   Nothing  -> lookupVar ss name
   Just val -> return val

accessField :: Value -> String -> IO Value
accessField val name = case val of
  VStruct _ fields ->
    case lookup name fields of
      Just ref ->
        readIORef ref
      Nothing ->
        error $ "accessing undefined field " ++ name
  _ ->
    error "accessing field in non-structure"


data Value
  = VInt Int
  | VFloat Float
  | VString String
  | VBool Bool
  | VStruct String [(String, IORef Value)]
  | VList [IORef Value]
  | VClosure Scope Function
  | VVoid
  | VBuiltIn String -- name of the built-in function
  deriving (Eq)

data Function
  = Function [String] StatementT
  deriving (Eq, Show)


type Scope = [IORef (Map String Value)]


toClosure :: Scope -> [String] -> StatementT -> Value
toClosure scope args stmt =
  VClosure scope $ Function args stmt


class Render a where
  render :: a -> IO String

instance Render Value where
  render val = case val of
    VInt i -> return $ show i
    VFloat f -> return $ show f
    VString s -> return $ show s
    VBool b -> return $ show b
    VStruct name fields -> do
      let header = name ++ " {"
      pairs <- mapM renderPair fields
      let body = intercalate ", " pairs
      return $ header ++ body ++ " }"
    VList refs -> do
      vals <- mapM readIORef refs
      rendered <- mapM render vals
      let body = intercalate ", " rendered
      return $ "[" ++ body ++ "]"
    VClosure _ (Function args _)  -> do
      let joinedArgs = intercalate ", " args
      return $ "fn(" ++ joinedArgs ++ ")"
    VVoid ->
      return "()"
    VBuiltIn name ->
      return name


renderPair :: (String, IORef Value) -> IO String
renderPair (name, ref) = do
  val <- readIORef ref
  rendered <- render val
  return $ name ++ ": " ++ rendered
