{-# LANGUAGE FlexibleInstances #-}

module Inference
  ( inferModule
  , mgu
  , startingEnv
  , runInfer
  , inferExpr
  , inferDecl
  , unifies
  , alphaSubstitues
  , makeBindGroup
  , implicitBindings
  , explicitBindings
  , instantiate
  , splitExplicit
  , BindGroup
  , Environment
  , TypedDecls
  , InferResult(..)
  ) where


import Prelude hiding (exp)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (isJust)

--import Debug.Trace

import Control.Monad (when, foldM, zipWithM)
import Control.Monad.State (StateT, modify, get, gets, put, lift, evalStateT, mapStateT)


import Util.Functions
import qualified AST.Annotation as Anno
import AST.Annotation (Annotation, Annotated, getLocation, addType)
import AST.Expression (UnaryOp(..), BinOp(..))
import qualified AST.Expression as E
import qualified AST.Statement as S
import qualified AST.Declaration as D
import qualified AST.Type as T

import Types
  ( Substitution
  , Scheme(..)
  , Type(..)
  , makeFuncType
  , applyTypes
  , getRoot
  , tInt
  , tFloat
  , tBool
  , tString
  , tUnit
  , apply
  , asScheme
  , composeSubs
  , emptySubstitution
  , freeTypeVars )
import Errors
  ( Error(..)
  , Result )
import FirstPass
  ( Module(..), Constructor(..), bindings, valueType )
import Util.Graph
  ( components )


type DeclarationT     = D.Declaration     Annotation
type ExpressionT      = E.Expression      Annotation
type ValueT           = E.Value           Annotation
type MatchCaseT       = S.MatchCase       Annotation
type MatchExpressionT = S.MatchExpression Annotation
type StatementT       = S.Statement       Annotation
type TypeDeclT        = T.TypeDecl        Annotation


data BindGroup
  = BindGroup
    -- the bindings without an explicit type are grouped by strongly connected
    -- component and the groups are toplogically sorted. Bindings in the head of
    -- the list may not refer to bindings in the tail, but bindings in the tail
    -- can refer to bindings in the head.
    { implicitBindings :: [[(String, DeclarationT)]]
    -- all bindings with an explicit type go in one group
    , explicitBindings :: [(String, DeclarationT)] }

type TypedDecls = [(String, DeclarationT)]

data InferResult
  = InferResult
    { topLevelBindings :: TypedDecls
    , topLevelEnv      :: Environment }
  deriving (Show)

inferModule :: Module -> Result InferResult
inferModule m = do
  let bindGroup = makeBindGroup m
  let ctors = constructors m
  (binds, env) <- runInfer ctors $ inferBindGroup bindGroup startingEnv
  return InferResult { topLevelBindings=binds, topLevelEnv=env }

makeBindGroup :: Module -> BindGroup
makeBindGroup m =
  let declarations = bindings m
      (di, de) = splitExplicit declarations
      explicitNames = Set.fromList $ Map.keys de
      graph = gatherGraph explicitNames di
      topoOrder = reverse $ components graph
      getBinding name = (name, mustLookup name declarations)
      impls = map (map getBinding) topoOrder
      expls = Map.toList de
  in BindGroup { implicitBindings=impls, explicitBindings=expls }

type DeclMap = Map String DeclarationT

splitExplicit :: DeclMap -> (DeclMap, DeclMap)
splitExplicit = Map.partition isImplicit

isImplicit :: DeclarationT -> Bool
isImplicit = not . isExplicit

isExplicit :: DeclarationT -> Bool
isExplicit decl = case decl of
  D.Let      _ _ mt _   -> isJust mt
  D.Function _ _ mt _ _ -> isJust mt
  D.TypeDef{}           -> error "shouldn't see a typedef here"

-- TODO: extend this into prelude (plus imported names)
startingDependencies :: Set String
startingDependencies = Set.fromList ["print"]

-- This walks each declaration to find out what the
-- dependency graph looks like.
-- This assumes that all the variables are defined (TODO: that's
-- never checked at the moment)
gatherGraph :: Set String -> Map String DeclarationT -> Map String [String]
gatherGraph explicitNames = Map.map (removeExpl . findDependencies startingDependencies)
  where removeExpl deps = Set.toList $ setSubtract explicitNames $ Set.fromList deps

setSubtract :: (Ord a) => Set a -> Set a -> Set a
setSubtract toRemove = Set.filter keep
  where keep e = not $ Set.member e toRemove


class Depencencies a where
  findDependencies :: Set String -> a -> [String]

instance Depencencies (D.Declaration Annotation) where
  findDependencies bound decl = case decl of
    D.Let _ name _  exp ->
      findDependencies (Set.insert name bound) exp
    D.Function _ name _ args stmt ->
      findDependencies (Set.union bound $ Set.fromList (name:args)) stmt
    D.TypeDef{} ->
      []

instance Depencencies (S.Statement Annotation) where
  findDependencies bound stmt = case stmt of
    S.Return _ mexp ->
      maybe [] (findDependencies bound) mexp
    S.Let _ name _ exp ->
      findDependencies (Set.insert name bound) exp
    S.Assign _ _ exp ->
      findDependencies bound exp
    S.Block _ stmts ->
      findDepBlock bound stmts
    S.Expr _ expr ->
      findDependencies bound expr
    S.If _ test body elseStmt ->
      let testDeps = findDependencies bound test
          bodyDeps = findDepBlock bound body
          elseDeps = maybe [] (findDependencies bound) elseStmt
      in testDeps ++ bodyDeps ++ elseDeps
    S.While _ test body ->
      let testDeps = findDependencies bound test
          bodyDeps = findDepBlock bound body
      in testDeps ++ bodyDeps
    S.Match _ expr matchCases ->
      let exprDeps = findDependencies bound expr
          caseDeps = concatMap (findDependencies bound) matchCases
      in exprDeps ++ caseDeps
    S.Pass _ ->
      []

instance Depencencies (S.MatchCase Annotation) where
  findDependencies bound matchCase =
    let (S.MatchCase matchExpr stmt) = matchCase
        exprNames = findNames matchExpr
        bound' = foldr Set.insert bound exprNames
    in findDependencies bound' stmt

findNames :: MatchExpressionT -> [String]
findNames matchExpr = case matchExpr of
  S.MatchAnything  _         -> []
  S.MatchVariable  _ name    -> [name]
  S.MatchStructure _ _ exprs -> concatMap findNames exprs

findDepBlock :: Set String -> [StatementT] -> [String]
findDepBlock bound stmts = case stmts of
     [] -> []
     (stmt:rest) ->
       let stmtDeps = findDependencies bound stmt
           bound' = case stmt of
             (S.Let _ name _ _) ->
               Set.insert name bound
             _ ->
               bound
           restDeps = findDepBlock bound' rest
       in stmtDeps ++ restDeps

instance Depencencies (E.Expression Annotation) where
  findDependencies bound exp = case exp of
    E.Paren _ inner ->
      findDependencies bound inner
    E.Val _ val ->
      findDependencies bound val
    E.Unary _ _ inner ->
      findDependencies bound inner
    E.Binary _ _ l r ->
      findDependencies bound l ++ findDependencies bound r
    E.Call _ fn args ->
      let fnDeps = findDependencies bound fn
          argDeps = concatMap (findDependencies bound) args
      in fnDeps ++ argDeps
    E.Cast _ _ inner ->
      findDependencies bound inner
    E.Var _ name ->
      if Set.member name bound then [] else [name]
    E.Access _ inner _ ->
      findDependencies bound inner

instance Depencencies (E.Value Annotation) where
  findDependencies bound val = case val of
    E.StructVal _ _ fields ->
      concatMap (findDependencies bound . snd) fields
    _ ->
      []

type CTors = Map String Constructor

data InferState
  = InferState
    { nextVarN :: Int
    , currentSub :: Substitution
    , constrs :: CTors }
  deriving (Show)

startingInferState :: CTors -> InferState
startingInferState ctors =
  InferState
  { nextVarN = 0
  , currentSub = Map.empty
  , constrs = ctors }

-- `Either Error` = `Result`, but somehow that
-- type synonym isn't allowed here.
type InferM = StateT InferState (Either Error)

type Environment = Map String Scheme

-- toAdd, existing
addToEnv :: Environment -> Environment -> Environment
addToEnv = Map.union

-- TODO: Make this a proper instance of the Types.Types class
applyEnv :: Substitution -> Environment -> Environment
applyEnv sub = Map.map (apply sub)

-- TODO: this should also start with prelude and imported names
startingEnv :: Environment
startingEnv =
  Map.fromList
  [ ("print", Scheme 1 (makeFuncType [TGen 1] tUnit)) ]

runInfer :: Monad m => Map String Constructor
         -> StateT InferState m a -> m a
runInfer ctors f =
  evalStateT f (startingInferState ctors)

inferErr :: Error -> InferM a
inferErr err = lift $ Left err

inferErrFor :: (Annotated a) => [a Annotation] -> Error -> InferM b
inferErrFor nodes err = withLocations nodes $ inferErr err

withLocations :: (Annotated a) => [a Annotation] -> InferM b -> InferM b
withLocations nodes inf =
  let regions = mapMaybe_ getLocation nodes
  in mapStateT (mapLeft (WithLocations regions)) inf


mapMaybe_ :: (a -> Maybe b) -> [a] -> [b]
mapMaybe_ f = map (mustJust . f)

mustJust :: Maybe a -> a
mustJust (Just a) = a
mustJust Nothing = error "Must be Just"

newTypeVar :: InferM Type
newTypeVar = do
  st <- get
  let n = nextVarN st
  put $ st { nextVarN = 1 + n }
  return $ TVar $ "_v" ++ show n

getSub :: InferM Substitution
getSub = gets currentSub

applyCurrentSub :: Type -> InferM Type
applyCurrentSub t = do
  sub <- getSub
  return $ apply sub t

extendSub :: Substitution -> InferM ()
extendSub sub = do
  s1 <- getSub
  let s = composeSubs s1 sub
  modify (\st -> st { currentSub=s })


inferBindGroup :: BindGroup -> Environment -> InferM (TypedDecls, Environment)
inferBindGroup bg env = do
  let expls = explicitBindings bg
  explicitBindingTypes <- getExplicitTypes expls

  let env1 = addToEnv explicitBindingTypes env
  let impls = implicitBindings bg

  (decls1, env2) <- inferGroups impls env1
  let env' = addToEnv env2 env1
  decls2 <- tiExpls expls env'
  return (decls1 ++ decls2, env')


tiExpls :: [(String, DeclarationT)] -> Environment -> InferM TypedDecls
tiExpls expls env = case expls of
  [] ->
    return []
  ((name, decl):es) -> do
    d <- tiExpl name decl env
    ds <- tiExpls es env
    return $ (name, d) : ds

tiExpl ::  String -> DeclarationT -> Environment -> InferM DeclarationT
tiExpl name decl env = do
  let sch = mustLookup name env
  t <- instantiate sch

  d <- inferDecl env decl
  let dt = getType d

  withLocations [decl] $ unify t dt

  sub <- getSub
  let sch' = generalize (applyEnv sub env) (apply sub dt)
  if not $ schemesEquivalent sch' sch
    then inferErrFor [decl] $ BindingTooGeneral $ name ++ ": " ++ show (sch', sch, dt)
    else return d


getExplicitTypes :: [(String, DeclarationT)] -> InferM Environment
getExplicitTypes expls = do
  typed <- mapM getExplicitType expls
  return $ Map.fromList typed

getExplicitType :: (name, DeclarationT) -> InferM (name, Scheme)
getExplicitType (name, decl) = case decl of
  D.Let _ _ mtdecl _ -> do
    let (Just tdecl) = mtdecl
    t <- withLocations [decl] $ typeFromDecl Map.empty tdecl
    return (name, asScheme t)

  D.Function _ _ mgendecl _ _ -> do
    let Just (gens, tdecl) = mgendecl
    gmap <- genericMap gens
    t <- withLocations [decl] $ typeFromDecl gmap tdecl
    let varSet = Set.fromList gens
    return (name, generalizeOver varSet t)

  D.TypeDef{} ->
    error "shouldn't see a typedef here"


genericMap :: [T.Type] -> InferM (Map String Type)
genericMap tnames = do
  gens <- mapM (const newTypeVar) tnames
  return $ Map.fromList $ zip tnames gens

inferGroups :: [[(String, DeclarationT)]] -> Environment ->
               InferM (TypedDecls, Environment)
inferGroups []     _   =
  return ([], Map.empty)
inferGroups (g:gs) env = do
  (typed, env1) <- inferGroup g env
  (rest, env2) <- inferGroups gs (Map.union env1 env)
  return (typed ++ rest, Map.union env1 env2)

inferGroup :: [(String, DeclarationT)] -> Environment ->
              InferM (TypedDecls, Environment)
inferGroup impls env = do
  -- Map each binding to a new type variable while recursively typing these bindings
  ts <- mapM (const newTypeVar) impls
  let bindingNames = map fst impls
  let bindingSchemes = map asScheme ts
  let groupBindings = Map.fromList $ zip bindingNames bindingSchemes
  let groupEnv = Map.union groupBindings env

  -- Do the actual inference
  typedDecls <- inferDecls groupEnv impls ts

  -- Apply the substitution to all the types and generalize them to schemes
  sub <- getSub
  let subbed = map (apply sub) ts
  let schemes = map (generalize (applyEnv sub env)) subbed
  let resultEnv = Map.fromList $ zip bindingNames schemes

  return (typedDecls, resultEnv)


--showTrace :: (Show a) => String -> a -> a
--showTrace s a = trace (s ++ ": " ++ show a) a

inferDecls :: Environment -> [(String, DeclarationT)] -> [Type] -> InferM TypedDecls
inferDecls env decls ts = mapM infer (zip decls ts)
  where infer ((name, decl), t) = do
          d <- inferDecl env decl
          withLocations [d] $ unify t (getType d)
          return (name, d)

generalize :: Environment -> Type -> Scheme
generalize env t =
  let envVars = foldl Set.union Set.empty $ map (freeTypeVars . snd) $ Map.toList env
  in generalizeOver envVars t

generalizeOver :: Set String -> Type -> Scheme
generalizeOver envVars t =
  let freeVars = map TVar $ Set.toList $ Set.difference (freeTypeVars t) envVars
      genVars = map TGen [1..]
      sub = Map.fromList $ zip freeVars genVars
  in Scheme (length freeVars) (apply sub t)

instantiate :: Scheme -> InferM Type
instantiate sch@(Scheme n t) = do
  let range = [1..n]
  newVars <- mapM (const newTypeVar) range
  let genVars = map TGen range
  let sub = Map.fromList $ zip genVars newVars
  let applied = apply sub t
  verifyContainsNoGenerics sch applied
  return applied


verifyContainsNoGenerics :: Scheme -> Type -> InferM ()
verifyContainsNoGenerics sch t =
  let message = "instantiated type " ++ show t ++ " from " ++ show sch ++  " contains generics"
  in when (containsGenerics t) (inferErr $ CompilerBug message)


containsGenerics :: Type -> Bool
containsGenerics t = case t of
  TGen _  -> True
  TAp a b -> any containsGenerics [a, b]
  TFunc _ -> False
  TCon _  -> False
  TVar _  -> False


inferDecl :: Environment -> DeclarationT -> InferM DeclarationT
inferDecl env decl = case decl of
  D.Let a name mtype expr -> do
    expr' <- inferExpr env expr
    let t = getType expr'
    return $ addType t $ D.Let a name mtype expr'

  D.Function a name mtype args stmt -> do
    argTs <- mapM (const newTypeVar) args
    retT <- newTypeVar

    -- This case statement exists so that, by the time
    -- a field access is reached, the type of that variable
    -- will already be known.
    case Map.lookup name env of
      Nothing -> return ()
      Just sc -> do
        dt <- instantiate sc
        withLocations [stmt] $ unify dt (makeFuncType argTs retT)

    let argEnv = Map.fromList $ zip args (map asScheme argTs)
    let env' = Map.union argEnv env
    (stmt', stmtReturns) <- inferStmt env' stmt
    let funcReturns = toFunctionReturns stmtReturns
    withLocations [stmt] $ unifyAll retT funcReturns
    sub <- getSub
    let argTypes = map (apply sub) argTs
    let returnT = apply sub retT
    let t = makeFuncType argTypes returnT
    return $ addType t $ D.Function a name mtype args stmt'

  D.TypeDef{} ->
    inferErr $ CompilerBug "TypeDefs are not bindings"

inferStmt :: Environment -> StatementT ->
             InferM (StatementT, DoesReturn)
inferStmt env stmt = case stmt of
  S.Return a Nothing ->
    return (S.Return a Nothing, AlwaysReturns [tUnit])
  S.Return a (Just expr) -> do
    expr' <- inferExpr env expr
    return (S.Return a (Just expr'), AlwaysReturns [getType expr'])

  S.Let a name mtype expr -> do
    -- TODO: recursive binding?
    -- Note that in recursive bindings, if the bound expression involves a
    -- closure, I need to think about whether that closure should be allowed to
    -- assign back to this variable
    expr' <- inferExpr env expr
    case mtype of
      Nothing    -> return()
      Just tdecl -> do
        -- TODO: allow generics here, too?
        t <- typeFromDecl Map.empty tdecl
        withLocations [stmt] $ unify (getType expr') t
    return (S.Let a name mtype expr', NeverReturns)

  S.Assign a names expr ->
    case names of
     []    -> inferErr $ CompilerBug "assignment to no names"
     [var] -> do
       -- Something to think about: Should this be required to be a variable
       -- defined in a `let` statement instead of an argument to a function?
       expr' <- inferExpr env expr
       let exprT = getType expr'
       sch <- withLocations [stmt] $ lookupName var env
       varT <- instantiate sch
       -- TODO: not quite right, since this may result in too narrow of a type
       -- getting assigned, but it's good enough for now
       withLocations [stmt] $ unify exprT varT
       return (S.Assign a names expr', NeverReturns)
     (var:fields) -> do
       expr' <- inferExpr env expr
       let exprT = getType expr'

       sch <- withLocations [stmt] $ lookupName var env
       -- Is it right for this to be working on an instantiation of sch?
       varT <- instantiate sch
       fieldT <- withLocations [stmt] $ getStructField varT fields
       withLocations [stmt] $ unify fieldT exprT

       return (S.Assign a names expr', NeverReturns)

  S.Block a stmts -> do
    (stmts', retT) <- inferBlock env stmts
    return (S.Block a stmts', retT)

  S.Expr a expr -> do
    expr' <- inferExpr env expr
    return (S.Expr a expr', NeverReturns)

  S.If a test blk els -> do
    testExpr <- inferExpr env test
    withLocations [testExpr] $ unify (getType testExpr) tBool
    (blk', blkReturns) <- inferBlock env blk
    (els', elsReturns) <- case els of
      Nothing ->
        return (Nothing, NeverReturns)
      Just st -> do
        (stmt', retT) <- inferStmt env st
        return (Just stmt', retT)
    let ifReturns = combineReturns blkReturns elsReturns
    return (S.If a testExpr blk' els', ifReturns)

  S.While a test blk -> do
    testExpr <- inferExpr env test
    withLocations [testExpr] $ unify (getType testExpr) tBool
    (blk', blkReturns) <- inferBlock env blk
    let whileReturns = demoteReturns blkReturns
    return (S.While a testExpr blk', whileReturns)

  S.Match a expr cases -> do
    expr' <- inferExpr env expr
    let exprType = getType expr'
    casesAndReturns <- mapM (inferMatchCase env exprType) cases
    let (cases', returns) = unzip casesAndReturns
    let resultType = getType (head cases')
    let result = addType resultType $ S.Match a expr' cases'
    return (result, foldl1 combineReturns returns)

  S.Pass a -> do
    let result = addType tUnit $ S.Pass a
    return (result, NeverReturns)


inferMatchCase :: Environment -> Type -> MatchCaseT
               -> InferM (MatchCaseT, DoesReturn)
inferMatchCase env t (S.MatchCase matchExpr stmt) = do
  (matchExpr', matchedVars) <- inferMatchExpr env t matchExpr
  let env' = insertAll env matchedVars
  (stmt', doesReturn) <- inferStmt env' stmt
  return (S.MatchCase matchExpr' stmt', doesReturn)


inferMatchExpr :: Environment -> Type -> MatchExpressionT
               -> InferM (MatchExpressionT, [(String, Scheme)])
inferMatchExpr env targetType matchExpr = case matchExpr of
  S.MatchAnything a ->
    return (addType targetType $ S.MatchAnything a, [])

  S.MatchVariable a name -> do
    let sch = generalize env targetType
    let bindings' = [(name, sch)]
    return (addType targetType $ S.MatchVariable a name, bindings')

  S.MatchStructure a enumName fields -> do
    ctor <- withLocations [matchExpr] $ getConstructor enumName

    when (length fields /= length (ctorFields ctor)) $
      inferErrFor [matchExpr] $ PatternErr $ "wrong number of fields matched for " ++ enumName

    let sch = ctorType ctor
    enumT <- instantiate sch
    (argTs, structT) <- destructFunctionType enumT

    -- Pattern matching is basically running the constructor function backwards
    withLocations [matchExpr] $ unify targetType structT

    sub1 <- getSub
    let patternTypes = map (apply sub1) argTs

    inner <- zipWithM (inferMatchExpr env) patternTypes fields
    let (typedFields, bindingLists) = unzip inner
    let binds = concat bindingLists

    sub2 <- getSub
    let structType = apply sub2 structT
    return (addType structType $ S.MatchStructure a enumName typedFields, binds)


destructFunctionType :: Type -> InferM ([Type], Type)
destructFunctionType (TAp t ret) = do
  argTypes <- getArgTypes t
  return (reverse argTypes, ret)
destructFunctionType _ =
  inferErr $ CompilerBug "invalid function type"

-- getArgTypes returns them backwards
getArgTypes :: Type -> InferM [Type]
getArgTypes (TFunc _) =
  return []
getArgTypes (TAp t a) = do
  rest <- getArgTypes t
  return (a : rest)
getArgTypes _ =
  inferErr $ CompilerBug "invalid function type"


inferBlock :: Environment -> [StatementT] ->
              InferM ([StatementT], DoesReturn)
inferBlock _   []     = return ([], NeverReturns)
inferBlock env [stmt] = do
  (stmt', retT) <- inferStmt env stmt
  return ([stmt'], retT)
inferBlock env (s:ss) = do
  (s', ret1) <- inferStmt env s
  let env' = case s' of
        S.Let _ name _ expr ->
          let sch = generalize env (getType expr)
          in Map.insert name sch env
        _                  -> env
  (ss', ret2) <- inferBlock env' ss
  return (s' : ss', appendReturns ret2 ret1)


getStructField :: Type -> [String] -> InferM Type
getStructField = foldM getStructFieldType

getStructFieldType :: Type -> String -> InferM Type
getStructFieldType t fieldName = case getRoot t of
  TCon structName -> do
    resultT <- newTypeVar

    ctor <- getConstructor structName
    let fields = ctorFields ctor
    fieldSch <- case lookup fieldName fields of
      Nothing -> inferErr $ UndefinedField structName fieldName
      Just sc -> return sc
    fieldFn <- instantiate fieldSch

    unify fieldFn (makeFuncType [t] resultT)
    sub <- getSub
    return $ apply sub resultT
  TVar _ ->
    inferErr InsufficientlyDefinedType
  TGen _ ->
    inferErr $ CompilerBug "got an unexpected generic"
  TAp _ _ ->
    inferErr $ WrongType t "expected a structure, got a type application"
  TFunc _ ->
    inferErr $ WrongType t "expected a structure, got a function constructor"


data DoesReturn
  = NeverReturns
  | SometimesReturns [Type]
  | AlwaysReturns [Type]
  deriving (Eq, Show)

combineReturns :: DoesReturn -> DoesReturn -> DoesReturn
combineReturns r1 r2 = case (r1, r2) of
  (NeverReturns, NeverReturns) ->
    NeverReturns
  (AlwaysReturns t1, AlwaysReturns t2) ->
    AlwaysReturns $ t1 ++ t2
  _ ->
    SometimesReturns $ getReturnTypes r1 ++ getReturnTypes r2

appendReturns :: DoesReturn -> DoesReturn -> DoesReturn
appendReturns r1 r2 = case r1 of
  NeverReturns        -> r2
  SometimesReturns t1 -> SometimesReturns (getReturnTypes r2 ++ t1)
  AlwaysReturns    t1 -> AlwaysReturns (getReturnTypes r2 ++ t1)

getReturnTypes :: DoesReturn -> [Type]
getReturnTypes NeverReturns          = []
getReturnTypes (SometimesReturns ts) = ts
getReturnTypes (AlwaysReturns ts)    = ts

demoteReturns :: DoesReturn -> DoesReturn
demoteReturns (AlwaysReturns ts) = SometimesReturns ts
demoteReturns ds                 = ds

toFunctionReturns :: DoesReturn -> [Type]
toFunctionReturns (AlwaysReturns ts) = ts
toFunctionReturns ds                 =
  -- Add tUnit to handle the case where the execution "fall off" the end of the
  -- function, since the return statements don't cover every case.
  tUnit : getReturnTypes ds


inferExpr :: Environment -> ExpressionT -> InferM ExpressionT
inferExpr env expr = case expr of
  E.Paren a e -> do
    e' <- inferExpr env e
    let t = getType e'
    return $ addType t $ E.Paren a e'

  E.Val a val -> do
    val' <- inferValue env val
    let t = getType val'
    return $ addType t $ E.Val a val'

  E.Unary a op exp -> do
    resultT <- newTypeVar
    exp' <- inferExpr env exp
    let expT = getType exp'
    let fnT = getUnaryFnType op
    withLocations [expr] $ unify fnT (makeFuncType [expT] resultT)
    sub <- getSub
    let t = apply sub resultT
    return $ addType t $ E.Unary a op exp'

  E.Binary a op l r -> do
    resultT <- newTypeVar
    l' <- inferExpr env l
    let lt = getType l'
    r' <- inferExpr env r
    let rt = getType r'
    let fnT = getBinaryFnType op
    withLocations [expr] $ unify fnT (makeFuncType [lt, rt] resultT)
    sub <- getSub
    let t = apply sub resultT
    return $ addType t $ E.Binary a op l' r'

  E.Call a fexp args -> do
    resultT <- newTypeVar
    fexp' <- inferExpr env fexp
    let ft = getType fexp'
    args' <- mapM (inferExpr env) args
    let argTs = map getType args'
    withLocations [expr] $ unify ft (makeFuncType argTs resultT)
    sub <- getSub
    let t = apply sub resultT
    return $ addType t $ E.Call a fexp' args'

  E.Cast a t exp -> do
    exp' <- inferExpr env exp
    let expT = getType exp'
    sch <- withLocations [expr] $ attemptCast t expT
    return $ addType sch $ E.Cast a t exp'

  E.Var a name -> do
    sch <- withLocations [expr] $ lookupName name env
    t <- instantiate sch
    return $ addType t $ E.Var a name

  E.Access a exp field -> do
    exp' <- inferExpr env exp
    sub <- getSub
    let etype = apply sub $ getType exp'
    t <- withLocations [expr] $ getStructFieldType etype field
    return $ addType t $ E.Access a exp' field

inferValue :: Environment -> ValueT -> InferM ValueT
inferValue env val = case val of
  E.StrVal a str ->
    return $ addType tString $ E.StrVal a str

  E.BoolVal a b ->
    return $ addType tBool $ E.BoolVal a b

  E.IntVal a i ->
    return $ addType tInt $ E.IntVal a i

  E.FloatVal a f ->
    return $ addType tFloat $ E.FloatVal a f

  E.StructVal a tname fields  -> do
    resultT <- newTypeVar

    ctor <- withLocations [val] $ getConstructor tname
    checkSameFields tname (ctorFields ctor) fields
    fnT <- instantiate (ctorType ctor)
    let fieldNamesInType = map fst $ ctorFields ctor

    -- Sort the fields in the same order they are in the type.
    -- This allows typing to ignore field order.
    let sorted = orderAs fieldNamesInType fields
    typedFields <- mapM (inferExpr env . snd) sorted
    let fieldTypes = map getType typedFields

    -- Unify that with the "function" type of the struct's constructor
    withLocations [val] $ unify fnT (makeFuncType fieldTypes resultT)
    -- and find out what the resulting type is
    sub <- getSub
    let t = apply sub resultT

    -- Finally, put the fields back in the right order since the order is
    -- significant in evaluation
    let namedTypedFields = zip (map fst sorted) typedFields
    let orderedFields = orderAs (map fst fields) namedTypedFields

    return $ addType t $ E.StructVal a tname orderedFields


orderAs :: (Ord a) => [a] -> [(a, b)] -> [(a, b)]
orderAs keys pairs = map getPair keys
  where getPair key = (key, lookup_ key pairs)

getStructType :: String -> InferM Type
getStructType name = do
  ctor <- getConstructor name
  let sch = valueType ctor
  instantiate sch

typeFromDecl :: Map String Type -> TypeDeclT -> InferM Type
typeFromDecl gmap tdecl = case tdecl of
  T.TypeName _ name ->
    case Map.lookup name gmap of
      Nothing -> typeFromName name
      Just t  -> return t
  T.Generic _ name genArgs -> do
    genTypes <- mapM (typeFromDecl gmap) genArgs
    t <- typeFromName name
    case t of
      TAp _ _ -> do
        unify t (applyTypes (getRoot t) genTypes)
        applyCurrentSub t
      _ -> error $ "TODO: figure out what causes this case " ++ show t
  T.Function _ argTs retT -> do
    argTypes <- mapM (typeFromDecl gmap) argTs
    retType <- typeFromDecl gmap retT
    return $ makeFuncType argTypes retType
  T.Struct{} ->
    error "shouldn't see a Struct here"
  T.Enum{} ->
    error "shouldn't see an Enum here"


-- TODO: Check the kind of the type (which will require a lot of plumbing)
typeFromName :: String -> InferM Type
typeFromName name
  | name `elem` builtinTypes =
    return $ TCon name
  | otherwise =
    getStructType name

builtinTypes :: [String]
builtinTypes = words "Int Float Bool Char String ()"


checkSameFields :: String -> [(String, a)] -> [(String, b)] -> InferM ()
checkSameFields tname given actual = do
  let givenSet = Set.fromList $ map fst given
  let actualSet = Set.fromList $ map fst actual
  when (Set.size givenSet /= length given) $
    inferErr $ StructFieldErr tname "a field was duplicated"
  when (givenSet /= actualSet) $
    inferErr $ StructFieldErr tname $
        "wrong set of fields given: " ++ show (givenSet, actualSet)


lookup_ :: (Eq a) => a -> [(a, b)] -> b
lookup_ name pairs =
  fromMaybe (error "lookup_ got Nothing") (lookup name pairs)


getConstructor :: String -> InferM Constructor
getConstructor tname = do
  ctors <- gets constrs
  case Map.lookup tname ctors of
    Just c  -> return c
    Nothing -> inferErr $ UndefinedType tname


lookupName :: String -> Environment -> InferM Scheme
lookupName name env = case Map.lookup name env of
  Nothing  -> inferErr $ UndefinedVar name
  Just sch -> return sch

attemptCast :: String -> Type -> InferM Type
attemptCast toTypeName fromType =
  let toType = TCon toTypeName
  in if canCast fromType toType
     then return toType
     else inferErr $ CannotCast $ "cannot cast " ++ show fromType ++ " to " ++ toTypeName

canCast :: Type -> Type -> Bool
canCast t1 t2
  | t1 == tInt && t2 == tFloat = True
  | t1 == tFloat && t2 == tInt = True
  | t2 == tString              = True
  | otherwise = False

getUnaryFnType :: UnaryOp -> Type
getUnaryFnType op = case op of
  BitInvert -> makeFuncType [tInt] tInt
  BoolNot   -> makeFuncType [tBool] tBool

getBinaryFnType :: BinOp -> Type
getBinaryFnType op
  | op `elem` intBinaryFuncs =
    makeFuncType [tInt, tInt] tInt
  | op `elem` numBinaryFuncs =
    makeFuncType [tInt, tInt] tInt -- TODO: fix this once there are typeclasses
  | op `elem` boolBinaryFuncs =
    makeFuncType [tBool, tBool] tBool
  | op `elem` equalityFuncs =
    makeFuncType [tInt, tInt] tBool -- TODO: fix this once there are typeclasses
  | op `elem` ordFuncs =
    makeFuncType [tInt, tInt] tBool -- TODO: fix this once there are typeclasses
  | otherwise =
    error $ "getBinaryFnType missing a case for " ++ show op

intBinaryFuncs :: [BinOp]
intBinaryFuncs = [Mod, BitAnd, BitOr, BitXor, LShift, RShift]

numBinaryFuncs :: [BinOp]
numBinaryFuncs = [Plus, Minus, Times, Divide, Power]

boolBinaryFuncs :: [BinOp]
boolBinaryFuncs = [BoolAnd, BoolOr]

equalityFuncs :: [BinOp]
equalityFuncs = [Eq, NotEq]

ordFuncs :: [BinOp]
ordFuncs = [Less, LessEq, Greater, GreaterEq]

unifyAll :: Type -> [Type] -> InferM ()
unifyAll t = mapM_ (unify t)

unify :: Type -> Type -> InferM ()
unify t1 t2 = do
  s <- getSub
  let message = "mgu " ++ show (apply s t1) ++ ", " ++ show (apply s t2)
  s2 <- lift $ traceErr message $ mgu (apply s t1) (apply s t2)
  extendSub s2

mismatch :: Type -> Type -> Result a
mismatch t1 t2 = Left $ Mismatch t1 t2

-- TODO: remove debugging:
traceErr :: String -> Result a -> Result a
traceErr _       (Right x) = Right x
--traceErr message (Left x)  = trace message (Left x)
traceErr _ (Left x)  = Left x


unifies :: Type -> Type -> Bool
unifies t1 t2 = isRight $ mgu t1 t2

-- mgu finds a substitution s such that
-- apply s t1 == apply s t2
mgu :: Type -> Type -> Result Substitution
mgu t1 t2 = case (t1, t2) of
  (TGen _, _) ->
    --Left $ CompilerBug "A generic variable should have been instantiated"
    error $ "A generic variable should have been instantiated: " ++ show (t1, t2)
  (_, TGen _) ->
    --Left $ CompilerBug "A generic variable should have been instantiated"
    error $ "A generic variable should have been instantiated: " ++ show (t1, t2)

  (TFunc n1, TFunc n2) ->
    if n1 == n2 then return emptySubstitution
    else mismatch t1 t2

  (TCon ac, TCon bc) ->
    if ac == bc then return emptySubstitution
    else mismatch t1 t2

  (TAp a1 b1, TAp a2 b2) -> do
    sub1 <- mgu a1 a2
    sub2 <- mgu (apply sub1 b1) (apply sub1 b2)
    return $ composeSubs sub1 sub2

  (TVar var, other) ->
    varBind var other

  (other, TVar var) ->
    varBind var other

  _ ->
    mismatch t1 t2

varBind :: String -> Type -> Result Substitution
varBind var other
  | other == TVar var =
    return emptySubstitution
  | Set.member var (freeTypeVars other) =
    Left $ InfiniteType var
  | otherwise =
    return $ Map.singleton (TVar var) other
-- TODO: check kinds in varBind

getType :: (Annotated a) => a Annotation -> Type
getType node =
  fromMaybe (error "must be typed") (Anno.getType node)

mustLookup :: (Ord k, Show k) => k -> Map k v -> v
mustLookup key m =
  let err = error $ "nothing bound for " ++ show key
  in fromMaybe err $ Map.lookup key m

fromMaybe :: a -> Maybe a -> a
fromMaybe _ (Just x) = x
fromMaybe d Nothing  = d


isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False

--uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
--uncurry3 fn (a, b, c) = fn a b c

insertAll :: (Ord k) => Map k v -> [(k, v)] -> Map k v
insertAll m ((k, v):kvs) =
  insertAll (Map.insert k v m) kvs
insertAll m [] =
  m

-- This is not a great solution, ideally generalization would
-- always pick new TGen generics from left to right,
-- but this is good enough for now.
schemesEquivalent :: Scheme -> Scheme -> Bool
schemesEquivalent (Scheme n1 t1) (Scheme n2 t2) =
  n1 == n2 && genSubstitutes t1 t2

alphaSubstitues :: Type -> Type -> Bool
alphaSubstitues t1 t2 = case getVarPairs t1 t2 of
  Nothing    -> False
  Just pairs -> isAlphaSub pairs

genSubstitutes :: Type -> Type -> Bool
genSubstitutes t1 t2 = case getGenPairs t1 t2 of
  Nothing    -> False
  Just pairs -> isAlphaSub pairs


isAlphaSub :: (Ord a) => [(a, a)] -> Bool
isAlphaSub items = isInjective items && isInjective (map swap items)

isInjective :: (Ord a) => [(a, a)] -> Bool
isInjective = check Map.empty
  where check _ [] = True
        check existing ((a,b):is) =
          let noConflict = case Map.lookup a existing of
                Nothing -> True
                Just b' -> b' == b
          in noConflict && check (Map.insert a b existing) is

getGenPairs :: Type -> Type -> Maybe [(Int, Int)]
getGenPairs t1 t2 = case (t1, t2) of
  (TGen a, TGen b) ->
    return [(a, b)]
  (TFunc n1, TFunc n2) -> do
    requireEq n1 n2
    return []
  (TCon cA, TCon cB) -> do
    requireEq cA cB
    return []
  (TAp a1 b1, TAp a2 b2) -> do
    gens1 <- getGenPairs a1 a2
    gens2 <- getGenPairs b1 b2
    return $ gens1 ++ gens2
  (TVar a, TVar b) -> do
    requireEq a b
    return []
  _ ->
    Nothing


getVarPairs :: Type -> Type -> Maybe [(String, String)]
getVarPairs t1 t2 = case (t1, t2) of
  (TGen _, _) ->
    Nothing
  (_, TGen _) ->
    Nothing
  (TCon cA, TCon cB) -> do
    requireEq cA cB
    return []
  (TFunc n1, TFunc n2) -> do
    requireEq n1 n2
    return []
  (TAp a1 b1, TAp a2 b2) -> do
    vars1 <- getVarPairs a1 a2
    vars2 <- getVarPairs b1 b2
    return $ vars1 ++ vars2
  (TVar a, TVar b) ->
    return [(a, b)]
  _ ->
    Nothing


require :: Bool -> Maybe ()
require True  = Just ()
require False = Nothing


requireEq :: (Eq a) => a -> a -> Maybe ()
requireEq a b = require $ a == b
