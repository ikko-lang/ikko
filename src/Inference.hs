{-# LANGUAGE FlexibleInstances #-}

module Inference
  ( inferModule
  , mgu
  , startingEnv
  , runInfer
  , runInferWithSub
  , inferExpr
  , inferDecl
  , tiExpl
  , inferGroup
  , unifies
  , alphaSubstitues
  , genSubstitutes
  , getVarPairs
  , makeBindGroup
  , implicitBindings
  , explicitBindings
  , splitExplicit
  , freshInst
  , getExplicitType
  , BindGroup
  , Environment
  , TypedDecls
  , InferResult(..)
  , Preds
  ) where


import Prelude hiding (exp)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (isJust)
import Data.List ((\\), partition)

import Debug.Trace

import Control.Monad (when, foldM, zipWithM, msum)
import Control.Monad.State (StateT, modify, get, gets, put, lift, evalStateT, mapStateT, runStateT)


import Util.Functions
import qualified AST.Annotation as Anno
import AST.Annotation (Annotation, Annotated, getLocation, addType, mapType)
import AST.Expression (UnaryOp(..), BinOp(..))
import qualified AST.Expression as E
import qualified AST.Statement as S
import qualified AST.Declaration as D
import qualified AST.Type as T

import Types
  ( Substitution
  , Scheme(..)
  , Type(..)
  , Kind(..)
  , TyVar(..)
  , Predicate(..)
  , Qualified(..)
  , QualType
  , Inst
  , Class(..)
  , ClassEnv(..)
  , Types
  , instantiate
  , makeFuncType
  , makeVar
  , applyTypes
  , getRoot
  , getKind
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


type Preds = [Predicate]

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

-- TODO: Remove TypedDecls after changing the type annotation to be a Scheme
type TypedDecls = [(String, DeclarationT, Preds)]

type Binding = (String, DeclarationT)
newtype Bindings = Bindings [Binding]
  deriving (Show)

data InferResult
  = InferResult
    { topLevelBindings :: [(String, DeclarationT)]
    , topLevelEnv      :: Environment }
  deriving (Show)

inferModule :: Module -> Result InferResult
inferModule m =
  let ctors = constructors m
      ce = classEnv m
  in runInfer ctors ce $ inferProgram m

inferProgram :: Module -> InferM InferResult
inferProgram m = do
  let bindGroup = makeBindGroup m
  (binds, env, ps) <- inferBindGroup bindGroup startingEnv

  -- Reduce the predicates
  ps' <- applyCurrentSub ps
  ce <- getClassEnv
  reduced <- reduce ce ps'

  -- Then apply create a default substitution
  defaultingSub <- defaultSubst ce reduced

  -- Combine that with the current substitution
  extendSub defaultingSub

  -- And finally apply that to the result
  Bindings binds' <- applyCurrentSub binds
  env' <- applyCurrentSub env
  return InferResult { topLevelBindings=binds', topLevelEnv=env' }

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
      [name | not (Set.member name bound)]
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
    , constrs :: CTors
    , cenv :: ClassEnv }
  deriving (Show)

startingInferState :: CTors -> ClassEnv -> InferState
startingInferState ctors ce =
  InferState
  { nextVarN = 0
  , currentSub = Map.empty
  , constrs = ctors
  , cenv = ce }

-- `Either Error` = `Result`, but somehow that
-- type synonym isn't allowed here.
type InferM = StateT InferState (Either Error)

type Environment = Map String Scheme

-- toAdd, existing
addToEnv :: Environment -> Environment -> Environment
addToEnv = Map.union

instance Types Environment where
  apply sub = Map.map (apply sub)
  freeTypeVars env =
    Map.foldr Set.union Set.empty (Map.map freeTypeVars env)

-- TODO: this should also start with prelude and imported names
startingEnv :: Environment
startingEnv =
  Map.fromList
  [ ("print", Scheme [Star] (Qual [] $ makeFuncType [TGen 0 Star] tUnit)) ]

runInfer :: Monad m => Map String Constructor
         -> ClassEnv -> StateT InferState m a -> m a
runInfer ctors ce f =
  evalStateT f (startingInferState ctors ce)

runInferWithSub :: Monad m =>
  Map String Constructor -> ClassEnv -> StateT InferState m a -> m (a, Substitution)
runInferWithSub ctors ce f = do
  (a, st) <- runStateT f (startingInferState ctors ce)
  return (a, currentSub st)

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

newTypeVar :: Kind -> InferM Type
newTypeVar kind = do
  st <- get
  let n = nextVarN st
  put $ st { nextVarN = 1 + n }
  return $ makeVar ("_v" ++ show n) kind

newStarVar :: InferM Type
newStarVar = newTypeVar Star

getSub :: InferM Substitution
getSub = gets currentSub

getClassEnv :: InferM ClassEnv
getClassEnv = gets cenv

applyCurrentSub :: (Types t) => t -> InferM t
applyCurrentSub t = do
  sub <- getSub
  return $ apply sub t

extendSub :: Substitution -> InferM ()
extendSub sub = do
  s1 <- getSub
  let s = composeSubs s1 sub
  modify (\st -> st { currentSub=s })

inferBindGroup :: BindGroup -> Environment -> InferM (Bindings, Environment, Preds)
inferBindGroup bg env = do
  let expls = explicitBindings bg
  explicitBindingTypes <- getExplicitTypes expls

  let env1 = addToEnv explicitBindingTypes env
  let impls = implicitBindings bg

  (decls1, env2, ps1) <- inferGroups env1 impls
  let (bindings1, _) = toBindings decls1
  let env' = addToEnv env2 env1
  decls2 <- tiExpls expls env'
  let (bindings2, ps2) = toBindings decls2
  -- ps1 and ps2 are _deferred_ predicates
  return (Bindings $ bindings1 ++ bindings2, env', ps1 ++ ps2)

toBindings :: TypedDecls -> ([Binding], Preds)
toBindings = foldl extractPreds ([], [])
  where extractPreds (binds, preds) (name, decl, ps) =
          ((name, decl) : binds, ps ++ preds)


-- TODO: The predicates in TypedDecls are actually the deferred predicates, so
-- the structure doesn't really make sense, but it works for now
tiExpls :: [(String, DeclarationT)] -> Environment -> InferM TypedDecls
tiExpls expls env = case expls of
  [] ->
    return []
  ((name, decl):es) -> do
    (d, ps) <- tiExpl name decl env
    ds <- tiExpls es env
    return $ (name, d, ps) : ds

tiExpl ::  String -> DeclarationT -> Environment -> InferM (DeclarationT, Preds)
tiExpl name decl env = do
  let sch = mustLookup name env
  (t, qs) <- freshInst sch

  (d, ps) <- inferDecl env decl
  let dt = getType d
  withLocations [decl] $ unify t dt

  sub <- getSub

  let qs' = apply sub qs
  let t' = apply sub t
  let fs = freeTypeVars (apply sub env)
  let gs = Set.difference (freeTypeVars t') fs

  let sch' = quantify gs (Qual qs' t')

  ce <- getClassEnv
  let ps' = filter (not . entail ce qs') (apply sub ps)
  (deferred, retained) <- split ce fs gs ps'

  if not $ schemesEquivalent sch' sch
    then inferErrFor [decl] $ BindingTooGeneral $ name ++ ": " ++ show (sch', sch)
    else if not (null retained)
         then inferErrFor [decl] $ ContextTooWeak name
         else return (d, deferred)


getExplicitTypes :: [(String, DeclarationT)] -> InferM Environment
getExplicitTypes expls = do
  typed <- mapM getExplicitType expls
  return $ Map.fromList typed

getExplicitType :: (name, DeclarationT) -> InferM (name, Scheme)
getExplicitType (name, decl) = case decl of
  D.Let _ _ mtdecl _ -> do
    let (Just tdecl) = mtdecl
    (t, ps) <- withLocations [decl] $ typeFromDecl Map.empty tdecl
    return (name, Scheme [] $ Qual ps t)

  D.Function _ _ mgendecl _ _ -> do
    let Just (gens, tdecl) = mgendecl
    gmap <- genericMap gens
    (t, ps) <- withLocations [decl] $ typeFromDecl gmap tdecl
    let varSet = freeTypeVars $ Map.elems gmap
    return (name, quantify varSet $ Qual ps t)

  D.TypeDef{} ->
    error "shouldn't see a typedef here"


genericMap :: [T.Type] -> InferM (Map String Type)
genericMap tnames = do
  gens <- mapM (const newStarVar) tnames
  return $ Map.fromList $ zip tnames gens

inferGroups :: Environment -> [[(String, DeclarationT)]] ->
               InferM (TypedDecls, Environment, Preds)
inferGroups _   []     =
  return ([], Map.empty, [])
inferGroups env (g:gs) = do
  (typed, env1, ps1) <- inferGroup env g
  (rest, env2, ps2) <- inferGroups (Map.union env1 env) gs
  return (typed ++ rest, Map.union env1 env2, ps1 ++ ps2)

inferGroup :: Environment -> [(String, DeclarationT)] ->
              InferM (TypedDecls, Environment, Preds)
inferGroup env impls = do
  -- Map each binding to a new type variable while recursively typing these bindings
  ts <- mapM (const newStarVar) impls
  let bindingNames = map fst impls
  let bindingSchemes = map asScheme ts
  let groupBindings = Map.fromList $ zip bindingNames bindingSchemes
  let groupEnv = Map.union groupBindings env

  -- Do the actual inference
  typedDecls <- inferDecls groupEnv impls ts
  let (_, _, preds) = unzip3 typedDecls

  -- Apply the substitution to all the types and generalize them to schemes
  sub <- getSub
  let preds' = apply sub (concat preds)
  let subbed = map (apply sub) ts
  let fs = freeTypeVars (apply sub env)
  let vss = map freeTypeVars subbed
  let gs = Set.difference (foldr1 Set.union vss) fs
  ce <- getClassEnv
  (deferred, retained) <- split ce fs (foldr1 Set.intersection vss) preds'
  let schemes = map (quantify gs . Qual retained) subbed
  let resultEnv = Map.fromList $ zip bindingNames schemes
  return (typedDecls, resultEnv, deferred)

{-
showTrace :: (Show a) => String -> a -> a
showTrace s a = trace (s ++ ": " ++ show a) a
-}

inferDecls :: Environment -> [(String, DeclarationT)] -> [Type] -> InferM TypedDecls
inferDecls env decls ts = mapM infer (zip decls ts)
  where infer ((name, decl), t) = do
          (d, ps) <- inferDecl env decl
          withLocations [d] $ unify t (getType d)
          return (name, d, ps)

generalize :: Environment -> QualType -> Scheme
generalize env qt =
  let envVars = foldl Set.union Set.empty $ map (freeTypeVars . snd) $ Map.toList env
  in generalizeOver envVars qt

generalizeOver :: Set TyVar -> QualType -> Scheme
generalizeOver envVars qt =
  let varList = Set.toList $ Set.difference (freeTypeVars qt) envVars
      kinds = map getKind varList
      freeVars = map TVar varList
      genVars = zipWith TGen [0..] kinds
      sub = Map.fromList $ zip freeVars genVars
  in Scheme kinds (apply sub qt)

quantify :: Set TyVar -> QualType -> Scheme
quantify vs qt =
  let vars = [ v | v <- Set.toList (freeTypeVars qt), Set.member v vs ]
      kinds = map getKind vars
      genVars = zipWith TGen [0..] kinds
      s = Map.fromList $ zip (map TVar vars) genVars
  in Scheme kinds (apply s qt)

freshInst :: Scheme -> InferM (Type, Preds)
freshInst sch@(Scheme kinds qt) = do
  newVars <- mapM newTypeVar kinds
  let (Qual ps t) = instantiate newVars qt
  verifyContainsNoGenerics sch t
  return (t, ps)

verifyContainsNoGenerics :: Scheme -> Type -> InferM ()
verifyContainsNoGenerics sch t =
  let message = "instantiated type " ++ show t ++ " from " ++ show sch ++  " contains generics"
  in when (containsGenerics t) (inferErr $ CompilerBug message)


containsGenerics :: Type -> Bool
containsGenerics t = case t of
  TGen _ _  -> True
  TAp a b   -> any containsGenerics [a, b]
  TFunc _ _ -> False
  TCon _ _  -> False
  TVar   _  -> False


inferDecl :: Environment -> DeclarationT -> InferM (DeclarationT, Preds)
inferDecl env decl = case decl of
  D.Let a name mtype expr -> do
    (expr', t, ps) <- inferExpr env expr
    return (addType t $ D.Let a name mtype expr', ps)

  D.Function a name mtype args stmt -> do
    argTs <- mapM (const newStarVar) args
    retT <- newStarVar

    -- This case statement exists so that, by the time
    -- a field access is reached, the type of that variable
    -- will already be known.
    ps1 <- case Map.lookup name env of
      Nothing -> return []
      Just sc -> do
        (dt, ps) <- freshInst sc
        withLocations [stmt] $ unify dt (makeFuncType argTs retT)
        return ps

    let argEnv = Map.fromList $ zip args (map asScheme argTs)
    let env' = Map.union argEnv env
    (stmt', stmtReturns, ps2) <- inferStmt env' stmt
    let funcReturns = toFunctionReturns stmtReturns
    withLocations [stmt] $ unifyAll retT funcReturns
    sub <- getSub
    let argTypes = map (apply sub) argTs
    let returnT = apply sub retT
    let t = makeFuncType argTypes returnT
    return (addType t $ D.Function a name mtype args stmt', ps1 ++ ps2)

  D.TypeDef{} ->
    inferErr $ CompilerBug "TypeDefs are not bindings"

inferStmt :: Environment -> StatementT ->
             InferM (StatementT, DoesReturn, Preds)
inferStmt env stmt = case stmt of
  S.Return a Nothing ->
    return (S.Return a Nothing, AlwaysReturns [tUnit], [])
  S.Return a (Just expr) -> do
    (expr', _, ps) <- inferExpr env expr
    return (S.Return a (Just expr'), AlwaysReturns [getType expr'], ps)

  S.Let a name mtype expr -> do
    -- TODO: recursive binding?
    -- Note that in recursive bindings, if the bound expression involves a
    -- closure, I need to think about whether that closure should be allowed to
    -- assign back to this variable
    (expr', _, ps1) <- inferExpr env expr
    ps2 <- case mtype of
      Nothing    -> return []
      Just tdecl -> do
        -- TODO: allow generics here, too?
        (t, ps) <- typeFromDecl Map.empty tdecl
        withLocations [stmt] $ unify (getType expr') t
        return ps
    return (S.Let a name mtype expr', NeverReturns, ps1 ++ ps2)

  S.Assign a names expr ->
    case names of
     []    -> inferErr $ CompilerBug "assignment to no names"
     [var] -> do
       -- Something to think about: Should this be required to be a variable
       -- defined in a `let` statement instead of an argument to a function?
       (expr', exprT, ps) <- inferExpr env expr
       sch <- withLocations [stmt] $ lookupName var env
       (varT, ps2) <- freshInst sch
       -- TODO: not quite right, since this may result in too narrow of a type
       -- getting assigned, but it's good enough for now
       withLocations [stmt] $ unify exprT varT
       return (S.Assign a names expr', NeverReturns, ps ++ ps2)
     (var:fields) -> do
       (expr', exprT, ps1) <- inferExpr env expr

       sch <- withLocations [stmt] $ lookupName var env
       -- Is it right for this to be working on an instantiation of sch?
       (varT, ps2) <- freshInst sch
       fieldT <- withLocations [stmt] $ getStructField varT fields
       withLocations [stmt] $ unify fieldT exprT

       return (S.Assign a names expr', NeverReturns, ps1 ++ ps2)

  S.Block a stmts -> do
    (stmts', retT, ps) <- inferBlock env stmts
    return (S.Block a stmts', retT, ps)

  S.Expr a expr -> do
    (expr', _, ps) <- inferExpr env expr
    return (S.Expr a expr', NeverReturns, ps)

  S.If a test blk els -> do
    (testExpr, _, ps1) <- inferExpr env test
    withLocations [testExpr] $ unify (getType testExpr) tBool
    (blk', blkReturns, ps2) <- inferBlock env blk
    (els', elsReturns, ps3) <- case els of
      Nothing ->
        return (Nothing, NeverReturns, [])
      Just st -> do
        (stmt', retT, ps) <- inferStmt env st
        return (Just stmt', retT, ps)
    let ifReturns = combineReturns blkReturns elsReturns
    return (S.If a testExpr blk' els', ifReturns, ps1 ++ ps2 ++ ps3)

  S.While a test blk -> do
    (testExpr, _, ps1) <- inferExpr env test
    withLocations [testExpr] $ unify (getType testExpr) tBool
    (blk', blkReturns, ps2) <- inferBlock env blk
    let whileReturns = demoteReturns blkReturns
    return (S.While a testExpr blk', whileReturns, ps1 ++ ps2)

  S.Match a expr cases -> do
    (expr', exprType, ps) <- inferExpr env expr
    casesAndReturns <- mapM (inferMatchCase env exprType) cases
    let (cases', returns, pss) = unzip3 casesAndReturns
    let resultType = getType (head cases')
    let result = addType resultType $ S.Match a expr' cases'
    return (result, foldl1 combineReturns returns, concat (ps : pss))

  S.Pass a -> do
    let result = addType tUnit $ S.Pass a
    return (result, NeverReturns, [])


inferMatchCase :: Environment -> Type -> MatchCaseT
               -> InferM (MatchCaseT, DoesReturn, Preds)
inferMatchCase env t (S.MatchCase matchExpr stmt) = do
  (matchExpr', matchedVars, ps1) <- inferMatchExpr env t matchExpr
  let env' = insertAll env matchedVars
  (stmt', doesReturn, ps2) <- inferStmt env' stmt
  return (S.MatchCase matchExpr' stmt', doesReturn, ps1 ++ ps2)


inferMatchExpr :: Environment -> Type -> MatchExpressionT
               -> InferM (MatchExpressionT, [(String, Scheme)], Preds)
inferMatchExpr env targetType matchExpr = case matchExpr of
  S.MatchAnything a ->
    return (addType targetType $ S.MatchAnything a, [], [])

  S.MatchVariable a name -> do
    let sch = generalize env (Qual [] targetType)
    let bindings' = [(name, sch)]
    return (addType targetType $ S.MatchVariable a name, bindings', [])

  S.MatchStructure a enumName fields -> do
    ctor <- withLocations [matchExpr] $ getConstructor enumName

    when (length fields /= length (ctorFields ctor)) $
      inferErrFor [matchExpr] $ PatternErr $ "wrong number of fields matched for " ++ enumName

    let sch = ctorType ctor
    (enumT, ps2) <- freshInst sch
    (argTs, structT) <- destructFunctionType enumT

    -- Pattern matching is basically running the constructor function backwards
    withLocations [matchExpr] $ unify targetType structT

    sub1 <- getSub
    let patternTypes = map (apply sub1) argTs

    inner <- zipWithM (inferMatchExpr env) patternTypes fields
    let (typedFields, bindingLists, pss) = unzip3 inner
    let binds = concat bindingLists
    let ps = concat pss

    sub2 <- getSub
    let structType = apply sub2 structT
    return (addType structType $ S.MatchStructure a enumName typedFields, binds, ps ++ ps2)


destructFunctionType :: Type -> InferM ([Type], Type)
destructFunctionType (TAp t ret) = do
  argTypes <- getArgTypes t
  return (reverse argTypes, ret)
destructFunctionType _ =
  inferErr $ CompilerBug "invalid function type"

-- getArgTypes returns them backwards
getArgTypes :: Type -> InferM [Type]
getArgTypes (TFunc _ _) =
  return []
getArgTypes (TAp t a) = do
  rest <- getArgTypes t
  return (a : rest)
getArgTypes _ =
  inferErr $ CompilerBug "invalid function type"


inferBlock :: Environment -> [StatementT] ->
              InferM ([StatementT], DoesReturn, Preds)
inferBlock _   []     = return ([], NeverReturns, [])
inferBlock env [stmt] = do
  (stmt', retT, ps) <- inferStmt env stmt
  return ([stmt'], retT, ps)
inferBlock env (s:ss) = do
  (s', ret1, ps1) <- inferStmt env s
  let env' = case s' of
        S.Let _ name _ expr ->
          let sch = generalize env (Qual [] $ getType expr)
          in Map.insert name sch env
        _                  -> env
  (ss', ret2, ps2) <- inferBlock env' ss
  return (s' : ss', appendReturns ret2 ret1, ps1 ++ ps2)


getStructField :: Type -> [String] -> InferM Type
getStructField = foldM getStructFieldType

getStructFieldType :: Type -> String -> InferM Type
getStructFieldType t fieldName = case getRoot t of
  TCon structName _ -> do
    resultT <- newStarVar

    ctor <- getConstructor structName
    let fields = ctorFields ctor
    fieldSch <- case lookup fieldName fields of
      Nothing -> inferErr $ UndefinedField structName fieldName
      Just sc -> return sc
    (fieldFn, ps) <- freshInst fieldSch -- TOOD: Use ps here?

    unify fieldFn (makeFuncType [t] resultT)
    sub <- getSub
    return $ apply sub resultT
  TVar _            ->
    inferErr InsufficientlyDefinedType
  TGen _ _          ->
    inferErr $ CompilerBug "got an unexpected generic"
  TAp _ _           ->
    inferErr $ WrongType t "expected a structure, got a type application"
  TFunc _ _         ->
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


inferExpr :: Environment -> ExpressionT -> InferM (ExpressionT, Type, Preds)
inferExpr env expr = case expr of
  E.Paren a e -> do
    (e', t, ps) <- inferExpr env e
    return (addType t $ E.Paren a e', t, ps)

  E.Val a val -> do
    (val', t, ps) <- inferValue env val
    return (addType t $ E.Val a val', t, ps)

  E.Unary a op exp -> do
    resultT <- newStarVar
    (exp', expT, ps) <- inferExpr env exp
    let fnT = getUnaryFnType op
    withLocations [expr] $ unify fnT (makeFuncType [expT] resultT)
    sub <- getSub
    let t = apply sub resultT
    return (addType t $ E.Unary a op exp', t, ps)

  E.Binary a op l r -> do
    resultT <- newStarVar
    (l', lt, ps1) <- inferExpr env l
    (r', rt, ps2) <- inferExpr env r
    (fnT, ps3) <- getBinaryFnType op
    withLocations [expr] $ unify fnT (makeFuncType [lt, rt] resultT)
    sub <- getSub
    let t = apply sub resultT
    return (addType t $ E.Binary a op l' r', t, ps1 ++ ps2 ++ ps3)

  E.Call a fexp args -> do
    resultT <- newStarVar
    (fexp', ft, ps) <- inferExpr env fexp
    typedArgs <- mapM (inferExpr env) args
    let (args', argTs, pss) = unzip3 typedArgs
    withLocations [expr] $ unify ft (makeFuncType argTs resultT)
    sub <- getSub
    let t = apply sub resultT
    return (addType t $ E.Call a fexp' args', t, concat $ ps : pss)

  E.Cast a t exp -> do
    (exp', expT, ps) <- inferExpr env exp
    sch <- withLocations [expr] $ attemptCast t expT
    return (addType sch $ E.Cast a t exp', sch, ps)

  E.Var a name -> do
    sch <- withLocations [expr] $ lookupName name env
    (t, ps) <- freshInst sch
    return (addType t $ E.Var a name, t, ps)

  E.Access a exp field -> do
    (exp', et, ps) <- inferExpr env exp
    sub <- getSub
    let etype = apply sub et
    t <- withLocations [expr] $ getStructFieldType etype field
    return (addType t $ E.Access a exp' field, t, ps)

inferValue :: Environment -> ValueT -> InferM (ValueT, Type,  Preds)
inferValue env val = case val of
  E.StrVal a str ->
    return (addType tString $ E.StrVal a str, tString, [])

  E.BoolVal a b ->
    return (addType tBool $ E.BoolVal a b, tBool, [])

  E.IntVal a i -> do
    t <- newTypeVar Star
    return (addType tInt $ E.IntVal a i, t, [Pred "Num" t])

  E.FloatVal a f ->
    return (addType tFloat $ E.FloatVal a f, tFloat, [])

  E.StructVal a tname fields  -> do
    resultT <- newStarVar

    ctor <- withLocations [val] $ getConstructor tname
    checkSameFields tname (ctorFields ctor) fields
    (fnT, ps) <- freshInst (ctorType ctor)
    let fieldNamesInType = map fst $ ctorFields ctor

    -- Sort the fields in the same order they are in the type.
    -- This allows typing to ignore field order.
    let sorted = orderAs fieldNamesInType fields
    typedFields <- mapM (inferExpr env . snd) sorted
    let fieldTypes = map snd3 typedFields

    -- Unify that with the "function" type of the struct's constructor
    withLocations [val] $ unify fnT (makeFuncType fieldTypes resultT)
    -- and find out what the resulting type is
    sub <- getSub
    let t = apply sub resultT

    -- Finally, put the fields back in the right order since the order is
    -- significant in evaluation
    let namedTypedFields = zip (map fst sorted) (map fst3 typedFields)
    let orderedFields = orderAs (map fst fields) namedTypedFields

    let pss = map trd3 typedFields

    return (addType t $ E.StructVal a tname orderedFields, t, concat (ps : pss))


orderAs :: (Ord a) => [a] -> [(a, b)] -> [(a, b)]
orderAs keys pairs = map getPair keys
  where getPair key = (key, lookup_ key pairs)

getStructType :: String -> InferM (Type, Preds)
getStructType name = do
  ctor <- getConstructor name
  let sch = valueType ctor
  freshInst sch

typeFromDecl :: Map String Type -> TypeDeclT -> InferM (Type, Preds)
typeFromDecl gmap tdecl = case tdecl of
  T.TypeName _ name ->
    case Map.lookup name gmap of
      Nothing -> typeFromName name
      Just t  -> return (t, [])
  T.Generic _ name genArgs -> do
    genTypesAndPreds <- mapM (typeFromDecl gmap) genArgs
    let (genTypes, genPreds) = unzip genTypesAndPreds
    (t, ps) <- typeFromName name
    case t of
      TAp _ _ -> do
        unify t (applyTypes (getRoot t) genTypes)
        t' <- applyCurrentSub t
        return (t', ps ++ concat genPreds)
      _ -> error $ "compiler bug " ++ show t
  T.Function _ predicates argTs retT -> do
    argTypes <- mapM (typeFromDecl gmap) argTs
    retType <- typeFromDecl gmap retT
    preds <- mapM (predFromAST gmap) predicates
    let innerPreds = concatMap snd (retType : argTypes)
    case innerPreds of
      (_:_) ->
        error $ "expected no inner predicates here, got " ++ show innerPreds
      []    ->
        return (makeFuncType (map fst argTypes) (fst retType), preds)
  T.Struct{} ->
    error "shouldn't see a Struct here"
  T.Enum{} ->
    error "shouldn't see an Enum here"
  T.ClassDecl{} ->
    error "shouldn't see an Class here"

predFromAST :: Map String Type -> T.Predicate a -> InferM Predicate
predFromAST gmap tpred = do
  let name = T.predType tpred
  (typ, ps) <- typeFromDecl gmap (T.TypeName [] name)
  case ps of
    [] -> return $ Pred (T.predClass tpred) typ
    _  -> error $ "expected no recursive predicates: " ++ show ps

typeFromName :: String -> InferM (Type, Preds)
typeFromName name
  | name `elem` builtinTypes =
    -- At the moment, all built-in types have the kind *
    return (TCon name Star, [])
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
  let toType = TCon toTypeName Star
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

getBinaryFnType :: BinOp -> InferM (Type, Preds)
getBinaryFnType op
  | op `elem` intBinaryFuncs =
      return (makeFuncType [tInt, tInt] tInt, [])
  | op `elem` numBinaryFuncs = do
      t <- newTypeVar Star
      return (makeFuncType [t, t] t, [Pred "Num" t])
  | op `elem` boolBinaryFuncs =
      return (makeFuncType [tBool, tBool] tBool, [])
  | op `elem` equalityFuncs = do
      t <- newTypeVar Star
      return (makeFuncType [t, t] tBool, [Pred "Eq" t])
  | op `elem` ordFuncs = do
      t <- newTypeVar Star
      return (makeFuncType [t, t] tBool, [Pred "Ord" t])
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
  (TGen _ _, _) ->
    --Left $ CompilerBug "A generic variable should have been instantiated"
    error $ "A generic variable should have been instantiated: " ++ show (t1, t2)
  (_, TGen _ _) ->
    --Left $ CompilerBug "A generic variable should have been instantiated"
    error $ "A generic variable should have been instantiated: " ++ show (t1, t2)

  (TFunc n1 _, TFunc n2 _) ->
    if n1 == n2 then return emptySubstitution
    else mismatch t1 t2

  (TCon ac ak, TCon bc bk) ->
    if ac == bc && ak == bk
    then return emptySubstitution
    else mismatch t1 t2

  (TAp a1 b1, TAp a2 b2) -> do
    sub1 <- mgu a1 a2
    sub2 <- mgu (apply sub1 b1) (apply sub1 b2)
    return $ composeSubs sub1 sub2

  (TVar tv, other) ->
    varBind tv other

  (other, TVar tv) ->
    varBind tv other

  _ ->
    mismatch t1 t2

varBind :: TyVar -> Type -> Result Substitution
varBind tv@(TyVar _ k) other
  | other == TVar tv =
    return emptySubstitution
  | Set.member tv (freeTypeVars other) =
    Left $ InfiniteType tv
  | k /= getKind other =
    Left $ KindMismatch tv k (getKind other)
  | otherwise =
    return $ Map.singleton (TVar tv) other


predsMatch :: Predicate -> Predicate -> Maybe Substitution
predsMatch (Pred c1 t1) (Pred c2 t2) =
  if c1 == c2
  then matches t1 t2
  else Nothing

matches :: Type -> Type -> Maybe Substitution
matches t1 t2 = case (t1, t2) of
  (TGen _ _, _) ->
    error $ "A generic variable should have been instantiated in matches: " ++ show (t1, t2)
  (_, TGen _ _) ->
    error $ "A generic variable should have been instantiated in matches: " ++ show (t1, t2)

  (TFunc n1 _, TFunc n2 _) ->
    if n1 == n2
    then return emptySubstitution
    else Nothing

  (TCon ac ak, TCon bc bk) ->
    if ac == bc && ak == bk
    then return emptySubstitution
    else Nothing

  (TAp a1 b1, TAp a2 b2) -> do
    sub1 <- matches a1 a2
    sub2 <- matches b1 b2
    merge sub1 sub2

  (TVar _, other) ->
    return $ Map.singleton t1 other

  _ ->
    Nothing

merge :: Substitution -> Substitution -> Maybe Substitution
merge s1 s2 =
  let agrees k v = case Map.lookup k s2 of
        Nothing -> True
        Just v2 -> v == v2
  in if all (uncurry agrees) (Map.toList s1)
     then return $ Map.union s1 s2
     else Nothing

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
schemesEquivalent (Scheme n1 (Qual _ t1)) (Scheme n2 (Qual _ t2)) =
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
  (TGen a k1, TGen b k2) -> do
    requireEq k1 k2
    return [(a, b)]
  (TFunc n1 k1, TFunc n2 k2) -> do
    requireEq n1 n2
    requireEq k1 k2
    return []
  (TCon cA k1, TCon cB k2) -> do
    requireEq cA cB
    requireEq k1 k2
    return []
  (TAp a1 b1, TAp a2 b2) -> do
    gens1 <- getGenPairs a1 a2
    gens2 <- getGenPairs b1 b2
    return $ gens1 ++ gens2
  (TVar (TyVar a k1), TVar (TyVar b k2)) -> do
    requireEq a b
    requireEq k1 k2
    return []
  _ ->
    Nothing


getVarPairs :: Type -> Type -> Maybe [(String, String)]
getVarPairs t1 t2 = case (t1, t2) of
  (TGen a _, TGen b _) ->
    if a == b then return [] else Nothing
  (TCon cA kA, TCon cB kB) -> do
    requireEq cA cB
    requireEq kA kB
    return []
  (TFunc n1 k1, TFunc n2 k2) -> do
    requireEq n1 n2
    requireEq k1 k2
    return []
  (TAp a1 b1, TAp a2 b2) -> do
    vars1 <- getVarPairs a1 a2
    vars2 <- getVarPairs b1 b2
    return $ vars1 ++ vars2
  (TVar (TyVar a k1), TVar (TyVar b k2)) -> do
    requireEq k1 k2
    return [(a, b)]
  _ ->
    Nothing


require :: Bool -> Maybe ()
require True  = Just ()
require False = Nothing


requireEq :: (Eq a) => a -> a -> Maybe ()
requireEq a b = require $ a == b

mustClass :: ClassEnv -> String -> Class
mustClass ce cls =
  fromMaybe (error $ "must have class " ++ cls)
    (Map.lookup cls (classes ce))

super :: ClassEnv -> String -> [String]
super ce cls = superclasses $ mustClass ce cls

insts :: ClassEnv -> String -> [Inst]
insts ce cls = instances $ mustClass ce cls

bySuper :: ClassEnv -> Predicate -> [Predicate]
bySuper ce p@(Pred i t)
  = p : concat [ bySuper ce (Pred i' t) | i' <- super ce i ]

byInst :: ClassEnv -> Predicate -> Maybe [Predicate]
byInst ce p@(Pred i _) = msum [ tryInst it | it <- insts ce i ]
  where tryInst (Qual ps h) = do
          u <- predsMatch h p
          Just (map (apply u) ps)

-- Assuming all of ps are true, does that mean we know that p is also true?
entail :: ClassEnv -> [Predicate] -> Predicate -> Bool
entail ce ps p =
  let isSuperClass = any (p `elem`) (map (bySuper ce) ps)
      matchingInstanceEntailed = case byInst ce p of
        Nothing -> False
        Just qs -> all (entail ce ps) qs
  in isSuperClass || matchingInstanceEntailed

-- remove redundant predicates, e.g. [Eq a, Ord a, Ord a] --> [Ord a]
simplify :: ClassEnv -> [Predicate] -> [Predicate]
simplify ce = loop []
  where loop rs [] = rs
        loop rs (p:ps) =
          if entail ce (rs++ps) p
          then loop rs ps
          else loop (p:rs) ps

inHNF :: Predicate -> Bool
inHNF (Pred _ t) = hnf t
  where hnf (TVar _)  = True
        hnf (TAp l _) = hnf l
        hnf _         = False

toHNFs :: ClassEnv -> [Predicate] -> InferM [Predicate]
toHNFs ce ps = do
  pss <- mapM (toHNF ce) ps
  return $ concat pss

toHNF :: ClassEnv -> Predicate -> InferM [Predicate]
toHNF ce p
  | inHNF p   = return [p]
  | otherwise = case byInst ce p of
      Nothing -> inferErr $ ContextReduction p
      Just ps -> toHNFs ce ps

reduce :: ClassEnv -> [Predicate] -> InferM [Predicate]
reduce ce ps = do
  qs <- toHNFs ce ps
  return $ simplify ce qs

type Ambiguity = (TyVar, [Predicate])

ambiguities :: Set TyVar -> [Predicate] -> [Ambiguity]
ambiguities vs ps =
  [ (v, filter (Set.member v . freeTypeVars) ps)
  | v <- Set.toList $ Set.difference (freeTypeVars ps) vs ]

numClasses :: [String]
numClasses  = ["Num", "Integral", "Floating", "Fractional",
               "Real", "RealFloat", "RealFrac"]

stdClasses :: [String]
stdClasses  = ["Eq", "Ord", "Show", "Read", "Bounded", "Enum", "Ix",
               "Functor", "Monad", "MonadPlus"] ++ numClasses

candidates :: ClassEnv -> Ambiguity -> [Type]
candidates ce (v, qs) =
  let pairs = [ (i, t) | Pred i t <- qs ]
      classNames = map fst pairs
      types = map snd pairs
      allTypesMatch = all (TVar v ==) types
      anyNumClass = any (`elem` numClasses) classNames
      allStdClass = all (`elem` stdClasses) classNames
  in if allTypesMatch && anyNumClass && allStdClass
     then [ t' | t' <- defaults ce,
            all (entail ce []) [Pred i t' | i <- classNames] ]
     else []

getDefaults :: ClassEnv -> Set TyVar -> [Predicate] -> InferM ([Ambiguity], [Type])
getDefaults ce vs ps =
  let vps = ambiguities vs ps
      tss = map (candidates ce) vps
  in if any null tss
     then inferErr $ Ambiguity ps
     else return (vps, map head tss)

defaultedPreds :: ClassEnv -> Set TyVar -> [Predicate] -> InferM [Predicate]
defaultedPreds ce vs ps = do
  (vps, _) <- getDefaults ce vs ps
  return $ concatMap snd vps

defaultSubst :: ClassEnv -> [Predicate] -> InferM Substitution
defaultSubst ce ps = do
  (vps, ts) <- getDefaults ce Set.empty ps
  return $ Map.fromList $ zip (map (TVar . fst) vps) ts

-- returns (deferred, retained) predicates
split :: ClassEnv -> Set TyVar -> Set TyVar -> [Predicate] -> InferM ([Predicate], [Predicate])
split ce fs gs ps = do
  ps' <- reduce ce ps
  let (ds, rs) = partition (\p -> Set.isSubsetOf (freeTypeVars p) fs) ps'
  rs' <- defaultedPreds ce (Set.union fs gs) rs
  return (ds, rs \\ rs')

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x

trd3 :: (a, b, c) -> c
trd3 (_, _, x) = x

instance Types Bindings where
  apply sub (Bindings bs) =
    Bindings $ map (applySnd (subDeclaration sub)) bs

  freeTypeVars (Bindings bs) =
    freeTypeVars $ map (getType . snd) bs

subDeclaration :: Substitution -> DeclarationT -> DeclarationT
subDeclaration sub = mapType (apply sub)
