{-# LANGUAGE FlexibleInstances #-}

module Inference
  ( inferModule
  , mgu
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
  , schemeIsAtLeastAsGeneral
  , BindGroup
  , Environment
  , TypedDecls
  , InferResult(..)
  , Preds
  ) where


import Prelude hiding (exp)
import Debug.Trace (trace)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (isJust, mapMaybe)
import Data.List ((\\), partition)

import Control.Monad (when, unless, foldM, zipWithM, msum)
import Control.Monad.State (StateT, modify, get, gets, put, lift, evalStateT, mapStateT, runStateT)

import Region (Region)

import Util.Functions

import Util.PrettyPrint
  ( PrettyPrint
  , prettyPrint
  )

import Util.Graph
  ( components
  )

import AST.Annotation
  ( Annotation
  , Annotated
  , addType
  , getLocation
  , mapType
  )
import qualified AST.Annotation as Anno
import AST (UnaryOp(..), BinOp(..))
import qualified AST

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
  , Environment(..)
  , envLookup
  , envInsert
  , envUnion
  , envKeys
  , makeEnv
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
  , freeTypeVars
  , unqualify
  , qualify
  )

import Errors
  ( Error(..)
  , Result
  )

import FirstPass
  ( Constructor(..)
  , InstanceDefinition(..)
  , InstanceMethod(..)
  , Module(..)
  , bindings
  , valueType
  )



type Preds = [Predicate]

type DeclarationT     = AST.Declaration     Annotation
type ExpressionT      = AST.Expression      Annotation
type ValueT           = AST.Value           Annotation
type MatchCaseT       = AST.MatchCase       Annotation
type MatchExpressionT = AST.MatchExpression Annotation
type StatementT       = AST.Statement       Annotation
type TypeDeclT        = AST.TypeDecl        Annotation


data BindGroup
  = BindGroup
    -- the bindings without an explicit type are grouped by strongly connected
    -- component and the groups are toplogically sorted. Bindings in the head of
    -- the list may not refer to bindings in the tail, but bindings in the tail
    -- can refer to bindings in the head.
    { implicitBindings :: [[(String, DeclarationT)]]
    -- all bindings with an explicit type go in one group
    , explicitBindings :: [(String, DeclarationT)]
    , instanceMethods :: [InstanceMethod]
    }

-- TODO: Remove TypedDecls after changing the type annotation to be a Scheme
type TypedDecls = [(String, DeclarationT, Preds)]

type Binding = (String, DeclarationT)
newtype Bindings = Bindings [Binding]
  deriving (Show)

data InferResult
  = InferResult
    { topLevelBindings :: [(String, DeclarationT)]
    , topLevelMethods  :: [InstanceMethod]
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
  (binds, env, ps, typedMethods) <- inferBindGroup bindGroup (rootEnv m)

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
  typedMethods' <- applyCurrentSub typedMethods
  return InferResult { topLevelBindings=binds', topLevelEnv=env', topLevelMethods=typedMethods' }

makeBindGroup :: Module -> BindGroup
makeBindGroup m =
  let declarations = bindings m
      (di, de) = splitExplicit declarations
      explicitNames = Set.fromList $ Map.keys de
      graph = gatherGraph (rootEnv m) explicitNames di
      topoOrder = reverse $ components graph
      getBinding name = (name, mustLookup name declarations)
      impls = map (map getBinding) topoOrder
      expls = Map.toList de
      instMethods = instanceDecls m
  in BindGroup { implicitBindings=impls, explicitBindings=expls, instanceMethods=instMethods }

type DeclMap = Map String DeclarationT

splitExplicit :: DeclMap -> (DeclMap, DeclMap)
splitExplicit = Map.partition isImplicit

isImplicit :: DeclarationT -> Bool
isImplicit = not . isExplicit

isExplicit :: DeclarationT -> Bool
isExplicit decl = case decl of
  AST.DLet      _ _ mt _   -> isJust mt
  AST.DFunction _ _ mt _ _ -> isJust mt
  AST.DTypeDef{}           -> error "shouldn't see a typedef here"
  AST.DInstance{}          -> True

-- This walks each declaration to find out what the
-- dependency graph looks like.
-- This assumes that all the variables are defined (TODO: that's
-- never checked at the moment)
gatherGraph :: Environment -> Set String -> Map String DeclarationT -> Map String [String]
gatherGraph env explicitNames = Map.map (removeExpl . findDependencies given)
  where removeExpl deps = Set.toList $ setSubtract explicitNames $ Set.fromList deps
        given = Set.fromList $ envKeys env

setSubtract :: (Ord a) => Set a -> Set a -> Set a
setSubtract toRemove = Set.filter keep
  where keep e = not $ Set.member e toRemove


class Depencencies a where
  findDependencies :: Set String -> a -> [String]

instance Depencencies (AST.Declaration Annotation) where
  findDependencies bound decl = case decl of
    AST.DLet _ name _  exp ->
      findDependencies (Set.insert name bound) exp
    AST.DFunction _ name _ args stmt ->
      findDependencies (Set.union bound $ Set.fromList (name:args)) stmt
    AST.DTypeDef{} ->
      []
    AST.DInstance{} ->
      -- while the methods of an instance can certainly call other methods, they
      -- don't matter for the order of typechecking, so they are ignored.
      []

instance Depencencies (AST.Statement Annotation) where
  findDependencies bound stmt = case stmt of
    AST.Return _ mexp ->
      maybe [] (findDependencies bound) mexp
    AST.Let _ name _ exp ->
      findDependencies (Set.insert name bound) exp
    AST.Assign _ _ exp ->
      findDependencies bound exp
    AST.Block _ stmts ->
      findDepBlock bound stmts
    AST.Expr _ expr ->
      findDependencies bound expr
    AST.If _ test body elseStmt ->
      let testDeps = findDependencies bound test
          bodyDeps = findDepBlock bound body
          elseDeps = maybe [] (findDependencies bound) elseStmt
      in testDeps ++ bodyDeps ++ elseDeps
    AST.While _ test body ->
      let testDeps = findDependencies bound test
          bodyDeps = findDepBlock bound body
      in testDeps ++ bodyDeps
    AST.Match _ expr matchCases ->
      let exprDeps = findDependencies bound expr
          caseDeps = concatMap (findDependencies bound) matchCases
      in exprDeps ++ caseDeps
    AST.Pass _ ->
      []

instance Depencencies (AST.MatchCase Annotation) where
  findDependencies bound matchCase =
    let (AST.MatchCase matchExpr stmt) = matchCase
        exprNames = findNames matchExpr
        bound' = foldr Set.insert bound exprNames
    in findDependencies bound' stmt

findNames :: MatchExpressionT -> [String]
findNames matchExpr = case matchExpr of
  AST.MatchAnything  _         -> []
  AST.MatchVariable  _ name    -> [name]
  AST.MatchStructure _ _ exprs -> concatMap findNames exprs

findDepBlock :: Set String -> [StatementT] -> [String]
findDepBlock bound stmts = case stmts of
     [] -> []
     (stmt:rest) ->
       let stmtDeps = findDependencies bound stmt
           bound' = case stmt of
             (AST.Let _ name _ _) ->
               Set.insert name bound
             _ ->
               bound
           restDeps = findDepBlock bound' rest
       in stmtDeps ++ restDeps

instance Depencencies (AST.Expression Annotation) where
  findDependencies bound exp = case exp of
    AST.Paren _ inner ->
      findDependencies bound inner
    AST.Val _ val ->
      findDependencies bound val
    AST.Unary _ _ inner ->
      findDependencies bound inner
    AST.Binary _ _ l r ->
      findDependencies bound l ++ findDependencies bound r
    AST.Call _ fn args ->
      let fnDeps = findDependencies bound fn
          argDeps = concatMap (findDependencies bound) args
      in fnDeps ++ argDeps
    AST.Cast _ _ inner ->
      findDependencies bound inner
    AST.Var _ name ->
      [name | not (Set.member name bound)]
    AST.Access _ inner _ ->
      findDependencies bound inner
    AST.Lambda _ args stmt ->
      findDependencies (Set.union bound $ Set.fromList args) stmt

instance Depencencies (AST.Value Annotation) where
  findDependencies bound val = case val of
    AST.StructVal _ _ fields ->
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

inferErrFor :: (PrettyPrint (a Annotation), Annotated a) => [a Annotation] -> Error -> InferM b
inferErrFor nodes err = withLocations nodes $ inferErr err

withLocations :: (PrettyPrint (a Annotation), Annotated a) => [a Annotation] -> InferM b -> InferM b
withLocations nodes inf =
  let regions = mapMaybe checkedGetLocation nodes
  in mapStateT (mapLeft (WithLocations regions)) inf

checkedGetLocation :: (PrettyPrint (a Annotation), Annotated a) => a Annotation -> Maybe Region
checkedGetLocation node = case getLocation node of
  Nothing -> trace ("\n [compiler issue] no location for node " ++ prettyPrint node) Nothing
  Just r  -> Just r

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

inferBindGroup :: BindGroup -> Environment -> InferM (Bindings, Environment, Preds, [InstanceMethod])
inferBindGroup bg env = do
  let expls = explicitBindings bg
  explicitBindingTypes <- getExplicitTypes expls

  let env1 = envUnion explicitBindingTypes env
  let impls = implicitBindings bg

  (decls1, env2, ps1) <- inferGroups env1 impls
  let (bindings1, _) = toBindings decls1
  let env' = envUnion env2 env1
  decls2 <- tiExpls expls env'
  let (bindings2, ps2) = toBindings decls2

  let instMethods = instanceMethods bg
  typedMethods <- mapM (tiInstanceMethod env) instMethods

  -- ps1 and ps2 are _deferred_ predicates
  return (Bindings $ bindings1 ++ bindings2, env', ps1 ++ ps2, typedMethods)


toBindings :: TypedDecls -> ([Binding], Preds)
toBindings = foldl extractPreds ([], [])
  where extractPreds (binds, preds) (name, decl, ps) =
          ((name, decl) : binds, ps ++ preds)

tiInstanceMethod :: Environment -> InstanceMethod -> InferM InstanceMethod
tiInstanceMethod env instMethod = do
  let name = imName instMethod
  -- Get the declared type
  sch <- createInstMethodScheme instMethod
  (t, preds) <- freshInst sch

  -- Typecheck it
  let decl = imBody instMethod
  (resultDecl, resultPreds) <- inferDecl env decl

  -- Unify that type with the declared type (which may narrow it)
  let resultType = unqualify $ getType resultDecl
  withLocations [decl] $ unify t resultType

  -- Update the declared type with the current substitution
  preds' <- applyCurrentSub preds
  t' <- applyCurrentSub t
  env' <- applyCurrentSub env

  -- Figure out the variables that are _now_ free in that type
  let varsFreeInEnv = freeTypeVars env'
  let varsInT = freeTypeVars t'
  let varsFreeInT = Set.difference varsInT varsFreeInEnv

  -- Create an updated version of sch'
  let sch' = quantify varsFreeInT (Qual preds' t')

  -- Find predicates in the result type that aren't entailed by the predicates
  -- in the declared type
  ce <- getClassEnv
  resultPreds' <- applyCurrentSub resultPreds
  let newPredicates = filter (not . entail ce preds') resultPreds'

  unless (schemesEquivalent sch' sch) $
    inferErrFor [decl] $ BindingTooGeneral $ name ++ ": " ++ show (sch', sch)

  unless (null newPredicates) $
    inferErrFor [decl] $ ContextTooWeak name

  return instMethod { imBody = resultDecl }

createInstMethodScheme :: InstanceMethod -> InferM Scheme
createInstMethodScheme instMethod = do
  let idef = imInstanceDef instMethod
  let className = idClass idef
  -- Find the type of the instance (e.g. `Bool` in `instance Show Bool`)
  let tdef = idType idef
  let tdecl = AST.typeDefToDecl tdef
  let typeVars = AST.gatherTypeVars tdecl
  instGMap <- genericMap (Set.toList typeVars)
  (instType, ps) <- withLocations [imBody instMethod] $
    typeFromDecl instGMap tdecl
  unless (null ps) (error "the instance type predicates shouldn't be there")
  instPreds <- mapM (predFromAST instGMap) (idPreds idef)

  -- Find the type of the method
  let typeDecl = imMethodType instMethod
  -- Handle generics (other than Self) within the type the class declares for
  -- the method
  -- e.g. `b` in `first(Pair(Self, b)) Self`
  let tVars = AST.gatherTypeVars typeDecl
  methodGMap <- genericMap (Set.toList tVars)
  -- In addition to making making fresh type vars for the declared type vars,
  -- also replace the `Self` type with the instance's type
  let methodGMap' = Map.insert "Self" instType methodGMap
  -- Convert the type
  (methodType, methodPreds) <- typeFromDecl methodGMap' typeDecl
  let classPred = Pred className instType
  let qual = Qual (classPred : instPreds ++ methodPreds) methodType
  -- Quantify it
  let tvs = freeTypeVars qual
  let sch = quantify tvs qual

  return sch


-- TODO: The predicates in TypedDecls are actually the deferred predicates, so
-- the structure doesn't really make sense, but it works for now
-- TODO: Can this be just mapM (uncurry $ tiExpl env)?
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
  let sch = mustLookupEnv name env
  (t, qs) <- freshInst sch

  (d, ps) <- inferDecl env decl
  let dt = unqualify $ getType d
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
  return $ makeEnv typed


getExplicitType :: (name, DeclarationT) -> InferM (name, Scheme)
getExplicitType (name, decl) = case decl of
  AST.DLet _ _ mtdecl _ -> do
    let (Just tdecl) = mtdecl
    (t, ps) <- withLocations [decl] $ typeFromDecl Map.empty tdecl
    return (name, Scheme [] $ Qual ps t)

  AST.DFunction _ _ mgendecl _ _ -> do
    let Just tdecl = mgendecl
    gmap <- genericMap $ Set.toList $ AST.gatherTypeVars tdecl
    (t, ps) <- withLocations [decl] $ typeFromDecl gmap tdecl
    let varSet = freeTypeVars $ Map.elems gmap
    return (name, quantify varSet $ Qual ps t)

  AST.DTypeDef{} ->
    error "shouldn't see a typedef here"

  AST.DInstance{} ->
    error "shouldn't see an instance here"


genericMap :: [AST.Type] -> InferM (Map String Type)
genericMap tnames = do
  gens <- mapM (const newStarVar) tnames
  return $ Map.fromList $ zip tnames gens

inferGroups :: Environment -> [[(String, DeclarationT)]] ->
               InferM (TypedDecls, Environment, Preds)
inferGroups _   []     =
  return ([], Environment Map.empty, [])
inferGroups env (g:gs) = do
  (typed, env1, ps1) <- inferGroup env g
  (rest, env2, ps2) <- inferGroups (envUnion env1 env) gs
  return (typed ++ rest, envUnion env1 env2, ps1 ++ ps2)

inferGroup :: Environment -> [(String, DeclarationT)] ->
              InferM (TypedDecls, Environment, Preds)
inferGroup env impls = do
  -- Map each binding to a new type variable while recursively typing these bindings
  ts <- mapM (const newStarVar) impls
  let bindingNames = map fst impls
  let bindingSchemes = map asScheme ts
  let groupBindings = makeEnv $ zip bindingNames bindingSchemes
  let groupEnv = envUnion groupBindings env

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
  let resultEnv = makeEnv $ zip bindingNames schemes
  return (typedDecls, resultEnv, deferred)

{-
showTrace :: (Show a) => String -> a -> a
showTrace s a = trace (s ++ ": " ++ show a) a
-}

inferDecls :: Environment -> [(String, DeclarationT)] -> [Type] -> InferM TypedDecls
inferDecls env decls ts = mapM infer (zip decls ts)
  where infer ((name, decl), t) = do
          (d, ps) <- inferDecl env decl
          let (Qual _ t') = getType d
          withLocations [d] $ unify t t'
          return (name, d, ps)

generalize :: Environment -> QualType -> Scheme
generalize (Environment env) qt =
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
  AST.DLet a name mtype expr -> do
    (expr', t, ps) <- inferExpr env expr
    return (addType (Qual ps t) $ AST.DLet a name mtype expr', ps)

  AST.DFunction a name mtype args stmt -> do
    argTs <- mapM (const newStarVar) args
    retT <- newStarVar

    -- This case statement exists so that, by the time
    -- a field access is reached, the type of that variable
    -- will already be known.
    ps1 <- case envLookup name env of
      Nothing -> return []
      Just sc -> do
        (dt, ps) <- freshInst sc
        withLocations [stmt] $ unify dt (makeFuncType argTs retT)
        return ps

    let argEnv = makeEnv $ zip args (map asScheme argTs)
    let env' = envUnion argEnv env
    (stmt', stmtReturns, ps2) <- inferStmt env' stmt
    let funcReturns = toFunctionReturns stmtReturns
    withLocations [stmt] $ unifyAll retT funcReturns
    sub <- getSub
    let argTypes = map (apply sub) argTs
    let returnT = apply sub retT
    let t = makeFuncType argTypes returnT
    let preds = ps1 ++ ps2
    return (addType (Qual preds t) $ AST.DFunction a name mtype args stmt', preds)

  AST.DTypeDef{} ->
    inferErr $ CompilerBug "TypeDefs are not bindings"

  AST.DInstance{} ->
    inferErr $ CompilerBug "Instances are not treated as bindings"

inferStmt :: Environment -> StatementT ->
             InferM (StatementT, DoesReturn, Preds)
inferStmt env stmt = case stmt of
  AST.Return a Nothing ->
    return (AST.Return a Nothing, AlwaysReturns [tUnit], [])
  AST.Return a (Just expr) -> do
    (expr', _, ps) <- inferExpr env expr
    return (AST.Return a (Just expr'), AlwaysReturns [unqualify $ getType expr'], ps)

  AST.Let a name mtype expr -> do
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
        withLocations [stmt] $ unify (unqualify $ getType expr') t
        return ps
    return (AST.Let a name mtype expr', NeverReturns, ps1 ++ ps2)

  AST.Assign a names expr ->
    case names of
     []    -> inferErr $ CompilerBug "assignment to no names"
     [var] -> do
       -- Something to think about: Should this be required to be a variable
       -- defined in a `let` statement instead of an argument to a function?
       (expr', exprT, ps) <- inferExpr env expr
       sch <- withLocations [stmt] $ lookupName var env
       (varT, ps2) <- freshInst sch

       -- Require that the expression's type match - not unify with - the
       -- variable's type, because the assignment shouldn't be able to narrow
       -- the type of the variable
       exprTSubbed <- applyCurrentSub exprT
       varTSubbed <- applyCurrentSub varT
       sub <- case matches exprTSubbed varTSubbed of
         Nothing ->
           withLocations [stmt] $ inferErr $ WrongType exprTSubbed var
         Just s  ->
           return s
       extendSub sub

       exprT' <- applyCurrentSub exprTSubbed
       env' <- applyCurrentSub env
       let exprScheme = generalize env' (Qual [] exprT')

       -- Ensure that the exprssion's type is polymorphic in the same ways that
       -- the variable's type was.
       unless (schemeIsAtLeastAsGeneral exprScheme sch) $
         withLocations [stmt] $ inferErr $
         AssignmentInsufficientlyGeneral varTSubbed exprT' var

       return (AST.Assign a names expr', NeverReturns, ps ++ ps2)
     (var:fields) -> do
       (expr', exprT, ps1) <- inferExpr env expr

       sch <- withLocations [stmt] $ lookupName var env
       (varT, ps2) <- freshInst sch
       fieldT <- withLocations [stmt] $ getStructField varT fields

       exprTSubbed <- applyCurrentSub exprT
       fieldTSubbed <- applyCurrentSub fieldT
       sub <- case matches exprTSubbed fieldTSubbed of
         Nothing ->
           withLocations [stmt] $ inferErr $ WrongType exprTSubbed var
         Just s ->
           return s
       extendSub sub

       exprT' <- applyCurrentSub exprTSubbed
       env' <- applyCurrentSub env
       let exprScheme = generalize env' (Qual [] exprT')

       unless (schemeIsAtLeastAsGeneral exprScheme sch) $
         withLocations [stmt] $ inferErr $
         AssignmentInsufficientlyGeneral fieldTSubbed exprT' var

       return (AST.Assign a names expr', NeverReturns, ps1 ++ ps2)

  AST.Block a stmts -> do
    (stmts', retT, ps) <- inferBlock env stmts
    return (AST.Block a stmts', retT, ps)

  AST.Expr a expr -> do
    (expr', _, ps) <- inferExpr env expr
    return (AST.Expr a expr', NeverReturns, ps)

  AST.If a test blk els -> do
    (testExpr, _, ps1) <- inferExpr env test
    withLocations [testExpr] $ unify (unqualify $ getType testExpr) tBool
    (blk', blkReturns, ps2) <- inferBlock env blk
    (els', elsReturns, ps3) <- case els of
      Nothing ->
        return (Nothing, NeverReturns, [])
      Just st -> do
        (stmt', retT, ps) <- inferStmt env st
        return (Just stmt', retT, ps)
    let ifReturns = combineReturns blkReturns elsReturns
    return (AST.If a testExpr blk' els', ifReturns, ps1 ++ ps2 ++ ps3)

  AST.While a test blk -> do
    (testExpr, _, ps1) <- inferExpr env test
    withLocations [testExpr] $ unify (unqualify $ getType testExpr) tBool
    (blk', blkReturns, ps2) <- inferBlock env blk
    let whileReturns = demoteReturns blkReturns
    return (AST.While a testExpr blk', whileReturns, ps1 ++ ps2)

  AST.Match a expr cases -> do
    (expr', exprType, ps) <- inferExpr env expr
    casesAndReturns <- mapM (inferMatchCase env exprType) cases
    let (cases', returns, pss) = unzip3 casesAndReturns
    let resultType = getType (head cases')
    let result = addType resultType $ AST.Match a expr' cases'
    return (result, foldl1 combineReturns returns, concat (ps : pss))

  AST.Pass a -> do
    let result = addType (qualify tUnit) $ AST.Pass a
    return (result, NeverReturns, [])


inferMatchCase :: Environment -> Type -> MatchCaseT
               -> InferM (MatchCaseT, DoesReturn, Preds)
inferMatchCase env t (AST.MatchCase matchExpr stmt) = do
  (matchExpr', matchedVars, ps1) <- inferMatchExpr env t matchExpr
  let env' = insertAll matchedVars env
  (stmt', doesReturn, ps2) <- inferStmt env' stmt
  return (AST.MatchCase matchExpr' stmt', doesReturn, ps1 ++ ps2)


inferMatchExpr :: Environment -> Type -> MatchExpressionT
               -> InferM (MatchExpressionT, [(String, Scheme)], Preds)
inferMatchExpr env targetType matchExpr = case matchExpr of
  AST.MatchAnything a ->
    return (addType (qualify targetType) $ AST.MatchAnything a, [], [])

  AST.MatchVariable a name -> do
    let sch = Scheme [] (Qual [] targetType)
    let bindings' = [(name, sch)]
    return (addType (qualify targetType) $ AST.MatchVariable a name, bindings', [])

  AST.MatchStructure a enumName fields -> do
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
    let preds = ps ++ ps2
    let structType = Qual preds $ apply sub2 structT
    return (addType structType $ AST.MatchStructure a enumName typedFields, binds, preds)


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
  sub <- getSub
  let env' = case s' of
        AST.Let _ name _ expr ->
          let exprT = apply sub  $ getType expr
              sch = generalize env exprT
          in envInsert name sch env
        _                   -> env
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
    (fieldFn, ps) <- freshInst fieldSch
    when (ps /= []) $
      inferErr $ CompilerBug $ "expected no predicates from a struct type, got " ++ show ps ++ " from " ++ structName

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
  AST.Paren a e -> do
    (e', t, ps) <- inferExpr env e
    return (addType (Qual ps t) $ AST.Paren a e', t, ps)

  AST.Val a val -> do
    (val', t, ps) <- inferValue env val
    return (addType (Qual ps t) $ AST.Val a val', t, ps)

  AST.Unary a op exp -> do
    resultT <- newStarVar
    (exp', expT, ps) <- inferExpr env exp
    let fnT = getUnaryFnType op
    withLocations [expr] $ unify fnT (makeFuncType [expT] resultT)
    sub <- getSub
    let t = apply sub resultT
    return (addType (Qual ps t) $ AST.Unary a op exp', t, ps)

  AST.Binary a op l r -> do
    resultT <- newStarVar
    (l', lt, ps1) <- inferExpr env l
    (r', rt, ps2) <- inferExpr env r
    (fnT, ps3) <- getBinaryFnType op
    withLocations [expr] $ unify fnT (makeFuncType [lt, rt] resultT)
    sub <- getSub
    let t = apply sub resultT
    let preds = ps1 ++ ps2 ++ ps3
    return (addType (Qual preds t) $ AST.Binary a op l' r', t, preds)

  AST.Call a fexp args -> do
    resultT <- newStarVar
    (fexp', ft, ps) <- inferExpr env fexp
    typedArgs <- mapM (inferExpr env) args
    let (args', argTs, pss) = unzip3 typedArgs
    withLocations [expr] $ unify ft (makeFuncType argTs resultT)
    sub <- getSub
    let t = apply sub resultT
    let preds = concat $ ps : pss
    return (addType (Qual preds t) $ AST.Call a fexp' args', t, preds)

  AST.Cast a t exp -> do
    (exp', expT, ps) <- inferExpr env exp
    t' <- withLocations [expr] $ attemptCast t expT
    return (addType (Qual ps t') $ AST.Cast a t exp', t', ps)

  AST.Var a name -> do
    sch <- withLocations [expr] $ lookupName name env
    (t, ps) <- freshInst sch
    return (addType (Qual ps t) $ AST.Var a name, t, ps)

  AST.Access a exp field -> do
    (exp', et, ps) <- inferExpr env exp
    sub <- getSub
    let etype = apply sub et
    t <- withLocations [expr] $ getStructFieldType etype field
    return (addType (Qual ps t) $ AST.Access a exp' field, t, ps)

  AST.Lambda a args stmt -> do
    argTs <- mapM (const newStarVar) args
    retT <- newStarVar

    -- add args to the env
    let argEnv = makeEnv $ zip args (map asScheme argTs)
    let env' = envUnion argEnv env

    -- infer the type of the body of the lambda
    (stmt', stmtReturns, ps) <- inferStmt env' stmt

    -- enforce that all return statements in the lambda return the same type
    let funcReturns = toFunctionReturns stmtReturns
    withLocations [stmt] $ unifyAll retT funcReturns

    -- assemble the resulting type
    let t = makeFuncType argTs retT
    t' <- applyCurrentSub t
    ps' <- applyCurrentSub ps
    let q = Qual ps' t'

    -- return the typed lambda
    let typedLambda = addType q $ AST.Lambda a args stmt'
    return (typedLambda, t', ps')

inferValue :: Environment -> ValueT -> InferM (ValueT, Type,  Preds)
inferValue env val = case val of
  AST.StrVal a str ->
    return (addType (qualify tString) $ AST.StrVal a str, tString, [])

  AST.BoolVal a b ->
    return (addType (qualify tBool) $ AST.BoolVal a b, tBool, [])

  AST.IntVal a i -> do
    t <- newTypeVar Star
    let preds = [Pred "Num" t]
    return (addType (Qual preds tInt) $ AST.IntVal a i, t, preds)

  AST.FloatVal a f ->
    return (addType (qualify tFloat) $ AST.FloatVal a f, tFloat, [])

  AST.StructVal a tname fields  -> do
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

    return (addType (qualify t) $ AST.StructVal a tname orderedFields, t, concat (ps : pss))

-- Check if the scheme `moreGeneral` is at least as general as `lessGeneral`,
-- meaning that `moreGeneral` has generic variables in all the places that
-- `lessGeneral` does, and those variables are distinct if they are distinct in
-- `lessGeneral`.
schemeIsAtLeastAsGeneral :: Scheme -> Scheme -> Bool
schemeIsAtLeastAsGeneral moreGeneral lessGeneral =
  let (Scheme _ (Qual _ mgt)) = moreGeneral
      (Scheme _ (Qual _ lgt)) = lessGeneral
  in isJust $ matchGenerality mgt lgt

matchGenerality :: Type -> Type -> Maybe Substitution
matchGenerality mgt lgt = case lgt of
  (TGen _ _) -> case mgt of
    (TGen _ _) ->
      return $ Map.singleton mgt lgt
    _          ->
      Nothing
  (TAp  la lb) -> case mgt of
    (TAp ma mb) -> do
      subA <- matchGenerality ma la
      subB <- matchGenerality mb lb
      merge subA subB
    _           ->
      Nothing
  _            ->
    return Map.empty

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
  AST.TName _ name ->
    case Map.lookup name gmap of
      Nothing -> typeFromName name
      Just t  -> return (t, [])
  AST.TGeneric _ name []      ->
    typeFromName name
  AST.TGeneric _ name genArgs -> do
    genTypesAndPreds <- mapM (typeFromDecl gmap) genArgs
    let (genTypes, genPreds) = unzip genTypesAndPreds
    (t, ps) <- typeFromName name
    case t of
      TAp _ _ -> do
        unify t (applyTypes (getRoot t) genTypes)
        t' <- applyCurrentSub t
        return (t', ps ++ concat genPreds)
      _ -> error $ "compiler bug, unexpected type " ++ show t
  AST.TFunction _ predicates argTs retT -> do
    argTypes <- mapM (typeFromDecl gmap) argTs
    retType <- typeFromDecl gmap retT
    preds <- mapM (predFromAST gmap) predicates
    let innerPreds = concatMap snd (retType : argTypes)
    case innerPreds of
      (_:_) ->
        error $ "expected no inner predicates here, got " ++ show innerPreds
      []    ->
        return (makeFuncType (map fst argTypes) (fst retType), preds)
  AST.TStruct{} ->
    error "shouldn't see a Struct here"
  AST.TEnum{} ->
    error "shouldn't see an Enum here"
  AST.TClassDecl{} ->
    error "shouldn't see an Class here"

predFromAST :: Map String Type -> AST.Predicate a -> InferM Predicate
predFromAST gmap tpred = do
  let name = AST.predType tpred
  (typ, ps) <- typeFromDecl gmap (AST.TName [] name)
  case ps of
    [] -> return $ Pred (AST.predClass tpred) typ
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
lookupName name env = case envLookup name env of
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

  (TFunc n1 k1, TFunc n2 k2) ->
    if n1 == n2 && k1 == k2
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

getType :: (Annotated a) => a Annotation -> QualType
getType node =
  fromMaybe (error "must be typed") (Anno.getType node)

mustLookupEnv :: String -> Environment -> Scheme
mustLookupEnv name env =
  let err = error $ "no variable bound for " ++ name
  in fromMaybe err $ envLookup name env

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

insertAll :: [(String, Scheme)] -> Environment -> Environment
insertAll ((k, v):kvs) env =
  insertAll kvs $ envInsert k v env
insertAll []           env =
  env

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
