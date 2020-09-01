module FirstPass where

import Control.Monad (foldM, when, unless)
import Data.Foldable (forM_)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (mapMaybe, fromMaybe)

import AST.Annotation (Annotated, Annotation, getLocation)
import AST.Declaration
  ( Declaration(..)
  , getDeclaredName )
import qualified AST.Declaration as D
import qualified AST.Expression as E
import qualified AST.Statement as S
import AST.Type ( defName, defGenerics )
import qualified AST.Type as T
import Errors
  ( Error(..)
  , Result )
import Types
  ( Scheme(..)
  , Type(..)
  , Kind(..)
  , Substitution
  , Qualified(..)
  , Predicate(..)
  , Inst
  , Class(..)
  , ClassEnv(..)
  , Environment(..)
  , envInsert
  , simpleType
  , simpleVar
  , makeSub
  , makeFuncType
  , makeVar
  , applyTypes
  , kindN
  , tInt
  , tFloat
  , tBool
  , tChar
  , tString
  , tUnit )
import Util.Functions


type DeclarationT     = D.Declaration     Annotation
type ExpressionT      = E.Expression      Annotation
type FileT            = D.File            Annotation
type MatchCaseT       = S.MatchCase       Annotation
type MatchExpressionT = S.MatchExpression Annotation
type StatementT       = S.Statement       Annotation
type TypeDeclT        = T.TypeDecl        Annotation
type TypeDefT         = T.TypeDef         Annotation
type FuncTypeT        = T.FuncType        Annotation
type ClassMethodT     = T.ClassMethod     Annotation
type PredicateT       = T.Predicate       Annotation


data Module =
  Module
  { bindings :: Map String DeclarationT
  , constructors :: Map String Constructor
  , classEnv :: ClassEnv
  -- rootEnv contains names that are assumed to be defined when type inference runs
  , rootEnv :: Environment
  }
  deriving (Show)

data Constructor =
  Constructor
  { ctorFields :: [(String, Scheme)]
  , ctorType :: Scheme -- type of the constructor
  }
  deriving (Eq, Show)


valueType :: Constructor -> Scheme
valueType ctor =
  let (Scheme ks (Qual ps (TAp _ ret))) = ctorType ctor
  in Scheme ks (Qual ps ret)

-- firstPass is the first thing run after parsing.
-- It prepares data for type inference.
--
-- Among other things, this:
-- * Checks for duplicate bindings
-- * Splits types from other bindings
-- * (TODO) Check that match statements have at least one case
-- * (TODO) Check that match cases do not completely overlap
firstPass :: FileT -> Result Module
firstPass file = do
  ctors <- gatherConstructors file
  checkTypesExist ctors

  uniqueDecls <- ensureDeclsAreUnique file
  binds <- gatherBindings uniqueDecls
  mapM_ checkReturns binds
  mapM_ checkDupVars binds

  let ce = startingClassEnv
  ce' <- foldM addClass ce (gatherClasses file)
  ce'' <- foldM addInstance ce' (gatherInstances file)

  let environment = foldl addMethods startingEnv (gatherClasses file)

  return Module
    { bindings=binds
    , constructors=ctors
    , classEnv=ce''
    , rootEnv=environment }


-- TODO: this should also start with prelude and imported names
startingEnv :: Environment
startingEnv =
  Environment $
  Map.fromList
  [ ("print", Scheme [Star] (Qual [] $ makeFuncType [TGen 0 Star] tUnit)) ]

selfType :: Type
selfType = simpleVar "Self"

addMethods :: Environment -> ClassDefinition -> Environment
addMethods e cd =
  let classPred = Pred (cdName cd) selfType
  in foldl (addMethod classPred) e (cdMethods cd)

addMethod :: Predicate -> Environment -> ClassMethodT -> Environment
addMethod p env (T.ClassMethod _ name funcType)  =
  let sch = convertMethodType p funcType
  in envInsert name sch env

convertMethodType :: Predicate -> FuncTypeT -> Scheme
convertMethodType p (gens, tdecl) =
  let gensWithSelf = "Self" : gens
      kinds = replicate (length gensWithSelf) Star
      (predicates, args, ret) = case tdecl of
        (T.Function _ ps a r) -> (ps, a, r)
        _                     -> error "compiler bug: should only see a function type"
      preds = p : map convertPredicate predicates
      fn = T.Function [] [] args ret
      -- annoying, but necessary to avoid creating a `TCon "Self"`:
      sub = Map.singleton selfType selfType
      typ = case convertDecl sub fn of
        Right t  -> t
        Left err -> error $ "unexpected error converting type: " ++ show err
  in Scheme kinds $ Qual preds typ

convertPredicate :: PredicateT -> Predicate
convertPredicate pt =
  Pred (T.predClass pt) $ simpleType (T.predType pt)

makeInst :: String -> Type -> Inst
makeInst c t = Qual [] (Pred c t)

{-
TODO:

Things to check for class declarations:

* Unique name (among classes and other types) - global
* All methods involve Self type at least once - done
* Methods don't use class's type besides as Self - done
* Superclasses exist - global
* Superclass hierarchy is acyclic - global
* Method names that overlap with other classes or other bindings - global
* Generics don't use Self as the variable

Things to check for instance declarations:

* Class exists (and type is a class)
* Unique type for instance - global
* Valid type for instance (not an alias)
* (later) Either the class or the type is declared in this module
* The only declarations inside the class are functions
* The methods match the class's methods (name, argument count, position of self)
* The methods don't have a declared type


Processing to do:

* Add classes to the class env
* Add instances to the class env
* Add methods from classes to the environment
* Convert instance methods to "hidden," explicitly typed bindings?


-}

startingClassEnv :: ClassEnv
startingClassEnv =
  let builtinClasses = Map.fromList
        [ ("Eq",   Class
                   { superclasses=[]
                   , instances=map (makeInst "Eq") [tUnit, tBool, tInt, tFloat, tChar, tString, tUnit] })
        , ("Ord",  Class
                   { superclasses=["Eq"]
                   , instances=map (makeInst "Ord") [tInt, tFloat, tChar, tString] })
        , ("Show", Class
                   { superclasses=[]
                   , instances=map (makeInst "Show") [tUnit, tInt, tFloat, tChar, tString, tUnit] })
        , ("Num",  Class
                   { superclasses=["Eq", "Show"]
                   , instances=map (makeInst "Num") [tInt, tFloat] })
        ]
  in ClassEnv
     { classes=builtinClasses,
       defaults=[tInt, tFloat] }


data ClassDefinition
  = ClassDefinition
    { cdName    :: String
    , cdSupers  :: [String]
    , cdMethods :: [ClassMethodT]
    }

data InstanceDefinition
  = InstanceDefinition
    { idType  :: TypeDefT
    , idClass :: String
    , idPreds :: [PredicateT]
    }

addClass :: ClassEnv -> ClassDefinition -> Result ClassEnv
addClass ce cdef = do
  checkClass cdef
  let name = cdName cdef
  let alreadyDefined = Map.member name (classes ce)
  when alreadyDefined $
    -- Improvement: Add location
    Left $ DuplicateClass name
  let cls = Class { superclasses=cdSupers cdef, instances=[] }
  let newClasses = Map.insert name cls (classes ce)
  return ce { classes=newClasses }

-- checkClass checks things that are local to this class definition:
--  * All methods involve Self type one or more times
--  * Method names are only used once within the class
--  * The class name is only used as Self
checkClass :: ClassDefinition -> Result ()
checkClass cdef = do
  let name = cdName cdef
  let methods = cdMethods cdef
  when ("Self" `elem` (cdSupers cdef)) $
    Left $ MalformedType ("the class " ++ name ++ " should not extend Self")
  ensureUniqueMethodNames name [n | T.ClassMethod _ n _ <- methods]
  mapM_ (checkMethodType name) methods

ensureUniqueMethodNames :: String -> [String] -> Result ()
ensureUniqueMethodNames className methodNames =
  let dups = duplicates methodNames
  in if null dups
     then return ()
     else Left $ MalformedType (
    "class " ++ className ++ " has multiple definitions for the method " ++ commaSep dups)

-- Checks that:
--  * the Self type is used at least once
--  * the class's name isn't otherwise used directly (except in predicates)
checkMethodType :: String -> ClassMethodT -> Result ()
checkMethodType className (T.ClassMethod _ name funcType) = do
  let (_generics, tdecl) = funcType
  (argTs, retT) <- case tdecl of
    T.Function _ _ps ats rt -> return (ats, rt)
    _                       -> error "compiler bug"
  let allTs = retT : argTs
  let namesUsed = getNamesUsed allTs

  let errPrefix = "method " ++ name ++ " in class " ++ className
  let selfErrMessage = errPrefix ++ " must refer to Self at least once"
  unless (Set.member "Self" namesUsed) $
    Left $ MalformedType $ selfErrMessage

  let classNameMessage = errPrefix ++ " should use Self to refer to the class " ++
        "instead of using the name directly"
  when (Set.member className namesUsed) $
    Left $ MalformedType $ classNameMessage

-- Get names of types, but not names in predicates
getNamesUsed :: [T.TypeDecl a] -> Set T.Type
getNamesUsed =
  foldl (\names decl -> Set.union names (getNames decl)) Set.empty

-- TODO: Should this consider non-generic names in the type part of a predicate?
getNames :: T.TypeDecl a -> Set T.Type
getNames tdecl = case tdecl of
  T.TypeName _ t        -> Set.singleton t
  -- TODO: Filter out generics from child:
  T.Generic _ _ tds     -> getNamesUsed tds
  T.Function _ _ args r -> getNamesUsed (r : args)
  T.Struct _ fields     -> getNamesUsed (map snd fields)
  T.Enum _ options      -> getNamesUsed (concatMap (map snd) $ map snd options)
  T.ClassDecl _ _ _     -> error "should not have a class decl here"

-- TODO: Extend this to check the validity of the instance
addInstance :: ClassEnv -> InstanceDefinition -> Result ClassEnv
addInstance ce idef = do
  let className = idClass idef
  cls <- case Map.lookup className (classes ce) of
    -- Improvement: Add location
    Nothing -> Left $ UndefinedClass className
    Just c  -> return c
  cls' <- insertInstance idef cls
  let newClasses = Map.insert className cls' (classes ce)
  return ce { classes=newClasses}

insertInstance :: InstanceDefinition -> Class -> Result Class
insertInstance idef cls = do
  typ <- convertDef $ idType idef
  let className = idClass idef
  let existingInstances = instances cls
  let inst = makeInst className typ
  when (hasMatchingInstance existingInstances inst) $
    Left $ DuplicateInstance className typ
  return cls { instances=inst:existingInstances }

hasMatchingInstance :: [Inst] -> Inst -> Bool
hasMatchingInstance existing inst =
  -- TODO: Should extend to more general 'unifies' logic
  inst `elem` existing

gatherClasses :: FileT -> [ClassDefinition]
gatherClasses declarations =
  [ ClassDefinition { cdName=getDeclaredName d, cdSupers=supers, cdMethods=methods }
  | d@(D.TypeDef _ _ (T.ClassDecl _ supers methods)) <- declarations ]

gatherInstances :: FileT -> [InstanceDefinition]
gatherInstances declarations =
  [ InstanceDefinition { idType=typ, idClass=cls, idPreds=preds }
  | D.Instance _ cls typ preds _ <- declarations ]

type DeclMap = Map String DeclarationT

ensureDeclsAreUnique :: FileT -> Result DeclMap
ensureDeclsAreUnique [] = return Map.empty
ensureDeclsAreUnique (Instance{}:ds) = ensureDeclsAreUnique ds
ensureDeclsAreUnique (d:ds) = do
  rest <- ensureDeclsAreUnique ds
  let name = getDeclaredName d
  case Map.lookup name rest of
    Nothing ->
      return $ Map.insert name d rest
    Just duplicate ->
      withLocations [d, duplicate] $ duplicateName name


checkTypesExist :: Map String Constructor -> Result ()
checkTypesExist ctors =
  mapM_ (checkRefedTypesExist ctors) (Map.toList ctors)

checkRefedTypesExist :: Map String Constructor -> (String, Constructor) -> Result ()
checkRefedTypesExist ctors (_, ctor) =
  mapM_ (checkTypeDefined ctors) (concatMap getReferencedType $ ctorFields ctor)

-- TODO: Make sure the classes in the predicates exist
getReferencedType :: (String, Scheme) -> [String]
getReferencedType (_, Scheme _ (Qual _ t)) = filter (not . isBuiltIn) (getTypeNames t)

isBuiltIn :: String -> Bool
isBuiltIn t = t `elem` ["Int", "Float", "Bool", "Char", "String", "()"]

getTypeNames :: Type -> [String]
getTypeNames t = case t of
  TCon name _ -> [name]
  TFunc _ _   -> []
  TAp a b     -> getTypeNames a ++ getTypeNames b
  TVar _      -> []
  TGen _ _    -> []

checkTypeDefined :: Map String a -> String -> Result ()
checkTypeDefined definitions name =
  unless (Map.member name definitions)
    (Left $ UndefinedType name)

gatherConstructors :: FileT -> Result (Map String Constructor)
gatherConstructors file =
  makeConstructors [(def, decl) | (TypeDef _ def decl) <- file] Map.empty

makeConstructors :: [(TypeDefT, TypeDeclT)] -> Map String Constructor -> Result (Map String Constructor)
makeConstructors []         constrs = return constrs
makeConstructors ((t,d):ts) constrs = do
  let name = defName t
  mustBeUnique name constrs

  let gens = defGenerics t
  let nGens = length gens
  -- Right now this doesn't support higher-kinded data types.  If it did, this
  -- list of kinds would have to be inferred by inspecting the data type.
  -- https://gitlab.haskell.org/ghc/ghc/wikis/ghc-kinds/kind-inference
  let kinds = replicate nGens Star
  let generalized = zipWith TGen [0..nGens-1] kinds
  let sub = makeSub $ zip (zipWith makeVar gens kinds) generalized

  constrs' <- case d of
    T.TypeName{}         -> error "type aliases not supported yet"
    T.Generic{}          -> error "type aliases not supported yet"
    T.Function{}         -> error "type aliases not supported yet"

    T.Struct   _ fields  -> do
      let k = kindN $ length generalized
      let typ = applyTypes (TCon name k) generalized

      let fieldNames = map fst fields
      fieldTypes <- mapM (convertDecl sub . snd) fields

      let cf = zipWith (\fname ftype -> (fname, Scheme kinds $ Qual [] (makeFuncType [typ] ftype))) fieldNames fieldTypes

      let sch = Scheme kinds (Qual [] $ makeFuncType fieldTypes typ)
      let ctor = Constructor { ctorFields=cf, ctorType=sch }
      return $ Map.insert name ctor constrs

    -- An enum like `Maybe T = Just T | Nothing` would need three constructors
    -- One for finding the type of `Maybe<T>` in type declarations, and
    -- one for each of `Just{val: val}` and `Nothing{}` constructors in the code.
    T.Enum     _ options ->
      let k = kindN $ length generalized
          typ = applyTypes (TCon name k) generalized
          sch = Scheme kinds (Qual [] $ makeFuncType [] typ)
          ctor = Constructor { ctorFields=[], ctorType=sch }
      in addEnumOptions kinds generalized sub typ options (Map.insert name ctor constrs)

    T.ClassDecl{}        ->
      -- Classes don't contribute constructors
      return constrs
  makeConstructors ts constrs'

mustBeUnique :: String -> Map String b -> Result ()
mustBeUnique key m =
  when (Map.member key m) $ duplicateName key

addEnumOptions :: [Kind] -> t -> Substitution -> Type -> [(String, [(String, TypeDeclT)])]  ->
  Map String Constructor -> Either Error (Map String Constructor)
addEnumOptions _     _           _   _   []         constrs = return constrs
addEnumOptions kinds generalized sub typ ((n,t):os) constrs = do
  mustBeUnique n constrs

  let fields = t
  let fieldNames = map fst fields
  fieldTypes <- mapM (convertDecl sub . snd) fields
  let cf = zipWith (\fn ft -> (fn, Scheme kinds (Qual [] $ makeFuncType [typ] ft))) fieldNames fieldTypes

  let sch = Scheme kinds (Qual [] $ makeFuncType fieldTypes typ)
  let ctor = Constructor { ctorFields=cf, ctorType=sch }
  addEnumOptions kinds generalized sub typ os (Map.insert n ctor constrs)


convertDecl :: Substitution -> TypeDeclT -> Result Type
convertDecl sub decl = case decl of
  T.TypeName _ tname      ->
    return $ fromMaybe (simpleType tname) $ Map.lookup (simpleVar tname) sub
  T.Generic  _ tname gens -> do
    genTypes <- mapM (convertDecl sub) gens
    let k = kindN $ length gens
    return $ applyTypes (TCon tname k) genTypes
  T.Function _ preds argTs retT -> do
    argTypes <- mapM (convertDecl sub) argTs
    retType <- convertDecl sub retT
    case preds of
      [] -> return $ makeFuncType argTypes retType
      ps -> error $ "didn't expect predicates here: " ++ show ps
  T.Struct{}              ->
    withLocations [decl] $ Left InvalidAnonStructure
  T.Enum{}                ->
    withLocations [decl] $ Left InvalidAnonStructure
  T.ClassDecl{}           ->
    withLocations [decl] $ Left InvalidAnonStructure

convertDef :: TypeDefT -> Result Type
convertDef (T.TypeDef _ name gens) = do
  let dups = duplicates gens
  unless (null dups) $
    Left $ MalformedType $ "type variable used twice: " ++ show dups
  let k = kindN $ length gens
  let genTypes = map simpleType gens
  return $ applyTypes (TCon name k) genTypes

-- select and deduplicate function and let bindings
gatherBindings :: DeclMap -> Result (Map String DeclarationT)
gatherBindings decls =
  let binds = Map.filter (not . isTypeDecl) decls
      addBinding bs decl =
        return $ Map.insert (getDeclaredName decl) decl bs
  in foldM addBinding Map.empty binds

checkReturns :: DeclarationT -> Result ()
checkReturns TypeDef{} =
  return ()
checkReturns Let{} =
  return ()
checkReturns Instance{} =
  return ()
checkReturns (Function _ name _ _ stmt) = do
  _ <- checkStmtsReturn name Never [stmt]
  return ()

checkDupVars :: DeclarationT -> Result ()
checkDupVars decl = case decl of
  TypeDef{}          -> return ()
  Instance{}         -> return ()
  Let _ _ _ e        -> checkDupVarsExpr e
  Function _ _ _ _ s -> checkDupVarsStmt s


checkDupVarsStmt :: StatementT -> Result ()
checkDupVarsStmt stmt = case stmt of
  S.Return _ me      -> forM_ me checkDupVarsExpr
  S.Let _ _ _ e      -> checkDupVarsExpr e
  S.Assign _ _ e     -> checkDupVarsExpr e
  S.Block _ stmts    -> checkDupVarsBlock stmts Set.empty
  S.Expr _ e         -> checkDupVarsExpr e
  S.If _ e stmts els -> do
    checkDupVarsExpr e
    checkDupVarsBlock stmts Set.empty
    forM_ els checkDupVarsStmt
  S.While _ e stmts  -> do
    checkDupVarsExpr e
    checkDupVarsBlock stmts Set.empty
  S.Match _ e cases  -> do
    checkDupVarsExpr e
    mapM_ checkDupVarsCase cases
  S.Pass _           -> return ()


checkDupVarsBlock :: [StatementT] -> Set String -> Result ()
checkDupVarsBlock [] _ = return ()
checkDupVarsBlock (s:ss) declared = do
  checkDupVarsStmt s
  case s of
    S.Let _ name _ _ ->
      if Set.member name declared
      then withLocations [s] $ Left $ DuplicateBinding name
      else checkDupVarsBlock ss (Set.insert name declared)
    _ ->
      return ()

checkDupVarsCase :: MatchCaseT -> Result ()
checkDupVarsCase (S.MatchCase me st) = do
  checkDupVarsME me
  checkDupVarsStmt st

checkDupVarsME :: MatchExpressionT -> Result ()
checkDupVarsME me =
  checkDuplicateBindings (gatherMEBindings me) Set.empty

gatherMEBindings :: MatchExpressionT -> [(String, MatchExpressionT)]
gatherMEBindings me = case me of
  S.MatchAnything _        -> []
  S.MatchVariable _ s      -> [(s, me)]
  S.MatchStructure _ _ mes ->
    concatMap gatherMEBindings mes

checkDuplicateBindings :: (Annotated ast) => [(String, ast Annotation)] -> Set String -> Result ()
checkDuplicateBindings []                  _        =
  return ()
checkDuplicateBindings ((name, node):rest) declared =
  if Set.member name declared
  then withLocations [node] $ Left $ DuplicateBinding name
  else checkDuplicateBindings rest (Set.insert name declared)


-- Update this once closures exist
checkDupVarsExpr :: ExpressionT -> Result ()
checkDupVarsExpr _ = return ()


checkStmtsReturn :: String -> DoesReturn -> [StatementT] -> Result DoesReturn
checkStmtsReturn fname prevReturns stmts =
  case prevReturns of
   Always -> case stmts of
     []    -> return Always
     (s:_) -> withLocations [s] $ Left $ Unreachable fname
   _ -> case stmts of
     []     -> return prevReturns
     (s:ss) -> case s of
       S.Return _ _ ->
         checkStmtsReturn fname Always ss
       S.Block _ blk -> do
         returns <- checkStmtsReturn fname prevReturns blk
         checkStmtsReturn fname returns ss
       S.If _ _ thenCase Nothing -> do
         returns <- checkStmtsReturn fname prevReturns thenCase
         let actuallyReturns = if returns == Always then Sometimes else returns
         checkStmtsReturn fname actuallyReturns ss
       S.If _ _ thenCase (Just elseCase) -> do
         thenReturns <- checkStmtsReturn fname prevReturns thenCase
         elseReturns <- checkStmtsReturn fname prevReturns [elseCase]
         let actuallyReturns = case (thenReturns, elseReturns) of
               (Always, Always) -> Always
               (Never,  Never)  -> Never
               (_,      _)      -> Sometimes
         checkStmtsReturn fname actuallyReturns ss
       S.While _ _ whileBody -> do
         whileReturns <- checkStmtsReturn fname prevReturns whileBody
         let actuallyReturns = if whileReturns == Always then Sometimes else whileReturns
         checkStmtsReturn fname actuallyReturns ss
       _ ->
         checkStmtsReturn fname prevReturns ss

data DoesReturn
  = Never
  | Sometimes
  | Always
  deriving (Eq, Show)

isTypeDecl :: DeclarationT -> Bool
isTypeDecl TypeDef{} = True
isTypeDecl _         = False

-- TODO: add the ability to combine multiple files into a module,
--   but check imports on a per-file basis

duplicateName :: String -> Result a
duplicateName name = Left $ DuplicateBinding name

withLocations :: (Annotated a) => [a Annotation] -> Result b -> Result b
withLocations code =
  mapLeft (WithLocations (mapMaybe getLocation code))
