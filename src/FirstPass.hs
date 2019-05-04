module FirstPass where

import Control.Monad (foldM, when, unless)
import Data.Foldable (forM_)
import Data.List (nub)
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
import Types (Scheme(..), Type(..), Substitution, makeSub)
import Util.Functions


type DeclarationT     = D.Declaration     Annotation
type ExpressionT      = E.Expression      Annotation
type FileT            = D.File            Annotation
type MatchCaseT       = S.MatchCase       Annotation
type MatchExpressionT = S.MatchExpression Annotation
type StatementT       = S.Statement       Annotation
type TypeDeclT        = T.TypeDecl        Annotation
type TypeDefT         = T.TypeDef         Annotation


data Module =
  Module
  { bindings :: Map String DeclarationT
  , constructors :: Map String Constructor
  }
  deriving (Show)

data Constructor =
  Constructor
  { ctorFields :: [(String, Scheme)]
  , ctorType :: Scheme
  }
  deriving (Eq, Show)


valueType :: Constructor -> Scheme
valueType ctor =
  let (Scheme n (TFunc _ ret)) = ctorType ctor
  in Scheme n ret

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
  checkGenericUsage (gatherTypes file)

  ctors <- gatherConstructors file
  checkTypesExist ctors

  uniqueDecls <- ensureDeclsAreUnique file
  binds <- gatherBindings uniqueDecls
  mapM_ checkReturns binds
  mapM_ checkDupVars binds

  return Module { bindings=binds, constructors=ctors }

-- gatherTypes selects the declarations that are type declarations
gatherTypes :: FileT -> [(TypeDefT, TypeDeclT)]
gatherTypes file =
  [(def, decl) | (TypeDef _ def decl) <- file]

-- and don't use generics not declared for the type
checkGenericUsage :: [(TypeDefT, TypeDeclT)] -> Result ()
checkGenericUsage decls = do
  mapM_ (checkGenNames . fst) decls
  mapM_ (uncurry checkGenDefined) decls

-- checkGenNames ensures that the type parameters used for generics
-- are valid (unique and lower case)
checkGenNames :: TypeDefT -> Result ()
checkGenNames def = do
  let generics = defGenerics def
  when (nub generics /= generics)
    (withLocations [def] $ Left $ DuplicateBinding $ show def)
  mapM_ (checkGenName def) generics

checkGenName :: TypeDefT -> T.Type -> Result ()
checkGenName def name =
  unless (T.isTypeVar name)
    (withLocations [def] $ Left $ InvalidGenericParam name)

-- checkGenDefined ensures that a top-level type declaration uses
-- only the generics it defines for itself.
checkGenDefined :: TypeDefT -> TypeDeclT -> Result ()
checkGenDefined def decl = case decl of
  T.TypeName _ typ        ->
    checkGenTypeDefined def typ
  T.Generic _ typ decls   -> do
    checkGenTypeDefined def typ
    mapM_ (checkGenDefined def) decls
  T.Function _ targs tret -> do
    mapM_ (checkGenDefined def) targs
    checkGenDefined def tret
  T.Struct _ tdecls       ->
    mapM_ (checkGenDefined def . snd) tdecls
  T.Enum _ options        ->
    mapM_ (mapM_ (checkGenDefined def . snd) . snd) options

checkGenTypeDefined :: TypeDefT -> T.Type -> Result ()
checkGenTypeDefined def typ =
  when (T.isTypeVar typ && typ `notElem` defGenerics def)
    (withLocations [def] $ Left $ UndefinedGeneric typ)


type DeclMap = Map String DeclarationT

ensureDeclsAreUnique :: FileT -> Result DeclMap
ensureDeclsAreUnique [] = return Map.empty
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

getReferencedType :: (String, Scheme) -> [String]
getReferencedType (_, Scheme _ t) = filter (not . isBuiltIn) (getTypeNames t)

isBuiltIn :: String -> Bool
isBuiltIn t = t `elem` ["Int", "Float", "Bool", "Char", "String", "()"]

getTypeNames :: Type -> [String]
getTypeNames t = case t of
  TCon name ts -> name : concatMap getTypeNames ts
  TFunc at rt  -> concatMap getTypeNames at ++ getTypeNames rt
  TVar _       -> []
  TGen _       -> []

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
  let generalized = map TGen [1..nGens]
  let sub = makeSub $ zip (map TVar gens) generalized

  constrs' <- case d of
    T.TypeName{}         -> error "type aliases not supported yet"
    T.Generic{}          -> error "type aliases not supported yet"
    T.Function{}         -> error "type aliases not supported yet"

    T.Struct   _ fields  -> do
      let typ = TCon name generalized

      let fieldNames = map fst fields
      fieldTypes <- mapM (convertDecl sub . snd) fields

      let cf = zipWith (\fname ftype -> (fname, Scheme nGens (TFunc [typ] ftype))) fieldNames fieldTypes

      let sch = Scheme nGens (TFunc fieldTypes typ)
      let ctor = Constructor { ctorFields=cf, ctorType=sch }
      return $ Map.insert name ctor constrs

    -- An enum like `Maybe T = Just T | Nothing` would need three constructors
    -- One for finding the type of `Maybe<T>` in type declarations, and
    -- one for each of `Just{val: val}` and `Nothing{}` constructors in the code.
    T.Enum     _ options ->
      let typ = TCon name generalized
          sch = Scheme nGens (TFunc [] typ)
          ctor = Constructor { ctorFields=[], ctorType=sch }
      in addEnumOptions nGens generalized sub typ options (Map.insert name ctor constrs)
  makeConstructors ts constrs'

mustBeUnique :: String -> Map String b -> Result ()
mustBeUnique key m =
  when (Map.member key m) $ duplicateName key


createStructFields :: Int -> Type -> [String] -> [Type] -> [(String, Scheme)]
createStructFields nGens typ = zipWith makePair
  where makePair fname ftype = (fname, Scheme nGens (TFunc [typ] ftype))


addEnumOptions :: Int -> t -> Substitution -> Type -> [(String, [(String, TypeDeclT)])]  ->
  Map String Constructor -> Either Error (Map String Constructor)
addEnumOptions _     _           _   _   []         constrs = return constrs
addEnumOptions nGens generalized sub typ ((n,t):os) constrs = do
  mustBeUnique n constrs

  let fields = t
  let fieldNames = map fst fields
  fieldTypes <- mapM (convertDecl sub . snd) fields
  let cf = zipWith (\fn ft -> (fn, Scheme nGens (TFunc [typ] ft))) fieldNames fieldTypes

  let sch = Scheme nGens (TFunc fieldTypes typ)
  let ctor = Constructor { ctorFields=cf, ctorType=sch }
  addEnumOptions nGens generalized sub typ os (Map.insert n ctor constrs)


convertDecl :: Substitution -> TypeDeclT -> Result Type
convertDecl sub decl = case decl of
  T.TypeName _ tname      ->
    return $ fromMaybe (TCon tname []) $ Map.lookup (TVar tname) sub
  T.Generic  _ tname gens -> do
    genTypes <- mapM (convertDecl sub) gens
    return $ TCon tname genTypes
  T.Function _ argTs retT -> do
    argTypes <- mapM (convertDecl sub) argTs
    retType <- convertDecl sub retT
    return $ TFunc argTypes retType
  T.Struct{}              ->
    withLocations [decl] $ Left InvalidAnonStructure
  T.Enum{}                ->
    withLocations [decl] $ Left InvalidAnonStructure

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
checkReturns (Function _ name _ _ stmt) = do
  _ <- checkStmtsReturn name Never [stmt]
  return ()

checkDupVars :: DeclarationT -> Result ()
checkDupVars decl = case decl of
  TypeDef{}          -> return ()
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
