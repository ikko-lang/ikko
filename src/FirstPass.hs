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
import Types (Kind(..), Scheme(..), Type(..), Substitution, makeSub, kindN)
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
  -- ctorFields is the list of fields, each containing a namd the type of the
  -- function to access that field.
  { ctorFields :: [(String, Scheme)]
  -- ctorType is a function type used to construct (or destruct) the structure.
  , ctorType :: Scheme
  -- ctorValue is a constructor type. This is used when dealing with the type in
  -- type applications. The type may need to be applied to other types, in which
  -- case the kind will be something like * -> *.
  , ctorValue :: Scheme
  }
  deriving (Eq, Show)

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

  return Module { bindings=binds, constructors=ctors }


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
  TCon name ts _ -> name : concatMap getTypeNames ts
  TFunc at rt _  -> concatMap getTypeNames at ++ getTypeNames rt
  TVar _ _       -> []
  TGen _         -> []

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
  let kinds = map (const Star) generalized
  let sub = makeSub $ zip (zipWith TVar gens kinds) generalized

  constrs' <- case d of
    T.TypeName{}         -> error "type aliases not supported yet"
    T.Generic{}          -> error "type aliases not supported yet"
    T.Function{}         -> error "type aliases not supported yet"

    T.Struct   _ fields  -> do
      -- To support advanced kinds, this type's kind might need to be some KFun,
      -- depending on how the type is used in the definition.  (E.g. if it is T
      -- and is used as T<Int, Int>)
      let typ = TCon name generalized Star
      let k = kindN nGens
      let valT = Scheme kinds $ TCon name [] k

      let fieldNames = map fst fields
      fieldTypes <- mapM (convertDecl sub . snd) fields

      let cf = zipWith (\fname ftype -> (fname, Scheme kinds (TFunc [typ] ftype Star))) fieldNames fieldTypes

      let sch = Scheme kinds (TFunc fieldTypes typ Star)
      let ctor = Constructor { ctorFields=cf, ctorType=sch, ctorValue=valT }
      return $ Map.insert name ctor constrs

    -- An enum like `Maybe T = Just T | Nothing` would need three constructors
    -- One for finding the type of `Maybe<T>` in type declarations, and
    -- one for each of `Just{val: val}` and `Nothing{}` constructors in the code.
    T.Enum     _ options ->
      let typ = TCon name generalized Star
          sch = Scheme kinds (TFunc [] typ Star)
          k = kindN nGens
          valT = Scheme kinds $ TCon name [] k
          ctor = Constructor { ctorFields=[], ctorType=sch, ctorValue=valT }
      in addEnumOptions kinds generalized sub typ valT options (Map.insert name ctor constrs)
  makeConstructors ts constrs'

mustBeUnique :: String -> Map String b -> Result ()
mustBeUnique key m =
  when (Map.member key m) $ duplicateName key


createStructFields :: [Kind] -> Type -> [String] -> [Type] -> [(String, Scheme)]
createStructFields kinds typ = zipWith makePair
  where makePair fname ftype = (fname, Scheme kinds (TFunc [typ] ftype Star))


addEnumOptions :: [Kind] -> t -> Substitution -> Type -> Scheme -> [(String, [(String, TypeDeclT)])]  ->
  Map String Constructor -> Either Error (Map String Constructor)
addEnumOptions _     _           _   _   _    []         constrs = return constrs
addEnumOptions kinds generalized sub typ valT ((n,t):os) constrs = do
  mustBeUnique n constrs

  let fields = t
  let fieldNames = map fst fields
  fieldTypes <- mapM (convertDecl sub . snd) fields
  let cf = zipWith (\fn ft -> (fn, Scheme kinds (TFunc [typ] ft Star))) fieldNames fieldTypes

  let sch = Scheme kinds (TFunc fieldTypes typ Star)
  let ctor = Constructor { ctorFields=cf, ctorType=sch, ctorValue=valT }
  addEnumOptions kinds generalized sub typ valT os (Map.insert n ctor constrs)


convertDecl :: Substitution -> TypeDeclT -> Result Type
convertDecl sub decl = case decl of
  T.TypeName _ tname      ->
    return $ fromMaybe (TCon tname [] Star) $ Map.lookup (TVar tname Star) sub
  T.Generic  _ tname gens -> do
    genTypes <- mapM (convertDecl sub) gens
    return $ TCon tname genTypes Star
  T.Function _ argTs retT -> do
    argTypes <- mapM (convertDecl sub) argTs
    retType <- convertDecl sub retT
    return $ TFunc argTypes retType Star
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
