module Types where

import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

data Kind
  = Star
  | KFun Kind Kind
  deriving (Eq, Ord, Show)

-- Type is the internal representation of a type as used by the type system.
-- Types that a user types in (no pun intended) are mapped to this sort of
-- type before anything useful happens.
data Type
  = TCon String Kind
  | TFunc Int -- number of arguments
          Kind -- it should require (1+num args) type params
  | TAp Type Type
  | TVar TyVar
  | TGen Int Kind
  deriving (Eq, Ord, Show)

data TyVar
  = TyVar String Kind
  deriving (Eq, Ord, Show)

simpleType :: String -> Type
simpleType name = TCon name Star


-- `(Show a)` becomes `Predicate "Show" (TVar "a")`
data Predicate
  = Predicate String Type
  deriving (Eq, Ord, Show)

data Qualified t
  = Qualified [Predicate] t
  deriving (Eq, Ord, Show)

-- e.g. (Show a) => a -> a for a function's type
type QualType = Qualified Type

-- e.g. (Show a) => Show [a] for a class implementation
type QualPred = Qualified Predicate


makeVar :: String -> Kind -> Type
makeVar name k = TVar (TyVar name k)

simpleVar :: String -> Type
simpleVar name = makeVar name Star

tInt    :: Type
tInt    = simpleType "Int"

tFloat  :: Type
tFloat  = simpleType "Float"

tBool   :: Type
tBool   = simpleType "Bool"

tChar   :: Type
tChar   = simpleType "Char"

tString :: Type
tString = simpleType "String"

tUnit   :: Type
tUnit   = simpleType "()"

kindN :: Int -> Kind
kindN n
  | n == 0    = Star
  | n >  0    = KFun Star (kindN $ n - 1)
  | otherwise = error "negative"

makeFuncType :: [Type] -> Type -> Type
makeFuncType argTs retT =
  let nargs = length argTs
      k = kindN (nargs + 1)
  in applyTypes (TFunc nargs k) (argTs ++ [retT])

applyTypes :: Type -> [Type] -> Type
applyTypes = foldl TAp

unApplyTypes :: Type -> (Type, [Type])
unApplyTypes (TAp a b) =
  let (t, ts) = unApplyTypes a
  in (t, b : ts)
unApplyTypes t         =
  (t, [])

getRoot :: Type -> Type
getRoot (TAp a _) = getRoot a
getRoot t         = t


type Substitution = Map Type Type

emptySubstitution :: Substitution
emptySubstitution = Map.empty

makeSub :: [(Type, Type)] -> Substitution
makeSub = Map.fromList

-- `apply (composeSubs a b) t`
-- is equivalent to
-- `apply b (apply a t)`
composeSubs :: Substitution -> Substitution -> Substitution
composeSubs a b =
  Map.union (Map.map (apply b) a) b

class HasKind t where
  getKind :: t -> Kind

instance HasKind Type where
  getKind t = case t of
    TCon  _ k -> k
    TFunc _ k -> k
    TAp   a b -> case getKind a of
      (KFun _ k') -> k'
      Star        ->
        let lhs = "lhs = " ++ show a
            rhs = ", rhs = " ++ show b
            msg = "compiler bug: invalid kind for LHS of type application, " ++ lhs ++ rhs
        in error msg
    TVar  tv  -> getKind tv
    TGen  _ k -> k

instance HasKind TyVar where
  getKind (TyVar _ k) = k

class Types t where
  apply :: Substitution -> t -> t
  freeTypeVars :: t -> Set TyVar

instance Types Type where
  apply sub t = case t of
    TCon{}  -> t
    TFunc{} -> t
    TAp a b -> TAp (apply sub a) (apply sub b)
    TVar{}  -> fromMaybe t $ Map.lookup t sub
    TGen{}  -> fromMaybe t $ Map.lookup t sub

  freeTypeVars t = case t of
    TCon{}  -> Set.empty
    TFunc{} -> Set.empty
    TAp a b -> Set.union (freeTypeVars a) (freeTypeVars b)
    TVar tv -> Set.singleton tv
    TGen{}  -> Set.empty

instance Types Predicate where
  apply sub (Predicate i t) =
    Predicate i (apply sub t)

  freeTypeVars (Predicate _ t) =
    freeTypeVars t


instance Types t => Types (Qualified t) where
  apply sub (Qualified ps t) =
    Qualified (apply sub ps) (apply sub t)

  freeTypeVars (Qualified ps t) =
    Set.union (freeTypeVars ps) (freeTypeVars t)

instance (Types a) => Types [a] where
  apply sub =
    map (apply sub)

  freeTypeVars ts =
    foldl Set.union Set.empty (map freeTypeVars ts)


data Scheme
  = Scheme [Kind] Type
  deriving (Eq, Show)

instance Types Scheme where
  apply sub (Scheme ks t) = Scheme ks (apply sub t)
  freeTypeVars (Scheme _ t) = freeTypeVars t

asScheme :: Type -> Scheme
asScheme = Scheme []

isGeneric :: Type -> Bool
isGeneric TGen{} = True
isGeneric _      = False

fromMaybe :: a -> Maybe a -> a
fromMaybe _ (Just x) = x
fromMaybe d Nothing  = d

prettyPrint :: Type -> String
prettyPrint t = case t of
  TCon name _      ->
    name
  TFunc _ _        ->
    "fn"
  TAp _ _          ->
    case getRoot t of
      TFunc _ _ -> prettyPrintFn t
      _         -> prettyPrintAp t
  TVar (TyVar s _) ->
    s
  TGen i _         ->
    "_t" ++ show i

prettyPrintFn :: Type -> String
prettyPrintFn t =
  let (_, ts) = unApplyTypes t
      retT = prettyPrint $ last ts
      argT = map prettyPrint $ init ts
      args = intercalate ", " argT
  in "fn(" ++ args ++ ") " ++ retT

prettyPrintAp :: Type -> String
prettyPrintAp t =
  let (c, ts) = unApplyTypes t
      gens = intercalate ", " $ map prettyPrint ts
  in prettyPrint c ++ "<" ++ gens ++ ">"
