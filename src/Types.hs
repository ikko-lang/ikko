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
  = TCon String
  | TFunc Int -- number of arguments
  | TAp Type Type
  | TVar String
  | TGen Int
  deriving (Eq, Ord, Show)

tInt    :: Type
tInt    = TCon "Int"

tFloat  :: Type
tFloat  = TCon "Float"

tBool   :: Type
tBool   = TCon "Bool"

tChar   :: Type
tChar   = TCon "Char"

tString :: Type
tString = TCon "String"

tUnit   :: Type
tUnit   = TCon "()"

kindN :: Int -> Kind
kindN n
  | n == 0    = Star
  | n >  0    = KFun Star (kindN $ n - 1)
  | otherwise = error "negative"

makeFuncType :: [Type] -> Type -> Type
makeFuncType argTs retT =
  applyTypes (TFunc $ length argTs) (argTs ++ [retT])

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


class Types t where
  apply :: Substitution -> t -> t
  freeTypeVars :: t -> Set String

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
    TVar v  -> Set.singleton v
    TGen{}  -> Set.empty


instance (Types a) => Types [a] where
  apply sub = map (apply sub)
  freeTypeVars ts = foldl Set.union Set.empty (map freeTypeVars ts)


-- TODO: Add kinds to Scheme
-- The number is how many generic variables the scheme has
data Scheme
  = Scheme Int Type
  deriving (Eq, Show)

instance Types Scheme where
  apply sub (Scheme nvars t) = Scheme nvars (apply sub t)
  freeTypeVars (Scheme _ t) = freeTypeVars t

asScheme :: Type -> Scheme
asScheme = Scheme 0

isGeneric :: Type -> Bool
isGeneric (TGen _) = True
isGeneric _        = False

fromMaybe :: a -> Maybe a -> a
fromMaybe _ (Just x) = x
fromMaybe d Nothing  = d

prettyPrint :: Type -> String
prettyPrint t = case t of
  TCon name ->
    name
  TFunc _ ->
    "fn"
  TAp _ _ ->
    case getRoot t of
      TFunc _ -> prettyPrintFn t
      _       -> prettyPrintAp t
  TVar s ->
    s
  TGen i ->
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
