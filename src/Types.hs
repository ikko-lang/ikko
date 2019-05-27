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

kindN :: Int -> Kind
kindN n
  | n == 0    = Star
  | n >  0    = KFun Star (kindN $ n - 1)
  | otherwise = error "must be positive"

-- Type is the internal representation of a type as used by the type system.
-- Types that a user types in (no pun intended) are mapped to this sort of
-- type before anything useful happens.
data Type
  = TCon String [Type] Kind
  | TFunc [Type] Type Kind
  | TVar String Kind
  | TGen Int
  deriving (Eq, Ord, Show)

-- TypeVar is mostly used as a response from freeTypeVars
data TypeVar =
  TypeVar String Kind
  deriving (Eq, Ord, Show)

{-
-- checkKinds returns false if the kinds are not correctly assembled
checkKinds :: Type -> Bool
checkKinds t = case t of
  TCon _ ts k ->
    all checkKinds ts &&
    checkKindApplication ts k
  TFunc at rt _ ->
    all checkKinds at &&
    checkKinds rt
  TVar _ _ ->
    True
  TGen _ ->
    True

-- This should allow:
--   Int
--   Maybe Int
--   Either Int Bool
--   Either (Maybe Int) Bool
-- This should not allow:
--   Maybe
--   Either Int
--   Either (Maybe) Bool
checkKindApplication :: [Type] -> Kind -> Bool
checkKindApplication ts k = case (ts, k) of
  _ -> undefined
-}

tcon0 :: String -> Type
tcon0 name = TCon name [] Star

tInt :: Type
tInt    = tcon0 "Int"

tFloat :: Type
tFloat  = tcon0 "Float"

tBool :: Type
tBool   = tcon0 "Bool"

tChar :: Type
tChar   = tcon0 "Char"

tString :: Type
tString = tcon0 "String"

tUnit :: Type
tUnit   = tcon0 "()"


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
  kind :: t -> Kind

instance HasKind Type where
  kind (TCon _ _ k) = k
  kind (TFunc _ _ k) = k
  kind (TVar _ k) = k
  kind _ = error "cannot get kind for that"


class Types t where
  apply :: Substitution -> t -> t
  freeTypeVars :: t -> Set TypeVar

instance Types Type where
  apply sub (TCon con ts k) =
    TCon con (apply sub ts) k
  apply sub (TFunc args ret k) =
    TFunc (apply sub args) (apply sub ret) k
  apply sub tv@(TVar _ _) =
    fromMaybe tv $ Map.lookup tv sub
  apply sub tg@(TGen _) =
    fromMaybe tg $ Map.lookup tg sub

  freeTypeVars (TCon _ ts _) =
    freeTypeVars ts
  freeTypeVars (TFunc args ret _) =
    Set.union (freeTypeVars args) (freeTypeVars ret)
  freeTypeVars (TVar tv k) =
    Set.singleton $ TypeVar tv k
  freeTypeVars (TGen _) =
    Set.empty


instance (Types a) => Types [a] where
  apply sub = map (apply sub)
  freeTypeVars ts = foldl Set.union Set.empty (map freeTypeVars ts)


-- The list of kinds has one kind for each generic type variable (TGen),
-- and the number in a TGen indexes into the list of kinds.
data Scheme
  = Scheme [Kind] Type
  deriving (Eq, Show)

instance Types Scheme where
  apply sub (Scheme kinds t) = Scheme kinds (apply sub t)
  freeTypeVars (Scheme _ t) = freeTypeVars t

asScheme :: Type -> Scheme
asScheme = Scheme []

isGeneric :: Type -> Bool
isGeneric (TGen _) = True
isGeneric _        = False

fromMaybe :: a -> Maybe a -> a
fromMaybe _ (Just x) = x
fromMaybe d Nothing  = d

prettyPrint :: Type -> String
prettyPrint t = case t of
  TCon name [] k ->
    name ++ "{{" ++ show k ++ "}}"
  TCon name ts k ->
    name ++ "<" ++ intercalate "," (map prettyPrint ts) ++ ">" ++ "{{" ++ show k ++ "}}"
  TFunc as r k ->
    "func(" ++ intercalate "," (map prettyPrint as) ++ ") " ++ prettyPrint r ++ "{{" ++ show k ++ "}}"
  TVar s k ->
    s ++ "{{" ++ show k ++ "}}"
  TGen i ->
    "_t" ++ show i
