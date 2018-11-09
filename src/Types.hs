module Types where

import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- TODO: Add kinds to TCon and TVar

-- Type is the internal representation of a type as used by the type system.
-- Types that a user types in (no pun intended) are mapped to this sort of
-- type before anything useful happens.
data Type
  = TCon String [Type]
  | TFunc [Type] Type
  | TVar String
  | TGen Int
  deriving (Eq, Ord, Show)

tcon0 :: String -> Type
tcon0 name = TCon name []

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


class Types t where
  apply :: Substitution -> t -> t
  freeTypeVars :: t -> Set String

instance Types Type where
  apply sub (TCon con ts) =
    TCon con (apply sub ts)
  apply sub (TFunc args ret) =
    TFunc (apply sub args) (apply sub ret)
  apply sub tv@(TVar _) =
    fromMaybe tv $ Map.lookup tv sub
  apply sub tg@(TGen _) =
    fromMaybe tg $ Map.lookup tg sub

  freeTypeVars (TCon _ ts) =
    freeTypeVars ts
  freeTypeVars (TFunc args ret) =
    Set.union (freeTypeVars args) (freeTypeVars ret)
  freeTypeVars (TVar tv) =
    Set.singleton tv
  freeTypeVars (TGen _) =
    Set.empty


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
  TCon name [] ->
    name
  TCon name ts ->
    name ++ "<" ++ intercalate "," (map prettyPrint ts) ++ ">"
  TFunc as r ->
    "func(" ++ intercalate "," (map prettyPrint as) ++ ") " ++ prettyPrint r
  TVar s ->
    s
  TGen i ->
    "_t" ++ show i
