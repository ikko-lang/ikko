module Types where

import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Debug.Trace

import Util.PrettyPrint (Render, render)

showTrace :: (Show a) => String -> a -> a
showTrace s a = trace (s ++ ": " ++ show a) a


data Kind
  = Star
  | KFun Kind Kind
  deriving (Eq, Ord, Show)

instance Render Kind where
  render Star = "*"
  render (KFun Star k2) = "* -> " ++ render k2
  render (KFun k1 k2) = "(" ++ render k1 ++ " -> " ++ render k2

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
unApplyTypes t =
  let (t', ts) = unApplyTypes' t
  in (t', reverse ts)

unApplyTypes' :: Type -> (Type, [Type])
unApplyTypes' (TAp a b) =
  let (t, ts) = unApplyTypes' a
  in (t, b : ts)
unApplyTypes' t         =
  (t, [])

getRoot :: Type -> Type
getRoot (TAp a _) = getRoot a
getRoot t         = t

containsGenerics :: Type -> Bool
containsGenerics t = case t of
  TGen _ _  -> True
  TAp a b   -> any containsGenerics [a, b]
  TFunc _ _ -> False
  TCon _ _  -> False
  TVar   _  -> False

type Substitution = Map Type Type

showSub :: Substitution -> String
showSub = showPairs . Map.toList
  where showPairs pairs = intercalate "; " $ map showPair pairs
        showPair (k, v) = render k ++ " => " ++ render v

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


instance (Types a) => Types [a] where
  apply sub =
    map (apply sub)
  freeTypeVars ts =
    foldl Set.union Set.empty (map freeTypeVars ts)


data Scheme
  = Scheme [Kind] QualType
  deriving (Eq, Show)

instance Render Scheme where
  render (Scheme _ qt) = -- ignore kinds
    render qt

instance Types Scheme where
  apply sub (Scheme ks t) = Scheme ks (apply sub t)
  freeTypeVars (Scheme _ t) = freeTypeVars t

asScheme :: Type -> Scheme
asScheme t =
  if containsGenerics t
  then error "t contains generics"
  else Scheme [] (Qual [] t)

isGeneric :: Type -> Bool
isGeneric TGen{} = True
isGeneric _      = False

fromMaybe :: a -> Maybe a -> a
fromMaybe _ (Just x) = x
fromMaybe d Nothing  = d

instance Render Type where
  render t = case t of
    TCon name _      ->
      name
    TFunc _ _        ->
      "fn"
    TAp _ _          ->
      case getRoot t of
        TFunc _ _ -> renderFn t
        _         -> renderAp t
    TVar (TyVar s _) ->
      s
    TGen i _         ->
      "_t" ++ show i

renderFn :: Type -> String
renderFn t =
  let (_, ts) = unApplyTypes t
      retT = last ts
      argT = map render $ init ts
      args = intercalate ", " argT
  in "fn(" ++ args ++ ") -> " ++ renderReturn retT

-- adds parens if return type is complex
renderReturn :: Type -> String
renderReturn retT =
  let rendered = render retT
  in case retT of
    TVar _   -> rendered
    TGen _ _ -> rendered
    TCon _ _ -> rendered
    _      -> "(" ++ rendered ++ ")"

renderAp :: Type -> String
renderAp t =
  let (c, ts) = unApplyTypes t
      gens = intercalate ", " $ map render ts
  in render c ++ "<" ++ gens ++ ">"

data Predicate
  = Pred String Type -- e.g. (Num a)
  deriving (Eq, Show)

data Qualified t
  = Qual [Predicate] t
  deriving (Eq, Show)

type QualType = Qualified Type

qualify :: Type -> QualType
qualify = Qual []

instance Render Predicate where
  render (Pred c t) =
    c ++ " " ++ render t

instance Types Predicate where
  apply sub (Pred s t) = Pred s (apply sub t)
  freeTypeVars (Pred _ t) = freeTypeVars t

instance (Render t) => Render (Qualified t) where
  render (Qual ps t)=
    if null ps
    then render t
    else "(" ++ intercalate ", " (map render ps) ++ ") => " ++ render t

instance (Types t) => Types (Qualified t) where
  apply sub (Qual ps t) = Qual (apply sub ps) (apply sub t)
  freeTypeVars (Qual ps t) = Set.union (freeTypeVars t) (freeTypeVars ps)

class Instantiate t where
  instantiate :: [Type] -> t -> t

instance Instantiate Type where
  instantiate ts t = case t of
    --TGen i _ -> ts !! (showTrace ("instantiate i=" ++ show i ++ ", ts=" ++ show ts) i)
    TGen i _ -> ts !! i
    TAp l r  -> TAp (instantiate ts l) (instantiate ts r)
    _        -> t

instance Instantiate a => Instantiate [a] where
  instantiate ts = map (instantiate ts)

instance Instantiate Predicate where
  instantiate ts (Pred s t) = Pred s (instantiate ts t)

instance Instantiate t => Instantiate (Qualified t) where
  instantiate ts (Qual ps t) = Qual (instantiate ts ps) (instantiate ts t)

type Inst = Qualified Predicate

data Class
  = Class
  { superclasses :: [String]
  , instances :: [Inst] }
  deriving (Eq, Show)

data ClassEnv
  = ClassEnv
  { classes :: Map String Class
  , defaults :: [Type] }
  deriving (Eq, Show)
