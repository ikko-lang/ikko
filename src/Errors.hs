module Errors where

import Data.List (intercalate)

import Region
import Types (Type, Kind, TyVar, Predicate)
import Util.PrettyPrint (render)

data Error
  = Mismatch Type Type
  | WrongType Type String
  | InfiniteType TyVar -- a type variable
  | KindMismatch TyVar Kind Kind
  | AssignmentInsufficientlyGeneral Type Type String -- var type, expr type, var name
  | BindingTooGeneral String -- name of binding
  | ContextTooWeak String
  | CompilerBug String
  | DuplicateBinding String -- binding name
  | DuplicateClassBinding String [String] -- Class, binding name[s]
  | DuplicateClass String
  | DuplicateInstance String Type -- class, type
  | ExtraInstanceMethod String [String] -- class, method names
  | MissingInstanceMethod String [String] -- class, method names
  | InstanceMethodWrongNumberArgs String String -- class, method name
  | InstanceMethodBadSelfParam String String -- class, method name
  | ParseError String
  | CannotCast String -- a message
  | UndefinedVar String -- variable name
  | UndefinedField String String -- struct name, field name
  | UndefinedType String -- type name
  | UndefinedTypes String [String] -- message, type names
  | UndefinedClass String
  | NonStructureType String -- type name
  | StructFieldErr String String -- type name, message
  | PatternErr String -- message
  | InvalidAnonStructure
  | InsufficientlyDefinedType
  | Unreachable String -- function name
  | MissingReturn String -- function name
  | MalformedType String -- explanation
  | CyclicClasses [String] -- names of classes
  | ContextReduction Predicate
  | Ambiguity [Predicate]
  | WithLocations [Region] Error -- wrap another error, adding a location
  deriving (Show, Eq)

type Result a = Either Error a

renderError :: Error -> String -> String
renderError err fileContent = case err of
  WithLocations regions err' ->
    addRegions regions fileContent (renderError err' fileContent)
  Unreachable fname ->
    "unreachable code in function " ++ fname
  Mismatch t1 t2 ->
    "Type mismatch between " ++ render t1 ++ " and " ++ render t2
  WrongType t1 s ->
    "Wrong type " ++ render t1 ++ " " ++ s
  _ ->
    show err


-- This is a very suboptimal way to do this, but it
-- works for now
addRegions :: [Region] -> String -> String -> String
addRegions regions fileContent errStr =
  let fileLines = lines fileContent
      fileLocations = map (showRegion fileLines) regions
  in intercalate "\n" (errStr : fileLocations)
