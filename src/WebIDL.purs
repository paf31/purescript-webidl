-- | A basic wrapper for the `webidl2` library, and some ADT sugar on top.

module WebIDL
  ( Node(..)
  , RecNodeInterface
  , RecNodeImplements
  , RecNodeTypeDef
  , RecNodeCallback
  , RecNodeDictionary
  , RecNodeException
  , RecNodeEnum
  , Member(..)
  , RecMemberOperation
  , RecMemberAttribute
  , RecMemberConstant
  , RecMemberField
  , Type(..)
  , Argument(..)
  , parse
  , readType
  , readArgument
  , readMember
  , readNode
  ) where

import Prelude
import Control.Alt ((<|>))
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Effect.Exception (Error, try)
import Control.Monad.Except (runExcept)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Foreign (F, Foreign, ForeignError, readArray, readBoolean, readInt, readNullOrUndefined, readString)
import Foreign.Index (index)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe)
import Data.Traversable (traverse)

data Type
  = UnionType { unifies :: Array Type }
  | ArrayType { nesting :: Int, elementType :: String }
  | NamedType { typeName :: String }
  | GenericType { family :: String, typeArgument :: Type }
  | NullableType Type

readType :: Foreign -> F Type
readType f = do
  nullable <- readBoolean =<< index f "nullable"
  ty <- do family <- readString =<< index f "generic"
           typeArgument <- readType =<< index f "idlType"
           pure $ GenericType { family, typeArgument }
    <|> do nesting <- readInt =<< index f "array"
           elementType <- readString =<< index f "idlType"
           pure $ ArrayType { nesting, elementType }
    <|> do unifies <- traverse readType =<< readArray =<< index f "idlType"
           pure $ UnionType { unifies }
    <|> do typeName <- readString =<< index f "idlType"
           pure $ NamedType { typeName }
  pure $ if nullable then NullableType ty else ty

derive instance genericType :: Generic Type _

instance showType :: Show Type where
  show x = genericShow x

newtype Argument = Argument
  { name           :: String
  , idlType        :: Type
  , optional       :: Boolean
  , variadic       :: Boolean
  }

readArgument :: Foreign -> F Argument
readArgument f = do
  name          <- readString  =<< index f "name"
  idlType       <- readType    =<< index f "idlType"
  optional      <- readBoolean =<< index f "optional"
  variadic      <- readBoolean =<< index f "variadic"
  pure $ Argument { name, idlType, optional, variadic }

derive instance genericArgument :: Generic Argument _

instance showArgument :: Show Argument where
  show x = genericShow x

type RecMemberOperation =
    { name            :: Maybe String
    , arguments       :: Array Argument
    , getter          :: Boolean
    , setter          :: Boolean
    , creator         :: Boolean
    , deleter         :: Boolean
    , legacycaller    :: Boolean
    , static          :: Boolean
    , stringifier     :: Boolean
    , idlType         :: Type
    }

type RecMemberAttribute =
    { name            :: String
    , inherit         :: Boolean
    , static          :: Boolean
    , stringifier     :: Boolean
    , readonly        :: Boolean
    , idlType         :: Type
    }

type RecMemberConstant =
    { name            :: String
    , idlType         :: String
    , nullable        :: Boolean
    }

type RecMemberField =
    { name            :: String
    , required        :: Boolean
    , idlType         :: Type
    }

data Member
  = OperationMember RecMemberOperation
  | AttributeMember RecMemberAttribute
  | ConstantMember RecMemberConstant
  | FieldMember RecMemberField
  | OtherMember String

derive instance genericMember :: Generic Member _

instance showMember :: Show Member where
  show x = genericShow x

readMember :: Foreign -> F Member
readMember f = do
  _type <- readString =<< index f "type"
  case _type of
    "operation" -> do
      name          <- traverse readString   =<< readNullOrUndefined =<< index f "name"
      arguments     <- traverse readArgument =<< readArray =<< index f "arguments"
      idlType       <- readType              =<< index f "idlType"
      getter        <- readBoolean           =<< index f "getter"
      setter        <- readBoolean           =<< index f "setter"
      creator       <- readBoolean           =<< index f "creator"
      deleter       <- readBoolean           =<< index f "deleter"
      legacycaller  <- readBoolean           =<< index f "legacycaller"
      static        <- readBoolean           =<< index f "static"
      stringifier   <- readBoolean           =<< index f "stringifier"
      pure $ OperationMember { name, arguments, idlType, getter, setter, creator, deleter, legacycaller, static, stringifier }
    "const" -> do
      name          <- readString  =<< index f "name"
      idlType       <- readString  =<< index f "idlType"
      nullable      <- readBoolean =<< index f "nullable"
      pure $ ConstantMember { name, idlType, nullable }
    "attribute" -> do
      name          <- readString  =<< index f "name"
      inherit       <- readBoolean =<< index f "inherit"
      static        <- readBoolean =<< index f "static"
      stringifier   <- readBoolean =<< index f "stringifier"
      readonly      <- readBoolean =<< index f "readonly"
      idlType       <- readType    =<< index f "idlType"
      pure $ AttributeMember { name, inherit, static, stringifier, readonly, idlType }
    "field" -> do
      name          <- readString  =<< index f "name"
      idlType       <- readType    =<< index f "idlType"
      required      <- readBoolean =<< index f "required"
      pure $ FieldMember { name, idlType, required }
    _ -> pure $ OtherMember _type

-- | A node represented as a PureScript data type.

type RecNodeInterface =
  { name            :: String
  , partial         :: Boolean
  , members         :: Array Member
  , inheritance     :: Maybe String
  }

type RecNodeImplements =
  { target          :: String
  , implements      :: String
  }

type RecNodeTypeDef =
  { name            :: String
  , idlType         :: Type
  }

type RecNodeCallback =
  { name            :: String
  , idlType         :: Type
  , arguments       :: Array Argument
  }

type RecNodeDictionary =
  { name            :: String
  , partial         :: Boolean
  , members         :: Array Member
  , inheritance     :: Maybe String
  }

type RecNodeException =
  { name            :: String
  , members         :: Array Member
  , inheritance     :: Maybe String
  }

type RecNodeEnum =
  { name            :: String
  , values          :: Array String
  }

data Node
  = InterfaceNode RecNodeInterface
  | ImplementsNode RecNodeImplements
  | TypeDefNode RecNodeTypeDef
  | CallbackNode RecNodeCallback
  | DictionaryNode RecNodeDictionary
  | ExceptionNode RecNodeException
  | EnumNode RecNodeEnum
  | OtherNode String

derive instance genericNode :: Generic Node _

instance showNode :: Show Node where
  show x = genericShow x

readNode :: Foreign -> F Node
readNode f = do
  _type <- readString =<< index f "type"
  case _type of
    "interface" -> do
      name          <- readString          =<< index f "name"
      partial       <- readBoolean         =<< index f "partial"
      members       <- traverse readMember =<< readArray =<< index f "members"
      inheritance   <- traverse readString =<< readNullOrUndefined =<< index f "inheritance"
      pure $ InterfaceNode { name, partial, members, inheritance }
    "implements" -> do
      target        <- readString =<< index f "target"
      implements    <- readString =<< index f "implements"
      pure $ ImplementsNode { target, implements }
    "typedef" -> do
      name          <- readString =<< index f "name"
      idlType       <- readType   =<< index f "idlType"
      pure $ TypeDefNode { name, idlType }
    "callback" -> do
      name          <- readString            =<< index f "name"
      idlType       <- readType              =<< index f "idlType"
      arguments     <- traverse readArgument =<< readArray =<< index f "arguments"
      pure $ CallbackNode { name, idlType, arguments }
    "dictionary" -> do
      name          <- readString          =<< index f "name"
      partial       <- readBoolean         =<< index f "partial"
      members       <- traverse readMember =<< readArray =<< index f "members"
      inheritance   <- traverse readString =<< readNullOrUndefined =<< index f "inheritance"
      pure $ DictionaryNode { name, partial, members, inheritance }
    "exception" -> do
      name          <- readString          =<< index f "name"
      members       <- traverse readMember =<< readArray =<< index f "members"
      inheritance   <- traverse readString =<< readNullOrUndefined =<< index f "inheritance"
      pure $ ExceptionNode { name, members, inheritance }
    "enum" -> do
      name          <- readString          =<< index f "name"
      values        <- traverse readString =<< readArray =<< index f "values"
      pure $ EnumNode { name, values }
    _ -> pure $ OtherNode _type

foreign import parseImpl :: String -> Effect (Array Foreign)

-- | Parse a WebIDL string.
parse :: String -> Either (Either Error (NonEmptyList ForeignError)) (Array Node)
parse =
  parseImpl
  >>> try
  >>> unsafePerformEffect
  >>> lmap Left
  >=> traverse (lmap Right <<< runExcept <<< readNode)
