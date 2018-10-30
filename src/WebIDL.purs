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
  , RecArgument
  , Argument(..)
  , parse
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
import Foreign (F, Foreign, ForeignError, readBoolean, readString)
import Foreign.Index (index)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe)
import Data.Traversable (traverse)
import Simple.JSON as JSON
import Data.Newtype (class Newtype, wrap)

data Type
  = UnionType { unifies :: Array Type }
  | ArrayType { nesting :: Int, elementType :: String }
  | NamedType { typeName :: String }
  | GenericType { family :: String, typeArgument :: Type }
  | NullableType Type

derive instance genericType :: Generic Type _

instance showType :: Show Type where
  show x = genericShow x

instance readForeignType :: JSON.ReadForeign Type where
  readImpl f = do
    nullable <- readBoolean =<< index f "nullable"
    ty <- GenericType <$> JSON.read' f
      <|> ArrayType <$> JSON.read' f
      <|> UnionType <$> JSON.read' f
      <|> NamedType <$> JSON.read' f
    pure $ if nullable then NullableType ty else ty


type RecArgument =
  { name           :: String
  , idlType        :: Type
  , optional       :: Boolean
  , variadic       :: Boolean
  }

newtype Argument = Argument RecArgument
derive instance newtypeArgument :: Newtype Argument _

derive instance genericArgument :: Generic Argument _

instance showArgument :: Show Argument where
  show x = genericShow x

instance readForeignArgument :: JSON.ReadForeign Argument where
  readImpl = map wrap <<< JSON.read'

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

instance readForeignMember :: JSON.ReadForeign Member where
  readImpl f = do
    _type <- readString =<< index f "type"
    case _type of
      "operation" -> OperationMember <$> JSON.read' f
      "const" -> ConstantMember <$> JSON.read' f
      "attribute" -> AttributeMember <$> JSON.read' f
      "field" -> FieldMember <$> JSON.read' f
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

instance readForeignNode :: JSON.ReadForeign Node where
  readImpl f = readNode f

readNode :: Foreign -> F Node
readNode f = do
  _type <- readString =<< index f "type"
  case _type of
    "interface" -> InterfaceNode <$> JSON.read' f
    "implements" -> ImplementsNode <$> JSON.read' f
    "typedef" -> TypeDefNode <$> JSON.read' f
    "callback" -> CallbackNode <$> JSON.read' f
    "dictionary" -> DictionaryNode <$> JSON.read' f
    "exception" -> ExceptionNode <$> JSON.read' f
    "enum" -> EnumNode <$> JSON.read' f
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
