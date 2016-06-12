-- | A basic wrapper for the `webidl2` library, and some ADT sugar on top.

module WebIDL
  ( Node(..)
  , Member(..)
  , Type(..)
  , Argument(..)
  , parse
  ) where

import Prelude (class Show, (<<<), show, (<>), ($), pure, bind, (<$>))

import Data.Maybe (Maybe)
import Data.Either (Either(Left, Right))
import Data.Generic (class Generic, gShow)
import Data.Foreign (ForeignError, Foreign, toForeign)
import Data.Foreign.Class (class IsForeign, read, readProp)
import Data.Foreign.NullOrUndefined (unNullOrUndefined)
import Data.Traversable (traverse)

import Control.Alt ((<|>))
import Control.Bind ((>=>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION(), error, throwException)

data Type
  = UnionType { unifies :: Array Type }
  | ArrayType { nesting :: Int, elementType :: String }
  | NamedType { typeName :: String }
  | GenericType { family :: String, typeArgument :: Type }
  | NullableType Type

instance isForeignType :: IsForeign Type where
  read f = do
    nullable <- readProp "nullable" f
    ty <- do family <- readProp "generic" f
             typeArgument <- readProp "idlType" f
             pure $ GenericType { family, typeArgument }
      <|> do nesting <- readProp "array" f
             elementType <- readProp "idlType" f
             pure $ ArrayType { nesting, elementType }
      <|> do unifies <- readProp "idlType" f
             pure $ UnionType { unifies }
      <|> do typeName <- readProp "idlType" f
             pure $ NamedType { typeName }
    pure $ if nullable then NullableType ty else ty

derive instance genericType :: Generic Type

instance showType :: Show Type where
  show = gShow

newtype Argument = Argument
  { name           :: String
  , idlType        :: Type
  , optional       :: Boolean
  , variadic       :: Boolean
  }

instance isForeignArgument :: IsForeign Argument where
  read f = do
    name          <- readProp "name" f
    idlType       <- readProp "idlType" f
    optional      <- readProp "optional" f
    variadic      <- readProp "variadic" f
    pure $ Argument { name, idlType, optional, variadic }

derive instance genericArgument :: Generic Argument

instance showArgument :: Show Argument where
  show = gShow

data Member
  = OperationMember
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
  | AttributeMember
    { name            :: String
    , inherit         :: Boolean
    , static          :: Boolean
    , stringifier     :: Boolean
    , readonly        :: Boolean
    , idlType         :: Type
    }
  | ConstantMember
    { name            :: String
    , idlType         :: String
    , nullable        :: Boolean
    }
  | FieldMember
    { name            :: String
    , required        :: Boolean
    , idlType         :: Type
    }
  | OtherMember String

derive instance genericMember :: Generic Member

instance showMember :: Show Member where
  show = gShow

instance isForeignMember :: IsForeign Member where
  read f = do
    _type <- readProp "type" f
    case _type of
      "operation" -> do
        name          <- unNullOrUndefined <$> readProp "name" f
        arguments     <- readProp "arguments" f
        idlType       <- readProp "idlType" f
        getter        <- readProp "getter" f
        setter        <- readProp "setter" f
        creator       <- readProp "creator" f
        deleter       <- readProp "deleter" f
        legacycaller  <- readProp "legacycaller" f
        static        <- readProp "static" f
        stringifier   <- readProp "stringifier" f
        pure $ OperationMember { name, arguments, idlType, getter, setter, creator, deleter, legacycaller, static, stringifier }
      "const" -> do
        name          <- readProp "name" f
        idlType       <- readProp "idlType" f
        nullable      <- readProp "nullable" f
        pure $ ConstantMember { name, idlType, nullable }
      "attribute" -> do
        name          <- readProp "name" f
        inherit       <- readProp "inherit" f
        static        <- readProp "static" f
        stringifier   <- readProp "stringifier" f
        readonly      <- readProp "readonly" f
        idlType       <- readProp "idlType" f
        pure $ AttributeMember { name, inherit, static, stringifier, readonly, idlType }
      "field" -> do
        name          <- readProp "name" f
        idlType       <- readProp "idlType" f
        required      <- readProp "required" f
        pure $ FieldMember { name, idlType, required }
      _ -> pure $ OtherMember _type

-- | A node represented as a PureScript data type.
data Node
  = InterfaceNode
    { name            :: String
    , partial         :: Boolean
    , members         :: Array Member
    , inheritance     :: Maybe String
    }
  | ImplementsNode
    { target          :: String
    , implements      :: String
    }
  | TypeDefNode
    { name            :: String
    , idlType         :: Type
    }
  | CallbackNode
    { name            :: String
    , idlType         :: Type
    , arguments       :: Array Argument
    }
  | DictionaryNode
    { name            :: String
    , partial         :: Boolean
    , members         :: Array Member
    , inheritance     :: Maybe String
    }
  | ExceptionNode
    { name            :: String
    , members         :: Array Member
    , inheritance     :: Maybe String
    }
  | EnumNode
    { name            :: String
    , values          :: Array String
    }
  | OtherNode String

derive instance genericNode :: Generic Node

instance showNode :: Show Node where
  show = gShow

instance isForeignNode :: IsForeign Node where
  read f = do
    _type <- readProp "type" f
    case _type of
      "interface" -> do
        name          <- readProp "name" f
        partial       <- readProp "partial" f
        members       <- readProp "members" f
        inheritance   <- unNullOrUndefined <$> readProp "inheritance" f
        pure $ InterfaceNode { name, partial, members, inheritance }
      "implements" -> do
        target        <- readProp "target" f
        implements    <- readProp "implements" f
        pure $ ImplementsNode { target, implements }
      "typedef" -> do
        name          <- readProp "name" f
        idlType       <- readProp "idlType" f
        pure $ TypeDefNode { name, idlType }
      "callback" -> do
        name          <- readProp "name" f
        idlType       <- readProp "idlType" f
        arguments     <- readProp "arguments" f
        pure $ CallbackNode { name, idlType, arguments }
      "dictionary" -> do
        name          <- readProp "name" f
        partial       <- readProp "partial" f
        members       <- readProp "members" f
        inheritance   <- unNullOrUndefined <$> readProp "inheritance" f
        pure $ DictionaryNode { name, partial, members, inheritance }
      "exception" -> do
        name          <- readProp "name" f
        members       <- readProp "members" f
        inheritance   <- unNullOrUndefined <$> readProp "inheritance" f
        pure $ ExceptionNode { name, members, inheritance }
      "enum" -> do
        name          <- readProp "name" f
        values        <- readProp "values" f
        pure $ EnumNode { name, values }
      _ -> pure $ OtherNode _type

foreign import parseImpl :: forall eff. String -> Eff (err :: EXCEPTION | eff) (Array Foreign)

-- | Parse a WebIDL string.
parse :: forall eff. String -> Eff (err :: EXCEPTION | eff) (Array Node)
parse = parseImpl >=> traverse (fromRight <<< read <<< toForeign)
  where
  fromRight :: Either ForeignError Node -> Eff (err :: EXCEPTION | eff) Node
  fromRight (Right node) = pure node
  fromRight (Left err) = throwException $ error $ "Unable to parse node: " <> show err
