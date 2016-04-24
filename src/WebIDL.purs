-- | A basic wrapper for the `webidl2` library, and some ADT sugar on top.

module WebIDL
  ( Node(..)
  , Member(..)
  , Type(..)
  , Argument(..)
  , parse
  ) where

import Prelude (class Show, (<<<), show, (<>), ($), return, bind, (<$>))

import Data.Maybe (Maybe)
import Data.Either (Either(Left, Right))
import Data.Generic (class Generic, gShow)
import Data.Foreign (ForeignError, Foreign, toForeign)
import Data.Foreign.Class (class IsForeign, read, readProp)
import Data.Foreign.NullOrUndefined (runNullOrUndefined)
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
             return $ GenericType { family, typeArgument }
      <|> do nesting <- readProp "array" f
             elementType <- readProp "idlType" f
             return $ ArrayType { nesting, elementType }
      <|> do unifies <- readProp "idlType" f
             return $ UnionType { unifies }
      <|> do typeName <- readProp "idlType" f
             return $ NamedType { typeName }
    return $ if nullable then NullableType ty else ty

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
    return $ Argument { name, idlType, optional, variadic }

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
        name          <- runNullOrUndefined <$> readProp "name" f
        arguments     <- readProp "arguments" f
        idlType       <- readProp "idlType" f
        getter        <- readProp "getter" f
        setter        <- readProp "setter" f
        creator       <- readProp "creator" f
        deleter       <- readProp "deleter" f
        legacycaller  <- readProp "legacycaller" f
        static        <- readProp "static" f
        stringifier   <- readProp "stringifier" f
        return $ OperationMember { name, arguments, idlType, getter, setter, creator, deleter, legacycaller, static, stringifier }
      "const" -> do
        name          <- readProp "name" f
        idlType       <- readProp "idlType" f
        nullable      <- readProp "nullable" f
        return $ ConstantMember { name, idlType, nullable }
      "attribute" -> do
        name          <- readProp "name" f
        inherit       <- readProp "inherit" f
        static        <- readProp "static" f
        stringifier   <- readProp "stringifier" f
        readonly      <- readProp "readonly" f
        idlType       <- readProp "idlType" f
        return $ AttributeMember { name, inherit, static, stringifier, readonly, idlType }
      "field" -> do
        name          <- readProp "name" f
        idlType       <- readProp "idlType" f
        required      <- readProp "required" f
        return $ FieldMember { name, idlType, required }
      _ -> return $ OtherMember _type

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
        inheritance   <- runNullOrUndefined <$> readProp "inheritance" f
        return $ InterfaceNode { name, partial, members, inheritance }
      "implements" -> do
        target        <- readProp "target" f
        implements    <- readProp "implements" f
        return $ ImplementsNode { target, implements }
      "typedef" -> do
        name          <- readProp "name" f
        idlType       <- readProp "idlType" f
        return $ TypeDefNode { name, idlType }
      "callback" -> do
        name          <- readProp "name" f
        idlType       <- readProp "idlType" f
        arguments     <- readProp "arguments" f
        return $ CallbackNode { name, idlType, arguments }
      "dictionary" -> do
        name          <- readProp "name" f
        partial       <- readProp "partial" f
        members       <- readProp "members" f
        inheritance   <- runNullOrUndefined <$> readProp "inheritance" f
        return $ DictionaryNode { name, partial, members, inheritance }
      "exception" -> do
        name          <- readProp "name" f
        members       <- readProp "members" f
        inheritance   <- runNullOrUndefined <$> readProp "inheritance" f
        return $ ExceptionNode { name, members, inheritance }
      "enum" -> do
        name          <- readProp "name" f
        values        <- readProp "values" f
        return $ EnumNode { name, values }
      _ -> return $ OtherNode _type

foreign import parseImpl :: forall eff. String -> Eff (err :: EXCEPTION | eff) (Array Foreign)

-- | Parse a WebIDL string.
parse :: forall eff. String -> Eff (err :: EXCEPTION | eff) (Array Node)
parse = parseImpl >=> traverse (fromRight <<< read <<< toForeign)
  where
  fromRight :: Either ForeignError Node -> Eff (err :: EXCEPTION | eff) Node
  fromRight (Right node) = return node
  fromRight (Left err) = throwException $ error $ "Unable to parse node: " <> show err
