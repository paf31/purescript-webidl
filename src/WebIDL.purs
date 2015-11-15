-- | A basic wrapper for the `webidl2` library, and some ADT sugar on top.

module WebIDL
  ( NodeView(..)
  , Type(..)
  , parse
  , toView
  , toViewWith
  ) where

import Prelude

import Data.Maybe
import Data.Either
import Data.Generic
import Data.Foreign
import Data.Foreign.Class
import Data.Foreign.NullOrUndefined

import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)

-- | Parse a WebIDL string. This function can throw exceptions.
foreign import parse :: String -> Foreign

newtype Type = Type
  { idlType   :: String
  , sequence  :: Boolean
  , generic   :: Maybe String
  , nullable  :: Boolean
  , array     :: Boolean
  , union     :: Boolean
  }

instance isForeignType :: IsForeign Type where
  read f = do
    idlType   <- readProp "idlType" f
    sequence  <- readProp "sequence" f
    generic   <- runNullOrUndefined <$> readProp "generic" f
    nullable  <- readProp "nullable" f
    array     <- readProp "array" f
    union     <- readProp "union" f
    return $ Type { idlType, sequence, generic, nullable, array, union }

derive instance genericType :: Generic Type

instance showType :: Show Type where
  show = gShow

-- | A node represented as a PureScript data type.
data NodeView node
  = InterfaceNode
    { name        :: String
    , partial     :: Boolean
    , members     :: Array node
    , inheritance :: Maybe String
    }
  | ImplementsNode
    { target      :: String
    , implements  :: String
    }
  | TypeDefNode
  | CallbackNode
  | DictionaryNode
  | ExceptionNode
  | OperationMember
    { name            :: String
    , arguments       :: Array node
    , idlType         :: Type
    , getter          :: Boolean
    , setter          :: Boolean
    , creator         :: Boolean
    , deleter         :: Boolean
    , legacycaller    :: Boolean
    , static          :: Boolean
    , stringifier     :: Boolean
    }
  | ArgumentMember
    { name           :: String
    , idlType        :: Type
    , static         :: Boolean
    , stringifier    :: Boolean
    , inherit        :: Boolean
    , readonly       :: Boolean
    }
  | ConstantMember
  | SerializerMember
  | IteratorMember
  | OtherNode String

derive instance genericNodeView :: (Generic node) => Generic (NodeView node)

instance showNodeView :: (Generic node) => Show (NodeView node) where
  show = gShow

-- | Unwrap the top level of a node.
toView :: Foreign -> NodeView Foreign
toView = toViewWith id

-- | Unwrap the top level of a node.
toViewWith :: forall node. (Foreign -> node) -> Foreign -> NodeView node
toViewWith fromForeign = fromRight <<< readView <<< toForeign
  where
  readView :: Foreign -> F (NodeView node)
  readView f = do
    _type <- readProp "type" f
    case _type of
      "interface" -> do
        name          <- readProp "name" f
        partial       <- readProp "partial" f
        members       <- map fromForeign <$> readProp "members" f
        inheritance   <- runNullOrUndefined <$> readProp "inheritance" f
        return $ InterfaceNode { name, partial, members, inheritance }
      "implements" -> do
        target        <- readProp "target" f
        implements    <- readProp "implements" f
        return $ ImplementsNode { target, implements }
      "typedef" -> do
        return $ TypeDefNode
      "callback" -> do
        return $ CallbackNode
      "dictionary" -> do
        return $ DictionaryNode
      "exception" -> do
        return $ ExceptionNode
      "operation" -> do
        name          <- readProp "name" f
        arguments     <- map fromForeign <$> readProp "arguments" f
        idlType       <- readProp "idlType" f
        getter        <- readProp "getter" f
        setter        <- readProp "setter" f
        creator       <- readProp "creator" f
        deleter       <- readProp "deleter" f
        legacycaller  <- readProp "legacycaller" f
        static        <- readProp "static" f
        stringifier   <- readProp "stringifier" f
        return $ OperationMember { name, arguments, idlType, getter, setter, creator, deleter, legacycaller, static, stringifier }
      "argument" -> do
        name          <- readProp "name" f
        idlType       <- readProp "idlType" f
        static        <- readProp "static" f
        stringifier   <- readProp "stringifier" f
        inherit       <- readProp "inherit" f
        readonly      <- readProp "readonly" f
        return $ ArgumentMember { name, idlType, static, stringifier, inherit, readonly }
      "constant" -> do
        return ConstantMember
      "serializer" -> do
        return SerializerMember
      "iterator" -> do
        return IteratorMember
      _ -> return $ OtherNode _type

  fromRight (Right view) = view
  fromRight (Left err) = unsafeThrow $ "Unable to parse node: " <> show err
