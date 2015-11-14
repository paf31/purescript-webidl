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
  | OtherNode

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
      "interface" -> readInterfaceNode f
      _ -> pure OtherNode

  readInterfaceNode :: Foreign -> F (NodeView node)
  readInterfaceNode f = do
    name <- readProp "name" f
    partial <- readProp "partial" f
    members <- map fromForeign <$> readProp "members" f
    inheritance <- runNullOrUndefined <$> readProp "inheritance" f
    return $ InterfaceNode { name, partial, members, inheritance }

  fromRight (Right view) = view
  fromRight (Left err) = unsafeThrow $ "Unable to parse node: " <> show err
