-- | A basic wrapper for the `webidl2` library, and some ADT sugar on top.

module WebIDL
  ( Node()
  , NodeView(..)
  , Type(..)
  , parse
  , toView
  , toViewWith
  ) where

import Prelude

import Data.Maybe
import Data.Generic

-- | A raw node, representing the output from the `webidl2` library.
foreign import data Node :: *

-- | Parse a WebIDL string. This function can throw exceptions.
foreign import parse :: String -> Node

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
toView :: Node -> NodeView Node
toView = toViewWith id

-- | Unwrap the top level of a node.
toViewWith :: forall node. (Node -> node) -> Node -> NodeView node
toViewWith n = toViewWith n
