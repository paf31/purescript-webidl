-- | A basic wrapper for the `webidl2` library, and some ADT sugar on top.

module WebIDL
  ( NodeView(..)
  , Type(..)
  , Argument(..)
  , parse
  , toView
  , toViewWith
  , Fix(..)
  , unFix
  , readFully
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
foreign import parse :: String -> Array Foreign

newtype Type = Type
  { sequence  :: Boolean
  , generic   :: Maybe String
  , nullable  :: Boolean
  , array     :: Boolean
  , union     :: Boolean
  }

instance isForeignType :: IsForeign Type where
  read f = do
    sequence  <- readProp "sequence" f
    generic   <- runNullOrUndefined <$> readProp "generic" f
    nullable  <- readProp "nullable" f
    array     <- readProp "array" f
    union     <- readProp "union" f
    return $ Type { sequence, generic, nullable, array, union }

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
  | EnumNode
  | OperationMember
    { name            :: Maybe String
    , arguments       :: Array Argument
    , idlType         :: Type
    , getter          :: Boolean
    , setter          :: Boolean
    , creator         :: Boolean
    , deleter         :: Boolean
    , legacycaller    :: Boolean
    , static          :: Boolean
    , stringifier     :: Boolean
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
      "enum" -> do
        return $ EnumNode
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
      "constant" -> do
        return ConstantMember
      "serializer" -> do
        return SerializerMember
      "iterator" -> do
        return IteratorMember
      _ -> return $ OtherNode _type

  fromRight (Right view) = view
  fromRight (Left err) = unsafeThrow $ "Unable to parse node: " <> show err

-- | Fixed point of the `NodeView` type constructor.
newtype Fix = Fix (NodeView Fix)

unFix :: Fix -> NodeView Fix
unFix (Fix view) = view

derive instance genericFix :: Generic Fix

instance showFix :: Show Fix where
  show = gShow

-- | Read every layer of a node.
readFully :: Foreign -> Fix
readFully f = Fix (toViewWith readFully f)
