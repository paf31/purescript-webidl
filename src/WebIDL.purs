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

import Control.Alt ((<|>))
import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)

-- | Parse a WebIDL string. This function can throw exceptions.
foreign import parse :: String -> Array Foreign

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
      "const" -> do
        return ConstantMember
      "attribute" -> do
        name          <- readProp "name" f
        inherit       <- readProp "inherit" f
        static        <- readProp "static" f
        stringifier   <- readProp "stringifier" f
        readonly      <- readProp "readonly" f
        idlType       <- readProp "idlType" f
        return $ AttributeMember { name, inherit, static, stringifier, readonly, idlType }
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
