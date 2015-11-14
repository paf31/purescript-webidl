## Module WebIDL

A basic wrapper for the `webidl2` library, and some ADT sugar on top.

#### `Node`

``` purescript
data Node :: *
```

A raw node, representing the output from the `webidl2` library.

#### `parse`

``` purescript
parse :: String -> Node
```

Parse a WebIDL string. This function can throw exceptions.

#### `Type`

``` purescript
newtype Type
  = Type { idlType :: String, sequence :: Boolean, generic :: Maybe String, nullable :: Boolean, array :: Boolean, union :: Boolean }
```

##### Instances
``` purescript
instance genericType :: Generic Type
instance showType :: Show Type
```

#### `NodeView`

``` purescript
data NodeView node
  = InterfaceNode { name :: String, partial :: Boolean, members :: Array node, inheritance :: Maybe String }
  | ImplementsNode { target :: String, implements :: String }
  | TypeDefNode
  | CallbackNode
  | DictionaryNode
  | ExceptionNode
  | OperationMember { name :: String, arguments :: Array node, idlType :: Type, getter :: Boolean, setter :: Boolean, creator :: Boolean, deleter :: Boolean, legacycaller :: Boolean, static :: Boolean, stringifier :: Boolean }
  | ArgumentMember { name :: String, idlType :: Type, static :: Boolean, stringifier :: Boolean, inherit :: Boolean, readonly :: Boolean }
  | ConstantMember
  | SerializerMember
  | IteratorMember
  | OtherNode
```

A node represented as a PureScript data type.

##### Instances
``` purescript
instance genericNodeView :: (Generic node) => Generic (NodeView node)
instance showNodeView :: (Generic node) => Show (NodeView node)
```

#### `toView`

``` purescript
toView :: Node -> NodeView Node
```

Unwrap the top level of a node.

#### `toViewWith`

``` purescript
toViewWith :: forall node. (Node -> node) -> Node -> NodeView node
```

Unwrap the top level of a node.


