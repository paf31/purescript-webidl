## Module WebIDL

A basic wrapper for the `webidl2` library, and some ADT sugar on top.

#### `parse`

``` purescript
parse :: String -> Array Foreign
```

Parse a WebIDL string. This function can throw exceptions.

#### `Type`

``` purescript
data Type
  = UnionType { unifies :: Array Type }
  | ArrayType { nesting :: Int, elementType :: String }
  | NamedType { typeName :: String }
  | GenericType { family :: String, typeArgument :: Type }
  | NullableType Type
```

##### Instances
``` purescript
IsForeign Type
Generic Type
Show Type
```

#### `Argument`

``` purescript
newtype Argument
  = Argument { name :: String, idlType :: Type, optional :: Boolean, variadic :: Boolean }
```

##### Instances
``` purescript
IsForeign Argument
Generic Argument
Show Argument
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
  | EnumNode
  | OperationMember { name :: Maybe String, arguments :: Array Argument, getter :: Boolean, setter :: Boolean, creator :: Boolean, deleter :: Boolean, legacycaller :: Boolean, static :: Boolean, stringifier :: Boolean, idlType :: Type }
  | AttributeMember { name :: String, inherit :: Boolean, static :: Boolean, stringifier :: Boolean, readonly :: Boolean, idlType :: Type }
  | ConstantMember
  | SerializerMember
  | IteratorMember
  | OtherNode String
```

A node represented as a PureScript data type.

##### Instances
``` purescript
(Generic node) => Generic (NodeView node)
(Generic node) => Show (NodeView node)
```

#### `toView`

``` purescript
toView :: Foreign -> NodeView Foreign
```

Unwrap the top level of a node.

#### `toViewWith`

``` purescript
toViewWith :: forall node. (Foreign -> node) -> Foreign -> NodeView node
```

Unwrap the top level of a node.

#### `Fix`

``` purescript
newtype Fix
  = Fix (NodeView Fix)
```

Fixed point of the `NodeView` type constructor.

##### Instances
``` purescript
Generic Fix
Show Fix
```

#### `unFix`

``` purescript
unFix :: Fix -> NodeView Fix
```

#### `readFully`

``` purescript
readFully :: Foreign -> Fix
```

Read every layer of a node.


