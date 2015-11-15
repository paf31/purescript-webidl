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

#### `Member`

``` purescript
data Member
  = OperationMember { name :: Maybe String, arguments :: Array Argument, getter :: Boolean, setter :: Boolean, creator :: Boolean, deleter :: Boolean, legacycaller :: Boolean, static :: Boolean, stringifier :: Boolean, idlType :: Type }
  | AttributeMember { name :: String, inherit :: Boolean, static :: Boolean, stringifier :: Boolean, readonly :: Boolean, idlType :: Type }
  | ConstantMember { name :: String, idlType :: String, nullable :: Boolean }
  | FieldMember { name :: String, required :: Boolean, idlType :: Type }
  | OtherMember String
```

##### Instances
``` purescript
Generic Member
Show Member
IsForeign Member
```

#### `Node`

``` purescript
data Node
  = InterfaceNode { name :: String, partial :: Boolean, members :: Array Member, inheritance :: Maybe String }
  | ImplementsNode { target :: String, implements :: String }
  | TypeDefNode { name :: String, idlType :: Type }
  | CallbackNode { name :: String, idlType :: Type, arguments :: Array Argument }
  | DictionaryNode { name :: String, partial :: Boolean, members :: Array Member, inheritance :: Maybe String }
  | ExceptionNode { name :: String, members :: Array Member, inheritance :: Maybe String }
  | EnumNode { name :: String, values :: Array String }
  | OtherNode String
```

A node represented as a PureScript data type.

##### Instances
``` purescript
Generic Node
Show Node
IsForeign Node
```

#### `toNode`

``` purescript
toNode :: Foreign -> Node
```

Unwrap the top level of a node.


