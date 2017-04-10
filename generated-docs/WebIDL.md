## Module WebIDL

A basic wrapper for the `webidl2` library, and some ADT sugar on top.

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
Generic Type _
Show Type
```

#### `readType`

``` purescript
readType :: Foreign -> F Type
```

#### `Argument`

``` purescript
newtype Argument
  = Argument { name :: String, idlType :: Type, optional :: Boolean, variadic :: Boolean }
```

##### Instances
``` purescript
Generic Argument _
Show Argument
```

#### `readArgument`

``` purescript
readArgument :: Foreign -> F Argument
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
Generic Member _
Show Member
```

#### `readMember`

``` purescript
readMember :: Foreign -> F Member
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
Generic Node _
Show Node
```

#### `readNode`

``` purescript
readNode :: Foreign -> F Node
```

#### `parse`

``` purescript
parse :: String -> Either (Either Error (NonEmptyList ForeignError)) (Array Node)
```

Parse a WebIDL string.


