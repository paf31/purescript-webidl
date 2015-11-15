module WebIDL.ToPureScript where

import Prelude

import Data.Maybe
import Data.Foldable (intercalate)

import WebIDL

-- | Generate PureScript code from a WebIDL node.
toPureScript :: Array Node -> Array String
toPureScript nodes = intercalate [""] $ [preamble] <> bind nodes toDecls
  where
  toDecls :: Node -> Array (Array String)
  toDecls (InterfaceNode interface) = typeDefinition <> bind interface.members toMember
    where
    typeDefinition | interface.partial = []
                   | otherwise = [ [ "data " <> interface.name ] ]

    toMember (OperationMember operation@{ name: Just operationName }) =
      [ [ name <> " :: forall eff. ... -> " <> interface.name <> " -> Eff eff Unit"
        , name <> " = invoke " <> show operationName
        ] ]
      where
      name = "_" <> interface.name <> "_" <> operationName
    toMember (AttributeMember attribute) = getter <> setter
      where
      getter = [ [ name "get" <> " :: forall eff. " <> interface.name <> " -> Eff eff " <> ty
                 , name "get" <> " = getter " <> show attribute.name
                 ] ]
      setter | attribute.readonly = []
             | otherwise = [ [ name "set" <> " :: forall eff. " <> interface.name <> " -> " <> ty <> " -> Eff eff Unit"
                             , name "set" <> " = setter " <> show attribute.name
                             ] ]
      name op = "_" <> interface.name <> "_" <> op <> "_" <> attribute.name
      ty = prettyPrintTypeAtom attribute.idlType
    toMember _ = []
  toDecls _ = []

  preamble :: Array String
  preamble =
    [ "import Control.Monad.Eff (Eff())"
    ]

  prettyPrintTypeAtom :: Type -> String
  prettyPrintTypeAtom (NamedType named) = named.typeName
  prettyPrintTypeAtom (ArrayType array@{ nesting: 0 }) = array.elementType
  prettyPrintTypeAtom ty = "(" <> prettyPrintType ty <> ")"

  prettyPrintType :: Type -> String
  prettyPrintType (NullableType ty) = "Maybe " <> prettyPrintTypeAtom ty
  prettyPrintType (UnionType _) = "Union"
  prettyPrintType (ArrayType array) = "Array " <> prettyPrintTypeAtom (ArrayType { nesting: array.nesting - 1, elementType: array.elementType })
  prettyPrintType (GenericType _) = "Sequence"
  prettyPrintType ty = prettyPrintTypeAtom ty
