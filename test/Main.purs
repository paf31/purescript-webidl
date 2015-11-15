module Test.Main where

import Prelude

import Data.Traversable (traverse)

import Control.Monad.Eff
import Control.Monad.Eff.Console (CONSOLE(), log)
import Control.Monad.Eff.Exception (EXCEPTION())

import WebIDL
import WebIDL.ToPureScript

idl :: String
idl = """
  interface Foo : Bar {
    object baz(string bam);
  };
  """

main :: forall eff. Eff (console :: CONSOLE, err :: EXCEPTION | eff) Unit
main = void do
  nodes <- parse idl
  let code = toPureScript nodes
  traverse log code
