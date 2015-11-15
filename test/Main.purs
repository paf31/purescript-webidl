module Test.Main where

import Prelude
import Data.Generic
import Control.Monad.Eff
import Control.Monad.Eff.Console (CONSOLE(), print)
import Control.Monad.Eff.Exception (EXCEPTION())
import WebIDL

idl :: String
idl = """
  interface Foo : Bar {
    object baz(string bam);
  };
  """

main :: forall eff. Eff (console :: CONSOLE, err :: EXCEPTION | eff) Unit
main = do
  nodes <- parse idl
  print nodes
