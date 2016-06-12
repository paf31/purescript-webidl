module Test.Main where

import Prelude (Unit, bind)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Monad.Eff.Exception (EXCEPTION)
import WebIDL (parse)

idl :: String
idl = """
  interface Foo : Bar {
    object baz(string bam);
  };
  """

main :: forall eff. Eff (console :: CONSOLE, err :: EXCEPTION | eff) Unit
main = do
  nodes <- parse idl
  logShow nodes
