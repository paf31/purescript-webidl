module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import WebIDL (parse)

idl :: String
idl = """
  interface Foo : Bar {
    object baz(string bam);
  };
  """

main :: Eff (console :: CONSOLE) Unit
main = logShow (parse idl)
