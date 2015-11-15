module Test.Main where

import Prelude
import Data.Generic
import Control.Monad.Eff
import Control.Monad.Eff.Console
import WebIDL

idl :: String
idl = """
  interface Foo : Bar {
    object baz(string bam);
  };
  """

main :: forall eff. Eff (console :: CONSOLE | eff) Unit
main = do
  let node = parse idl
      view = map readFully node
  print view
