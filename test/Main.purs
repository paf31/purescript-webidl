module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Console (logShow)
import WebIDL (parse)

interface :: String
interface = """
  interface Foo {
  };
  """

interfaceOperation :: String
interfaceOperation = """
  interface Foo : Bar {
    object baz(string bam);
  };
  """

main :: Effect Unit
main = logShow (parse interface)
