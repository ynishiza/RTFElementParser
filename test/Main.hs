module Main (main) where

import Test.Hspec
import Spec qualified

main :: IO ()
main =
  hspec Spec.spec
