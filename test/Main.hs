module Main (
  main,
) where

import Properties qualified
import Spec qualified
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.Hspec

main :: IO ()
main = do
  specTree <- testSpecs $ describe "Hspec" $ do
    Spec.spec

  defaultMain
    $ testGroup "main"
    $ [fromGroup Properties.groups]
    <> specTree
