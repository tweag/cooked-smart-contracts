module Main where

import qualified SplitSpec
import Test.Tasty

main :: IO ()
main = do
  defaultMain $
    testGroup
      "main"
      [ SplitSpec.tests
      ]
