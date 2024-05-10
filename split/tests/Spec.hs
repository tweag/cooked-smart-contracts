module Main where

import qualified SplitSpec
import qualified SplitUPLCSpec
import Test.Tasty

main :: IO ()
main = do
  defaultMain $
    testGroup
      "main"
      [ SplitSpec.tests -- ,
      -- SplitUPLCSpec.tests
      ]
