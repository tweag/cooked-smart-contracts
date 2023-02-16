module Main where

import qualified PMultiSigStatefulSpec
import Test.Tasty

main :: IO ()
main = do
  defaultMain $
    testGroup
      "main"
      [ PMultiSigStatefulSpec.tests ]
