module Main where

import qualified CrowdfundingSpec
import Test.Tasty
import qualified UseCaseCrowdfundingSpec

main :: IO ()
main = do
  defaultMain $
    testGroup
      "main"
      [ UseCaseCrowdfundingSpec.tests,
        CrowdfundingSpec.tests
      ]
