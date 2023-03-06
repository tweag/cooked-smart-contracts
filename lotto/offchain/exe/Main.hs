module Main where

import qualified Cooked
import qualified Cooked.MockChain.Testing as Testing
import qualified Cooked.MockChain.Staged
import Data.Default (def)
import qualified Lib
import qualified Scenarii
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase)
import Prelude

testOnchainFails :: (Testing.IsProp prop, Show a) => Cooked.MockChain.Staged.StagedMockChain a -> prop
testOnchainFails = Testing.testFailsFrom' def (Testing.isCekEvaluationFailure def) def

tests :: TestTree
tests =
  testGroup
    "Lotto tests"
    [ testCase "Don't gamble enough" . testOnchainFails $
        Scenarii.alicePlaysAlone def "salt" "secret" "guess" (Just $ Lib.ada 9),
      testCase "Play without seal" . testOnchainFails $
        Scenarii.playFoul def "salt" "secret" "guess" Nothing,
      testCase "Alice plays alone" . Testing.testSucceeds def $
        Scenarii.alicePlaysAlone def "salt" "secret" "guess" (Just $ Lib.ada 11),
      testCase "Alice plays alone with malformed guess" . Testing.testSucceeds def $
        Scenarii.alicePlaysAloneMalformed def "salt" "secret" (Just $ Lib.ada 11),
      -- testCase "Alice plays alone with malformed guess and tries to resolve" . testOnchainFails $
      --   Scenarii.alicePlaysAloneWithMalformedGuessAndTriesToResolve def "salt" "secret" (Lib.ada 11),
      testCase "Administration must sign seal mint" . testOnchainFails $
        Scenarii.aliceTriesToMint def,
      testCase "Administration must sign seal mint (other manner)" . testOnchainFails $
        Scenarii.aliceTriesToMintAgain def,
      testCase "Two players" . Testing.testSucceeds def $
        Scenarii.aliceAndBobPlay def "salt" "secret",
      testCase "Alice plays for Bob" . Testing.testSucceeds def $
        Scenarii.alicePlaysForBob def,
      testCase "Failing double satisfaction" . testOnchainFails $
        Scenarii.doubleSatisfaction def "sharedSalt" "sharedSecret",
      testCase "Double satisfaction (resolution)" . Testing.testSucceeds def $
        Scenarii.doubleSatisfactionResolution "sharedSalt" "sharedSecret",
      testCase "Playing with a reference script" . Testing.testSucceeds def $
        Scenarii.alicePlaysWithRF,
      -- Automated attacks
      testCase "Token duplication" . testOnchainFails $
        Scenarii.tryTokenDuplication
    ]

main :: IO ()
main = defaultMain tests
