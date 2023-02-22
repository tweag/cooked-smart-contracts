{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module SplitUPLCSpec where

import Cooked.MockChain
import qualified Plutus.Script.Utils.V1.Typed.Scripts.Validators as Scripts
import qualified Split
import Split.OffChain
import Split.ToUPLC (splitBS)
import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.HUnit

-- | Parameters to share 400 among wallets 2 and 3
lockParams :: Split.SplitDatum
lockParams =
  Split.SplitDatum
    { Split.recipient1 = walletPKHash (wallet 2),
      Split.recipient2 = walletPKHash (wallet 3),
      Split.amount = 20_000_000
    }

tests :: TestTree
tests =
  testGroup
    "SplitSpec imported from UPLC"
    [ testCase "Simple example succeeds" $
        testSucceeds $ do
          script <- case unsafeTypedValidatorFromBS splitBS of
            Left _ -> fail "couldn't load the Split contract from its binary repr"
            Right r -> return r
          txLock script lockParams `as` wallet 1
          txUnlock script `as` wallet 2,
      -- This is marked as an expected failure until we sort issue 57 out
      expectFail $
        testCase "Same address as compiled script" $
          case unsafeTypedValidatorFromBS @Split.Split splitBS of
            Left _ -> assertFailure "couldn't load the Split contract from its binary repr"
            Right res ->
              let defAddr = Scripts.validatorAddress Split.splitValidator
                  bsAddr = Scripts.validatorAddress res
               in defAddr @=? bsAddr
    ]
