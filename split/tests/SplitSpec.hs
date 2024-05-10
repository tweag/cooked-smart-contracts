{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}

module SplitSpec where

import qualified Cooked
import Data.Default
import Optics.Core
import qualified Plutus.Script.Utils.Ada as Script
import qualified PlutusLedgerApi.V3 as Api
import qualified PlutusTx.Prelude as PlutusTx
import Split (SplitDatum (SplitDatum), splitValidator)
import Split.OffChain (txLock, txUnlock)
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.ExpectedFailure as Tasty
import qualified Test.Tasty.HUnit as Tasty

-- * Setup

alice, bob, carol, dave, eve :: Cooked.Wallet
alice = Cooked.wallet 1
bob = Cooked.wallet 2
carol = Cooked.wallet 3
dave = Cooked.wallet 4
eve = Cooked.wallet 5

alicePkh, bobPkh, carolPkh, davePkh, evePkh :: Api.PubKeyHash
alicePkh = Cooked.walletPKHash alice
bobPkh = Cooked.walletPKHash bob
carolPkh = Cooked.walletPKHash carol
davePkh = Cooked.walletPKHash dave
evePkh = Cooked.walletPKHash eve

initDist :: Cooked.InitialDistribution
initDist =
  Cooked.distributionFromList $
    [ (alice, [Script.lovelaceValueOf 100_000_000]),
      (bob, [Script.lovelaceValueOf 100_000_000]),
      (carol, [Script.lovelaceValueOf 100_000_000]),
      (dave, [Script.lovelaceValueOf 100_000_000]),
      (eve, [Script.lovelaceValueOf 100_000_000])
    ]

pcOpts :: Cooked.PrettyCookedOpts
pcOpts =
  def
    { Cooked.pcOptHashes =
        def
          { Cooked.pcOptHashNames =
              Cooked.hashNamesFromList
                [ (alicePkh, "Alice"),
                  (bobPkh, "Bob"),
                  (carolPkh, "Carol"),
                  (davePkh, "Dave"),
                  (evePkh, "Eve")
                ]
                <> Cooked.hashNamesFromList [(splitValidator, "Split")]
                <> Cooked.defaultHashNames
          }
    }

-- * Tests cases

testSuccessfulUsage :: Tasty.TestTree
testSuccessfulUsage =
  Tasty.testCase
    "Simple example succeeds"
    ( Cooked.testSucceedsFrom' pcOpts predicate initDist $ do
        splitTxOutRef <- txLock alice (SplitDatum bobPkh carolPkh 40_000_001)
        (bobTxOutRef, carolTxOutRef) <- txUnlock splitTxOutRef dave
        Just valueBob <- Cooked.valueFromTxOutRef bobTxOutRef
        Just valueCarol <- Cooked.valueFromTxOutRef carolTxOutRef
        return (valueBob, valueCarol)
    )
  where
    predicate (valueBob, valueCarol) _ =
      Cooked.testBool $
        valueBob == Script.lovelaceValueOf 20_000_000
          && valueCarol == Script.lovelaceValueOf 20_000_001

testOneGetsAll :: Tasty.TestTree
testOneGetsAll =
  Tasty.testCase
    "Cannot give everything to one recipient"
    ( Cooked.testFailsFrom pcOpts (Cooked.isCekEvaluationFailure def) initDist $ do
        splitTxOutRef <- txLock alice (SplitDatum bobPkh carolPkh 40_000_000)
        _ <- Cooked.withTweak (txUnlock splitTxOutRef dave) $ do
          _ <-
            Cooked.removeOutputTweak
              ( \(Cooked.Pays output) ->
                  Cooked.outputAddress output == Cooked.walletAddress carol
              )
          Cooked.overTweak
            (Cooked.txSkelOutsL % _head % Cooked.txSkelOutValueL)
            (<> Script.lovelaceValueOf 20_000_000)
        return ()
    )

testPartialUnlocks :: Tasty.TestTree
testPartialUnlocks =
  Tasty.testCase
    "Cannot unlock in multiple steps"
    ( Cooked.testFailsFrom pcOpts (Cooked.isCekEvaluationFailure def) initDist $ do
        splitTxOutRef <- txLock alice (SplitDatum bobPkh carolPkh 40_000_000)
        let halfUnlock =
              Cooked.withTweak (txUnlock splitTxOutRef dave) $ do
                Cooked.overTweak
                  (Cooked.txSkelOutsL % element 1 % Cooked.txSkelOutValueL)
                  (<> PlutusTx.negate (Script.lovelaceValueOf 10_000_000))
                Cooked.overTweak
                  (Cooked.txSkelOutsL % element 2 % Cooked.txSkelOutValueL)
                  (<> PlutusTx.negate (Script.lovelaceValueOf 10_000_000))
        _ <- halfUnlock
        _ <- halfUnlock
        return ()
    )

testDoubleSatisfaction :: Tasty.TestTree
testDoubleSatisfaction =
  -- The Split validator is actually vulnerable to double satisfaction attacks.
  -- This test is marked as an expected failure for illustration purposes. In
  -- the real world, this would be an actual failure to investigate.
  Tasty.expectFail $
    Tasty.testCase
      "Split is resilient to double satisfaction attack"
      ( Cooked.testFailsFrom pcOpts (Cooked.isCekEvaluationFailure def) initDist $ do
          splitTxOutRef1 <- txLock alice (SplitDatum bobPkh davePkh 40_000_000)
          splitTxOutRef2 <- txLock alice (SplitDatum carolPkh davePkh 40_000_000)
          _ <- Cooked.withTweak (txUnlock splitTxOutRef1 eve) $ do
            -- Remove Dave's share he gets from first split
            _ <-
              Cooked.removeOutputTweak
                ( \(Cooked.Pays output) ->
                    Cooked.outputAddress output == Cooked.walletAddress dave
                )
            -- Spend second split
            Cooked.addInputTweak splitTxOutRef2 (Cooked.TxSkelRedeemerForScript ())
            -- Pay Carol and Dave as expected
            Cooked.addOutputTweak (Cooked.paysPK carolPkh (Script.lovelaceValueOf 20_000_000))
            Cooked.addOutputTweak (Cooked.paysPK davePkh (Script.lovelaceValueOf 20_000_000))
            -- Eve takes the share Dave was owned in the first split
            Cooked.addOutputTweak (Cooked.paysPK evePkh (Script.lovelaceValueOf 20_000_000))
          return ()
      )

-- * Test suite

tests :: Tasty.TestTree
tests =
  Tasty.testGroup
    "SplitSpec"
    [ testSuccessfulUsage,
      testOneGetsAll,
      testPartialUnlocks,
      testDoubleSatisfaction
    ]
