{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module AuctionSpec where

import qualified Auction as A
import qualified Auction.Offchain as A
import Control.Applicative
import Control.Monad
import Cooked
import Data.Default
import Data.List (isPrefixOf)
import qualified Data.Map as Map
import Data.Maybe
import Optics.Core
import qualified Plutus.Script.Utils.Ada as Script
import qualified Plutus.Script.Utils.Scripts as Script
import qualified Plutus.Script.Utils.V3.Typed.Scripts as Script
import qualified Plutus.Script.Utils.Value as Script
import qualified PlutusLedgerApi.V1.Interval as Api
import qualified PlutusLedgerApi.V3 as Api
import qualified PlutusTx.Numeric as PlutusTx
import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.HUnit

-- * Parameters and initial distributions

-- A few peers
alice, bob, carrie, david, eve :: Wallet
alice = wallet 1
bob = wallet 2
carrie = wallet 3
david = wallet 4
eve = wallet 5

aliceAddr, bobAddr, carrieAddr, davidAddr, eveAddr :: Api.Address
aliceAddr = walletAddress alice
bobAddr = walletAddress bob
carrieAddr = walletAddress carrie
davidAddr = walletAddress david
eveAddr = walletAddress eve

-- Just so we have something to sell in our auction that's not Ada:
-- Have a banana.

bananaAssetClass :: Script.AssetClass
bananaAssetClass = permanentAssetClass "Banana"

-- | Value representing a number of bananas
banana :: Integer -> Script.Value
banana = Script.assetClassValue bananaAssetClass

-- | How many bananas are in the given value? This is a left inverse of 'banana'.
bananasIn :: Script.Value -> Integer
bananasIn v = Script.assetClassValueOf v bananaAssetClass

-- | initial distribution s.t. everyone owns five bananas
testInit :: InitialDistribution
testInit = distributionFromList [(i, (ada 10 <> banana 5) : replicate 5 (ada 100)) | i <- knownWallets]

-- * Successful single-trace runs

-- These runs use the transactions from Auction.Offchain as they are meant to be
-- used.

hammerToWithdraw :: (MonadBlockChain m) => m ()
hammerToWithdraw = do
  offerOref <- A.txOffer alice (banana 2) 30_000_000
  A.txHammer alice offerOref

noBids :: (MonadBlockChain m) => m ()
noBids = do
  (_, t0) <- currentTime
  let deadline = t0 + 60_000
  offerOref <- A.txOffer alice (banana 2) 30_000_000
  void $ A.txSetDeadline alice offerOref deadline
  deadlineSlot <- getEnclosingSlot deadline
  void $ awaitSlot $ deadlineSlot + 1
  A.txHammer alice offerOref
  return ()

oneBid :: (MonadBlockChain m) => m ()
oneBid = do
  (_, t0) <- currentTime
  let deadline = t0 + 60_000
  offerOref <- A.txOffer alice (banana 2) 30_000_000
  void $ A.txSetDeadline alice offerOref deadline
  void $ A.txBid bob offerOref 30_000_000
  deadlineSlot <- getEnclosingSlot deadline
  void $ awaitSlot $ deadlineSlot + 1
  A.txHammer carrie offerOref -- It doesn't matter which wallet hammers here

twoBids :: (MonadBlockChain m) => m ()
twoBids = do
  (_, t0) <- currentTime
  let deadline = t0 + 60_000
  offerOref <- A.txOffer alice (banana 2) 30_000_000
  void $ A.txSetDeadline alice offerOref deadline
  void $ A.txBid bob offerOref 30_000_000
  void $ A.txBid carrie offerOref 40_000_000
  deadlineSlot <- getEnclosingSlot deadline
  void $ awaitSlot $ deadlineSlot + 1
  A.txHammer alice offerOref

twoAuctions :: (MonadBlockChain m) => m ()
twoAuctions = do
  (_, t0) <- currentTime
  let deadline1 = t0 + 60_000
      deadline2 = t0 + 90_000
  offerOref1 <- A.txOffer alice (banana 2) 30_000_000
  offerOref2 <- A.txOffer alice (banana 3) 50_000_000
  void $ A.txSetDeadline alice offerOref1 deadline1
  void $ A.txSetDeadline alice offerOref2 deadline2
  void $ A.txBid bob offerOref1 30_000_000
  void $ A.txBid carrie offerOref2 50_000_000
  void $ A.txBid david offerOref2 60_000_000
  deadline1Slot <- getEnclosingSlot deadline1
  void $ awaitSlot $ deadline1Slot + 1
  A.txHammer alice offerOref1
  deadline2Slot <- getEnclosingSlot deadline2
  void $ awaitSlot $ deadline2Slot + 1
  A.txHammer alice offerOref2

successfulSingle :: TestTree
successfulSingle =
  testGroup
    "Successful single-trace runs"
    [ testCase "hammer to withdraw" $ testSucceedsFrom def testInit hammerToWithdraw,
      testCase "zero bids" $ testSucceedsFrom def testInit noBids,
      testCase "one bid" $ testSucceedsFrom def testInit oneBid,
      testCase "two bids on the same auction" $
        testSucceedsFrom'
          def
          (\_ s -> testBool $ 7 == bananasIn (holdsInState carrieAddr s))
          testInit
          twoBids,
      testCase
        "two concurrent auctions"
        $ testSucceedsFrom'
          def
          ( \_ s ->
              testBool (7 == bananasIn (holdsInState bobAddr s))
                .&&. testBool (8 == bananasIn (holdsInState davidAddr s))
          )
          testInit
          twoAuctions
    ]

-- * Failing single-trace runs

failingOffer :: (MonadBlockChain m) => m ()
failingOffer =
  void $
    A.txOffer bob (banana 100) 20_000_000

forbiddenHammerToWithdraw :: (MonadBlockChain m) => m ()
forbiddenHammerToWithdraw = do
  offerOref <- A.txOffer alice (banana 2) 30_000_000
  A.txHammer bob offerOref

failingTwoBids :: (MonadBlockChain m) => m ()
failingTwoBids = do
  (_, t0) <- currentTime
  let deadline = t0 + 60_000
  offerOref <- A.txOffer alice (banana 2) 30_000_000
  void $ A.txSetDeadline alice offerOref deadline
  void $ A.txBid bob offerOref 30_000_000
  void $ A.txBid carrie offerOref 30_000_000
  deadlineSlot <- getEnclosingSlot deadline
  void $ awaitSlot $ deadlineSlot + 1
  A.txHammer alice offerOref

failingSingle :: TestTree
failingSingle =
  testGroup
    "Single-trace runs that are expected to fail"
    [ testCase "opening banana auction while owning too few bananas" $
        testFailsFrom def (const $ testBool True) testInit failingOffer,
      testCase "wrong user hammers to withdraw" $
        testFailsFrom def (isCekEvaluationFailure def) testInit forbiddenHammerToWithdraw,
      testCase "second bid not higher than first" $
        testFailsFrom def (isCekEvaluationFailure def) testInit failingTwoBids
    ]

-- * failing attacks

simpleTraces :: (Alternative m, MonadBlockChain m) => m ()
simpleTraces = noBids <|> oneBid <|> twoBids <|> twoAuctions

-- | Token duplication attack: Whenever we see a transaction that mints
-- something, try to mint one more token and pay it to the attacker. This should
-- be ruled out by the minting policy of the thread token.
tryDupTokens :: (Alternative m, MonadModalBlockChain m) => m ()
tryDupTokens =
  somewhere
    ( dupTokenAttack
        (\_ n -> n + 1) -- the modification of the minted value
        eve -- the attacker's wallet
    )
    simpleTraces

-- | Datum hijacking attack: Try to steal outputs from a validator.
tryDatumHijack :: (Alternative m, MonadModalBlockChain m) => m ()
tryDatumHijack =
  somewhere
    ( datumHijackingAttack @A.Auction
        ( \(ConcreteOutput _ _ _ txSkelOutDatum _) -> case txSkelOutTypedDatum txSkelOutDatum of
            -- try to steal all outputs that have the 'Bidding' datum, no matter
            -- their validator or value.
            Just A.Bidding {} -> True
            -- try to steal during the 'SetDeadline' transaction. This
            -- vulnerability existed before PR #161.
            Just A.NoBids {} -> True
            _ -> False
        )
        (0 ==) -- if there is more than one 'Bidding' output, try stealing only the first
    )
    simpleTraces

-- | Double satisfaction attack. This attack tries to add extra 'Bid' inputs to
-- transactions that already 'Bid'.
tryDoubleSat :: (Alternative m, MonadModalBlockChain m) => m ()
tryDoubleSat =
  somewhere
    ( doubleSatAttack
        (map (: []))
        (txSkelInsL % itraversed)
        ( \_ redeemer ->
            case txSkelTypedRedeemer @A.Auction redeemer of
              Just (A.Bid (A.BidderInfo _bid bidder)) -> do
                extraUtxos <-
                  runUtxoSearch $
                    utxosAtSearch (Script.validatorAddress A.auctionValidator)
                      `filterWith` resolveDatum
                      `filterWithPure` isOutputWithInlineDatumOfType @A.AuctionState
                return $
                  mapMaybe
                    ( \(oref, output) -> case output ^. outputDatumL of
                        (A.NoBids _ minBid _) ->
                          let red = TxSkelRedeemerForScript (A.Bid (A.BidderInfo minBid bidder))
                           in Just (red, (Map.singleton oref red, [], mempty))
                        (A.Bidding _ _ (A.BidderInfo prevBid _)) ->
                          let red = TxSkelRedeemerForScript (A.Bid (A.BidderInfo (prevBid + 10_000_000) bidder))
                           in Just (red, (Map.singleton oref red, [], mempty))
                        _ -> Nothing
                    )
                    extraUtxos
              _ -> return []
        )
        -- pay the surplus to wallet 6
        eve
    )
    simpleTraces

-- | datum tampering attack that tries to change the seller to wallet 6 on every
-- datum but 'Offer' (which is any time we pay to the 'auctionValidator' and
-- there are actual checks happening).
tryTamperDatum :: (Alternative m, MonadModalBlockChain m) => m ()
tryTamperDatum =
  somewhere
    ( tamperDatumTweak @A.AuctionState
        ( \case
            A.NoBids _ minBid deadline ->
              Just $ A.NoBids (walletPKHash $ wallet 6) minBid deadline
            A.Bidding _ deadline bidderInfo ->
              Just $ A.Bidding (walletPKHash $ wallet 6) deadline bidderInfo
            _ -> Nothing
        )
    )
    simpleTraces

failingAttacks :: TestTree
failingAttacks =
  testGroup
    "failing attacks"
    [ testCase "token duplication" $
        testFailsFrom
          def
          -- Ensure that the trace fails and gives back an error message satisfying a specific condition
          ( isCekEvaluationFailureWithMsg
              def
              (\msg -> "not minting or burning" `isPrefixOf` msg || "Hammer does not burn" `isPrefixOf` msg)
          )
          testInit
          tryDupTokens,
      testCase "datum hijacking" $
        testFailsFrom
          def
          (isCekEvaluationFailure def)
          testInit
          tryDatumHijack,
      testCase "datum tampering" $
        testFailsFrom
          def
          (isCekEvaluationFailure def)
          testInit
          tryTamperDatum,
      testCase "double satisfaction" $
        testFailsFrom
          def
          (isCekEvaluationFailure def)
          testInit
          tryDoubleSat
    ]

-- * Known successful attacks and exploits

-- | Try to mint an additional token of the token name "exampleTokenName"
-- whenever anything is minted.
tryAddToken :: (Alternative m, MonadModalBlockChain m) => m ()
tryAddToken =
  somewhere
    ( addTokenAttack
        (const [(Script.TokenName "exampleTokenName", 1)])
        eve
    )
    simpleTraces

-- | This trace exploits the fact, discovered with the 'addTokenAttack' above,
-- that one can mint extra tokens on the 'SetDeadline' transaction, in order to
-- steal a bid from one auction with a separate auction.
exploitAddToken :: (MonadModalBlockChain m) => m ()
exploitAddToken = do
  -- Alice makes an offer (for a big amount of bananas).
  aliceOfferOref <- A.txOffer alice (banana 5) 50_000_000
  let aliceNftTokenName = A.tokenNameFromTxOutRef aliceOfferOref
      aliceNft = A.threadToken aliceOfferOref
  -- Eve sees Alice's offer and quickly makes an offer, for which she immediately
  -- sets the deadline. (As you can see, Eve can be very cheap, her offer
  -- contains nothing at all, and the minimum bid is 1 Lovelace!) On the
  -- 'setDeadline' transaction, she uses the fact that one can mint extra tokens
  -- in order to mint an extra token of Alice's auction's thread token asset
  -- class.
  eveOfferOref <- A.txOffer eve mempty 1
  (_, t0) <- currentTime
  let eveDeadline = t0 + 60_000
  void $
    A.txSetDeadline eve eveOfferOref eveDeadline
      `withTweak` ( do
                      addMintTweak
                        ( Script.Versioned A.threadTokenPolicy Script.PlutusV2,
                          SomeMintsRedeemer eveOfferOref, -- Use the redeemer that is already on the transaction!
                          aliceNftTokenName,
                          1
                        )
                      addOutputTweak $ paysPK (walletPKHash eve) aliceNft
                  )

  -- Eve bids on her own offer, and also pays her forged NFT for Alice's auction
  -- on the same UTxO. This means that now there is a UTxO at the
  -- 'auctionValidator' which is identified by two NFTs: one for Alice's
  -- auction, and one for Eve's.
  void $
    A.txBid eve eveOfferOref 1
      `withTweak` overTweak
        ( singular $
            txSkelOutsL
              % traversed
              % txSkelOutOwnerTypeP @(Script.TypedValidator A.Auction)
              % outputValueL
        )
        (<> aliceNft)

  -- Bob thinks he's bidding for Alice's auction (which is not even opened
  -- yet!). In fact, his money is put into Eve's auction.
  void $ A.txBid bob aliceOfferOref 40_000_000

  -- After the deadline of Eve's auction, anyone can hammer it, and this will
  -- pay Bob's money to Eve, while Bob will only get the forged NFT in exchange.
  slotEveDeadline <- getEnclosingSlot eveDeadline
  void $ awaitSlot $ slotEveDeadline + 1
  A.txHammer eve eveOfferOref

-- | This trace exploits the double satifsaction vulnerability in 'validBid' to
-- steal a bid. The idea is to bid on two auctions that currently have the same
-- highest bidder Bob, but only return one of the two bids to Bob in doing so.
--
-- The trace also illustrates that 'doubleSatAttack' is only as good as its
-- user, and also strongly depends on the variety of the scenarios described by
-- the 'simpleTraces'. We have here a double satisfaction scenario that simply
-- didn't come up as one of the cases tried by the 'doubleSatAttack' above.
exploitDoubleSat :: (MonadModalBlockChain m) => m ()
exploitDoubleSat = do
  -- Alice opens two auctions and sets the deadlines (it does not matter that
  -- they both belong to her, this vulnerability applies to any two auctions)
  offer1 <- A.txOffer alice (banana 2) 40_000_000
  offer2 <- A.txOffer alice (banana 3) 60_000_000
  (_, t0) <- currentTime
  let t1 = t0 + 60_000
      t2 = t0 + 90_000
  void $ A.txSetDeadline alice offer1 t1
  void $ A.txSetDeadline alice offer2 t2
  -- People now bid on the auctions until Bob is the highest bidder on both
  -- auctions.
  void $ A.txBid bob offer1 40_000_000
  void $ A.txBid bob offer2 60_000_000
  -- The UTxO at the first auction that represents the current state:
  [(theLastBidOref, theLastBidOutput)] <-
    runUtxoSearch $
      utxosAtSearch (Script.validatorAddress A.auctionValidator)
        `filterWithPred` ((`Script.geq` A.threadToken offer1) . outputValue)
  -- Eve now bids on the second auction. Among other things this ensures that
  -- there's an output containing 40_000_000 Lovelace to Bob on the
  -- transaction. This means that, if she simultaneously bids on the first
  -- auction, she can fool the validator of the first auction, which expects an
  -- output of at least 20_000_000 Lovelace to go to Bob. She can keep this
  -- money to herself, effectively stealing Bob's bid on the first auction.
  void $
    A.txBid eve offer2 70_000_000
      `withTweak` ( do
                      t1slot <- getEnclosingSlot t1 <&> (+ (-1))
                      overTweak txSkelValidityRangeL (`Api.intersection` Api.to t1slot)
                      addInputTweak theLastBidOref $
                        TxSkelRedeemerForScript
                          (A.Bid $ A.BidderInfo 50_000_000 (walletPKHash eve))
                      addOutputTweak $
                        paysScript
                          A.auctionValidator
                          (A.Bidding (walletPKHash alice) t1 (A.BidderInfo 50_000_000 (walletPKHash eve)))
                          ( outputValue theLastBidOutput
                              <> PlutusTx.negate (Script.lovelaceValueOf 40_000_000) -- subtract Bob's bid
                              <> Script.lovelaceValueOf 50_000_000 -- add Eve's bid
                          )
                      addOutputTweak $
                        paysPK
                          (walletPKHash eve)
                          (Script.lovelaceValueOf 50_000_000)
                  )
  -- Both auctions are closed normally. Eve is the highest bidder on both of
  -- them.
  slotT1 <- getEnclosingSlot t1
  void $ awaitSlot slotT1
  void $ A.txHammer eve offer1
  slotT2 <- getEnclosingSlot t2
  void $ awaitSlot slotT2
  A.txHammer eve offer2

successfulAttacks :: TestTree
successfulAttacks =
  testGroup "successful attacks and exploits" $
    map
      expectFail
      [ testCase "adding extra tokens" $
          testFailsFrom
            def
            (isCekEvaluationFailure def)
            testInit
            tryAddToken,
        testCase "exploit extra tokens to steal a bid" $
          testFailsFrom
            def
            (isCekEvaluationFailure def)
            testInit
            exploitAddToken,
        testCase "exploit double satisfaction to steal a bid" $
          testFailsFrom
            def
            (isCekEvaluationFailure def)
            testInit
            exploitDoubleSat
      ]

-- * Comparing two outcomes with 'testBinaryRelatedBy'

-- Produce two outcomes, which differ only by who the (only) bidder in the
-- auction was. Then test that the sellers and buyers in both "worlds" have paid
-- the same amounts.

bidderAlternativeTrace :: (Alternative m, MonadBlockChain m) => m ()
bidderAlternativeTrace = do
  (_, t0) <- currentTime
  let deadline = t0 + 60_000
  offerOref <- A.txOffer alice (banana 2) 30_000_000
  void $ A.txSetDeadline alice offerOref deadline
  void $ A.txBid bob offerOref 30_000_000 <|> A.txBid carrie offerOref 30_000_000
  deadlineSlot <- getEnclosingSlot deadline
  void $ awaitSlot $ deadlineSlot + 1
  A.txHammer alice offerOref

bidderAlternative :: TestTree
bidderAlternative =
  testCase "change in possessions independent of bidder" $
    testBinaryRelatedBy
      def
      ( \a b ->
          testBool $
            holdsInState aliceAddr a == holdsInState aliceAddr b
              && holdsInState bobAddr a == holdsInState carrieAddr b
      )
      testInit
      bidderAlternativeTrace

-- * Collecting all the tests in this module

miscTests :: TestTree
miscTests =
  testGroup
    "Miscellaneous tests"
    [bidderAlternative]

tests :: TestTree
tests =
  testGroup
    "AuctionSpec"
    [ successfulSingle,
      failingSingle,
      failingAttacks,
      miscTests,
      successfulAttacks
    ]
