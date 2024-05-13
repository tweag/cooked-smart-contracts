{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}

-- These language extensions are just what Split.hs uses

-- | Arrange an auction with a preset deadline and minimum bid.
module Auction where

import qualified Cooked
import qualified Plutus.Script.Utils.Ada as Script
import qualified Plutus.Script.Utils.Scripts as Script
import qualified Plutus.Script.Utils.Typed as Script
import qualified Plutus.Script.Utils.V3.Typed.Scripts as Script
import qualified Plutus.Script.Utils.Value as Script
import qualified PlutusLedgerApi.V1.Interval as Api
import qualified PlutusLedgerApi.V3 as Api
import qualified PlutusLedgerApi.V3.Contexts as Api
import qualified PlutusTx
import PlutusTx.Prelude
import Prettyprinter ((<+>))
import qualified Prettyprinter as PP
import qualified Prelude as Haskell

{-

Brief overview of the contract
------------------------------

There are four transactions involved in this contract:

1. Making an offer (off-chain implemented by 'txOffer'). Anyone who wishes to
   sell something can just pay it to the 'auctionValidator' with the 'Offer'
   datum specifiying the seller's address (which is the address that will at the
   end of the auction be paid the winning bid, or, if there have been no bids,
   receive the offer back) and the minimum bid. No checks are involved in this
   transaction.

The UTxO at the 'auctionValidator' that contains this initial offer
parameterises the rest of the auction. We shall call it the _offer UTxO_ of the
auction.

2. Setting the deadline of the auction (off-chain implemented by
   'txSetDeadline'). This transaction consumes the offer UTxO and returns an
   UTxO to the 'auctionValidator' with the 'NoBids' datum, which contains

   - the value originally offered, and

   - the thread NFT. This NFT ensures the authenticity of the auction from that
     point onwards, and its token name is uniquely derived from the offer UTxO
     (it is computed by 'tokenNameFromTxOutRef').

3. Bidding on the auction (off-chain implemented by 'txBid'). This transaction
   consumes an UTxO at the 'auctionValidator' with either 'NoBids' or 'Bidding'
   datum, and returns an UTxO with strictly greater value to the validator (in
   particular, it has to return the thread NFT) with a 'Bidding' datum recording
   the new highest bid and bidder. If there was a previous bidder, they are paid
   back their bid.

4. Hammer to end the auction (off-chain implemented by 'txHammer'). This
   transaction consumes an UTxO at the 'auctionValidator', and pays the Ada
   amount corresponding to the highest bid to the seller, and the value
   originally offered to the highest bidder. If there were no bids, the offer is
   returned to the seller.

Further details of these transactions are explained at the relevant places in
the code.

Remark on the design of the first two transactions
--------------------------------------------------

The following discussion is rather technical and not specific to this
contract. Rather, it describes a general design problem for smart contracts on
Cardano, so feel free to skip this if you merely want to get to know the
contract.

On a previous version of this contract, there was only one transaction to make
the offer and set the deadline. This caused the following problem, which we
think is fundamentally unsolvable: How do you ensure that some freshly minted
tokens end up at a specific validator, using only one transaction? -- If you want
to use only one transaction to mint some tokens with a policy P and make sure
that they end up at the correct validator V,

- the minting policy P has to know the address of V, which is the hash of V's
  (compiled and normalised) source code. In particular, there is no way to
  compute this address on-chain, which means that this can only be accomplished
  by parameterising P with the address of V.

- Conversely, the validator V needs to know the 'CurrencySymbol' of the thread
  token, which is the hash of the (compiled and normalised) code of P.

So, each of the two scripts P and V has to have the other's hash as a parameter,
and have it known at compile time. This is patently an impossible cycle.

The only generic solution that we know of is to turn any initial payment of
freshly minted tokens to the validator into a process that involves two
transactions: The first transaction does not involve any checks at all, does not
mint any tokens that should be locked in the validator script, and creates
"unchecked" UTxOs (Here, these are the UTxOs with the 'Offer' datum). The second
transaction consumes unchecked UTxOs (with an additional redeemer, which here is
'SetDeadline'), mints the required tokens, and pays a checked UTxO back to the
same validator, which contains the newly minted tokens as a proof of their
soundness, and a datum signalling that they have been checked (here, that datum
is 'NoBids'). Since the second transaction uses a redeemer, it can make whatever
checks are needed to ensure the tokens are minted correctly and paid to the
correct script.

This solves the issue: The only thing that P has to enforce is that tokens are
only minted if a specific (unchecked) UTxO is spent on the same transaction. The
check that the tokens actually end up in V where they belong can be done by V
itself, using something like 'getContinuingOutputs' to find an output that has
the correct tokens.

-}

-- * Data types

-- | Parameters for the validator. Currently, the only information it is
-- parameterised by is the currency symbol of the thread token, and that's only
-- a trick to get that currency symbol into the validator, because it can not be
-- computed on-chain. It is constant, and if you look at the very bottom of this
-- file, you will find 'auctionValidator' defined with the constant currency
-- symbol derived from the 'threadTokenPolicy'.
newtype ValParams = ValParams Api.CurrencySymbol

PlutusTx.makeLift ''ValParams
PlutusTx.unstableMakeIsData ''ValParams

-- | Information on the last bidder and their bid.
data BidderInfo = BidderInfo
  { -- | the last bidder's offer in Ada
    bid :: Integer,
    -- | the last bidder's address
    bidder :: Api.PubKeyHash
  }
  deriving (Haskell.Show)

instance Cooked.PrettyCooked BidderInfo where
  prettyCookedOpt opts (BidderInfo bid bidder) =
    Cooked.prettyItemize
      "BidderInfo"
      "-"
      [ "bid:" <+> PP.pretty bid,
        "bidder:" <+> Cooked.prettyCookedOpt opts bidder
      ]

PlutusTx.makeLift ''BidderInfo
PlutusTx.unstableMakeIsData ''BidderInfo

instance Eq BidderInfo where
  {-# INLINEABLE (==) #-}
  BidderInfo a b == BidderInfo x y = a == x && b == y

-- | The state of the auction. This will be the 'DatumType'.
data AuctionState
  = -- | state of an auction where an offer has already been made. The address
    -- is the seller's, the integer is the minimum bid in Lovelaces.
    Offer Api.PubKeyHash Integer
  | -- | state of an auction with a given seller, minimum bid, and deadline that
    -- has not yet had any bids
    NoBids Api.PubKeyHash Integer Api.POSIXTime
  | -- | state of an auction with a given seller and deadline that has had at
    -- least one bid.
    Bidding Api.PubKeyHash Api.POSIXTime BidderInfo
  deriving (Haskell.Show)

getSeller :: AuctionState -> Api.PubKeyHash
getSeller (Offer s _) = s
getSeller (NoBids s _ _) = s
getSeller (Bidding s _ _) = s

getBidDeadline :: AuctionState -> Maybe Api.POSIXTime
getBidDeadline (Offer _ _) = Nothing
getBidDeadline (NoBids _ _ t) = Just t
getBidDeadline (Bidding _ t _) = Just t

PlutusTx.makeLift ''AuctionState
PlutusTx.unstableMakeIsData ''AuctionState

instance Eq AuctionState where
  {-# INLINEABLE (==) #-}
  Offer s m == Offer s' m' = s == s' && m == m'
  NoBids s m t == NoBids s' m' t' = s == s' && m == m' && t == t'
  Bidding s t b == Bidding s' t' b' = s == s' && t == t' && b == b'
  _ == _ = False

-- | This will make the output of cooked-validators much more readable
instance Cooked.PrettyCooked AuctionState where
  prettyCookedOpt opts (Offer seller minBid) =
    Cooked.prettyItemize
      "Offer"
      "-"
      [ "seller:" <+> Cooked.prettyCookedOpt opts seller,
        "minimum bid:" <+> Cooked.prettyCookedOpt opts (Script.lovelaceValueOf minBid)
      ]
  prettyCookedOpt opts (NoBids seller minBid deadline) =
    Cooked.prettyItemize
      "NoBids"
      "-"
      [ "seller:" <+> Cooked.prettyCookedOpt opts seller,
        "minimum bid:" <+> Cooked.prettyCookedOpt opts (Script.lovelaceValueOf minBid),
        "deadline" <+> Cooked.prettyCookedOpt opts deadline
      ]
  prettyCookedOpt opts (Bidding seller deadline (BidderInfo lastBid lastBidder)) =
    Cooked.prettyItemize
      "Bidding"
      "-"
      [ "seller:" <+> Cooked.prettyCookedOpt opts seller,
        "deadline" <+> Cooked.prettyCookedOpt opts deadline,
        "previous bidder:" <+> Cooked.prettyCookedOpt opts lastBidder,
        "previous bid:" <+> Cooked.prettyCookedOpt opts (Script.lovelaceValueOf lastBid)
      ]

-- | Actions to be taken in an auction. This will be the 'RedeemerType'.
data Action
  = -- | redeemer to set the deadline of the auction
    SetDeadline
  | -- | redeemer to make a bid
    Bid BidderInfo
  | -- | redeemer to close the auction. The 'TxOutRef' points to the original
    --  'Offer' UTxO.
    Hammer Api.TxOutRef
  deriving (Haskell.Show)

instance Cooked.PrettyCooked Action where
  prettyCookedOpt _ SetDeadline = "SetDeadline"
  prettyCookedOpt opts (Bid bidderInfo) = "Bid" <+> Cooked.prettyCookedOpt opts bidderInfo
  prettyCookedOpt opts (Hammer txOutRef) = "Hammer" <+> Cooked.prettyCookedOpt opts txOutRef

instance Eq Action where
  {-# INLINEABLE (==) #-}
  SetDeadline == SetDeadline = True
  Bid bi1 == Bid bi2 = bi1 == bi2
  Hammer o1 == Hammer o2 = o1 == o2
  _ == _ = False

PlutusTx.makeLift ''Action
PlutusTx.unstableMakeIsData ''Action

-- * The minting policy of the thread token

-- | This minting policy controls the thread token of the auction. From the
-- transaction that sets the deadline onwards, this NFT will belong to the
-- 'auctionValidator'; its presence proves the authenticity of the
-- auction. Here, we only check that exactly one thread token is minted,
-- enforcing that the appropriate offer utxo, whose hash as computed by
-- 'tokenNameFromTxOutRef' must be the token name of the minted token, is
-- consumed. The rest of the necessary checks are performed by
-- 'validSetDeadline'.
--
-- The final 'Hammer' transaction of the auction burns the thread token. This
-- transaction is checked by 'validHammer', so that this minting policy only has
-- to check that at exactly one token is burned.
{-# INLINEABLE mkPolicy #-}
mkPolicy :: Api.TxOutRef -> Api.ScriptContext -> Bool
mkPolicy offerOref ctx
  | amnt == 1 =
      traceIfFalse
        "Offer UTxO not consumed"
        (any (\i -> Api.txInInfoOutRef i == offerOref) $ Api.txInfoInputs txi)
  -- no further checks here since 'validSetDeadline' checks the remaining conditions
  | amnt == -1 =
      True -- no further checks here; 'validHammer' checks everything
  | otherwise = trace "not minting or burning the right amount" False
  where
    txi = Api.scriptContextTxInfo ctx

    -- the amount of minted tokens whose token name is the hash of the
    -- 'offerOref'.
    --
    -- ############################################################
    -- # This introduces a KNOWN VULNERABILITY into the contract: Since we do not
    -- # check that no tokens of other token names are minted, it'll be possible
    -- # to forge the thread NFT of one auction while seting the deadline of
    -- # another auction, for example. See the "known vulnerabilities and
    -- # exploits" section in "tests/AuctionSpec.hs" for a worked-out exploit.
    -- ############################################################
    --
    amnt :: Integer
    amnt =
      foldr
        ( \(cs, tn, a) n ->
            if cs == Api.ownCurrencySymbol ctx && tn == tokenNameFromTxOutRef offerOref
              then n + a
              else n
        )
        0
        $ Script.flattenValue (Api.txInfoMint txi)

threadTokenPolicy :: Script.MintingPolicy
threadTokenPolicy =
  Script.mkMintingPolicyScript
    $$(PlutusTx.compile [||Script.mkUntypedMintingPolicy mkPolicy||])

-- | Compute the thread token of the auction with the given offer UTxO.
threadToken :: Api.TxOutRef -> Api.Value
threadToken offerOref = Script.assetClassValue (threadTokenAssetClassFromOref offerOref) 1

threadTokenAssetClassFromOref :: Api.TxOutRef -> Script.AssetClass
threadTokenAssetClassFromOref offerOref =
  Script.assetClass
    threadCurrencySymbol
    (tokenNameFromTxOutRef offerOref)

threadCurrencySymbol :: Api.CurrencySymbol
threadCurrencySymbol = Cooked.currencySymbolFromLanguageAndMP Script.PlutusV3 threadTokenPolicy

-- | Compute the token name of the thread token of an auction from its offer
-- Utxo. This must be a 32-byte string, apparently.
{-# INLINEABLE tokenNameFromTxOutRef #-}
tokenNameFromTxOutRef :: Api.TxOutRef -> Api.TokenName
tokenNameFromTxOutRef (Api.TxOutRef (Api.TxId tid) i) =
  -- Remark, because I spent quite some time digging for this information: Why
  -- do we directly use the constructor 'TokenName' here? -- The point is that
  -- we want to use this function on-chain, and that the library function
  -- 'tokenName' (note the lower-case initial letter!) expects *Haskell* byte
  -- strings. See this issue on plutus-apps for some background:
  --
  -- https://github.com/input-output-hk/plutus-apps/issues/498
  Script.TokenName . takeByteString 32 . sha2_256 $ tid <> "-" <> encodeInteger i
  where
    -- we know that the numbers (indices of transaction outputs) we're working
    -- with here are non-negative.
    encodeInteger :: Integer -> BuiltinByteString
    encodeInteger n
      | n `quotient` 10 == 0 = encodeDigit n
      | otherwise = encodeInteger (n `quotient` 10) <> encodeDigit (n `remainder` 10)
      where
        encodeDigit :: Integer -> BuiltinByteString
        -- 48 is the ASCII code for '0'
        encodeDigit d = consByteString (d + 48) emptyByteString

-- | Compute the thread token of an auction from the currency symbol and the
-- original offer UTxO. This is for on-chain computations of the thread token,
-- where the currency symbol is known as a parameter.
{-# INLINEABLE threadTokenOnChain #-}
threadTokenOnChain :: Api.CurrencySymbol -> Api.TxOutRef -> Api.Value
threadTokenOnChain threadCS offerOref = Script.assetClassValue (Script.AssetClass (threadCS, tokenNameFromTxOutRef offerOref)) 1

-- * The validator and its helpers

-- | Extract an auction state from an output (if it has one)
{-# INLINEABLE outputAuctionState #-}
outputAuctionState :: Api.TxInfo -> Api.TxOut -> Maybe AuctionState
outputAuctionState txi o =
  case Api.txOutDatum o of
    Api.NoOutputDatum -> Nothing
    Api.OutputDatumHash h -> do
      Api.Datum d <- Api.findDatum h txi
      PlutusTx.fromBuiltinData d
    Api.OutputDatum (Api.Datum d) -> PlutusTx.fromBuiltinData d

-- | Test that the value paid to the given public key address is at
-- least the given value
{-# INLINEABLE receivesFrom #-}
receivesFrom :: Api.TxInfo -> Api.PubKeyHash -> Api.Value -> Bool
receivesFrom txi who what = Api.valuePaidTo txi who `Script.geq` what

-- | To set the deadline of an auction, you must
-- * consume an UTxO with the 'Offer' datum
-- * pay back with the 'NoBids' datum for the same seller and minimum bid, and
--   add the thread token
-- * sign the transaction as the seller
{-# INLINEABLE validSetDeadline #-}
validSetDeadline :: Api.CurrencySymbol -> AuctionState -> Api.ScriptContext -> Bool
validSetDeadline _ NoBids {} _ = trace "Cannot re-set the deadline in 'NoBids' state" False
validSetDeadline _ Bidding {} _ = trace "Cannot re-set the deadline in 'Bidding' state" False
validSetDeadline threadCS (Offer seller minbid) ctx =
  case Api.findOwnInput ctx of
    Nothing -> trace "Unable to find own input" False
    Just (Api.TxInInfo offerOref offerOut) ->
      traceIfFalse
        "SetDeadline transaction must be signed by seller"
        (Api.scriptContextTxInfo ctx `Api.txSignedBy` seller)
        && traceIfFalse
          "there must be a 'NoBids' output containing the lot and the thread token"
          ( any
              ( \o ->
                  Api.txOutValue o
                    `Script.geq` (threadTokenOnChain threadCS offerOref <> Api.txOutValue offerOut)
                    && case outputAuctionState (Api.scriptContextTxInfo ctx) o of
                      Just (NoBids seller' minbid' _deadline) ->
                        (seller, minbid) == (seller', minbid')
                      _ -> False
              )
              (Api.getContinuingOutputs ctx)
          )

-- | A new bid is valid if
-- * it is made before the bidding deadline
-- * it has been signed by the bidder
-- * it is greater than the last bid (or at least the minimum bid, if it's the first one)
-- * after the transaction:
--    * the state of the auction is 'Bidding' with the new bid and bidder
--    * the validator locks the lot, the new bid, and the thread token with that datum
--    * the last bidder has gotten their money back from the validator
{-# INLINEABLE validBid #-}
validBid :: AuctionState -> Integer -> Api.PubKeyHash -> Api.ScriptContext -> Bool
validBid datum bid bidder ctx =
  case Api.findOwnInput ctx of
    Nothing -> trace "Unable to find own input" False
    Just (Api.TxInInfo _ Api.TxOut {Api.txOutValue = originalLockedValue}) ->
      let txi = Api.scriptContextTxInfo ctx
          checkDeadlineAndSignature deadline =
            traceIfFalse
              "Bidding past the deadline is not permitted"
              -- This line is sometimes wrong by one millisecond, but it's not our fault. See
              --
              -- https://github.com/tweag/cooked-validators/issues/309
              --
              -- for context.
              (Api.to deadline `Api.contains` Api.txInfoValidRange txi)
              && traceIfFalse "Bid transaction not signed by bidder" (txi `Api.txSignedBy` bidder)
          checkLocked seller deadline v =
            traceIfFalse
              "Validator does not lock lot, bid, and thread token with the correct 'Bidding' datum"
              ( any
                  ( \o ->
                      outputAuctionState txi o
                        == Just (Bidding seller deadline (BidderInfo bid bidder))
                        && Api.txOutValue o
                        `Script.geq` v
                  )
                  (Api.getContinuingOutputs ctx)
              )
       in case datum of
            Offer {} -> trace "Cannot bid on an auction that hasn't yet got a deadline" False
            NoBids seller minBid deadline ->
              checkDeadlineAndSignature deadline
                && traceIfFalse "Cannot bid less than the minimum bid" (minBid <= bid)
                && checkLocked seller deadline (originalLockedValue <> Script.lovelaceValueOf bid)
            Bidding seller deadline (BidderInfo prevBid prevBidder) ->
              checkDeadlineAndSignature deadline
                && traceIfFalse "Must bid strictly more than the previous bid" (prevBid < bid)
                && checkLocked
                  seller
                  deadline
                  ( originalLockedValue
                      <> negate (Script.lovelaceValueOf prevBid)
                      <> Script.lovelaceValueOf bid
                  )
                &&
                -- #############################################################
                -- # This usage of 'receivesFrom' introduces a double satisfaction
                -- # vulnerability in the contract. The problem is that the required
                -- # outputs to the last bidder are not identified by anything but
                -- # their value. However, there might be an output containing a
                -- # sufficient amount of money to the last bidder's address for
                -- # completely unrelated reasons. This output is then taken by this
                -- # validator to satisfy the requirement below.
                -- #
                -- # For a completely worked-out exploit of this vulnerability, that
                -- # steals the output being checked here, see the trace
                -- # 'exploitDoubleSat' in "AuctionSpec.hs".
                -- #
                -- # The 'receives' lines in 'validHammer' suffer of the same problem.
                -- #############################################################
                traceIfFalse
                  "Last bidder is not paid back"
                  (receivesFrom txi prevBidder $ Script.lovelaceValueOf prevBid)

-- | A hammer ends the auction. It is valid if
-- * it is made after the bidding deadline
-- * it burns the thread NFT associated with the auction
-- * after the transaction, if there have been bids:
--    * the last bidder has received the lot
--    * the seller has received payment of the highest bid
-- * afer the transaction, if there have been no bids:
--    * the seller gets the lot
{-# INLINEABLE validHammer #-}
validHammer :: Api.CurrencySymbol -> AuctionState -> Api.TxOutRef -> Api.ScriptContext -> Bool
validHammer threadCS datum offerOref ctx =
  case Api.findOwnInput ctx of
    Nothing -> trace "Unable to find own input" False
    Just (Api.TxInInfo _ Api.TxOut {Api.txOutValue = lockedValue}) ->
      let txi = Api.scriptContextTxInfo ctx
          receives = receivesFrom txi
          theNFT = threadTokenOnChain threadCS offerOref
          threadTokenIsBurned = Api.txInfoMint txi == negate theNFT
          checkDeadlineAndBurn deadline =
            traceIfFalse
              "Hammer before the deadline is not permitted"
              (Api.from deadline `Api.contains` Api.txInfoValidRange txi)
              && traceIfFalse
                "Hammer does not burn exactly one thread token"
                threadTokenIsBurned
       in case datum of
            Offer seller _minbid ->
              traceIfFalse "Seller must sign the hammer to withdraw the offer" (txi `Api.txSignedBy` seller)
                && traceIfFalse "Seller must get the offer back" (seller `receives` lockedValue)
            NoBids seller _minbid deadline ->
              checkDeadlineAndBurn deadline
                && traceIfFalse
                  "Seller must get the offer back"
                  (seller `receives` (lockedValue <> negate theNFT))
            Bidding seller deadline (BidderInfo lastBid lastBidder) ->
              checkDeadlineAndBurn deadline
                && traceIfFalse
                  "last bidder must get the lot"
                  ( lastBidder
                      `receives` ( lockedValue
                                     <> negate theNFT
                                     <> negate (Script.lovelaceValueOf lastBid)
                                 )
                  )
                && traceIfFalse
                  "Seller must get the last bid"
                  (seller `receives` Script.lovelaceValueOf lastBid)

{-# INLINEABLE validate #-}
validate :: ValParams -> AuctionState -> Action -> Api.ScriptContext -> Bool
validate (ValParams threadCS) datum redeemer ctx = case redeemer of
  SetDeadline -> validSetDeadline threadCS datum ctx
  Bid (BidderInfo bid bidder) -> validBid datum bid bidder ctx
  Hammer offerOref -> validHammer threadCS datum offerOref ctx

-- Plutus boilerplate to compile the validator

data Auction

instance Script.ValidatorTypes Auction where
  type RedeemerType Auction = Action
  type DatumType Auction = AuctionState

auctionValidator' :: ValParams -> Script.TypedValidator Auction
auctionValidator' =
  Script.mkTypedValidatorParam @Auction
    $$(PlutusTx.compile [||validate||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Script.mkUntypedValidator

auctionValidator :: Script.TypedValidator Auction
auctionValidator = auctionValidator' $ ValParams threadCurrencySymbol
