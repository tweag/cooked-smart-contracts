{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Split.OffChain where

import qualified Cooked
import Data.Default
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Plutus.Script.Utils.Ada as Script
import qualified PlutusLedgerApi.V3 as Api
import Prettyprinter ((<+>))
import qualified Prettyprinter
import Split

instance Cooked.PrettyCooked SplitDatum where
  prettyCookedOpt pcOpts SplitDatum {recipient1, recipient2, amount} =
    Cooked.prettyItemize
      "SplitDatum"
      "-"
      [ "Amount:" <+> Prettyprinter.pretty amount,
        "Recipient 1:" <+> Cooked.prettyCookedOpt pcOpts recipient1,
        "Recipient 2:" <+> Cooked.prettyCookedOpt pcOpts recipient2
      ]

-- * Transaction Skeleton Generators

-- | Transaction to lock some amount into the split contract; note that
-- we receive the split contract as parameter because we use this same function
-- in the @tests/SplitSpec.hs@ and @tests/SplitUPLCSpec.hs@. The later loads
-- the split contract as a raw untyped PlutusCore contract.
txLock :: (Cooked.MonadBlockChain m) => Cooked.Wallet -> SplitDatum -> m Api.TxOutRef
txLock signer datum@SplitDatum {amount} = do
  splitTxOutRef : _ <-
    Cooked.validateTxSkel' $
      Cooked.txSkelTemplate
        { Cooked.txSkelOuts = [Cooked.paysScript splitValidator datum (Script.lovelaceValueOf amount)],
          Cooked.txSkelOpts = def {Cooked.txOptEnsureMinAda = True},
          Cooked.txSkelSigners = [signer],
          Cooked.txSkelLabel = Set.singleton (Cooked.TxLabel (TxLock datum))
        }
  return splitTxOutRef

-- | Label for 'txLock' skeleton, making it immediately recognizable
-- when printing traces.
newtype TxLock = TxLock SplitDatum deriving (Show, Eq, Ord)

instance Cooked.PrettyCooked TxLock where
  prettyCookedOpt pcOpts (TxLock splitDatum) = "TxLock" <+> Cooked.prettyCookedOpt pcOpts splitDatum

-- | Whether a script output concerns a public key hash
isARecipient :: Api.PubKeyHash -> SplitDatum -> Bool
isARecipient pkh SplitDatum {recipient1, recipient2} = pkh `elem` [recipient1, recipient2]

-- | Unlocks the first 'SplitDatum' where our wallet is a recipient of.
txUnlock ::
  (Cooked.MonadBlockChain m) =>
  Api.TxOutRef ->
  Cooked.Wallet ->
  m (Api.TxOutRef, Api.TxOutRef)
txUnlock splitTxOutRef signer = do
  Just SplitDatum {recipient1, recipient2, amount} <-
    Cooked.typedDatumFromTxOutRef @SplitDatum splitTxOutRef
  let share1 = div amount 2
      share2 = amount - share1
  share1TxOutRef : share2TxOutRef : _ <-
    Cooked.validateTxSkel' $
      Cooked.txSkelTemplate
        { Cooked.txSkelIns = Map.singleton splitTxOutRef (Cooked.TxSkelRedeemerForScript ()),
          Cooked.txSkelOuts =
            [ Cooked.paysPK recipient1 (Script.lovelaceValueOf share1),
              Cooked.paysPK recipient2 (Script.lovelaceValueOf share2)
            ],
          Cooked.txSkelSigners = [signer],
          Cooked.txSkelOpts = def {Cooked.txOptEnsureMinAda = True},
          Cooked.txSkelLabel = Set.singleton (Cooked.TxLabel TxUnlock)
        }
  return (share1TxOutRef, share2TxOutRef)

-- | Label for 'txUnlock' skeleton
data TxUnlock = TxUnlock deriving (Show, Eq, Ord)

instance Cooked.PrettyCooked TxUnlock where
  prettyCooked _ = "TxUnlock"
