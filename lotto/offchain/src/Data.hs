{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Data definitions and utilies shared between the onchain and offchain code.
-- Namely datums, redeemers &c. These datatypes are documented in the onchain code.
module Data
  ( SealRedeemer,
    mintSeal,
    burnSeal,
    LottoDatum,
    secretHash,
    secretSalt,
    deadline,
    bidAmount,
    players,
    margin,
    Lotto,
    datumOfTxOut,
    initLottoDatum,
    addPlayer,
    mkRedeemer,
    initialise,
    play,
    resolve,
    rinitialise,
    rplay,
    rresolve,
  )
where

import Cooked (TxSkelRedeemer)
import qualified Cooked
import qualified Data.ByteString as B
import Optics (makeLenses, over, view)
import Plutus.Script.Utils.Typed (DatumType, RedeemerType, ValidatorTypes)
import Plutus.V2.Ledger.Api
  ( BuiltinByteString,
    Map,
    POSIXTime,
    PubKeyHash,
    TokenName,
    TxOutRef,
    Value,
  )
import qualified Plutus.V2.Ledger.Api as LedgerV2
import qualified PlutusTx
import qualified PlutusTx.AssocMap as Map
import qualified PlutusTx.Prelude as Tx
import qualified PlutusTx.Ratio as Tx
import Prettyprinter (align, braces, pretty, vsep, (<+>))
import qualified Prettyprinter as PP
import Prelude

-- | Redeemer of the seal minting policy
data SealRedeemer
  = Mint TxOutRef
  | Burn TokenName
  deriving (Show)

instance Cooked.PrettyCooked SealRedeemer where
  prettyCookedOpt o (Mint r) = "Seal mint (" <> Cooked.prettyCookedOpt o r <> ")"
  prettyCookedOpt _ (Burn r) = "Seal burn (" <> pretty r <> ")"

PlutusTx.makeIsDataIndexed ''SealRedeemer [('Mint, 0), ('Burn, 1)]
PlutusTx.makeLift ''SealRedeemer

mintSeal :: TxOutRef -> Cooked.MintsRedeemer
mintSeal outref = Cooked.SomeMintsRedeemer (Mint outref)

burnSeal :: TokenName -> Cooked.MintsRedeemer
burnSeal seal = Cooked.SomeMintsRedeemer (Burn seal)

-- * Main lotto datum

data LottoDatum = LottoDatum
  { _secretHash :: BuiltinByteString,
    _secretSalt :: BuiltinByteString,
    _deadline :: POSIXTime,
    _bidAmount :: Value,
    _players :: Map PubKeyHash BuiltinByteString,
    _margin :: Tx.Rational
  }
  deriving (Show, Eq)

makeLenses ''LottoDatum

instance Cooked.PrettyCooked LottoDatum where
  prettyCookedOpt _ dat =
    braces $
      align
        ( vsep
            [ "secret (hash):" <+> "0x" <> prettyByteString (view secretHash dat),
              "salt:" <+> pretty (view secretSalt dat),
              "deadline:" <+> pretty (view deadline dat),
              "bidAmount:" <+> pretty (view bidAmount dat),
              "margin:" <+> prettyRat (view margin dat),
              "players:" <+> pretty (view players dat)
            ]
        )
    where
      prettyRat r =
        pretty (Tx.numerator r) <+> "/" <+> pretty (Tx.denominator r)
      -- Print a sequence of words
      prettyByteString = B.foldr (mappend . PP.viaShow) mempty . Tx.fromBuiltin

instance Tx.Eq LottoDatum where
  (==) = (==)

PlutusTx.makeIsDataIndexed ''LottoDatum [('LottoDatum, 0)]
PlutusTx.makeLift ''LottoDatum

-- | Extract and type the datum of a 'LedgerV2.TxOut' as a Lotto datum
datumOfTxOut :: Cooked.MonadBlockChainWithoutValidation m => LedgerV2.TxOutRef -> m (Maybe LottoDatum)
datumOfTxOut = Cooked.typedDatumFromTxOutRef

-- | Setup a lotto by providing an initial datum.
initLottoDatum ::
  -- | The hash of a secret (hashed with a salt)
  BuiltinByteString ->
  -- | The salt used to hash the secret
  BuiltinByteString ->
  -- | Gamblers must play before this deadline
  POSIXTime ->
  -- | The minimum value required to bid.
  Value ->
  -- | The share of the pot that goes to the administrator.
  Tx.Rational ->
  LottoDatum
initLottoDatum _secretHash _secretSalt _deadline _bidAmount _margin =
  LottoDatum
    { _secretHash,
      _secretSalt,
      _deadline,
      _bidAmount,
      _players = LedgerV2.fromList [],
      _margin
    }

-- | Create a new map simply adding a new key/value pair on top of the old one.
-- This is not equivalent to 'Map.insert', old bindings are not overridden.
-- append "a" 0 (Map.fromList [("a", 1)]) == Map.fromList [("a", 0), ("a", 1)]

-- | Add a player on top of the players list of a datum. In particular,
-- it performs a cons rather than an insert in the players list,
-- so that the tail of the new players list is the same as the old one.
-- tail (view players (addPlayer p b d)) == view players
addPlayer :: Cooked.Wallet -> BuiltinByteString -> LottoDatum -> LottoDatum
addPlayer w bid = over players (mapAppend pk bid)
  where
    pk = Cooked.walletPKHash w
    mapAppend :: k -> v -> Map k v -> Map k v
    mapAppend key val m =
      Map.fromList $ (key, val) : Map.toList m

-- * Redeemer

data LottoRedeemer
  = Initialise
  | Play
  | Resolve BuiltinByteString
  deriving (Show, Eq)

instance Cooked.PrettyCooked LottoRedeemer where
  prettyCookedOpt _ = PP.viaShow

instance Tx.Eq LottoRedeemer where
  (==) = (==)

PlutusTx.makeIsDataIndexed ''LottoRedeemer [('Initialise, 0), ('Play, 1), ('Resolve, 2)]
PlutusTx.makeLift ''LottoRedeemer

data Lotto

instance ValidatorTypes Lotto where
  type RedeemerType Lotto = LottoRedeemer
  type DatumType Lotto = LottoDatum

-- | A helper function to create a skeleton redeemer.
mkRedeemer :: LottoRedeemer -> TxSkelRedeemer
mkRedeemer = Cooked.TxSkelRedeemerForScript

-- | Initialise a lotto or play on it.
initialise, play :: TxSkelRedeemer
initialise = mkRedeemer Initialise
play = mkRedeemer Play

-- | Initialise a lotto output, using a referenced validator
rinitialise, rplay :: TxSkelRedeemer
rinitialise = Cooked.TxSkelRedeemerForReferencedScript Initialise
rplay = Cooked.TxSkelRedeemerForReferencedScript Play

-- | Resolve a lotto: compute scores and dispatch money
resolve, rresolve :: BuiltinByteString -> TxSkelRedeemer
resolve = mkRedeemer . Resolve
rresolve = Cooked.TxSkelRedeemerForReferencedScript . Resolve
