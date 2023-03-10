{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Utility functions for transactions and scenarii.
module Lib
  ( txSkelTypedDatumT,
    txSkelOutValuesT,
    -- Managing lotto scripts
    LottoScripts,
    readLottoScripts,
    unsafeReadLottoScripts,
    sealCurrencySymbol,
    Sealed (isSealed, hasSeal),
    mkValidator,
    mkTypedValidator,
    mkMintingPolicy,
    burnSeal,
    mintSeal,
    ada,
    countAda,
    withSeal,
    -- Procedures for the lotto
    hashSecret,
    txOutRefToToken,
    constantScore,
    scoreDiffZeros,
    payGamblers,
    -- Skeleton validations and output selection
    validateAndGetOuts,
    validateAndGetUniqueLottoOut,
    isValidatorOutput,
    validateAndGetUniqueLottoOutWithSeal,
  )
where

import Cooked (MonadBlockChain, TxSkel)
import qualified Cooked
import qualified Data
import qualified Data.Bifunctor as Bifun
import Data.Coerce (coerce)
import Data.Maybe (fromMaybe, isJust)
import qualified Ledger.Typed.Scripts as TScripts
import Optics (Traversal', traversed, view, (%))
import qualified Plutus.Script.Utils.Ada as Ada
import qualified Plutus.Script.Utils.Scripts as Scripts
import Plutus.V2.Ledger.Api
  ( BuiltinByteString,
    CurrencySymbol,
    MintingPolicy,
    PubKeyHash,
    TokenName,
    Value,
  )
import qualified Plutus.V2.Ledger.Api as LedgerV2
import Plutus.V2.Ledger.Contexts (TxOutRef)
import qualified PlutusTx.AssocMap as Map
import qualified PlutusTx.Prelude as Tx
import qualified PlutusTx.Ratio as Tx.Rat
import Ply ((#))
import qualified Ply
import System.IO.Unsafe (unsafePerformIO)
import Type.Reflection (Typeable)
import Prelude

-- | A traversal to mess with datums of a skeleton.
txSkelTypedDatumT :: forall a. (LedgerV2.FromData a, Typeable a) => Traversal' Cooked.TxSkel a
txSkelTypedDatumT = Cooked.txSkelOutsL % traversed % Cooked.txSkelOutputDatumTypeAT @a

-- | A traversal to mess with the values of the outputs of a skeleton.
txSkelOutValuesT :: Traversal' Cooked.TxSkel LedgerV2.Value
txSkelOutValuesT = Cooked.txSkelOutsL % traversed % Cooked.txSkelOutValueL

-- | Function used to hash the secret of the lotto.
-- NOTE: this function must be the same as the onchain version 'phashSecret'.
hashSecret :: BuiltinByteString -> Maybe BuiltinByteString -> BuiltinByteString
hashSecret secret salt = Tx.sha3_256 $ fromMaybe "" salt <> secret

-- | Transform a Transaction output reference to a token name.
-- NOTE: this function must be the same as its onchain counterpart
-- 'txOutRefToToken'.
txOutRefToToken :: TxOutRef -> LedgerV2.TokenName
txOutRefToToken outref = toToken $ idOfOutref outref <> idxOfOutref outref
  where
    -- Transform a bytestring into a token name. The length of the
    -- token name must be 32 (hence the 'take').
    toToken = LedgerV2.TokenName . Tx.takeByteString 32 . Tx.sha2_256
    idOfOutref = LedgerV2.getTxId . LedgerV2.txOutRefId
    idxOfOutref = encodeInteger . LedgerV2.txOutRefIdx
    -- Must be identical to its onchain counterpart (has the same name).
    encodeInteger :: Integer -> BuiltinByteString
    encodeInteger n =
      if n < 256
        then Tx.consByteString n ""
        else encodeInteger (n `Tx.quotient` 256) <> Tx.consByteString (n `Tx.modulo` 256) ""

-- | All scripts required by the lotto. The potential parameters have already
-- been applied.
data LottoScripts = LottoScripts
  { _validator :: Ply.TypedScript 'Ply.ValidatorRole '[],
    _sealPolicy :: Ply.TypedScript 'Ply.MintingPolicyRole '[]
  }

-- | Get the currency symbol of the seal.
sealCurrencySymbol :: LottoScripts -> LedgerV2.CurrencySymbol
sealCurrencySymbol scripts =
  Scripts.scriptCurrencySymbol $
    Scripts.Versioned (mkMintingPolicy scripts) Scripts.PlutusV2

-- | Elements of this class are substructures of a sealed output.
class Sealed a where
  -- | Is there a seal present?
  isSealed :: LottoScripts -> a -> Bool

  -- | Does the structure contain a particular seal?
  hasSeal :: LottoScripts -> TokenName -> a -> Bool

-- | Values are sealed if they have a seal.
instance Sealed Value where
  isSealed scripts value =
    let currency = sealCurrencySymbol scripts
     in isJust $ Map.lookup currency (LedgerV2.getValue value)
  hasSeal scripts seal value =
    case Map.lookup (sealCurrencySymbol scripts) (LedgerV2.getValue value) of
      Nothing -> False
      Just tokens ->
        case Map.lookup seal tokens of
          Nothing -> False
          Just amount -> amount >= 1 -- This should be exactly one anyway

-- | An output is sealed if its value is.
instance Sealed LedgerV2.TxOut where
  isSealed scripts out = isSealed scripts $ view Cooked.outputValueL out
  hasSeal scripts seal out = hasSeal scripts seal $ view Cooked.outputValueL out

-- | Do something over a value if it is sealed. Otherwise, it is identity.
withSeal :: LottoScripts -> (Value -> Value) -> Value -> Value
withSeal scripts action val =
  if isSealed scripts val then action val else val

-- | Load scripts from the disk and apply their parameters.
-- Filepaths are hardcoded.
readLottoScripts :: PubKeyHash -> IO LottoScripts
readLottoScripts administrator = do
  mkValidator_ <- mkValidator'
  sealPolicy <- sealPolicy'
  let versionedSealPolicy =
        Scripts.Versioned (Ply.toMintingPolicy sealPolicy) Scripts.PlutusV2
      sealCurrency = Scripts.scriptCurrencySymbol versionedSealPolicy
  return $
    LottoScripts
      (mkValidator_ # sealCurrency # administrator)
      sealPolicy
  where
    -- We put these in @where@ scope to have precise types
    sealPolicy' :: IO (Ply.TypedScript 'Ply.MintingPolicyRole '[])
    sealPolicy' = Ply.readTypedScript "scripts/sealMinting.plutus"
    mkValidator' ::
      IO
        ( Ply.TypedScript
            'Ply.ValidatorRole
            '[CurrencySymbol, PubKeyHash]
        )
    mkValidator' = Ply.readTypedScript "scripts/lottoValidator.plutus"

-- | Because we don't want to add IO content to the 'MonadBlockChain'.
unsafeReadLottoScripts :: PubKeyHash -> LottoScripts
unsafeReadLottoScripts = unsafePerformIO . readLottoScripts

-- | Extract a validator (in the sense of Plutus Ledger) out of 'LottoScripts'
mkValidator :: LottoScripts -> LedgerV2.Validator
mkValidator = Ply.toValidator . _validator

-- | Extract a typed validator. Arguments are documented in 'mkValidator'.
mkTypedValidator :: LottoScripts -> TScripts.TypedValidator Data.Lotto
mkTypedValidator =
  coerce . TScripts.unsafeMkTypedValidator
    . flip TScripts.Versioned TScripts.PlutusV2
    . mkValidator

-- | Extract a minting policy in the sense of Plutus Ledgers out of
-- 'LottoScripts'.
mkMintingPolicy :: LottoScripts -> LedgerV2.MintingPolicy
mkMintingPolicy = Ply.toMintingPolicy . _sealPolicy

-- | Burn a seal
burnSeal ::
  LottoScripts ->
  TokenName ->
  ( TScripts.Versioned MintingPolicy,
    Cooked.MintsRedeemer,
    TokenName,
    Integer
  )
burnSeal scripts seal =
  ( TScripts.Versioned (Lib.mkMintingPolicy scripts) TScripts.PlutusV2,
    Data.burnSeal seal,
    seal,
    -1
  )

mintSeal ::
  LottoScripts ->
  TokenName ->
  TxOutRef ->
  ( TScripts.Versioned MintingPolicy,
    Cooked.MintsRedeemer,
    TokenName,
    Integer
  )
mintSeal scripts seal outRef =
  ( TScripts.Versioned (Lib.mkMintingPolicy scripts) TScripts.PlutusV2,
    Data.mintSeal outRef,
    seal,
    1
  )

-- | Return an amount of Ada.
ada :: Integer -> LedgerV2.Value
ada n = Ada.lovelaceValueOf $ n * 1_000_000

-- | Return the amount of Ada of a value.
countAda :: LedgerV2.Value -> Integer
countAda v =
  fromMaybe 0 $ Map.lookup LedgerV2.adaSymbol (LedgerV2.getValue v) >>= Map.lookup LedgerV2.adaToken

-- * Score computation.

type Score = BuiltinByteString -> BuiltinByteString -> Integer

-- | A constant score function. Mainly for debugging purposes.
constantScore :: Integer -> Score
constantScore = const . const

foldBS :: (Integer -> a -> a) -> a -> BuiltinByteString -> a
foldBS f init' bs = loop 0
  where
    loop k =
      if k < Tx.lengthOfByteString bs
        then f (Tx.indexByteString bs k) (loop (k + 1))
        else init'

-- | Let @z(x)@  the number of zeros in bytestring @x@. Then this is
-- @\ x y -> |z(x) - z(y)|
scoreDiffZeros :: Score
scoreDiffZeros secret guess = Tx.lengthOfByteString secret - abs (numZero secret - numZero guess)
  where
    numZero = foldBS (\n acc -> if n == 0 then acc + 1 else acc) 1

payGamblers ::
  -- | Score computation
  Score ->
  -- | Pot
  Integer ->
  -- | Margin
  Tx.Rational ->
  -- | Secret
  BuiltinByteString ->
  -- | Bids
  [(thing, BuiltinByteString)] ->
  [(thing, Integer)]
payGamblers score pot margi secret bids =
  Bifun.second scoreOf <$> bids
  where
    total = foldr (\(_, e) acc -> score secret e + acc) 0 bids
    scoreOf bid =
      Tx.round $
        Tx.fromInteger pot
          Tx.* (Tx.fromInteger 1 Tx.- margi)
          Tx.* Tx.fromInteger (score secret bid)
          Tx.* Tx.Rat.recip (Tx.fromInteger $ 1 + total)

-- | Validates a transactions and retrieves its outputs
validateAndGetOuts :: MonadBlockChain m => TxSkel -> m [(TxOutRef, LedgerV2.TxOut)]
validateAndGetOuts txskel =
  Cooked.utxosFromCardanoTx <$> Cooked.validateTxSkel txskel

-- | Returns true if a certain output belong to a given validator
isValidatorOutput :: Cooked.IsTxInfoOutput o => TScripts.TypedValidator a -> o -> Bool
isValidatorOutput v = isJust . Cooked.isScriptOutputFrom v

-- | Validates a transaction and returns the (unique) output that belongs
-- to the lotto
validateAndGetUniqueLottoOut ::
  MonadBlockChain m =>
  LottoScripts ->
  TxSkel ->
  m (TxOutRef, LedgerV2.TxOut)
validateAndGetUniqueLottoOut lScripts txSkel = do
  let lottoValidator = mkTypedValidator lScripts
  outs <- validateAndGetOuts txSkel
  case filter (isValidatorOutput lottoValidator . snd) outs of
    [] -> fail "There should be one output at the lotto address"
    [output] -> return output
    _ : _ : _ -> fail "there should be only one output at the lotto address"

-- | Validates a transaction and returns the (unique) output
-- whose value is locked by the lotto and which contains a seal.
validateAndGetUniqueLottoOutWithSeal ::
  MonadBlockChain m =>
  LottoScripts ->
  TxSkel ->
  TokenName ->
  m (TxOutRef, LedgerV2.TxOut)
validateAndGetUniqueLottoOutWithSeal lScripts txSkel sealName = do
  let lottoValidator = mkTypedValidator lScripts
  outs <- validateAndGetOuts txSkel
  let isValidatorAndSealed (_, o) =
        isValidatorOutput lottoValidator o
          && hasSeal lScripts sealName o
  case filter isValidatorAndSealed outs of
    [] ->
      fail "There should be one sealed output at the lotto address"
    [output] -> return output
    _ : _ : _ ->
      fail "There should be only one sealed output at the lotto address"
