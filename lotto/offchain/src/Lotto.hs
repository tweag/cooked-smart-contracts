-- | Provide atomic transactions for the lotto.
--
-- This module is expected to be imported qualified
module Lotto
  ( Setup (..),
    organiser,
    script,
    sopen,
    open,
    smintSeal,
    mintSeal,
    splay,
    play,
    resolve,
    sresolve,
    spost,
  )
where

import qualified Cardano.Node.Emulator.TimeSlot as TimeSlot
import qualified Cooked
import qualified Data
import Data.Default (Default, def)
import qualified Data.Map as HMap
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Time.Clock as Time
import qualified Ledger
import qualified MaybeMalformed as MM
import qualified Lib
import Optics (set, view, (%), (&), (<&>))
import qualified Plutus.Script.Utils.Ada as Ada
import qualified Plutus.Script.Utils.Typed as TScripts
import qualified Plutus.V2.Ledger.Api as LedgerV2
import qualified PlutusTx.AssocMap as Map
import PlutusTx.Prelude (BuiltinByteString)
import qualified PlutusTx.Prelude as Tx
import Prelude

-- * The scripts and an organiser

-- | Wallet of organiser (we take 4 to just not take 1, which is the default
-- one).
organiser :: Cooked.Wallet
organiser = Cooked.wallet 4

-- | Lotto scripts administrated by fdj
script :: Lib.LottoScripts
script = Lib.unsafeReadLottoScripts $ Cooked.walletPKHash organiser

-- | The setup of a lotto: all parameters except the secret and salt.
data Setup = Setup
  { -- | The remaining time to gamble
    duration :: Time.NominalDiffTime,
    -- | How much to bid
    bidAmount :: LedgerV2.Value,
    -- | How much goes to the administrator
    margin :: Tx.Rational
  }

-- | Some sane default parameters for the lotto. The deadline defaults to 3
-- days, the bid amount to 10 Ada and the margin to 3%.
instance Default Setup where
  def =
    Setup
      { duration = 3 * Time.nominalDay,
        bidAmount = Lib.ada 10,
        margin = Tx.unsafeRatio 3 100
      }

-- * "Endpoints". There are two paradigms,

-- - either we build a skeleton in a block chain monad,
-- - or be build transactions.
--
-- Transactions can be 'Cooked.Tweak'ed while skeletons can be further
-- modified and then validated as one wants.
--
-- There is also a third approach which is to tweak a minimal skeleton.

-- | Opening skeleton. Produces a skeleton whose output has 10 Ada locked by
-- the lotto.
sopen ::
  Cooked.MonadBlockChainWithoutValidation m =>
  -- | Hashed secret
  BuiltinByteString ->
  -- | Salt used to hash the secret
  BuiltinByteString ->
  m Cooked.TxSkel
sopen secretHash secretSalt = do
  time <- Cooked.currentTime
  let someDatum =
        Data.initLottoDatum
          secretHash
          secretSalt
          (time + TimeSlot.nominalDiffTimeToPOSIXTime (duration def))
          (bidAmount def)
          (margin def)
  return $
    Cooked.txSkelTemplate
      { Cooked.txSkelOuts =
          [ Cooked.paysScript
              (Lib.mkTypedValidator script)
              someDatum
              (Lib.ada 10)
          ],
        Cooked.txSkelSigners = [organiser]
      }

-- | Open a lotto, with additional parameters.
open ::
  Cooked.MonadBlockChain m =>
  -- | The scripts (validator and seal minting policy) TODO say where they can be found
  Setup ->
  -- | Hash of secret.
  BuiltinByteString ->
  -- | Salt of secret.
  BuiltinByteString ->
  m (LedgerV2.TxOutRef, LedgerV2.TxOut)
open Setup {duration, bidAmount, margin} secretHash secretSalt = do
  time <- Cooked.currentTime
  skeleton <-
    sopen secretHash secretSalt
      <&> set (Lib.txSkelTypedDatumT % Data.bidAmount) bidAmount
      <&> set (Lib.txSkelTypedDatumT % Data.margin) margin
      <&> set (Lib.txSkelTypedDatumT % Data.deadline) (time + TimeSlot.nominalDiffTimeToPOSIXTime duration)
  Lib.validateAndGetUniqueLottoOut script skeleton

-- | Skeleton for authentication.
smintSeal ::
  Cooked.MonadBlockChain m =>
  -- | Output reference used to mint the seal
  LedgerV2.TxOutRef ->
  -- | Value to be propagated
  LedgerV2.Value ->
  -- | The name of the seal and the skeleton
  m (LedgerV2.TokenName, Cooked.TxSkel)
smintSeal outRef value = do
  let validator = Lib.mkTypedValidator script
      sealPolicy =
        TScripts.Versioned (Lib.mkMintingPolicy script) TScripts.PlutusV2
      sealName = Lib.txOutRefToToken outRef
      currency = Ledger.scriptCurrencySymbol sealPolicy
      mintedValue =
        LedgerV2.Value $ Map.singleton currency $ Map.singleton sealName 1
  datum <- fromJust <$> Data.datumOfTxOut outRef
  return
    ( sealName,
      Cooked.txSkelTemplate
        { Cooked.txSkelIns = HMap.singleton outRef Data.initialise,
          Cooked.txSkelOuts =
            [Cooked.paysScript validator datum (mintedValue <> value)],
          Cooked.txSkelMints =
            Cooked.txSkelMintsFromList [Lib.mintSeal script sealName outRef],
          Cooked.txSkelSigners = [organiser]
        }
    )

-- | Mint a seal authenticating the lotto, and yield the authenticated lotto.
-- The output value only contains the minted seal.
mintSeal ::
  Cooked.MonadBlockChain m =>
  -- | The seal needs a TxOutRef to ensure its uniqueness.
  LedgerV2.TxOutRef ->
  -- | The value to be propagated
  LedgerV2.Value ->
  -- | The new (authentic) lotto and the name of the seal.
  m (LedgerV2.TxOutRef, LedgerV2.TxOut, LedgerV2.TokenName)
mintSeal outRef value = do
  (sealName, skeleton) <- smintSeal outRef value
  (oref, o) <- Lib.validateAndGetUniqueLottoOutWithSeal script skeleton sealName
  return (oref, o, sealName)

-- | Generate a skeleton to play as a gambler. The output locked by the lotto
-- contains the seal and the value gambled, nothing more.
-- Typically one has to propagate the value of the previous lotto input his or herself.
splay ::
  Cooked.MonadBlockChainWithoutValidation m =>
  -- | Reference to an output {locked by a lotto}
  LedgerV2.TxOutRef ->
  -- | Propagated value
  LedgerV2.Value ->
  -- | The guess
  BuiltinByteString ->
  -- | Who will receive the money, and also signer of the transaction.
  Cooked.Wallet ->
  -- | Value gambled.
  LedgerV2.Value ->
  m Cooked.TxSkel
splay lottoRef value secret forWho gambled = do
  let txSkelIns = HMap.singleton lottoRef Data.play
  inDatum <- fromJust <$> Data.datumOfTxOut lottoRef
  return $
    Cooked.txSkelTemplate
      { -- The transaction is valid up to the deadline
        Cooked.txSkelValidityRange = LedgerV2.to $ view Data.deadline inDatum - 1,
        -- That @- 1@ has no real reason to be there, but without that,
        -- validation fails. The interval displayed by the onchain code
        -- is incremented by 1 for some reason (maybe because of
        -- boundaries inclusiveness)
        Cooked.txSkelOuts =
          [ Cooked.paysScript
              (Lib.mkTypedValidator script)
              (Data.addPlayer forWho secret inDatum)
              (value <> gambled)
          ],
        Cooked.txSkelIns,
        Cooked.txSkelSigners = [forWho]
      }

-- | Take part in the lotto as a gambler. The transaction validity range always
-- stops just before the deadline. Also propagates the value of the input
-- lotto.
play ::
  (Cooked.MonadBlockChain m) =>
  -- | The UTxO resulting from the previous lotto transaction
  LedgerV2.TxOutRef ->
  -- | The name of the seal authenticated the lotto.
  LedgerV2.TokenName ->
  LedgerV2.Value ->
  -- | The secret gambled, not hashed
  BuiltinByteString ->
  -- | In the name of who (this may not be the transaction signatory)
  Cooked.Wallet ->
  -- | The value gambled
  LedgerV2.Value ->
  m (LedgerV2.TxOutRef, LedgerV2.TxOut)
play lottoRef sealName value secret forWho gambled = do
  skeleton <- splay lottoRef value secret forWho gambled
  Lib.validateAndGetUniqueLottoOutWithSeal script skeleton sealName

sresolve ::
  Cooked.MonadBlockChainWithoutValidation m =>
  BuiltinByteString ->
  (LedgerV2.TxOutRef, LedgerV2.TxOut) ->
  LedgerV2.TokenName ->
  m Cooked.TxSkel
sresolve secret (lottoRef, lotto) sealName = do
  Just datum <- Data.datumOfTxOut lottoRef
  let potAda =
        Map.lookup
          LedgerV2.adaSymbol
          (LedgerV2.getValue $ view Cooked.outputValueL lotto)
          >>= Map.lookup LedgerV2.adaToken & fromMaybe 0
      payments =
        Lib.payGamblers
          Lib.scoreDiffZeros
          potAda
          (view Data.margin datum)
          secret
          (map (fmap MM.fromWellFormed) $ Map.toList $ view Data.players datum)
  return $
    Cooked.txSkelTemplate
      { Cooked.txSkelIns = HMap.singleton lottoRef $ Data.resolve secret,
        Cooked.txSkelOuts = [Cooked.paysPK pk (Ada.lovelaceValueOf v) | (pk, v) <- payments],
        Cooked.txSkelMints = Cooked.txSkelMintsFromList [Lib.burnSeal script sealName],
        Cooked.txSkelSigners = [organiser]
      }

resolve ::
  Cooked.MonadBlockChain m =>
  -- | The secret, in clear.
  BuiltinByteString ->
  -- | The lotto to resolve
  (LedgerV2.TxOutRef, LedgerV2.TxOut) ->
  -- | The name of the seal
  LedgerV2.TokenName ->
  m [(LedgerV2.TxOutRef, LedgerV2.TxOut)]
resolve secret (lottoRef, lotto) sealName =
  sresolve secret (lottoRef, lotto) sealName >>= Lib.validateAndGetOuts

-- | Generate a skeleton that posts the lotto as a reference script on an
-- output locked by the organiser.
spost ::
  Cooked.MonadBlockChainWithoutValidation m =>
  m Cooked.TxSkel
spost =
  return $
    Cooked.txSkelTemplate
      { Cooked.txSkelOpts = def {Cooked.txOptEnsureMinAda = True},
        Cooked.txSkelOuts =
          [ Cooked.paysPKWithReferenceScript
              (Cooked.walletPKHash organiser)
              mempty
              (Lib.mkTypedValidator script)
          ],
        Cooked.txSkelSigners = [organiser]
      }
