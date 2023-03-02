{-# LANGUAGE TypeApplications #-}

-- | Define sequences of transactions.
module Scenarii where

import qualified Cardano.Node.Emulator.TimeSlot as TimeSlot
import Control.Applicative (Alternative)
import Control.Monad (void)
import qualified Cooked
import qualified Data
import Data.Default (def)
import Data.Functor ((<&>))
import qualified Data.Map as HMap
import Data.Maybe (fromMaybe, fromJust)
import qualified Data.Set as Set
import qualified Lib
import qualified Lotto
import Optics (set, view, (&), (^.))
import qualified Plutus.Script.Utils.Ada as Ada
import qualified Plutus.V2.Ledger.Api as LedgerV2
import qualified PlutusTx.AssocMap as Map
import PlutusTx.Prelude (BuiltinByteString)
import qualified PlutusTx.Prelude as Tx

import qualified Ledger.Scripts as Pl hiding (validatorHash)
import qualified Ledger.Slot as Pl
import qualified Ledger.Typed.Scripts as Pl
import qualified Plutus.Script.Utils.Ada as Pl
import qualified Plutus.Script.Utils.Value as Pl hiding (adaSymbol, adaToken)
import qualified Plutus.V1.Ledger.Interval as Pl
import qualified Plutus.V2.Ledger.Api as Pl hiding (TxOut, adaSymbol, adaToken)
import qualified Plutus.V2.Ledger.Tx as Pl
import qualified PlutusTx.Prelude as Pl

import Prelude

infixr 9 //

-- | A convenience for arguments with default.
(//) :: Maybe a -> a -> a
(//) = flip fromMaybe

-- | Players
alice, bob :: Cooked.Wallet
alice = Cooked.wallet 2
bob = Cooked.wallet 3

-- | Try to play on a lotto that hasn't been properly initialised.
playFoul ::
  Cooked.MonadModalBlockChain m =>
  Lotto.Setup ->
  -- | Salt
  BuiltinByteString ->
  -- | Secret
  BuiltinByteString ->
  -- | Guess
  BuiltinByteString ->
  -- | Money gambled (defaults to 10 Ada)
  Maybe LedgerV2.Value ->
  m ()
playFoul setup salt secret guess gambled = do
  let hashedSecret = Lib.hashSecret secret (Just salt)
  (initLottoRef, initLotto) <- Lotto.open setup hashedSecret salt
  let dummyToken = LedgerV2.TokenName $ Tx.sha3_256 "0"
  void $
    Lotto.play
      initLottoRef
      dummyToken
      (view Cooked.outputValueL initLotto)
      (Lib.hashSecret guess (Just salt))
      Lotto.organiser
      (gambled // Lib.ada 10)

-- | A simple play where the administrator opens the lotto and Alice plays once
-- on it.
alicePlaysAlone ::
  Cooked.MonadModalBlockChain m =>
  Lotto.Setup ->
  -- | Salt
  BuiltinByteString ->
  -- | Secret
  BuiltinByteString ->
  -- | Guess
  BuiltinByteString ->
  -- | Amount she bids (defaults to 10 Ada)
  Maybe LedgerV2.Value ->
  m ()
alicePlaysAlone setup salt secret guess amount = do
  let hashedSecret = Lib.hashSecret secret (Just salt)
  (initLottoRef, initLotto) <-
    Lotto.open setup hashedSecret salt
      -- Alice signs
      `Cooked.withTweak` Cooked.setTweak
        Cooked.txSkelSignersL
        [alice]
  (authenticatedLottoRef, authenticatedLotto, seal) <-
    Lotto.mintSeal initLottoRef (view Cooked.outputValueL initLotto)
  play <-
    Lotto.play
      authenticatedLottoRef
      seal
      (view Cooked.outputValueL authenticatedLotto)
      (Lib.hashSecret guess (Just salt))
      alice
      (amount // Lib.ada 10)
  void $ Lotto.resolve secret play seal

-- | A simple play where the administrator opens the lotto and Alice plays once
-- on it, except her guess is malformed, in the sense that it does not
-- (de)serialises to a 'BuiltinByteString'.
alicePlaysAloneWithMalformedGuess ::
  Cooked.MonadModalBlockChain m =>
  Lotto.Setup ->
  -- | Salt
  BuiltinByteString ->
  -- | Secret
  BuiltinByteString ->
  -- | Amount she bids
  LedgerV2.Value ->
  m ()
alicePlaysAloneWithMalformedGuess setup salt secret amount = do
  let hashedSecret = Lib.hashSecret secret (Just salt)
  (initLottoRef, initLotto) <-
    Lotto.open setup hashedSecret salt
      -- Alice signs
      `Cooked.withTweak` Cooked.setTweak
        Cooked.txSkelSignersL
        [alice]
  (authenticatedLottoRef, authenticatedLotto, seal) <-
    Lotto.mintSeal initLottoRef (view Cooked.outputValueL initLotto)
  let txSkelIns = HMap.singleton authenticatedLottoRef Data.play
  inDatum <- fromJust <$> Data.datumOfTxOut authenticatedLottoRef
  let skeleton = Cooked.txSkelTemplate
          { -- The transaction is valid up to the deadline
            Cooked.txSkelValidityRange = LedgerV2.to $ view Data.deadline inDatum - 1,
            -- That @- 1@ has no real reason to be there, but without that,
            -- validation fails. The interval displayed by the onchain code
            -- is incremented by 1 for some reason (maybe because of
            -- boundaries inclusiveness)
            Cooked.txSkelOuts =
              [ Cooked.Pays
                  ( Cooked.ConcreteOutput
                      (Lib.mkTypedValidator Lotto.script)
                      Nothing
                      (view Cooked.outputValueL authenticatedLotto <> amount)
                      (Cooked.TxSkelOutDatum
                          (Data.addPlayer
                              alice
                              (Lib.hashSecret "FIXME" (Just salt))
                              inDatum))
                      (Nothing @(Pl.Versioned Pl.Script))
                  )
              ],
            Cooked.txSkelIns,
            Cooked.txSkelSigners = [alice]
          }
  play <- Lib.validateAndGetUniqueLottoOutWithSeal Lotto.script skeleton seal
  void $ Lotto.resolve secret play seal

-- | Alice tries to sign the initialisation transaction (which mints the
-- seal) on her own. This is forbidden.
aliceTriesToMint ::
  Cooked.MonadModalBlockChain m =>
  Lotto.Setup ->
  m ()
aliceTriesToMint setup = do
  let hashedSecret = Lib.hashSecret "foo" (Just "bar")
  (unauthLottoRef, unauthLotto) <- Lotto.open setup hashedSecret "bar"
  void $
    Lotto.mintSeal unauthLottoRef (unauthLotto ^. Cooked.outputValueL)
      `Cooked.withTweak` Cooked.setTweak
        Cooked.txSkelSignersL
        [alice]

-- | Same as above, but we don't use tweaks and use simple optics instead
aliceTriesToMintAgain :: Cooked.MonadBlockChain m => Lotto.Setup -> m ()
aliceTriesToMintAgain setup = do
  let hashedSecret = Lib.hashSecret "foo" (Just "bar")
  (unauthLottoRef, unauthLotto) <- Lotto.open setup hashedSecret "bar"
  void $
    Lotto.smintSeal unauthLottoRef (unauthLotto ^. Cooked.outputValueL)
      <&> snd
      <&> set Cooked.txSkelSignersL [alice]
      >>= Cooked.validateTxSkel

-- | A trace where two players ('alice' and 'bob') play for themselves.
-- They bet their respective name.
aliceAndBobPlay ::
  Cooked.MonadModalBlockChain m =>
  Lotto.Setup ->
  -- | Salt
  BuiltinByteString ->
  -- | Secret
  BuiltinByteString ->
  m ()
aliceAndBobPlay setup salt secret = do
  let hashedSecret = Lib.hashSecret secret (Just salt)
  (initLottoRef, initLotto) <- Lotto.open setup hashedSecret salt
  (authenticatedLottoRef, authenticatedLotto, seal) <-
    Lotto.mintSeal initLottoRef (initLotto ^. Cooked.outputValueL)
  bobPlays <-
    Lotto.play
      authenticatedLottoRef
      seal
      (authenticatedLotto ^. Cooked.outputValueL)
      (Lib.hashSecret "bob" (Just salt))
      bob
      (Lib.ada 10)
  alicePlays <-
    Lotto.play
      (fst bobPlays)
      seal
      (snd bobPlays ^. Cooked.outputValueL)
      (Lib.hashSecret "alice" (Just salt))
      alice
      (Lib.ada 10)
  void $ Lotto.resolve secret alicePlays seal

alicePlaysForBob :: Cooked.MonadModalBlockChain m => Lotto.Setup -> m ()
alicePlaysForBob setup = do
  let salt = "sel"
      hashedSecret = Lib.hashSecret "secret" (Just salt)
  (initLottoRef, initLotto) <- Lotto.open setup hashedSecret salt
  (authenticatedLottoRef, authenticatedLotto, seal) <-
    Lotto.mintSeal initLottoRef (initLotto ^. Cooked.outputValueL)
  alicePlays <-
    Lotto.play
      authenticatedLottoRef
      seal
      (authenticatedLotto ^. Cooked.outputValueL)
      (Lib.hashSecret "bob" $ Just salt)
      bob
      (Lib.ada 10)
      `Cooked.withTweak` Cooked.setTweak
        Cooked.txSkelSignersL
        [alice]
  void $ Lotto.resolve "secret" alicePlays seal

-- alicePlaysOnTwoLottos :: Cooked.MonadBlockChain m => m ()
-- alicePlaysOnTwoLottos = do
--   let salt1 = "sel1"
--       hashedSecret1 = Lib.hashSecret "secret1" (Just salt1)
--       salt2 = "sel2"
--       hashedSecret2 = Lib.hashSecret "secret2" (Just salt2)

-- | A double satisfaction attempt on the Play resolution that fails.
-- This transaction tries to make a single output locked by a lotto script
-- satisfy two inputs locked by the same lotto script.
-- It fails because each input wants to see its seal on the output, but we
-- also check that an output locked by a lotto has at most one seal.
doubleSatisfaction ::
  Cooked.MonadModalBlockChain m =>
  Lotto.Setup ->
  -- | Shared Salt
  BuiltinByteString ->
  -- | Shared Secret
  BuiltinByteString ->
  m ()
doubleSatisfaction setup salt secret = do
  let validator = Lib.mkTypedValidator Lotto.script
  -- Compute parameters.
  let hashedSecret = Lib.hashSecret secret (Just salt)
  time <- Cooked.currentTime
  let deadline = time + TimeSlot.nominalDiffTimeToPOSIXTime (Lotto.duration setup)
  -- The administrator (Wallet 1) opens two lottos with the exact same datums.
  -- This is a big restriction for the attack; but still it goes through \o
  let initDatum = Data.initLottoDatum hashedSecret salt deadline (Lotto.bidAmount setup) (Lotto.margin setup)
      initSkeleton =
        Cooked.txSkelTemplate
          { Cooked.txSkelOpts = def,
            Cooked.txSkelOuts = [Cooked.paysScript validator initDatum (Lib.ada 10)],
            Cooked.txSkelSigners = [Cooked.wallet 1]
          }
  (initLotto1, i1) <- Lib.validateAndGetUniqueLottoOut Lotto.script initSkeleton
  (initLotto2, i2) <- Lib.validateAndGetUniqueLottoOut Lotto.script initSkeleton
  -- Authenticate both lottos.
  (authedLottoRef1, authedLotto1, _seal1) <- Lotto.mintSeal initLotto1 (i1 ^. Cooked.outputValueL)
  (authedLottoRef2, authedLotto2, _seal2) <- Lotto.mintSeal initLotto2 (i2 ^. Cooked.outputValueL)
  -- Get the values in those outputs
  let authedValue1 = Cooked.outputValue authedLotto1
      authedValue2 = Cooked.outputValue authedLotto2
  -- Create output datums, validity range and values. Apart from the values,
  -- this is just playing normally.
  let outDatum = Data.addPlayer alice "word1" initDatum
      validityRange = LedgerV2.to $ view Data.deadline initDatum - 1
      joinValue = authedValue1 Tx.\/ authedValue2
      restValue = (authedValue1 Tx.+ authedValue2) Tx.- joinValue
  -- create the transaction skeleton.
  let skeleton =
        Cooked.txSkelTemplate
          { Cooked.txSkelOpts = def,
            Cooked.txSkelIns =
              HMap.fromList
                [ (authedLottoRef1, Data.play),
                  (authedLottoRef2, Data.play)
                ],
            Cooked.txSkelOuts =
              [ -- An output to make both validators happy. It contains more value
                -- than input + pot for both of the inputs, including both seals.
                -- REVIEW: It probably leads to un-resolvable lottos as well?
                Cooked.paysScript validator outDatum (joinValue Tx.+ Lib.ada 10),
                -- An output going to us and taking whatever we can. This can
                -- potentially be a lot; basically one of the pots minus 10 ADA.
                Cooked.paysPK (Cooked.walletPKHash alice) restValue
              ],
            Cooked.txSkelValidityRange = validityRange,
            Cooked.txSkelSigners = [alice]
          }
  -- And... score!
  void $ Cooked.validateTxSkel skeleton

-- | Double satisfaction on resolution. Alice plays on two lottos, but instead
-- of receiving two payments, for the two lottos she played on, she receives
-- only one payment containing the max of the two.
doubleSatisfactionResolution ::
  Cooked.MonadModalBlockChain m =>
  -- | Shared Salt
  BuiltinByteString ->
  -- | Shared Secret
  BuiltinByteString ->
  m ()
doubleSatisfactionResolution salt secret = do
  -- Compute parameters.
  let hashedSecret = Lib.hashSecret secret (Just salt)
  -- Open two lottos with the same parameters (REVIEW can we change them?)
  (initLotto1, i1) <- Lotto.open def hashedSecret salt
  (initLotto2, i2) <- Lotto.open def hashedSecret salt
  -- Authenticate both lottos.
  (authedLottoRef1, authedLotto1, seal1) <- Lotto.mintSeal initLotto1 (i1 ^. Cooked.outputValueL)
  (authedLottoRef2, authedLotto2, seal2) <- Lotto.mintSeal initLotto2 (i2 ^. Cooked.outputValueL)
  -- Bob plays 10 Ada on each lotto
  (lottoRef1, lotto1) <-
    Lotto.play authedLottoRef1 seal1 (authedLotto1 ^. Cooked.outputValueL) "word1alice" alice (Lib.ada 10)
  (lottoRef2, lotto2) <-
    Lotto.play authedLottoRef2 seal2 (authedLotto2 ^. Cooked.outputValueL) "word2alice" alice (Lib.ada 10)
  -- The resolution
  datum1 <- Data.datumOfTxOut lottoRef1 <&> fromMaybe (error "zut")
  datum2 <- Data.datumOfTxOut lottoRef2 <&> fromMaybe (error "zut")
  -- Compute the payments to tamper them
  let payments1 =
        Lib.payGamblers
          Lib.scoreDiffZeros
          (Lib.countAda $ lotto1 ^. Cooked.outputValueL)
          (datum1 ^. Data.margin)
          secret
          (Map.toList $ datum1 ^. Data.players)
      payments2 =
        Lib.payGamblers
          Lib.scoreDiffZeros
          (Lib.countAda $ lotto2 ^. Cooked.outputValueL)
          (datum2 ^. Data.margin)
          secret
          (Map.toList $ datum2 ^. Data.players)
      -- The values Alice receive from both payments. We unstructure them
      -- for easier manipulation.
      vFrom1 = snd . head $ payments1
      vFrom2 = snd . head $ payments2
      -- Give to Alice only the maximum between the two quantities of Ada she
      -- deserves, instead of the sum.
      fraudulousPayment = [(Cooked.walletPKHash alice, max vFrom1 vFrom2)]
      -- Bob steals what he can
      theft = [(Cooked.walletPKHash bob, vFrom1 + vFrom2 - max vFrom1 vFrom2)]
      skeleton =
        Cooked.txSkelTemplate
          { Cooked.txSkelIns =
              HMap.fromList
                [ (lottoRef1, Data.resolve secret),
                  (lottoRef2, Data.resolve secret)
                ],
            Cooked.txSkelOuts =
              [ Cooked.paysPK pk (Ada.lovelaceValueOf v)
                | (pk, v) <- fraudulousPayment ++ theft
              ],
            Cooked.txSkelMints =
              Cooked.txSkelMintsFromList
                [ Lib.burnSeal Lotto.script seal1,
                  Lib.burnSeal Lotto.script seal2
                ],
            -- The organiser still has to sign, but they can be a partner of
            -- Bob
            Cooked.txSkelSigners = [Lotto.organiser]
          }
  void $ Cooked.validateTxSkel skeleton

-- * Plutus V2 features.

-- RF suffixed function experiment with reference scripts.

-- | Make a transaction skeleton use a referenced script
setReferenceScript ::
  -- | Reference to the script
  LedgerV2.TxOutRef ->
  -- | Reference to input
  LedgerV2.TxOutRef ->
  Cooked.TxSkelRedeemer ->
  Cooked.TxSkel ->
  Cooked.TxSkel
setReferenceScript refscript input redeemer =
  set Cooked.txSkelInsReferenceL (Set.singleton refscript)
    . set Cooked.txSkelInsL (HMap.singleton input redeemer)

-- | A normal play session using reference scripts
alicePlaysWithRF ::
  Cooked.MonadBlockChain m =>
  m ()
alicePlaysWithRF = do
  let hashedSecret = Lib.hashSecret "secret" (Just "salt")
  (initLottoRef, initLotto) <- Lotto.open def hashedSecret "salt"
  setLottoRefScript <-
    Lotto.spost >>= Lib.validateAndGetOuts <&> head
      <&> fst
      <&> setReferenceScript
  (seal, authSkel) <-
    Lotto.smintSeal initLottoRef (initLotto ^. Cooked.outputValueL)
  (authLottoRef, authLotto) <-
    Lib.validateAndGetUniqueLottoOutWithSeal
      Lotto.script
      (authSkel & setLottoRefScript initLottoRef (Data.rinitialise))
      seal
  (lottoPlayedRef, lottoPlayed) <-
    Lotto.splay authLottoRef (authLotto ^. Cooked.outputValueL) "guess" alice (Lotto.bidAmount def)
      <&> setLottoRefScript authLottoRef Data.rplay
        >>= flip
          (Lib.validateAndGetUniqueLottoOutWithSeal Lotto.script)
          seal
  void $
    Lotto.sresolve "secret" (lottoPlayedRef, lottoPlayed) seal
      <&> setLottoRefScript lottoPlayedRef (Data.rresolve "secret")

-- * Attacks

standardTrace :: (Alternative m, Cooked.MonadModalBlockChain m) => m ()
standardTrace = aliceAndBobPlay def "string1" "string2"

tryTokenDuplication :: (Alternative m, Cooked.MonadModalBlockChain m) => m ()
tryTokenDuplication =
  Cooked.somewhere
    (Cooked.dupTokenAttack (\_ n -> n + 1) (Cooked.wallet 1))
    standardTrace
