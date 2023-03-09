{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
-- For continutations
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | The main lotto validator.
module Validators (main) where

import Lib
  ( PEmptyRec,
    fromTypedValidator,
    pdatumOf,
    pfindOwnInput,
    phasSomeAsset,
    phashSecret,
    ptxOutRefToToken,
  )
import qualified Lib
import qualified Plutarch.Api.V1.AssocMap as PMap
import Plutarch.Api.V1.Value (plovelaceValueOf)
import qualified Plutarch.Api.V1.Value as V
import Plutarch.Api.V2
  ( AmountGuarantees (Positive),
    KeyGuarantees (Sorted, Unsorted),
    PCurrencySymbol,
    PDatum,
    PDatumHash,
    PInterval,
    PMap,
    PPOSIXTime,
    PPubKeyHash,
    PScriptContext,
    PScriptPurpose,
    PTxOut,
    PValidator,
    PValue,
  )
import Plutarch.Api.V2.Tx (PTxInInfo)
import Plutarch.DataRepr (PDataFields)
import qualified Plutarch.Extra.Interval as I (pto)
import Plutarch.Extra.RationalData (PRationalData, prationalFromData)
import Plutarch.Extra.TermCont (pguardC, pletC)
import Plutarch.Internal (punsafeCoerce)
import qualified Plutarch.List as List
import Plutarch.Prelude

-- * Global lotto parameters

-- | Function type accepting the parameters of the lotto. It abstracts
-- over the currency used to authenticate the lotto and the public key hash
-- of the organiser.
-- We don't use a record because Ply would require more administrative
-- code (instances of 'PlyArg', properly qualified names &c.).
type PParameterised a = PCurrencySymbol :--> PPubKeyHash :--> a

-- * Data types

type PBidWord = PByteString

-- | On the choice of 'PPlayersBids'
--
-- To collect the bids of players, we need an association list mapping
-- public key hashes to bids. We use an association map 'PMap' but
-- we mainly use the association list wrapped by the 'PMap' rather than
-- the 'PMap' API.
--
-- The most straightforward type @PBuiltinList (PBuiltinPair _ _)@ raises
-- the error @PBuiltinPair _ _ is not a subtype of PData@. I don't know what
-- to do to mitigate this. The same happens when replacing 'PBuiltinPair'
-- with 'PPair'.
--
-- Type 'PTuple' requires an instance of 'PUnsafeLiftDecl' which seems non
-- trivial to provide. The easy way would be to provide the same instance as
-- 'PBuiltinPair', but this breaks injectivity annotation.
--
-- Finally, the 'PMap' type is a wrapper around an association list and using
-- it does not raise any of the previous errors. Particularly, @'Unsorted@
-- association maps are close to simple association lists.
type PPlayersBids = PMap 'Unsorted PPubKeyHash PBidWord

-- REVIEW should we add the TxId of the initialisation used as NFT
-- token name?
newtype PLottoDatum s
  = PLottoDatum
      ( Term
          s
          ( PDataRecord
              '[ "secretHash" ':= PByteString,
                 "secretSalt" ':= PByteString,
                 "deadline" ':= PPOSIXTime,
                 "bidAmount" ':= PValue 'Sorted 'Positive,
                 "players" ':= PPlayersBids,
                 "margin" ':= PRationalData
                 -- The margin must be accessible for users to compute their share
               ]
          )
      )
  deriving stock (Generic)
  -- NOTE: No @PTryFrom PData@ in @deriving anyclass@; cf note for 'pcoerceDatumFrom'.
  deriving anyclass
    ( PlutusType,
      PIsData,
      -- | Required for the 'pfield' method
      PDataFields,
      PEq,
      PShow
    )

-- | Coerces the type-tag of a term from @PMaybe PData@ to @PMaybe PLottoDatum@.
--
-- NOTE: See note for @pcoerceDatumFrom@.
pcoerceMaybeLottoDatumFrom ::
  forall (s :: S).
  Term s (PMaybe PData) ->
  Term s (PMaybe PLottoDatum)
pcoerceMaybeLottoDatumFrom = punsafeCoerce

instance DerivePlutusType PLottoDatum where type DPTStrat _ = PlutusTypeData

-- | Redeemer for the main validator
data PLottoRedeemer (s :: S)
  = Initialise (PEmptyRec s)
  | Play (PEmptyRec s)
  | -- | The secret of the administrator
    Resolve (Term s (PDataRecord '["_0" ':= PBidWord]))
  deriving stock (Generic)
  -- NOTE: No @PTryFrom PData@ in @deriving anyclass@; cf note for @pcoerceDatumFrom@.
  deriving anyclass (PlutusType, PIsData, PShow)

instance DerivePlutusType PLottoRedeemer where type DPTStrat _ = PlutusTypeData

-- * Validators

initialiseValidator ::
  Term
    s
    ( PPubKeyHash -- Administrator
        :--> PBuiltinList PPubKeyHash
        :--> PCurrencySymbol
        :--> PLottoDatum -- The datum of the validator
        :--> PBuiltinList PTxInInfo -- Inputs
        :--> PBuiltinList PTxOut -- Outputs
        :--> PMap 'Unsorted PDatumHash PDatum -- Data
        :--> PScriptPurpose
        :--> PUnit
    )
initialiseValidator =
  plam $
    \admin
     signatories
     (currency :: Term s PCurrencySymbol)
     datum
     inputs
     outputs
     datums
     purpose -> unTermCont $ do
        pguardC
          ( "The administration hasn't signed the transaction, \
            \the signatories are "
              <> pshow signatories
              <> " while the administrator is "
              <> pshow admin
          )
          $ pelem # admin # signatories
        inLottoInfo <- pletC $ pfindOwnInput # inputs # purpose
        inLotto <- pletC $ pfromData $ pfield @"resolved" # inLottoInfo
        pguardC
          ( "Validator (initialise): input lotto is already sealed: \
            \its value is "
              <> pshow (pfield @"value" # inLotto)
              <> "The input lotto must have no token of the currency "
              <> pshow currency
              <> "."
          )
          $ pnot
            #$ phasSomeAsset
            # (pfield @"value" # inLotto)
            # currency
        outLotto <-
          let inAddr = pfield @"address" # inLotto
           in pletC $ Lib.pfindFollowingWithAsset # outputs # inAddr # currency
        let outLottoDatum =
              Lib.pfromJust'
                # "Validator (initialise): couldn't get datum"
                #$ pcoerceMaybeLottoDatumFrom (pdatumOf # outLotto # datums)
        pguardC "datum changed" $ datum #== outLottoDatum
        margin <- pletC $ prationalFromData #$ pfield @"margin" # datum
        pguardC
          ( "Validator (initialise) the margin isn't in [0, 100], it is"
              <> pshow margin
          )
          $ margin #< 1 #&& 0 #< margin
        pguardC
          ( "Validator (initialise): players list is not empty "
              <> pshow datum
              <> "The initial player list must be empty"
          )
          $ PMap.pnull #$ pfromData
          $ pfield @"players" # datum
        -- TODO check that the hash is well-formed
        token <- pletC $ ptxOutRefToToken #$ pfield @"outRef" # inLottoInfo
        pguardC
          ( "Validator (initialise): \
            \ Output lotto value doesn't contain any seal of currency "
              <> pshow currency
              <> " with token name "
              <> pshow token
          )
          $ phasSomeAsset
            # (pfield @"value" # outLotto)
            # currency
        return $ pconstant ()

-- | The validator for the 'Play' redeemer
playValidator ::
  Term
    s
    ( PCurrencySymbol
        :--> PLottoDatum
        :--> PBuiltinList PTxInInfo -- Inputs
        :--> PBuiltinList PTxOut
        :--> PMap 'Unsorted PDatumHash PDatum
        :--> PScriptPurpose
        :--> PInterval PPOSIXTime
        :--> PUnit
    )
playValidator =
  plam $
    \currency
     (datum :: Term s PLottoDatum)
     inputs
     outputs
     datums
     purpose
     txValidTimeRange -> unTermCont $ do
        -- The transaction must happen before the deadline
        deadline <- pletC $ pfield @"deadline" # datum
        pguardC
          ( "Validator (play): Invalid time range, \
            \the validity interval of the transaction \
            \must terminate before "
              <> pshow deadline
              <> " but "
              <> pshow txValidTimeRange
              <> " ends after"
          )
          $ txValidTimeRange #<= I.pto # deadline
        -- Get the input whose validator is running
        inLotto <- pletC $ pfindOwnInput # inputs # purpose
        -- The address of the lotto
        inAddress <- pletC $ pfield @"address" #$ pfield @"resolved" # inLotto
        inValue :: Term s (PValue 'V.Sorted 'V.Positive) <-
          pletC $ pfield @"value" #$ pfield @"resolved" # inLotto
        outLotto <-
          pletC
            $ pmatch
              (Lib.pfindFollowingWithGreaterValue # outputs # inAddress # inValue)
            $ \case
              PNothing ->
                ptraceError
                  "Validator (play): output with greater value and \
                  \same locking address is either non existent or \
                  \non unique."
              PJust out -> out
        outLottoDatum :: Term s PLottoDatum <-
          pletC $
            Lib.pfromJust'
              # "Validator (play): couldn't get datum"
              #$ pcoerceMaybeLottoDatumFrom (pdatumOf # outLotto # datums)
        -- We check that 'outLottoDatum' and the datum are equal point to point,
        -- except for the 'players' field
        pguardC "Validator (play): datum's secretSalt altered" $
          (pfield @"secretSalt" # datum #== pfield @"secretSalt" # outLottoDatum)
        pguardC "Validator (play): datum's secretHash altered" $
          (pfield @"secretHash" # datum #== pfield @"secretHash" # outLottoDatum)
        pguardC "Validator (play): datum's deadline altered" $
          (pfield @"deadline" # datum #== pfield @"deadline" # outLottoDatum)
        pguardC "Validator (play): datum's bidAmount altered" $
          (pfield @"bidAmount" # datum #== pfield @"bidAmount" # outLottoDatum)
        pguardC "Validator (play): datum's margin altered" $
          (pfield @"margin" # datum #== pfield @"margin" # outLottoDatum)
        outPlayers <- pletC $ Lib.plistOfMap #$ pfromData $ pfield @"players" # outLottoDatum
        pguardC
          ( "Validator (play): the gambler's list is empty, "
              <> "it should contain at least one public key hash/bid pair."
          )
          $ pnot #$ pnull # outPlayers
        datumPlayers <- pletC $ Lib.plistOfMap #$ pfromData $ pfield @"players" # datum
        pguardC "Validator (play): There should be exactly one more new player" $
          (pnull # datumPlayers #&& Lib.pisSingleton # outPlayers)
            -- Otherwise, the tail of the out players list is the same as the
            -- previous players list
            #|| (ptail # outPlayers #== datumPlayers)
        -- Check there's an NFT in the inputs
        pguardC "No seal in the input lotto" $
          let hasSeal input =
                phasSomeAsset
                  # (pfield @"value" #$ pfield @"resolved" # input)
                  # currency
           in pany # plam hasSeal # inputs
        let outValue = pfield @"value" # outLotto
        -- Ensure there's only one seal in the output
        pguardC "Validator (play): The output lotto has more than a single seal" $
          (Lib.pwithOneTokenOf # currency #$ V.pforgetSorted $ V.pforgetPositive outValue)
        let wanted = inValue <> pfield @"bidAmount" # datum
         in pguardC
              ( "Validator (play): Not enough money added to the pot, \
                \expects at least "
                  <> pshow wanted
                  <> " but there is only "
                  <> pshow outValue
              )
              $ wanted #<= outValue
        return $ pconstant ()

-- | We never need to retrieve the token name of the seal of the running lotto
-- because the validator requires the value of the output lotto to be greater
-- than the value of the running lotto, so the seal must be passed to the
-- output lotto.
resolutionValidator ::
  Term
    s
    ( PCurrencySymbol -- Lotto parameter
        :--> PPubKeyHash -- Lotto parameter
        :--> PAsData PByteString -- The secret
        :--> PLottoDatum -- Validator datum
        :--> PBuiltinList PTxInInfo -- Inputs
        :--> PBuiltinList PTxOut -- Outputs
        :--> PBuiltinList PPubKeyHash -- Signatories
        :--> PScriptPurpose
        :--> PUnit
    )
resolutionValidator =
  plam $
    \currency
     administrator
     (secret :: Term s (PAsData PByteString))
     datum
     inputs
     outputs
     signatories
     purpose ->
        let inLotto = pfindOwnInput # inputs # purpose
            inLottoOut = pfield @"resolved" # inLotto
            inLottoVal = pfield @"value" # inLottoOut
         in unTermCont $ do
              -- REVIEW may be optional, one could be allowed to close an illegal lotto
              pguardC "No seal in intput lotto" $
                phasSomeAsset # inLottoVal # currency
              -- Check that the administrator signs the transaction
              pguardC "Resolution not signed by the administrator" $
                pelem # administrator # signatories
              -- Verify the authenticity of the secret
              secretSalt <- pletC $ pfield @"secretSalt" # datum
              builtHash <- pletC $ phashSecret # secretSalt # pfromData secret
              pguardC
                ( "Unauthentic secret: hashing ["
                    <> pshow (pfromData secret)
                    <> "] with salt ["
                    <> pshow secretSalt
                    <> "] yields hash ["
                    <> pshow builtHash
                    <> "] but ["
                    <> pshow (pfield @"secretHash" # datum)
                    <> "] was expected."
                )
                $ builtHash #== pfield @"secretHash" # datum
              -- Ensure the seal has been burned (no output has the NFT)
              pguardC "There is still a seal" $
                pnot #$ pany # (Lib.pwithAsset # currency) # outputs
              -- Elect the winners, and ensure they can retrieve their money
              let -- REVIEW what if Ada is not the money gambled?
                  pot :: Term s PInteger
                  pot = plovelaceValueOf # inLottoVal
                  margin = prationalFromData #$ pfield @"margin" # datum
                  adminDue = Lib.payOrganiser # pot # margin
              pguardC "Organisation is not paid enough" $
                V.psingleton
                  # V.padaSymbol
                  # V.padaToken
                  # adminDue
                  #<= Lib.pcollectPaymentFor
                  # administrator
                  # outputs
              let bids = Lib.plistOfMap #$ pfield @"players" # datum
                  payments = Lib.payGamblers # Lib.scoreDiffZeros # pot # margin # pfromData secret # bids
                  checkPayment :: Term s (PBuiltinPair (PAsData PPubKeyHash) (PAsData PInteger) :--> PUnit :--> PUnit)
                  checkPayment = plam $ \gamblerDue _ -> unTermCont $ do
                    let gambler = pfromData $ pfstBuiltin # gamblerDue
                        due = pfromData $ psndBuiltin # gamblerDue
                        collected = Lib.pcollectPaymentFor # gambler # outputs
                    pguardC
                      ( "Gambler "
                          <> pshow gambler
                          <> " is not paid enough. "
                          <> "Expected at least ["
                          <> pshow due
                          <> "] but got ["
                          <> pshow collected
                          <> "]. In context"
                          <> ("pot = " <> pshow pot)
                      )
                      (V.psingleton # V.padaSymbol # V.padaToken # due #<= collected)
                    return $ pconstant ()
              -- Compute players share and give the rest to the admin
              return $ List.pfoldr # checkPayment # pconstant () # payments

main' ::
  ClosedTerm
    ( PCurrencySymbol
        :--> PPubKeyHash
        :--> PAsData PLottoDatum
        :--> PAsData PLottoRedeemer
        :--> PScriptContext
        :--> POpaque
    )
main' = plam $ \currency administrator datum redeemer ctx ->
  let txInfo = pfield @"txInfo" # ctx
      purpose = pfield @"purpose" # ctx
      inputs = pfield @"inputs" # txInfo
      outputs = pfield @"outputs" # txInfo
      datums = pfield @"datums" # txInfo
      signatories = List.pmap # plam pfromData #$ pfield @"signatories" # txInfo
      validRange = pfield @"validRange" # txInfo
   in pmatch
        (pfromData redeemer)
        $ \case
          Initialise _ ->
            popaque $
              initialiseValidator
                # administrator
                # signatories
                # currency
                # pfromData datum
                # inputs
                # outputs
                # datums
                # purpose
          Play _ ->
            popaque $
              playValidator
                # currency
                # pfromData datum
                # inputs
                # outputs
                # datums
                # purpose
                # validRange
          Resolve s ->
            let secret = pfield @"_0" # s
             in popaque $
                  resolutionValidator
                    # currency
                    # administrator
                    # secret
                    # pfromData datum
                    # inputs
                    # outputs
                    # signatories
                    # purpose

-- | The properly typed validator.
main :: ClosedTerm (PParameterised PValidator)
main =
  plam $ \currency administrator ->
    fromTypedValidator $ main' # currency # administrator
