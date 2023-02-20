{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
-- For continutations
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Plutarch related utility functions
module Lib
  ( plistOfMap,
    pfoldrElements,
    pisSingleton,
    pfindDatum,
    pfindOwnInput,
    pspendsOutput,
    pfromJust',
    ptxOutRefToToken,
    phasSomeAsset,
    phashSecret,
    pcollectPaymentFor,
    PEmptyRec,
    pdatumOf,
    fromTypedValidator,
    fromTypedMintingPolicy,
    constantScore,
    scoreDiffZeros,
    payGamblers,
    payOrganiser,
    pfindFollowingWithAsset,
    pfindFollowingWithGreaterValue,
    pwithAsset,
    pwithOneTokenOf,
  )
where

import Plutarch.Api.V1.Address (PCredential (PPubKeyCredential, PScriptCredential))
import qualified Plutarch.Api.V1.AssocMap as PMap
import qualified Plutarch.Api.V1.Value as V
import Plutarch.Api.V2
  ( KeyGuarantees (Unsorted),
    PAddress,
    PCurrencySymbol,
    PDatum,
    PDatumHash,
    PMap (PMap),
    PMintingPolicy,
    POutputDatum (PNoOutputDatum, POutputDatum, POutputDatumHash),
    PPubKeyHash,
    PScriptContext,
    PScriptPurpose (PSpending),
    PTokenName (PTokenName),
    PTxOut,
    PValidator,
    PValue (PValue),
  )
import Plutarch.Api.V2.Tx (PTxInInfo, PTxOutRef)
import Plutarch.Builtin (ppairDataBuiltin)
import Plutarch.Extra.RationalData (PRationalData, prationalFromData)
import Plutarch.Extra.TermCont (pmatchC)
import Plutarch.Internal (punsafeCoerce)
import qualified Plutarch.Monadic as P
import Plutarch.Num (PNum ((#*), (#+), (#-)))
import qualified Plutarch.Num as Num
import Plutarch.Prelude
import Plutarch.Rational (pfromInteger, (#/))
import Plutarch.Show (PShow (pshow'))

-- | An empty Plutarch record
type PEmptyRec s = Term s (PDataRecord '[])

-- * Some plutarch utilities

-- To derive automatically 'Show'
instance PShow PRationalData where
  pshow' _ prat = pshow $ prationalFromData # prat

plistOfMap :: ClosedTerm (PMap any k v :--> PBuiltinList (PBuiltinPair (PAsData k) (PAsData v)))
plistOfMap = phoistAcyclic $ plam $ \m ->
  pmatch m $ \case PMap e -> e

-- | Fold over the elements of a map.
pfoldrElements :: PIsData a => ClosedTerm ((a :--> b :--> b) :--> b :--> PMap any k a :--> b)
pfoldrElements = phoistAcyclic $ plam $ \f i m ->
  pfoldr # plam (\p acc -> f # pfromData (psndBuiltin # p) # acc) # i #$ plistOfMap # m

-- | True if the list contains only one element.
pisSingleton :: (PListLike l, PElemConstraint l a) => ClosedTerm (l a :--> PBool)
pisSingleton = phoistAcyclic $ plam $ \l ->
  (pnot #$ pnull # l) #&& (pnull #$ ptail # l)

-- | Map a function over the second components of the elements of a
-- list of pairs.
pmapOverSnd ::
  (PIsData b, PIsData c) =>
  ClosedTerm
    ( (b :--> c)
        :--> PBuiltinList (PBuiltinPair (PAsData a) (PAsData b))
        :--> PBuiltinList (PBuiltinPair (PAsData a) (PAsData c))
    )
pmapOverSnd = phoistAcyclic $ plam $ \f l ->
  pmap # plam (\p -> ppairDataBuiltin # (pfstBuiltin # p) # pdata (f # pfromData (psndBuiltin # p))) # l

-- * Some predicates to filter outputs.

pwithAddress :: ClosedTerm (PAddress :--> PTxOut :--> PBool)
pwithAddress = phoistAcyclic $ plam $ \addr o ->
  pfield @"address" # o #== addr

pwithAsset :: ClosedTerm (PCurrencySymbol :--> PTxOut :--> PBool)
pwithAsset = phoistAcyclic $ plam $ \currency o ->
  Lib.phasSomeAsset # (pfield @"value" # o) # currency

pwithGreaterValue :: ClosedTerm (PValue 'V.Sorted 'V.Positive :--> PTxOut :--> PBool)
pwithGreaterValue = phoistAcyclic $ plam $ \value o ->
  value #<= (pfield @"value" # o)

-- | Ensure that the value has only one token of a currency.
pwithOneTokenOf :: ClosedTerm (PCurrencySymbol :--> PValue any 'V.NoGuarantees :--> PBool)
pwithOneTokenOf = phoistAcyclic $ plam $ \currency value -> P.do
  pmatch (pcountTotalCurrency # currency # value) $ \case
    PNothing -> pcon PFalse
    PJust k -> k #== 1

-- | Finds the output whose value is greater than the given one and whose
-- money is locked by the given address.
pfindFollowingWithGreaterValue ::
  ClosedTerm
    ( PBuiltinList PTxOut
        :--> PAddress
        :--> PValue 'V.Sorted 'V.Positive
        :--> PMaybe PTxOut
    )
pfindFollowingWithGreaterValue = phoistAcyclic $ plam $ \outputs address value ->
  let fand = plam $ \f g e -> (f # e) #&& (g # e)
      gvalAndAddr =
        pfilter
          # (fand # (pwithGreaterValue # value) # (pwithAddress # address))
          # outputs
   in pif
        (pnull # gvalAndAddr)
        (pcon PNothing)
        ( pif
            (pnull #$ ptail # gvalAndAddr)
            (pcon $ PJust $ phead # gvalAndAddr)
            (pcon PNothing)
        )

-- | Finds an output whose money is locked by the given address and with some
-- given asset. This function may fail.
pfindFollowingWithAsset ::
  ClosedTerm
    ( PBuiltinList PTxOut
        :--> PAddress
        :--> PCurrencySymbol
        :--> PTxOut
    )
pfindFollowingWithAsset = phoistAcyclic $ plam $ \outputs address currency ->
  let fand = plam $ \f g e -> (f # e) #&& (g # e)
      sealAndAddr = pfilter # (fand # (pwithAsset # currency) # (pwithAddress # address)) # outputs
   in pif
        (pnull # sealAndAddr)
        (ptraceError "pfindFollowingWithAsset: could not find any following lotto")
        ( pif
            (pnull #$ ptail # sealAndAddr)
            (phead # sealAndAddr)
            (ptraceError "pfindFollowingWithAsset: found too many following lottos")
        )

-- | Return the total amount of tokens of a currency.
pcountTotalCurrency :: ClosedTerm (PCurrencySymbol :--> PValue any 'V.NoGuarantees :--> PMaybe PInteger)
pcountTotalCurrency = phoistAcyclic $ plam $ \currency value -> unTermCont $ do
  PValue value' <- pmatchC value -- OK: PValue is the only constructor
  pmatchC (PMap.plookup # currency # value') >>= \case
    PNothing -> return (pcon PNothing)
    PJust sealTokens' -> do
      PMap sealTokens <- pmatchC sealTokens'
      let total = pfoldr # plam (\elt acc -> (pfromData $ psndBuiltin # elt) #+ acc) # 0 # sealTokens
      return (pcon $ PJust total)

-- * Transaction tools

-- | Given a 'POutputDatum' (which may be either a datum, a hash, or nothing),
-- find a datum.
pfindDatum :: ClosedTerm (PMap 'Unsorted PDatumHash PDatum :--> POutputDatum :--> PMaybe PDatum)
pfindDatum = phoistAcyclic $ plam $ \datums outDatum ->
  pmatch
    outDatum
    $ \case
      POutputDatum d -> pcon $ PJust $ pfield @"outputDatum" # d
      POutputDatumHash h ->
        let datumHash = pfield @"datumHash" # h
         in PMap.plookup # datumHash # datums
      PNoOutputDatum _ -> pcon PNothing

-- | Coerces the type-tag of a term from @PData@ to @PAsData datum@ for any
-- @PIsData datum@.
--
-- NOTE: This sounds like a job for the @PTryFrom@ class; however, using
-- @PTryFrom@ leads to a significant increase in the size of the resulting
-- Plutus Core script, such that even the Plutarch developers recommend using
-- @punsafeCoerce@.
--
-- REVIEW: Plutarch v2 should make @punsafeCoerce@ efficient again.
pcoerceDatumFrom ::
  forall (s :: S) datum.
  PIsData datum =>
  Term s PData ->
  Term s (PAsData datum)
pcoerceDatumFrom = punsafeCoerce

-- | Coerces the type-tag of a term from @PData@ to @PAsData redeemer@ for any
-- @PIsData redeemer@.
--
-- NOTE: See note for @pcoerceDatumFrom@.
pcoerceRedeemerFrom ::
  forall (s :: S) redeemer.
  PIsData redeemer =>
  Term s PData ->
  Term s (PAsData redeemer)
pcoerceRedeemerFrom = punsafeCoerce

-- | Get the datum out of a 'PTxOut'.
pdatumOf :: ClosedTerm (PTxOut :--> PMap 'Unsorted PDatumHash PDatum :--> PMaybe PData)
pdatumOf = phoistAcyclic $ plam $ \outp datums ->
  pmatch (pfindDatum # datums #$ pfield @"datum" # outp) $ \case
    PNothing -> pcon PNothing
    PJust datum -> pcon $ PJust $ pto datum

-- | Return the UTxO among inputs whose address is the same as the address of
-- the spent UTxO
-- REVIEW: This function is defined in @Extra.Api@, but only for Plutarch V1
pfindOwnInput ::
  ClosedTerm
    ( PBuiltinList PTxInInfo
        :--> PScriptPurpose
        :--> PTxInInfo
    )
pfindOwnInput = phoistAcyclic $ plam $ \inputs (purpose :: Term s PScriptPurpose) -> unTermCont $ do
  PSpending lottoInRef' <- pmatchC purpose
  let lottoInRef :: Term s PTxOutRef
      lottoInRef = pfield @"_0" # lottoInRef'
      hasSameOutRef :: Term s (PTxInInfo :--> PBool)
      hasSameOutRef = plam $ \inInfo -> lottoInRef #== (pfield @"outRef" # inInfo)
  res <- pmatchC $ pfind # hasSameOutRef # inputs
  case res of
    PJust lottoIn -> return lottoIn
    PNothing -> return $ ptraceError "Input not found"

-- | Check that 'PTxOutRef' has been spent.
pspendsOutput :: ClosedTerm (PTxOutRef :--> PBuiltinList (PAsData PTxInInfo) :--> PBool)
pspendsOutput = phoistAcyclic $ plam $ \txRef ins ->
  pany # plam (\inInfo -> (pfield @"outRef" # inInfo) #== txRef) # ins

-- | Create a token name out of a 'PTxOutRef'. This function must be identical
-- to its offchain counterpart.
ptxOutRefToToken :: ClosedTerm (PTxOutRef :--> PTokenName)
ptxOutRefToToken = phoistAcyclic $ plam $ \ref -> do
  let txId' = pfield @"id" # ref
      txId = pfield @"_0" # txId'
      txIdx = pfield @"idx" # ref
  pcon . PTokenName $ psha2_256 #$ txId <> encodeInteger # txIdx

encodeInteger :: ClosedTerm (PInteger :--> PByteString)
encodeInteger = phoistAcyclic $ pfix #$ plam $ \self x ->
  pif
    (x #< 256)
    (pconsBS # x # emptyBS)
    ((self # (pquot # x # 256)) <> (pconsBS # (pmod # x # 256) # emptyBS))
  where
    emptyBS = pconstant ""

-- | Ensure that the given value has a positive amount of some specific
-- currency.
phasSomeAsset :: ClosedTerm (PValue k v :--> PCurrencySymbol :--> PBool)
phasSomeAsset = phoistAcyclic $ plam $ \val currency -> unTermCont $ do
  PValue currMap <- pmatchC val -- Cannot fail, PValue has only one constructor
  res <- pmatchC $ PMap.plookup # currency # currMap
  case res of
    PNothing -> return $ pconstant False
    PJust tkNameMap ->
      return $ PMap.pany # plam (\e -> pconstant 0 #< e) # tkNameMap

-- | The function used to hash the lotto secret.
phashSecret :: ClosedTerm (PByteString :--> PByteString :--> PByteString)
phashSecret = phoistAcyclic $ plam $ \salt e -> psha3_256 # (salt <> e)

-- | Collects all payments (in Ada) for some individual over a list of 'PTxOut'.
pcollectPaymentFor ::
  ClosedTerm (PPubKeyHash :--> PBuiltinList PTxOut :--> PValue 'V.Sorted 'V.NonZero)
pcollectPaymentFor = phoistAcyclic $ plam $ \who outputs ->
  pfoldr # collect who # (V.psingleton # V.padaSymbol # V.padaToken # pconstant 0) # outputs
  where
    collect ::
      Term s PPubKeyHash ->
      Term
        s
        ( PTxOut
            :--> PValue 'V.Sorted 'V.NonZero
            :--> PValue 'V.Sorted 'V.NonZero
        )
    collect who = plam $ \out (accumVal :: Term s (PValue 'V.Sorted 'V.NonZero)) ->
      let addr = pfield @"address" # out
       in pmatch (pfield @"credential" # addr) $ \case
            PPubKeyCredential h' ->
              pif
                (pfield @"_0" # h' #== who)
                (accumVal <> V.pforgetPositive (pfield @"value" # out))
                accumVal
            PScriptCredential _ -> accumVal

-- | Convert a typed validator to a 'PValidator'. It is used to
-- transform a validator with type information to a bare
-- 'PValidator' so that it can be serialised.
fromTypedValidator ::
  forall (s :: S) datum redeemer.
  (PIsData datum, PIsData redeemer) =>
  Term s (PAsData datum :--> PAsData redeemer :--> PScriptContext :--> POpaque) ->
  Term s PValidator
fromTypedValidator term = plam $ \datum redeemer ctx ->
  term # pcoerceDatumFrom datum # pcoerceRedeemerFrom redeemer # ctx

-- | Convert a typed minting policy to a 'PMintingPolicy'.
fromTypedMintingPolicy ::
  forall (s :: S) redeemer.
  PIsData redeemer =>
  Term s (PAsData redeemer :--> PScriptContext :--> POpaque) ->
  Term s PMintingPolicy
fromTypedMintingPolicy term = plam $ \redeemer ctx ->
  term # pcoerceRedeemerFrom redeemer # ctx

-- | Like 'Plutarch.Maybe.pfromJust' but with a custom error message.
pfromJust' :: Term s (PString :--> PMaybe a :--> a)
pfromJust' = phoistAcyclic $ plam $ \message x ->
  pmatch x $ \case
    PJust y -> y
    PNothing -> ptraceError message

-- * Score computation

-- | Takes first the secret, then the guess and return a score.
type Score = ClosedTerm (PByteString :--> PByteString :--> PInteger)

-- | A constant score function. Mainly for debugging purposes.
constantScore :: Integer -> Score
constantScore n = plam $ \_ _ -> pconstant n

-- | Fold over a bytestring.
pfoldBS :: ClosedTerm ((PInteger :--> a :--> a) :--> a :--> PByteString :--> a)
pfoldBS = phoistAcyclic $ plam $ \f init' bs ->
  loop # f # init' # bs # 0
  where
    loop = pfix #$ plam $ \self f init' bs k ->
      pif
        (k #< plengthBS # bs)
        (f # (pindexBS # bs # k) # (self # f # init' # bs # (k #+ 1)))
        init'

-- | Compute the difference of number of zeros between the secret and the
-- guess, and subtract it from the length of the secret. Said differently,
-- let @Z(x)@ the number of zeros in 'x', it computes |secret| - |Z(s) - Z(g)|
-- where @|.|@ is both the length and the absolute value.
-- The secret and the guess should have the same size.
-- scoreDiffZeros x y == scoreDiffZeros y x
scoreDiffZeros :: Score
scoreDiffZeros = phoistAcyclic $ plam $ \secret guess ->
  (plengthBS # secret) #- (Num.pabs #$ (numZero # secret) #- (numZero # guess))
  where
    numZero :: ClosedTerm (PByteString :--> PInteger)
    numZero = plam $ \bs -> pfoldBS # (plam $ \i acc -> pif (i #== 0) (acc #+ 1) acc) # 0 # bs

-- | Given a score function, transforms an association list from things to
-- guesses to an association list from things to share of the pot
-- (a natural number).
payGamblers ::
  ClosedTerm
    ( (PByteString :--> PByteString :--> PInteger) -- The score function
        :--> PInteger -- The pot
        :--> PRational -- Margin
        :--> PByteString -- The secret
        :--> PBuiltinList (PBuiltinPair (PAsData thing) (PAsData PByteString))
        :--> PBuiltinList (PBuiltinPair (PAsData thing) (PAsData PInteger))
    )
payGamblers = phoistAcyclic $ plam $ \score pot margin secret bids ->
  let total =
        pfoldr
          # plam (\e acc -> (score # secret # pfromData (psndBuiltin # e)) #+ acc)
          # pconstant 0
          # bids
      share = plam $ \bid ->
        pround
          #$ (pfromInteger # pot)
          #* (1 #- margin)
          #* (pfromInteger #$ score # secret # bid)
          #/ (pfromInteger # 1 #+ total)
   in pmapOverSnd # share # bids

-- | Compute the amount to be given to the administration.
payOrganiser :: Term s (PInteger :--> PRational :--> PInteger)
payOrganiser = plam $ \pot margin -> pround #$ margin #* (pfromInteger # pot)
