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
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | This module defines the minting policy for seals.
-- Seals are used to identify uniquely lotto instances.
module Seal (sealPolicy) where

import Lib (fromTypedMintingPolicy, ptxOutRefToToken)
import Plutarch.Api.V1.Value (pvalueOf)
import qualified Plutarch.Api.V1.Value as Value
import Plutarch.Api.V2
  ( AmountGuarantees (NoGuarantees),
    KeyGuarantees (Sorted),
    PCurrencySymbol,
    PMintingPolicy,
    PScriptContext,
    PScriptPurpose (PMinting),
    PTokenName,
    PTxInInfo,
    PTxOutRef,
    PValue,
  )
import Plutarch.Extra.TermCont (pguardC, pmatchC)
import Plutarch.Prelude

-- | Redeemer for the seal minting policy
data PRedeemer (s :: S)
  = -- | Carry the reference of the initial lotto UTxO
    Mint (Term s (PDataRecord '["_0" ':= PTxOutRef]))
  | Burn (Term s (PDataRecord '["_0" ':= PTokenName]))
  deriving stock (Generic)
  deriving anyclass
    ( PlutusType,
      -- | NOTE: No @PTryFrom PData@; cf note for @pcoerceDatumFrom@.
      PIsData
    )

instance DerivePlutusType PRedeemer where type DPTStrat _ = PlutusTypeData

-- | The minting policy for seals.
sealPolicy :: ClosedTerm PMintingPolicy
sealPolicy = fromTypedMintingPolicy sealPolicy'

-- | The typed version of the minting policy.
sealPolicy' :: ClosedTerm (PAsData PRedeemer :--> PScriptContext :--> POpaque)
sealPolicy' = plam $ \redeemer ctx -> unTermCont $ do
  let txInfo = pfield @"txInfo" # ctx
  let purpose = pfield @"purpose" # ctx
  --  ^^^ FIXME: those two could be a pletFields
  let mint = pfield @"mint" # txInfo
  -- NOTE: The following line might fail if 'purpose' is not a 'PMinting'.
  -- However, in our case, this would be a wrong way to call the minting policy,
  -- and therefore it is not an issue.
  PMinting currency' <- pmatchC purpose
  let currency = pfield @"_0" # currency'
  return $ pmatch (pfromData redeemer) $ \case
    Mint spent' ->
      let spent = pfield @"_0" # spent'
          inputs = pfield @"inputs" # txInfo
       in sealPolicyMint # currency # spent # inputs # mint
    Burn spent' ->
      let spent = pfield @"_0" # spent'
       in sealPolicyBurn # currency # spent # mint

-- | Precisely mint a single token whose name is 'ptxOutRefToToken'
-- of the input. It also checks that the input is spent.
sealPolicyMint ::
  ClosedTerm
    ( PCurrencySymbol -- The current minting policy
        :--> PTxOutRef -- The TxOutRef that ought to be unique
        :--> PBuiltinList PTxInInfo -- Inputs
        :--> PValue 'Sorted 'NoGuarantees -- Minting map
        :--> POpaque
    )
sealPolicyMint = plam $ \(currency :: Term s PCurrencySymbol) spent inputs mint ->
  let isTxOutRefSpent =
        plam $ \i -> spent #== (pfield @"outRef" # i)
   in unTermCont $ do
        -- Ensure the TxOutRef used to create the token name is spent
        pguardC "NFT name is not a TxO reference" $
          pany # isTxOutRefSpent # inputs
        pguardC
          "Not exactly one token minted for that currency \
          \ or inappropriate token name"
          $ let value =
                  Value.psingleton
                    # currency
                    # (ptxOutRefToToken # spent)
                    # pconstant 1
             in Value.pnormalize # mint #== value
        return $ popaque spent

-- | Given a currency symbol, returns a Plutarch relation which holds
-- for pairs @(o, m)@ for which 'm' burns a single token obtained
-- from @'ptxOutRefToToken' o@.
sealPolicyBurn ::
  ClosedTerm
    ( PCurrencySymbol
        :--> PTokenName
        :--> PValue 'Sorted 'NoGuarantees
        :--> POpaque
    )
sealPolicyBurn = plam $ \currency seal mint -> unTermCont $ do
  pguardC ("Seal not burned, or absent" <> pshow mint) $
    pvalueOf # mint # currency # seal #== pconstant (-1)
  return $ popaque mint

-- TODO: probably we could ask the burning policy to only ensure that
-- a single token is burned, giving the 'seal' as an argument does not
-- bring any further guarantee.
