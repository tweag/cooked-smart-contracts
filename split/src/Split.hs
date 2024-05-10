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
{-# OPTIONS_GHC -fno-specialise #-}

-- | Lock an amount and split it equally among two recipients.
module Split where

import GHC.Generics (Generic)
import qualified Plutus.Script.Utils.Ada as Script
import qualified Plutus.Script.Utils.Typed as Script
import qualified Plutus.Script.Utils.V3.Typed.Scripts as Script
import qualified Plutus.Script.Utils.Value as Script
import qualified PlutusLedgerApi.V3 as Api
import qualified PlutusLedgerApi.V3.Contexts as Api
import qualified PlutusTx
import PlutusTx.Prelude hiding (Applicative (..))
import qualified Prelude as Haskell

data SplitDatum = SplitDatum
  { recipient1 :: Api.PubKeyHash,
    recipient2 :: Api.PubKeyHash,
    amount :: Integer
  }
  deriving stock (Haskell.Show, Haskell.Eq, Haskell.Ord, Generic)

instance Eq SplitDatum where
  SplitDatum r1'1 r2'1 a1 == SplitDatum r1'2 r2'2 a2 =
    r1'1
      == r1'2
      && r2'1
      == r2'2
      && a1
      == a2

type SplitRedeemer = ()

{-# INLINEABLE validateSplit #-}
validateSplit :: SplitDatum -> SplitRedeemer -> Api.ScriptContext -> Bool
validateSplit (SplitDatum r1 r2 amount) _ Api.ScriptContext {Api.scriptContextTxInfo} =
  let halfAda = divide amount 2
      isPaid :: Api.PubKeyHash -> Integer -> Bool
      isPaid recipient ada =
        Api.valuePaidTo scriptContextTxInfo recipient `Script.geq` Script.lovelaceValueOf ada
   in traceIfFalse "R1 not paid enough" (r1 `isPaid` halfAda)
        && traceIfFalse "R2 not paid enough" (r2 `isPaid` (amount - halfAda))

-- Plutus boilerplate

data Split

PlutusTx.makeLift ''SplitDatum
PlutusTx.unstableMakeIsData ''SplitDatum

instance Script.ValidatorTypes Split where
  type RedeemerType Split = ()
  type DatumType Split = SplitDatum

splitValidator :: Script.TypedValidator Split
splitValidator =
  Script.mkTypedValidator @Split
    $$(PlutusTx.compile [||validateSplit||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Script.mkUntypedValidator
