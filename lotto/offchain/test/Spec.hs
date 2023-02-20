{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Cooked
import qualified Data
import qualified Lib
import qualified Plutus.Script.Utils.Ada as Ada
import qualified Plutus.V2.Ledger.Api as V2
import qualified PlutusTx.Prelude as T
import Test.Tasty (TestTree, defaultMain, testGroup)
-- import Test.Tasty.HUnit
import Prelude

secret, aliceGuess, bobGuess :: T.BuiltinByteString
secret = "secret"
aliceGuess = "alice"
bobGuess = "bob"

tests :: TestTree
tests =
  testGroup
    "Code tests"
    []

main :: IO ()
main = do
  -- Some visual feedback on datum pretty printing
  let datum =
        Data.initLottoDatum
          (Lib.hashSecret "shhhhh" (Just "salt"))
          "salt"
          (V2.POSIXTime 2)
          (Ada.lovelaceValueOf 30)
          (T.unsafeRatio 3 100)
  print $ Cooked.prettyCooked datum
  defaultMain tests
