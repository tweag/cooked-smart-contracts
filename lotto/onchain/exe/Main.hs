{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- | This module serialises the smart contract of the lotto.
-- The serialisation is performed with 'Ply', the scripts are written in the
-- @scripts/@ directory.
module Main where

import qualified Data.ByteString as B
import qualified Data.ByteString.Short as B
import Data.Default (def)
import Options.Applicative
  ( Parser,
    execParser,
    flag,
    fullDesc,
    header,
    help,
    helper,
    info,
    long,
    progDesc,
    (<**>),
    (<|>),
  )
import qualified Plutarch
import qualified Plutarch.Script as Script
import Ply.Plutarch (writeTypedScript)
import qualified Seal as S
import qualified Validators as V

-- | The different kinds of outputs available.
data OutputFormat
  = -- | Plutus Core serialised script
    Plc
  | -- | Ply serialised scripts
    Ply

-- | Configuration of the output
newtype ConfigOutput = ConfigOutput
  { -- | Whether the output should be readable by a human or a computer
    forWho :: OutputFormat
  }

-- | A command line parser for the 'ConfigOutput'.
configOutput :: Parser ConfigOutput
configOutput =
  ConfigOutput
    <$> ( flag Ply Plc (long "plc" <> help "Plutus Core scripts")
            <|> flag Ply Ply (long "ply" <> help "Write scripts readable by Ply")
        )

main :: IO ()
main = display =<< execParser opts
  where
    opts =
      info
        (configOutput <**> helper)
        ( fullDesc
            <> progDesc "Print lotto scripts"
            <> header "lotto contract"
        )

display :: ConfigOutput -> IO ()
display ConfigOutput {forWho = Plc} =
  case Plutarch.compile def V.main of
    Right script ->
      B.writeFile "./scripts/lottoValidator.plc" $
        B.fromShort $
          Script.serialiseScript script
    Left _ -> error "Failed compiling validator"
    >> case Plutarch.compile def S.sealPolicy of
      Right script ->
        B.writeFile "./scripts/sealMinting.plc" $
          B.fromShort $
            Script.serialiseScript script
      Left _ -> error "Failed compiling seal policy"
display ConfigOutput {forWho = Ply} =
  writeTypedScript
    def {Plutarch.tracingMode = Plutarch.DoTracing}
    "Lotto validator"
    "./scripts/lottoValidator.plutus"
    V.main
    >> writeTypedScript
      def {Plutarch.tracingMode = Plutarch.DoTracing}
      "Seal minting policy"
      "./scripts/sealMinting.plutus"
      S.sealPolicy
