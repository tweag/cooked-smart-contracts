{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A module to easily inject malformed pieces of data in well-typed haskell
-- code.
module MaybeMalformed where

import Data.Maybe (fromJust)
import Prettyprinter (Pretty, pretty, (<+>))
import qualified PlutusTx as Pl
import Prelude

data MaybeMalformed a = WellFormed a | Malformed Pl.BuiltinData deriving (Show, Eq)

Pl.makeLift ''MaybeMalformed

instance Pl.ToData a => Pl.ToData (MaybeMalformed a) where
  toBuiltinData (WellFormed x) = Pl.toBuiltinData x
  toBuiltinData (Malformed y) = y

instance Pl.FromData a => Pl.FromData (MaybeMalformed a) where
  fromBuiltinData bd =
    case Pl.fromBuiltinData bd of
      Just x -> Just $ WellFormed x
      Nothing -> Just $ Malformed bd

instance Pl.FromData a => Pl.UnsafeFromData (MaybeMalformed a) where
  unsafeFromBuiltinData = fromJust . Pl.fromBuiltinData

instance Pretty a => Pretty (MaybeMalformed a) where
  pretty (WellFormed x) = pretty x
  pretty (Malformed y) = "/!\\ MALFORMED:" <+> pretty y

fromWellFormed :: MaybeMalformed a -> a
fromWellFormed (WellFormed x) = x
fromWellFormed _ = error "fromWellFormed"

fromMalformed :: MaybeMalformed a -> Pl.BuiltinData
fromMalformed (Malformed y) = y
fromMalformed _ = error "fromMalformed"

wellFormed :: a -> MaybeMalformed a
wellFormed = WellFormed

malformed :: Pl.ToData b => b -> MaybeMalformed a
malformed = Malformed . Pl.toBuiltinData
