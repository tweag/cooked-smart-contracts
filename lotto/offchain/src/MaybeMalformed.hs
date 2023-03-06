-- | A module to easily inject malformed pieces of data in well-typed haskell
-- code.

module MaybeMalformed where

import qualified PlutusTx as Pl

data MaybeMalformed a = WellFormed a | Malformed Pl.BuiltinData

instance Pl.ToData a => Pl.ToData (MaybeMalformed a) where
  toBuiltinData (WellFormed x) = Pl.toBuiltinData x
  toBuiltinData (Malformed y) = y
