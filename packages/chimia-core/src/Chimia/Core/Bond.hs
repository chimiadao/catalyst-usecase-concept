{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Chimia.Core.Bond
  ( BondType(..)
  , Bond
  , AtomIndex
  , mkBond
  , bondOrder
  ) where

import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)

-- | Index of atom in molecule (0-based)
type AtomIndex = Int

-- | Types of chemical bonds
data BondType
  = Single      -- Single bond (σ)
  | Double      -- Double bond (σ + π)
  | Triple      -- Triple bond (σ + 2π)
  | Aromatic    -- Aromatic bond (delocalized)
  deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

-- | Chemical bond between two atoms
-- Invariant: fromAtom < toAtom (canonical ordering)
type Bond = (AtomIndex, AtomIndex, BondType)

-- | Smart constructor for bonds (ensures canonical ordering)
mkBond :: AtomIndex -> AtomIndex -> BondType -> Bond
mkBond i j bt
  | i < j     = (i, j, bt)
  | otherwise = (j, i, bt)

-- | Get bond order (single=1, double=2, triple=3, aromatic=1.5)
bondOrder :: BondType -> Double
bondOrder Single   = 1.0
bondOrder Double   = 2.0
bondOrder Triple   = 3.0
bondOrder Aromatic = 1.5
