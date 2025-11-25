{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{-|
Module      : Chimia.Core.MO
Description : Molecular Orbital theory via LCAO (Linear Combination of Atomic Orbitals)
Copyright   : (c) ChimiaDAO, 2024
License     : Apache-2.0
Maintainer  : contact@chimiadao.org
Stability   : experimental

LCAO-MO Ansatz: Molecular orbitals formed as linear combinations of atomic orbitals.
Implements bonding, antibonding, and non-bonding MO construction.

φ(MO) = Σᵢ cᵢ φᵢ(AO)

Where:
- φ(MO) is the molecular orbital
- cᵢ are the LCAO coefficients
- φᵢ(AO) are atomic orbitals from constituent atoms
-}

module Chimia.Core.MO
  ( -- * Molecular Orbital Types
    MolecularOrbital(..)
  , MOType(..)
  , AOContribution(..)
    -- * LCAO Construction
  , mkBondingMO
  , mkAntibondingMO
  , mkNonbondingMO
    -- * Orbital Overlap
  , overlapIntegral
  , bondOrder
    -- * Helper Functions
  , moEnergy
  , isOccupied
  ) where

import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)
import Chimia.Core.Orbitals (PureOrbital)

-- | Atomic orbital contribution to a molecular orbital
data AOContribution = AOContribution
  { atomIndex    :: Int           -- Which atom this AO belongs to
  , orbital      :: PureOrbital   -- Which atomic orbital
  , coefficient  :: Double        -- LCAO coefficient cᵢ
  , phase        :: Double        -- Orbital phase (±1 for bonding/antibonding)
  } deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

-- | Type of molecular orbital
data MOType
  = Sigma          -- σ: end-on overlap (single bonds, e.g., H₂)
  | SigmaStar      -- σ*: antibonding sigma
  | Pi             -- π: side-by-side overlap (double bonds, e.g., C=C)
  | PiStar         -- π*: antibonding pi
  | Delta          -- δ: face-to-face overlap (rare, transition metal complexes)
  | DeltaStar      -- δ*: antibonding delta
  | Nonbonding     -- n: lone pair, no overlap
  deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

-- | Molecular orbital constructed from atomic orbitals
data MolecularOrbital = MolecularOrbital
  { moType         :: MOType
  , contributions  :: [AOContribution]   -- LCAO expansion
  , electronCount  :: Int                -- 0-2 electrons in this MO
  , energy         :: Double             -- Relative energy (hartree or eV)
  , bondingAtoms   :: Maybe (Int, Int)   -- Atom indices if bonding/antibonding
  } deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

-- | Construct a bonding σ molecular orbital (φ₁ + φ₂)
-- Example: H₂ molecule from two 1s orbitals
mkBondingMO :: Int -> Int -> PureOrbital -> PureOrbital -> MolecularOrbital
mkBondingMO atom1 atom2 ao1 ao2 = MolecularOrbital
  { moType = Sigma
  , contributions = 
      [ AOContribution atom1 ao1 (1/sqrt 2) 1.0   -- In-phase
      , AOContribution atom2 ao2 (1/sqrt 2) 1.0   -- In-phase
      ]
  , electronCount = 0  -- Fill with electrons separately
  , energy = -1.0      -- Lower energy than isolated AOs
  , bondingAtoms = Just (atom1, atom2)
  }

-- | Construct an antibonding σ* molecular orbital (φ₁ - φ₂)
mkAntibondingMO :: Int -> Int -> PureOrbital -> PureOrbital -> MolecularOrbital
mkAntibondingMO atom1 atom2 ao1 ao2 = MolecularOrbital
  { moType = SigmaStar
  , contributions = 
      [ AOContribution atom1 ao1 (1/sqrt 2) 1.0    -- In-phase
      , AOContribution atom2 ao2 (1/sqrt 2) (-1.0) -- Out-of-phase (node)
      ]
  , electronCount = 0
  , energy = 1.0       -- Higher energy than isolated AOs
  , bondingAtoms = Just (atom1, atom2)
  }

-- | Construct a non-bonding molecular orbital (lone pair)
mkNonbondingMO :: Int -> PureOrbital -> MolecularOrbital
mkNonbondingMO atom ao = MolecularOrbital
  { moType = Nonbonding
  , contributions = [AOContribution atom ao 1.0 1.0]
  , electronCount = 0
  , energy = 0.0       -- Same energy as isolated AO
  , bondingAtoms = Nothing
  }

-- | Simplified overlap integral ⟨φᵢ|φⱼ⟩
-- In reality, this requires numerical integration over space
-- Here we use a heuristic based on orbital type and distance
overlapIntegral :: PureOrbital -> PureOrbital -> Double -> Double
overlapIntegral _ao1 _ao2 distance
  | distance < 1.0  = 0.8  -- Strong overlap (short bond)
  | distance < 2.0  = 0.5  -- Moderate overlap
  | distance < 3.0  = 0.2  -- Weak overlap
  | otherwise       = 0.0  -- Negligible overlap

-- | Bond order = (bonding electrons - antibonding electrons) / 2
-- Example: O₂ has bond order 2 (double bond)
bondOrder :: [MolecularOrbital] -> Maybe (Int, Int) -> Double
bondOrder mos atoms =
  let bondingElectrons = sum 
        [ electronCount mo 
        | mo <- mos
        , moType mo `elem` [Sigma, Pi, Delta]
        , bondingAtoms mo == atoms
        ]
      antibondingElectrons = sum
        [ electronCount mo
        | mo <- mos
        , moType mo `elem` [SigmaStar, PiStar, DeltaStar]
        , bondingAtoms mo == atoms
        ]
  in fromIntegral (bondingElectrons - antibondingElectrons) / 2.0

-- | Molecular orbital energy (simplified Hückel approximation)
moEnergy :: MOType -> Double
moEnergy Sigma      = -1.0   -- Bonding (lower energy)
moEnergy SigmaStar  =  1.0   -- Antibonding (higher energy)
moEnergy Pi         = -0.8   -- Bonding (slightly less stable than σ)
moEnergy PiStar     =  0.8   -- Antibonding
moEnergy Delta      = -0.5   -- Bonding (weakest)
moEnergy DeltaStar  =  0.5   -- Antibonding
moEnergy Nonbonding =  0.0   -- Non-bonding (same as AO)

-- | Check if a molecular orbital is occupied (has electrons)
isOccupied :: MolecularOrbital -> Bool
isOccupied mo = electronCount mo > 0
