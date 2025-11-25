{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{-|
Module      : Chimia.Core.Orbitals
Description : Type-safe atomic orbital representation with spherical harmonics
Copyright   : (c) ChimiaDAO, 2024
License     : Apache-2.0
Maintainer  : contact@chimiadao.org
Stability   : experimental

Declarative descriptions of atomic orbitals using spherical harmonic basis.
Includes support for hybrid orbitals (sp, sp², sp³, sp³d, sp³d²) and
molecular orbital formation via LCAO.

Based on Oliver Goldstein's work, corrected to use standard spherical harmonics.
-}

module Chimia.Core.Orbitals
  ( -- * Orbital Types
    SOrbital(..)
  , POrbital(..)
  , DOrbital(..)
  , FOrbital(..)
  , PureOrbital(..)
  , HybridOrbital(..)
    -- * Orbital Container
  , Orbital(..)
  , SubShell(..)
  , Shell(..)
  , Shells
    -- * Hybrid Orbital Types
  , sp, sp2, sp3, sp3d, sp3d2
    -- * Helper Functions
  , orbitalEnergy
  , maxElectrons
  ) where

import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)

-- | S orbital (spherical symmetry, l=0, ml=0)
data SOrbital = S
  deriving (Eq, Show, Ord, Read, Generic, ToJSON, FromJSON)

-- | P orbitals (l=1, ml=-1,0,+1)
data POrbital 
  = Px  -- ml = +1 (real combination)
  | Py  -- ml = -1 (real combination)
  | Pz  -- ml = 0
  deriving (Eq, Show, Ord, Read, Generic, ToJSON, FromJSON)

-- | D orbitals (l=2, ml=-2,-1,0,+1,+2) - spherical harmonics
data DOrbital
  = Dxy    -- ml = -2,+2 combination
  | Dyz    -- ml = -1,+1 combination
  | Dxz    -- ml = -1,+1 combination (orthogonal to Dyz)
  | Dx2y2  -- ml = -2,+2 combination (orthogonal to Dxy)
  | Dz2    -- ml = 0
  deriving (Eq, Show, Ord, Read, Generic, ToJSON, FromJSON)

-- | F orbitals (l=3, ml=-3,-2,-1,0,+1,+2,+3) - CORRECTED to spherical harmonics
data FOrbital
  = Fz3      -- ml = 0:  f(z³)
  | Fxz2     -- ml = ±1: f(xz²)
  | Fyz2     -- ml = ±1: f(yz²)
  | Fxyz     -- ml = ±1: f(xyz)
  | Fzx2y2   -- ml = ±2: f(z(x²-y²))
  | Fxx23y2  -- ml = ±3: f(x(x²-3y²))
  | Fy3x2y2  -- ml = ±3: f(y(3x²-y²))
  deriving (Eq, Show, Ord, Read, Generic, ToJSON, FromJSON)

-- | Pure orbital types (non-hybrid)
data PureOrbital
  = PureS SOrbital
  | PureP POrbital
  | PureD DOrbital
  | PureF FOrbital
  deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

-- | Common hybrid orbital types
data HybridOrbital
  = SP       -- Linear (2 orbitals, 180°) - e.g., acetylene C
  | SP2      -- Trigonal planar (3 orbitals, 120°) - e.g., ethylene C
  | SP3      -- Tetrahedral (4 orbitals, 109.5°) - e.g., methane C
  | SP3D     -- Trigonal bipyramidal (5 orbitals) - e.g., PCl₅
  | SP3D2    -- Octahedral (6 orbitals) - e.g., SF₆
  deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

-- | Orbital with electron configuration and optional orientation
data Orbital subshellType = Orbital
  { orbitalType      :: subshellType
  , electronCount    :: Int  -- 0-2 for s,p,d,f; 0-1 for unpaired
  , orientation      :: Maybe (Double, Double, Double)  -- (x, y, z) direction vector
  , hybridComponents :: Maybe [(Double, PureOrbital)]   -- For hybrid orbitals
  } deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

-- | Subshell: collection of orbitals with same quantum numbers (n, l)
newtype SubShell subshellType = SubShell
  { orbitals :: [Orbital subshellType]
  } deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

-- | Shell: all subshells for a given principal quantum number n
data Shell = Shell
  { principalQuantumNumber :: Int  -- n = 1, 2, 3, ...
  , sSubShell              :: Maybe (SubShell SOrbital)
  , pSubShell              :: Maybe (SubShell POrbital)
  , dSubShell              :: Maybe (SubShell DOrbital)
  , fSubShell              :: Maybe (SubShell FOrbital)
  } deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

-- | Complete electron configuration for an atom
type Shells = [Shell]

-- | Hybrid orbital constructors (LCAO coefficients)
-- Normalized so that sum of squares = 1

-- | sp hybrid: 50% s, 50% p (linear geometry)
sp :: PureOrbital -> [(Double, PureOrbital)]
sp p = [(1/sqrt 2, PureS S), (1/sqrt 2, p)]

-- | sp² hybrid: 33% s, 67% p (trigonal planar)
sp2 :: PureOrbital -> PureOrbital -> [(Double, PureOrbital)]
sp2 p1 p2 = [(1/sqrt 3, PureS S), (sqrt (2/3), p1), (sqrt (2/3), p2)]

-- | sp³ hybrid: 25% s, 75% p (tetrahedral)
sp3 :: [(Double, PureOrbital)]
sp3 = 
  [ (0.5, PureS S)
  , (0.5, PureP Px)
  , (0.5, PureP Py)
  , (0.5, PureP Pz)
  ]

-- | sp³d hybrid: 20% s, 60% p, 20% d (trigonal bipyramidal)
sp3d :: DOrbital -> [(Double, PureOrbital)]
sp3d d = 
  [ (1/sqrt 5, PureS S)
  , (sqrt (3/5), PureP Px)
  , (sqrt (3/5), PureP Py)
  , (sqrt (3/5), PureP Pz)
  , (1/sqrt 5, PureD d)
  ]

-- | sp³d² hybrid: ~17% s, 50% p, ~33% d (octahedral)
sp3d2 :: DOrbital -> DOrbital -> [(Double, PureOrbital)]
sp3d2 d1 d2 = 
  [ (1/sqrt 6, PureS S)
  , (0.5, PureP Px)
  , (0.5, PureP Py)
  , (0.5, PureP Pz)
  , (1/sqrt 6, PureD d1)
  , (1/sqrt 6, PureD d2)
  ]

-- | Approximate orbital energy (relative scale)
orbitalEnergy :: Int -> Char -> Double
orbitalEnergy n subshell = case subshell of
  's' -> fromIntegral n
  'p' -> fromIntegral n + 0.5
  'd' -> fromIntegral n + 1.0
  'f' -> fromIntegral n + 1.5
  _   -> fromIntegral n

-- | Maximum electrons per orbital type
maxElectrons :: Char -> Int
maxElectrons 's' = 2   -- 1 orbital × 2 electrons
maxElectrons 'p' = 6   -- 3 orbitals × 2 electrons
maxElectrons 'd' = 10  -- 5 orbitals × 2 electrons
maxElectrons 'f' = 14  -- 7 orbitals × 2 electrons
maxElectrons _   = 0
