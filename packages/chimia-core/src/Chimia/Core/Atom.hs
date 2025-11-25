{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{-|
Module      : Chimia.Core.Atom
Description : Type-safe atomic representation
Copyright   : (c) ChimiaDAO, 2024
License     : Apache-2.0
Maintainer  : contact@chimiadao.org
Stability   : experimental

Type-safe representation of chemical elements with formal properties.
Inspired by Oliver Goldstein's molecular representation work.
-}

module Chimia.Core.Atom
  ( Atom(..)
  , AtomicNumber
  , Charge(..)
  , atomicNumber
  , atomicMass
  , symbol
  , isOrganic
  ) where

import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)

-- | Atomic number type (1-118 for periodic table)
type AtomicNumber = Int

-- | Formal charge on an atom
data Charge 
  = Neutral
  | Plus Int    -- Positive charge (+1, +2, etc.)
  | Minus Int   -- Negative charge (-1, -2, etc.)
  deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

-- | Chemical elements
-- Organic elements + metalloids + first-row transition metals
data Atom
  = H   -- Hydrogen (Z=1)
  | C   -- Carbon (Z=6)
  | N   -- Nitrogen (Z=7)
  | O   -- Oxygen (Z=8)
  | F   -- Fluorine (Z=9)
  | P   -- Phosphorus (Z=15)
  | S   -- Sulfur (Z=16)
  | Cl  -- Chlorine (Z=17)
  | Br  -- Bromine (Z=35)
  | I   -- Iodine (Z=53)
  -- Metalloids (group 14)
  | Si  -- Silicon (Z=14)
  | Ge  -- Germanium (Z=32)
  | Sn  -- Tin (Z=50)
  -- Chalcogens (additional)
  | Se  -- Selenium (Z=34)
  | Te  -- Tellurium (Z=52)
  -- Transition metals (first row, Sc-Zn)
  | Sc  -- Scandium (Z=21)
  | Ti  -- Titanium (Z=22)
  | V   -- Vanadium (Z=23)
  | Cr  -- Chromium (Z=24)
  | Mn  -- Manganese (Z=25)
  | Fe  -- Iron (Z=26)
  | Co  -- Cobalt (Z=27)
  | Ni  -- Nickel (Z=28)
  | Cu  -- Copper (Z=29)
  | Zn  -- Zinc (Z=30)
  deriving (Eq, Show, Ord, Enum, Bounded, Generic, ToJSON, FromJSON)

-- | Get atomic number for an element
atomicNumber :: Atom -> AtomicNumber
atomicNumber H  = 1
atomicNumber C  = 6
atomicNumber N  = 7
atomicNumber O  = 8
atomicNumber F  = 9
atomicNumber Si = 14
atomicNumber P  = 15
atomicNumber S  = 16
atomicNumber Cl = 17
atomicNumber Sc = 21
atomicNumber Ti = 22
atomicNumber V  = 23
atomicNumber Cr = 24
atomicNumber Mn = 25
atomicNumber Fe = 26
atomicNumber Co = 27
atomicNumber Ni = 28
atomicNumber Cu = 29
atomicNumber Zn = 30
atomicNumber Ge = 32
atomicNumber Se = 34
atomicNumber Br = 35
atomicNumber Sn = 50
atomicNumber Te = 52
atomicNumber I  = 53

-- | Get approximate atomic mass (in amu)
atomicMass :: Atom -> Double
atomicMass H  = 1.008
atomicMass C  = 12.011
atomicMass N  = 14.007
atomicMass O  = 15.999
atomicMass F  = 18.998
atomicMass Si = 28.085
atomicMass P  = 30.974
atomicMass S  = 32.065
atomicMass Cl = 35.453
atomicMass Sc = 44.956
atomicMass Ti = 47.867
atomicMass V  = 50.942
atomicMass Cr = 51.996
atomicMass Mn = 54.938
atomicMass Fe = 55.845
atomicMass Co = 58.933
atomicMass Ni = 58.693
atomicMass Cu = 63.546
atomicMass Zn = 65.380
atomicMass Ge = 72.630
atomicMass Se = 78.971
atomicMass Br = 79.904
atomicMass Sn = 118.710
atomicMass Te = 127.600
atomicMass I  = 126.904

-- | Get element symbol
symbol :: Atom -> String
symbol H  = "H"
symbol C  = "C"
symbol N  = "N"
symbol O  = "O"
symbol F  = "F"
symbol Si = "Si"
symbol P  = "P"
symbol S  = "S"
symbol Cl = "Cl"
symbol Sc = "Sc"
symbol Ti = "Ti"
symbol V  = "V"
symbol Cr = "Cr"
symbol Mn = "Mn"
symbol Fe = "Fe"
symbol Co = "Co"
symbol Ni = "Ni"
symbol Cu = "Cu"
symbol Zn = "Zn"
symbol Ge = "Ge"
symbol Se = "Se"
symbol Br = "Br"
symbol Sn = "Sn"
symbol Te = "Te"
symbol I  = "I"

-- | Check if atom is typically found in organic chemistry
isOrganic :: Atom -> Bool
isOrganic H = True
isOrganic C = True
isOrganic N = True
isOrganic O = True
isOrganic _ = False
