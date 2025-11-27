{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Chimia.Core.Molecule
Description : Type-safe molecular representation
Copyright   : (c) ChimiaDAO, 2024
License     : Apache-2.0
Maintainer  : contact@chimiadao.org
Stability   : experimental

Type-safe molecular representation with support for:
- Atoms with formal charges
- Bonds (single, double, triple, aromatic)
- SMILES string parsing (via RDKit bridge)
- Molecular mass calculation
- Stoichiometry validation

This extends Oliver Goldstein's work to support the full ORD integration pipeline.
-}

module Chimia.Core.Molecule
  ( -- * Core Types
    Molecule(..)
  , AtomWithCharge
  , MolecularFormula
    -- * Constructors
  , mkMolecule
  , mkMoleculeFromSMILES
  , mkSimpleMolecule
    -- * Properties
  , molecularMass
  , molecularFormula
  , atomCount
  , bondCount
    -- * Validation
  , validateMolecule
  , isConnected
    -- * Stoichiometry
  , countAtoms
  , countAtomsByElement
  ) where

import Chimia.Core.Atom (Atom(..), Charge(..), atomicMass)
import Chimia.Core.Bond (Bond, BondType(..), AtomIndex)
import Data.Aeson (ToJSON, FromJSON)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List (sort, group)
import GHC.Generics (Generic)

-- | Atom with formal charge
type AtomWithCharge = (Atom, Charge)

-- | Molecular formula (e.g., C6H12O6 → [(C,6), (H,12), (O,6)])
type MolecularFormula = [(Atom, Int)]

-- | Molecule: collection of atoms and bonds
data Molecule = Molecule
  { atoms :: [AtomWithCharge]  -- List of atoms with charges
  , bonds :: [Bond]             -- List of bonds (atom pairs + bond type)
  , smiles :: Maybe String      -- Original SMILES string (if available)
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Smart constructor: Create molecule with validation
mkMolecule :: Maybe String -> [AtomWithCharge] -> [Bond] -> Either String Molecule
mkMolecule smilesStr atomList bondList = 
  let mol = Molecule atomList bondList smilesStr
  in case validateMolecule mol of
    [] -> Right mol
    errs -> Left (unlines errs)

-- | Create molecule from SMILES string
-- NOTE: This is a stub that requires RDKit bridge (Python subprocess)
-- For now, we create a placeholder molecule
mkMoleculeFromSMILES :: String -> Molecule
mkMoleculeFromSMILES smilesStr = 
  -- TODO: Call RDKit via Python subprocess to parse SMILES
  -- For now, create a minimal molecule
  Molecule 
    { atoms = [(C, Neutral)] -- Placeholder
    , bonds = []
    , smiles = Just smilesStr
    }

-- | Create simple molecule (just atoms, no bonds)
mkSimpleMolecule :: [Atom] -> Molecule
mkSimpleMolecule atomList = Molecule
  { atoms = map (\a -> (a, Neutral)) atomList
  , bonds = []
  , smiles = Nothing
  }

-- | Calculate molecular mass (sum of atomic masses)
molecularMass :: Molecule -> Double
molecularMass mol = sum $ map (atomicMass . fst) (atoms mol)

-- | Get molecular formula (sorted by Hill system: C, H, then alphabetical)
molecularFormula :: Molecule -> MolecularFormula
molecularFormula mol = 
  let atomList = map fst (atoms mol)
      grouped = map (\xs -> (head xs, length xs)) . group . sort $ atomList
      -- Hill system: C first, then H, then alphabetical
      hillSort = sortBy hillCompare
  in hillSort grouped
  where
    hillCompare (C, _) _ = LT
    hillCompare _ (C, _) = GT
    hillCompare (H, _) (H, _) = EQ
    hillCompare (H, _) _ = LT
    hillCompare _ (H, _) = GT
    hillCompare (a1, _) (a2, _) = compare a1 a2

-- Import for hillCompare
import Data.List (sortBy)

-- | Count total number of atoms
atomCount :: Molecule -> Int
atomCount = length . atoms

-- | Count total number of bonds
bondCount :: Molecule -> Int
bondCount = length . bonds

-- | Validate molecule structure
-- Returns list of validation errors (empty list = valid)
validateMolecule :: Molecule -> [String]
validateMolecule mol = concat
  [ validateAtomIndices mol
  , validateBondIndices mol
  ]

-- | Check that atom list is non-empty
validateAtomIndices :: Molecule -> [String]
validateAtomIndices mol
  | null (atoms mol) = ["Molecule has no atoms"]
  | otherwise = []

-- | Check that all bonds reference valid atom indices
validateBondIndices :: Molecule -> [String]
validateBondIndices mol =
  let n = atomCount mol
      invalidBonds = filter (\(i, j, _) -> i < 0 || j < 0 || i >= n || j >= n) (bonds mol)
  in if null invalidBonds
     then []
     else ["Invalid bond indices: " ++ show invalidBonds]

-- | Check if molecule is connected (all atoms reachable via bonds)
-- Useful for validating that molecule is not multiple fragments
isConnected :: Molecule -> Bool
isConnected mol
  | atomCount mol <= 1 = True
  | null (bonds mol) = False
  | otherwise = 
      let adjList = buildAdjacencyList mol
          visited = dfs adjList [0] []
      in length visited == atomCount mol

-- | Build adjacency list for graph connectivity check
buildAdjacencyList :: Molecule -> Map AtomIndex [AtomIndex]
buildAdjacencyList mol =
  let n = atomCount mol
      emptyMap = Map.fromList [(i, []) | i <- [0..n-1]]
  in foldr addEdge emptyMap (bonds mol)
  where
    addEdge (i, j, _) m = 
      Map.adjust (j:) i $ Map.adjust (i:) j m

-- | Depth-first search for connectivity check
dfs :: Map AtomIndex [AtomIndex] -> [AtomIndex] -> [AtomIndex] -> [AtomIndex]
dfs _ [] visited = visited
dfs adjList (node:queue) visited
  | node `elem` visited = dfs adjList queue visited
  | otherwise =
      let neighbors = Map.findWithDefault [] node adjList
          newQueue = neighbors ++ queue
      in dfs adjList newQueue (node:visited)

-- | Count atoms (returns map of atom → count)
countAtoms :: [AtomWithCharge] -> Map Atom Int
countAtoms atomList =
  let atoms' = map fst atomList
  in Map.fromListWith (+) [(a, 1) | a <- atoms']

-- | Count atoms by element (for stoichiometry checking)
countAtomsByElement :: Molecule -> Map Atom Int
countAtomsByElement = countAtoms . atoms

-- | Example molecules for testing

-- | Water (H₂O)
water :: Molecule
water = Molecule
  { atoms = [(O, Neutral), (H, Neutral), (H, Neutral)]
  , bonds = [(0, 1, Single), (0, 2, Single)]
  , smiles = Just "O"
  }

-- | Methane (CH₄)
methane :: Molecule
methane = Molecule
  { atoms = [(C, Neutral), (H, Neutral), (H, Neutral), (H, Neutral), (H, Neutral)]
  , bonds = [(0, 1, Single), (0, 2, Single), (0, 3, Single), (0, 4, Single)]
  , smiles = Just "C"
  }

-- | Benzene (C₆H₆)
benzene :: Molecule
benzene = Molecule
  { atoms = 
      [ (C, Neutral), (C, Neutral), (C, Neutral)
      , (C, Neutral), (C, Neutral), (C, Neutral)
      , (H, Neutral), (H, Neutral), (H, Neutral)
      , (H, Neutral), (H, Neutral), (H, Neutral)
      ]
  , bonds = 
      [ (0, 1, Aromatic), (1, 2, Aromatic), (2, 3, Aromatic)
      , (3, 4, Aromatic), (4, 5, Aromatic), (5, 0, Aromatic)
      , (0, 6, Single), (1, 7, Single), (2, 8, Single)
      , (3, 9, Single), (4, 10, Single), (5, 11, Single)
      ]
  , smiles = Just "c1ccccc1"
  }

-- | Sodium chloride (Na⁺Cl⁻) - ionic compound
sodiumChloride :: Molecule
sodiumChloride = Molecule
  { atoms = [(Cl, Minus 1)]  -- Simplified: only Cl is in our Atom type
  , bonds = []
  , smiles = Just "[Na+].[Cl-]"
  }
