{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{-|
Module      : Chimia.Core.Reaction
Description : Type-safe chemical reaction representation
Copyright   : (c) ChimiaDAO, 2024
License     : Apache-2.0
Maintainer  : contact@chimiadao.org
Stability   : experimental

Type-safe chemical reaction representation with:
- Reagents (reactants, products, catalysts, solvents)
- Reaction conditions (temperature, pressure, time)
- Stoichiometric validation
- Mass balance checking
- Reaction classification

This is the foundation for Plutus validator compilation.
-}

module Chimia.Core.Reaction
  ( -- * Core Types
    Reaction(..)
  , Reagent(..)
  , ReagentRole(..)
  , ReactionClass(..)
  , ReactionConditions(..)
    -- * Constructors
  , mkReaction
  , mkReagent
    -- * Validation
  , checkStoichiometry
  , checkMassBalance
  , checkChargeBalance
  , validateReaction
    -- * Properties
  , reactants
  , products
  , catalysts
  , solvents
  ) where

import Chimia.Core.Molecule 
  ( Molecule(..)
  , molecularMass
  , countAtomsByElement
  )
import Chimia.Core.Atom (Atom, Charge(..))
import Data.Aeson (ToJSON, FromJSON)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)

-- | Role of a reagent in a reaction
data ReagentRole
  = Reactant   -- Consumed in reaction
  | Product    -- Produced in reaction
  | Catalyst   -- Facilitates reaction, not consumed
  | Solvent    -- Reaction medium, not consumed
  deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

-- | Reagent: molecule with stoichiometric coefficient and role
data Reagent = Reagent
  { molecule     :: Molecule
  , coefficient  :: Double      -- Stoichiometric coefficient (1.0 = 1 mole)
  , role         :: ReagentRole
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Classification of chemical reactions
data ReactionClass
  = Synthesis           -- A + B → AB
  | Decomposition       -- AB → A + B
  | SingleReplacement   -- A + BC → AC + B
  | DoubleReplacement   -- AB + CD → AD + CB
  | Combustion          -- Hydrocarbon + O₂ → CO₂ + H₂O
  | Redox               -- Oxidation-reduction
  | AcidBase            -- Proton transfer
  | Addition            -- C=C + X → C-C-X
  | Elimination         -- C-C-X → C=C + X
  | Substitution        -- R-X + Nu → R-Nu + X
  | Rearrangement       -- Molecular rearrangement
  | DielsAlder          -- [4+2] cycloaddition
  | Custom String       -- User-defined class
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Reaction conditions (temperature, pressure, time, etc.)
data ReactionConditions = ReactionConditions
  { temperature :: Maybe Double  -- Kelvin
  , pressure    :: Maybe Double  -- Atmospheres
  , time        :: Maybe Double  -- Seconds
  , solvent     :: Maybe String  -- Solvent name (e.g., "THF", "DCM")
  , atmosphere  :: Maybe String  -- Atmosphere (e.g., "N₂", "Ar", "air")
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Default reaction conditions (room temperature, atmospheric pressure)
defaultConditions :: ReactionConditions
defaultConditions = ReactionConditions
  { temperature = Just 298.15  -- 25°C
  , pressure = Just 1.0        -- 1 atm
  , time = Nothing
  , solvent = Nothing
  , atmosphere = Just "air"
  }

-- | Chemical reaction: reagents + conditions + metadata
data Reaction = Reaction
  { reagents   :: [Reagent]
  , conditions :: ReactionConditions
  , reactionClass :: ReactionClass
  , ordId      :: Maybe String  -- ORD reaction ID (if from ORD)
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Smart constructor for reagent
mkReagent :: Molecule -> Double -> ReagentRole -> Reagent
mkReagent mol coeff r = Reagent mol coeff r

-- | Smart constructor for reaction
mkReaction :: [Reagent] -> ReactionClass -> Maybe String -> Reaction
mkReaction reags rc ordID = Reaction
  { reagents = reags
  , conditions = defaultConditions
  , reactionClass = rc
  , ordId = ordID
  }

-- | Get all reactants from reaction
reactants :: Reaction -> [Reagent]
reactants rxn = filter (\r -> role r == Reactant) (reagents rxn)

-- | Get all products from reaction
products :: Reaction -> [Reagent]
products rxn = filter (\r -> role r == Product) (reagents rxn)

-- | Get all catalysts from reaction
catalysts :: Reaction -> [Reagent]
catalysts rxn = filter (\r -> role r == Catalyst) (reagents rxn)

-- | Get all solvents from reaction
solvents :: Reaction -> [Reagent]
solvents rxn = filter (\r -> role r == Solvent) (reagents rxn)

-- | Validate reaction (returns list of errors, empty = valid)
validateReaction :: Reaction -> [String]
validateReaction rxn = concat
  [ if null (reactants rxn) then ["No reactants"] else []
  , if null (products rxn) then ["No products"] else []
  , if not (checkStoichiometry rxn) then ["Stoichiometry imbalance"] else []
  , if not (checkMassBalance rxn) then ["Mass balance violation"] else []
  ]

-- | Check stoichiometric balance (atoms conserved)
-- Returns True if # of each element is same on both sides
checkStoichiometry :: Reaction -> Bool
checkStoichiometry rxn =
  let reactantAtoms = countAtomsInReagents (reactants rxn)
      productAtoms = countAtomsInReagents (products rxn)
  in reactantAtoms == productAtoms

-- | Count atoms in list of reagents (weighted by stoichiometric coefficient)
countAtomsInReagents :: [Reagent] -> Map Atom Double
countAtomsInReagents reags =
  let counts = map reagentAtomCounts reags
  in Map.unionsWith (+) counts
  where
    reagentAtomCounts :: Reagent -> Map Atom Double
    reagentAtomCounts (Reagent mol coeff _) =
      let atomCounts = countAtomsByElement mol
      in Map.map (* coeff) (Map.map fromIntegral atomCounts)

-- | Check mass balance (total mass conserved)
-- Returns True if mass(reactants) ≈ mass(products) within 1% tolerance
checkMassBalance :: Reaction -> Bool
checkMassBalance rxn =
  let reactantMass = sum $ map reagentMass (reactants rxn)
      productMass = sum $ map reagentMass (products rxn)
      tolerance = 0.01  -- 1% tolerance
  in abs (reactantMass - productMass) / max reactantMass productMass < tolerance
  where
    reagentMass (Reagent mol coeff _) = molecularMass mol * coeff

-- | Check charge balance (total charge conserved)
-- Returns True if sum of charges is conserved
checkChargeBalance :: Reaction -> Bool
checkChargeBalance rxn =
  let reactantCharge = sum $ map reagentCharge (reactants rxn)
      productCharge = sum $ map reagentCharge (products rxn)
  in reactantCharge == productCharge
  where
    reagentCharge :: Reagent -> Int
    reagentCharge (Reagent mol coeff _) =
      let charges = map snd (atoms mol)
          totalCharge = sum $ map chargeToInt charges
      in round (fromIntegral totalCharge * coeff)
    
    chargeToInt :: Charge -> Int
    chargeToInt Neutral = 0
    chargeToInt (Plus n) = n
    chargeToInt (Minus n) = -n

-- | Example reactions for testing

-- | Water formation: 2H₂ + O₂ → 2H₂O
waterFormation :: Reaction
waterFormation =
  let h2 = Molecule {atoms = [(H, Neutral), (H, Neutral)], bonds = [], smiles = Just "[H][H]"}
      o2 = Molecule {atoms = [(O, Neutral), (O, Neutral)], bonds = [], smiles = Just "O=O"}
      h2o = Molecule {atoms = [(O, Neutral), (H, Neutral), (H, Neutral)], bonds = [], smiles = Just "O"}
  in mkReaction
      [ mkReagent h2 2.0 Reactant
      , mkReagent o2 1.0 Reactant
      , mkReagent h2o 2.0 Product
      ]
      Synthesis
      Nothing

-- | Methane combustion: CH₄ + 2O₂ → CO₂ + 2H₂O
methaneCombustion :: Reaction
methaneCombustion =
  let ch4 = Molecule {atoms = [(C, Neutral), (H, Neutral), (H, Neutral), (H, Neutral), (H, Neutral)], bonds = [], smiles = Just "C"}
      o2 = Molecule {atoms = [(O, Neutral), (O, Neutral)], bonds = [], smiles = Just "O=O"}
      co2 = Molecule {atoms = [(C, Neutral), (O, Neutral), (O, Neutral)], bonds = [], smiles = Just "O=C=O"}
      h2o = Molecule {atoms = [(O, Neutral), (H, Neutral), (H, Neutral)], bonds = [], smiles = Just "O"}
  in mkReaction
      [ mkReagent ch4 1.0 Reactant
      , mkReagent o2 2.0 Reactant
      , mkReagent co2 1.0 Product
      , mkReagent h2o 2.0 Product
      ]
      Combustion
      Nothing

-- | Aspirin synthesis (simplified): Salicylic acid + Acetic anhydride → Aspirin + Acetic acid
aspirinSynthesis :: Reaction
aspirinSynthesis =
  let salicylicAcid = Molecule 
        { atoms = replicate 7 (C, Neutral) ++ replicate 6 (H, Neutral) ++ replicate 3 (O, Neutral)
        , bonds = []
        , smiles = Just "O=C(O)c1ccccc1O"
        }
      aceticAnhydride = Molecule
        { atoms = replicate 4 (C, Neutral) ++ replicate 6 (H, Neutral) ++ replicate 3 (O, Neutral)
        , bonds = []
        , smiles = Just "CC(=O)OC(=O)C"
        }
      aspirin = Molecule
        { atoms = replicate 9 (C, Neutral) ++ replicate 8 (H, Neutral) ++ replicate 4 (O, Neutral)
        , bonds = []
        , smiles = Just "CC(=O)Oc1ccccc1C(=O)O"
        }
      aceticAcid = Molecule
        { atoms = replicate 2 (C, Neutral) ++ replicate 4 (H, Neutral) ++ replicate 2 (O, Neutral)
        , bonds = []
        , smiles = Just "CC(=O)O"
        }
  in mkReaction
      [ mkReagent salicylicAcid 1.0 Reactant
      , mkReagent aceticAnhydride 1.0 Reactant
      , mkReagent aspirin 1.0 Product
      , mkReagent aceticAcid 1.0 Product
      ]
      Synthesis
      Nothing
