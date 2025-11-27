#!/usr/bin/env python3
"""
ChimiaDAO Synthesis Demo: Acid-Base Neutralization

Demonstrates:
- Typed molecular representation
- Stoichiometry validation
- Mass balance checking
- IPFS CID generation (simulation)

Reaction: HCl + NaOH → NaCl + H₂O
"""

from dataclasses import dataclass
from typing import List, Dict, Tuple
import json
import hashlib
from enum import Enum


class Element(Enum):
    """Basic elements for demo (extend for full periodic table)"""
    H = ("Hydrogen", 1.008)
    C = ("Carbon", 12.011)
    N = ("Nitrogen", 14.007)
    O = ("Oxygen", 15.999)
    Na = ("Sodium", 22.990)
    Cl = ("Chlorine", 35.453)
    
    def __init__(self, name: str, mass: float):
        self.element_name = name
        self.atomic_mass = mass


@dataclass
class Atom:
    """Single atom with element and charge"""
    element: Element
    charge: int = 0
    
    def __repr__(self):
        charge_str = f"({self.charge:+d})" if self.charge != 0 else ""
        return f"{self.element.name}{charge_str}"


@dataclass
class Molecule:
    """
    Type-safe molecular representation
    
    Instead of string-based SMILES, we use typed structures
    This enables compile-time validation in strongly-typed languages (Haskell/Plutus)
    """
    name: str
    atoms: List[Atom]
    bonds: List[Tuple[int, int, int]]  # (atom1_idx, atom2_idx, bond_order)
    
    def molecular_formula(self) -> str:
        """Generate molecular formula (e.g., H2O)"""
        element_counts: Dict[Element, int] = {}
        for atom in self.atoms:
            element_counts[atom.element] = element_counts.get(atom.element, 0) + 1
        
        # Sort by hill system (C, H, then alphabetical)
        sorted_elements = sorted(element_counts.keys(), 
                                key=lambda e: (e.name != 'C', e.name != 'H', e.name))
        
        formula_parts = []
        for element in sorted_elements:
            count = element_counts[element]
            formula_parts.append(f"{element.name}{count if count > 1 else ''}")
        
        return "".join(formula_parts)
    
    def molecular_mass(self) -> float:
        """Calculate molecular mass"""
        return sum(atom.element.atomic_mass for atom in self.atoms)
    
    def net_charge(self) -> int:
        """Calculate net charge"""
        return sum(atom.charge for atom in self.atoms)
    
    def to_dict(self) -> dict:
        """Serialize for IPFS storage"""
        return {
            "name": self.name,
            "formula": self.molecular_formula(),
            "mass": self.molecular_mass(),
            "charge": self.net_charge(),
            "atoms": [
                {
                    "element": atom.element.name,
                    "charge": atom.charge
                }
                for atom in self.atoms
            ],
            "bonds": self.bonds
        }


@dataclass
class Reaction:
    """
    Chemical reaction with reactants and products
    
    Enforces:
    - Mass balance (conservation of mass)
    - Charge balance (conservation of charge)
    - Stoichiometry (integer coefficients)
    """
    reactants: List[Tuple[int, Molecule]]  # (coefficient, molecule)
    products: List[Tuple[int, Molecule]]
    conditions: Dict[str, str] = None
    
    def __post_init__(self):
        if self.conditions is None:
            self.conditions = {}
    
    def check_mass_balance(self) -> Tuple[bool, float]:
        """Verify conservation of mass"""
        reactant_mass = sum(coeff * mol.molecular_mass() 
                           for coeff, mol in self.reactants)
        product_mass = sum(coeff * mol.molecular_mass() 
                          for coeff, mol in self.products)
        
        # Allow small floating-point error
        difference = abs(reactant_mass - product_mass)
        is_balanced = difference < 0.001
        
        return is_balanced, difference
    
    def check_charge_balance(self) -> Tuple[bool, int]:
        """Verify conservation of charge"""
        reactant_charge = sum(coeff * mol.net_charge() 
                             for coeff, mol in self.reactants)
        product_charge = sum(coeff * mol.net_charge() 
                            for coeff, mol in self.products)
        
        difference = reactant_charge - product_charge
        is_balanced = difference == 0
        
        return is_balanced, difference
    
    def check_atom_balance(self) -> Tuple[bool, Dict[Element, int]]:
        """Verify conservation of atoms (each element)"""
        def count_atoms(molecules: List[Tuple[int, Molecule]]) -> Dict[Element, int]:
            counts: Dict[Element, int] = {}
            for coeff, mol in molecules:
                for atom in mol.atoms:
                    counts[atom.element] = counts.get(atom.element, 0) + coeff
            return counts
        
        reactant_atoms = count_atoms(self.reactants)
        product_atoms = count_atoms(self.products)
        
        # Find differences
        all_elements = set(reactant_atoms.keys()) | set(product_atoms.keys())
        differences = {}
        for element in all_elements:
            r_count = reactant_atoms.get(element, 0)
            p_count = product_atoms.get(element, 0)
            if r_count != p_count:
                differences[element] = r_count - p_count
        
        is_balanced = len(differences) == 0
        return is_balanced, differences
    
    def validate(self) -> Tuple[bool, List[str]]:
        """Run all validation checks"""
        errors = []
        
        # Check mass balance
        mass_ok, mass_diff = self.check_mass_balance()
        if not mass_ok:
            errors.append(f"Mass not balanced: difference = {mass_diff:.4f} g/mol")
        
        # Check charge balance
        charge_ok, charge_diff = self.check_charge_balance()
        if not charge_ok:
            errors.append(f"Charge not balanced: difference = {charge_diff:+d}")
        
        # Check atom balance
        atom_ok, atom_diffs = self.check_atom_balance()
        if not atom_ok:
            for element, diff in atom_diffs.items():
                errors.append(f"Atom {element.name} not balanced: difference = {diff:+d}")
        
        return len(errors) == 0, errors
    
    def to_dict(self) -> dict:
        """Serialize for IPFS storage"""
        return {
            "reactants": [
                {"coefficient": coeff, "molecule": mol.to_dict()}
                for coeff, mol in self.reactants
            ],
            "products": [
                {"coefficient": coeff, "molecule": mol.to_dict()}
                for coeff, mol in self.products
            ],
            "conditions": self.conditions
        }
    
    def generate_cid(self) -> str:
        """
        Simulate IPFS CID generation
        
        In production, this would:
        1. Serialize reaction to JSON
        2. Upload to IPFS
        3. Return actual CID (e.g., Qm...)
        
        For demo, we hash the JSON
        """
        reaction_json = json.dumps(self.to_dict(), sort_keys=True)
        hash_digest = hashlib.sha256(reaction_json.encode()).hexdigest()
        # Simulate IPFS CID format
        return f"Qm{hash_digest[:44]}"
    
    def __repr__(self):
        def format_side(molecules: List[Tuple[int, Molecule]]) -> str:
            parts = []
            for coeff, mol in molecules:
                coeff_str = f"{coeff} " if coeff > 1 else ""
                parts.append(f"{coeff_str}{mol.molecular_formula()}")
            return " + ".join(parts)
        
        return f"{format_side(self.reactants)} → {format_side(self.products)}"


# ============================================================================
# Demo: Acid-Base Neutralization (HCl + NaOH → NaCl + H₂O)
# ============================================================================

def create_hcl() -> Molecule:
    """Hydrochloric acid: H-Cl"""
    return Molecule(
        name="Hydrochloric acid",
        atoms=[
            Atom(Element.H, charge=1),   # H⁺
            Atom(Element.Cl, charge=-1)  # Cl⁻
        ],
        bonds=[(0, 1, 1)]  # Single bond
    )


def create_naoh() -> Molecule:
    """Sodium hydroxide: Na-O-H"""
    return Molecule(
        name="Sodium hydroxide",
        atoms=[
            Atom(Element.Na, charge=1),  # Na⁺
            Atom(Element.O, charge=-2),  # O²⁻
            Atom(Element.H, charge=1)    # H⁺
        ],
        bonds=[(0, 1, 1), (1, 2, 1)]  # Na-O, O-H bonds
    )


def create_nacl() -> Molecule:
    """Sodium chloride: Na-Cl"""
    return Molecule(
        name="Sodium chloride",
        atoms=[
            Atom(Element.Na, charge=1),  # Na⁺
            Atom(Element.Cl, charge=-1)  # Cl⁻
        ],
        bonds=[(0, 1, 1)]  # Ionic bond
    )


def create_water() -> Molecule:
    """Water: H-O-H"""
    return Molecule(
        name="Water",
        atoms=[
            Atom(Element.H),
            Atom(Element.O),
            Atom(Element.H)
        ],
        bonds=[(0, 1, 1), (1, 2, 1)]  # Two O-H bonds
    )


def demo_synthesis_reaction():
    """Run the synthesis demo"""
    print("=" * 70)
    print("ChimiaDAO Synthesis Demo: Acid-Base Neutralization")
    print("=" * 70)
    print()
    
    # Create molecules
    hcl = create_hcl()
    naoh = create_naoh()
    nacl = create_nacl()
    water = create_water()
    
    print("Reactants:")
    print(f"  • {hcl.name} ({hcl.molecular_formula()})")
    print(f"    Mass: {hcl.molecular_mass():.3f} g/mol, Charge: {hcl.net_charge():+d}")
    print()
    print(f"  • {naoh.name} ({naoh.molecular_formula()})")
    print(f"    Mass: {naoh.molecular_mass():.3f} g/mol, Charge: {naoh.net_charge():+d}")
    print()
    
    print("Products:")
    print(f"  • {nacl.name} ({nacl.molecular_formula()})")
    print(f"    Mass: {nacl.molecular_mass():.3f} g/mol, Charge: {nacl.net_charge():+d}")
    print()
    print(f"  • {water.name} ({water.molecular_formula()})")
    print(f"    Mass: {water.molecular_mass():.3f} g/mol, Charge: {water.net_charge():+d}")
    print()
    
    # Create reaction
    reaction = Reaction(
        reactants=[
            (1, hcl),
            (1, naoh)
        ],
        products=[
            (1, nacl),
            (1, water)
        ],
        conditions={
            "temperature": "room temperature (25°C)",
            "solvent": "aqueous",
            "reaction_type": "acid-base neutralization"
        }
    )
    
    print("-" * 70)
    print(f"Reaction: {reaction}")
    print("-" * 70)
    print()
    
    # Validate
    print("Validation:")
    is_valid, errors = reaction.validate()
    
    if is_valid:
        print("  ✓ Reaction is chemically valid!")
        print()
        
        # Show conservation laws
        mass_ok, mass_diff = reaction.check_mass_balance()
        print(f"  ✓ Mass balanced (Δm = {mass_diff:.6f} g/mol)")
        
        charge_ok, charge_diff = reaction.check_charge_balance()
        print(f"  ✓ Charge balanced (Δq = {charge_diff:+d})")
        
        atom_ok, atom_diffs = reaction.check_atom_balance()
        print(f"  ✓ Atoms balanced")
        print()
        
        # Generate CID
        cid = reaction.generate_cid()
        print(f"Simulated IPFS CID: {cid}")
        print()
        
        # Show JSON serialization
        print("Reaction data (for IPFS storage):")
        print(json.dumps(reaction.to_dict(), indent=2))
        print()
        
        print("=" * 70)
        print("✅ Demo completed successfully!")
        print()
        print("Next steps:")
        print("  1. Extend to more complex reactions (organic synthesis)")
        print("  2. Add support for reaction mechanisms (step-by-step)")
        print("  3. Integrate with IPFS for real CID generation")
        print("  4. Connect to smart contract for on-chain verification")
        print("=" * 70)
        
    else:
        print("  ❌ Reaction validation failed:")
        for error in errors:
            print(f"     • {error}")
        print()


if __name__ == "__main__":
    demo_synthesis_reaction()
