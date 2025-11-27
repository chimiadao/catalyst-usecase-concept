#!/usr/bin/env python3
"""
ChimiaDAO Organic Synthesis Demo: Fischer Esterification

Demonstrates:
- Organic molecule representation (C, H, O atoms)
- Functional group transformation (carboxylic acid + alcohol → ester)
- More complex stoichiometry
- Reaction conditions tracking

Reaction: CH₃COOH + CH₃OH → CH₃COOCH₃ + H₂O
(Acetic acid + Methanol → Methyl acetate + Water)
"""

import sys
from pathlib import Path

# Import from synthesis_demo
sys.path.insert(0, str(Path(__file__).parent))
from synthesis_demo import Element, Atom, Molecule, Reaction


def create_acetic_acid() -> Molecule:
    """
    Acetic acid: CH₃COOH
    
    Structure:
        H  O
        |  ||
    H - C - C - O - H
        |
        H
    """
    return Molecule(
        name="Acetic acid",
        atoms=[
            # Methyl group (CH₃)
            Atom(Element.C),      # 0: C
            Atom(Element.H),      # 1: H
            Atom(Element.H),      # 2: H
            Atom(Element.H),      # 3: H
            # Carboxylic acid group (COOH)
            Atom(Element.C),      # 4: C=O
            Atom(Element.O),      # 5: O (double bond)
            Atom(Element.O),      # 6: O-H
            Atom(Element.H),      # 7: H
        ],
        bonds=[
            # Methyl group bonds
            (0, 1, 1),   # C-H
            (0, 2, 1),   # C-H
            (0, 3, 1),   # C-H
            # Connect methyl to carbonyl
            (0, 4, 1),   # C-C
            # Carboxylic acid group
            (4, 5, 2),   # C=O (double bond)
            (4, 6, 1),   # C-O (single bond)
            (6, 7, 1),   # O-H
        ]
    )


def create_methanol() -> Molecule:
    """
    Methanol: CH₃OH
    
    Structure:
        H
        |
    H - C - O - H
        |
        H
    """
    return Molecule(
        name="Methanol",
        atoms=[
            Atom(Element.C),      # 0: C
            Atom(Element.H),      # 1: H
            Atom(Element.H),      # 2: H
            Atom(Element.H),      # 3: H
            Atom(Element.O),      # 4: O
            Atom(Element.H),      # 5: H
        ],
        bonds=[
            (0, 1, 1),   # C-H
            (0, 2, 1),   # C-H
            (0, 3, 1),   # C-H
            (0, 4, 1),   # C-O
            (4, 5, 1),   # O-H
        ]
    )


def create_methyl_acetate() -> Molecule:
    """
    Methyl acetate: CH₃COOCH₃
    
    Structure:
        H     O      H
        |     ||     |
    H - C - C - O - C - H
        |            |
        H            H
    """
    return Molecule(
        name="Methyl acetate",
        atoms=[
            # First methyl group (CH₃ from acetic acid)
            Atom(Element.C),      # 0: C
            Atom(Element.H),      # 1: H
            Atom(Element.H),      # 2: H
            Atom(Element.H),      # 3: H
            # Ester group (COO)
            Atom(Element.C),      # 4: C=O
            Atom(Element.O),      # 5: O (double bond)
            Atom(Element.O),      # 6: O (single bond to methyl)
            # Second methyl group (CH₃ from methanol)
            Atom(Element.C),      # 7: C
            Atom(Element.H),      # 8: H
            Atom(Element.H),      # 9: H
            Atom(Element.H),      # 10: H
        ],
        bonds=[
            # First methyl group
            (0, 1, 1),   # C-H
            (0, 2, 1),   # C-H
            (0, 3, 1),   # C-H
            (0, 4, 1),   # C-C
            # Ester group
            (4, 5, 2),   # C=O
            (4, 6, 1),   # C-O
            # Connect to second methyl
            (6, 7, 1),   # O-C
            # Second methyl group
            (7, 8, 1),   # C-H
            (7, 9, 1),   # C-H
            (7, 10, 1),  # C-H
        ]
    )


def create_water() -> Molecule:
    """Water: H₂O"""
    return Molecule(
        name="Water",
        atoms=[
            Atom(Element.H),
            Atom(Element.O),
            Atom(Element.H)
        ],
        bonds=[(0, 1, 1), (1, 2, 1)]
    )


def demo_fischer_esterification():
    """Run the Fischer esterification demo"""
    print("=" * 70)
    print("ChimiaDAO Organic Synthesis Demo: Fischer Esterification")
    print("=" * 70)
    print()
    print("This reaction demonstrates:")
    print("  • Organic molecule representation (C-H-O structures)")
    print("  • Functional group transformation (acid + alcohol → ester)")
    print("  • Catalyst and temperature conditions")
    print("  • Reversible reaction equilibrium")
    print()
    
    # Create molecules
    acetic_acid = create_acetic_acid()
    methanol = create_methanol()
    methyl_acetate = create_methyl_acetate()
    water = create_water()
    
    print("Reactants:")
    print(f"  • {acetic_acid.name} ({acetic_acid.molecular_formula()})")
    print(f"    Mass: {acetic_acid.molecular_mass():.3f} g/mol")
    print(f"    Functional group: Carboxylic acid (COOH)")
    print()
    print(f"  • {methanol.name} ({methanol.molecular_formula()})")
    print(f"    Mass: {methanol.molecular_mass():.3f} g/mol")
    print(f"    Functional group: Alcohol (OH)")
    print()
    
    print("Products:")
    print(f"  • {methyl_acetate.name} ({methyl_acetate.molecular_formula()})")
    print(f"    Mass: {methyl_acetate.molecular_mass():.3f} g/mol")
    print(f"    Functional group: Ester (COOR)")
    print()
    print(f"  • {water.name} ({water.molecular_formula()})")
    print(f"    Mass: {water.molecular_mass():.3f} g/mol")
    print(f"    By-product of condensation")
    print()
    
    # Create reaction
    reaction = Reaction(
        reactants=[
            (1, acetic_acid),
            (1, methanol)
        ],
        products=[
            (1, methyl_acetate),
            (1, water)
        ],
        conditions={
            "catalyst": "H₂SO₄ (sulfuric acid, 5% v/v)",
            "temperature": "60-70°C",
            "time": "2-4 hours",
            "mechanism": "Fischer esterification",
            "reversible": "yes (equilibrium favors products at high temperature)",
            "notes": "Remove water to drive equilibrium forward (Le Chatelier)"
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
        
        reactant_mass = sum(c * m.molecular_mass() for c, m in reaction.reactants)
        product_mass = sum(c * m.molecular_mass() for c, m in reaction.products)
        print(f"    Reactants: {reactant_mass:.3f} g/mol")
        print(f"    Products:  {product_mass:.3f} g/mol")
        print()
        
        charge_ok, charge_diff = reaction.check_charge_balance()
        print(f"  ✓ Charge balanced (Δq = {charge_diff:+d})")
        print()
        
        atom_ok, atom_diffs = reaction.check_atom_balance()
        print(f"  ✓ Atoms balanced")
        
        # Count atoms
        from collections import Counter
        reactant_atoms = Counter()
        for coeff, mol in reaction.reactants:
            for atom in mol.atoms:
                reactant_atoms[atom.element.name] += coeff
        
        product_atoms = Counter()
        for coeff, mol in reaction.products:
            for atom in mol.atoms:
                product_atoms[atom.element.name] += coeff
        
        print("    Atom inventory:")
        for element in sorted(set(reactant_atoms.keys()) | set(product_atoms.keys())):
            r_count = reactant_atoms[element]
            p_count = product_atoms[element]
            print(f"      {element}: {r_count} → {p_count}")
        print()
        
        # Generate CID
        cid = reaction.generate_cid()
        print(f"Simulated IPFS CID: {cid}")
        print()
        
        print("Reaction conditions:")
        for key, value in reaction.conditions.items():
            print(f"  • {key.replace('_', ' ').title()}: {value}")
        print()
        
        print("=" * 70)
        print("✅ Fischer esterification validated successfully!")
        print()
        print("Key insights:")
        print("  • Type-safe representation scales to organic molecules")
        print("  • Functional group transformations are traceable")
        print("  • Reaction conditions captured for reproducibility")
        print("  • This reaction is foundational for polymer chemistry")
        print()
        print("Next steps:")
        print("  • Add support for stereochemistry (R/S configuration)")
        print("  • Implement reaction mechanism steps (proton transfer, etc.)")
        print("  • Connect to RDKit for 2D/3D structure visualization")
        print("  • Integrate with retrosynthetic analysis (AiZynthFinder)")
        print("=" * 70)
        
    else:
        print("  ❌ Reaction validation failed:")
        for error in errors:
            print(f"     • {error}")
        print()


if __name__ == "__main__":
    demo_fischer_esterification()
