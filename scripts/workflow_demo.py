#!/usr/bin/env python3
"""
ChimiaDAO Complete Workflow Demo

Simulates the full lifecycle of a reaction submission:
1. Define reaction (type-safe structures)
2. Validate (mass, charge, atom balance)
3. Serialize to JSON
4. Generate IPFS CID (simulated)
5. Submit to smart contract (simulated)
6. Challenge period (simulated)
7. Reputation update (simulated)

This demonstrates how ChimiaDAO integrates:
- Off-chain: Type-safe validation, IPFS storage
- On-chain: Plutus validators, EVM contracts, Solana programs
"""

import sys
import json
import time
from pathlib import Path
from dataclasses import dataclass, asdict
from typing import Optional

# Import reaction library
sys.path.insert(0, str(Path(__file__).parent))
from synthesis_demo import Element, Atom, Molecule, Reaction, create_hcl, create_naoh, create_nacl, create_water


@dataclass
class Submitter:
    """Chemist submitting a reaction"""
    address: str  # Wallet address (0x... or addr1...)
    reputation: int
    stake: float  # In ETH or ADA
    
    
@dataclass
class ChainState:
    """Simulated multi-chain state"""
    cardano_block: int
    monad_block: int
    solana_slot: int
    filecoin_height: int


def simulate_ipfs_upload(reaction: Reaction) -> tuple[str, dict]:
    """
    Simulate uploading reaction to IPFS
    
    In production:
    - Upload JSON to IPFS node
    - Pin to Filecoin via Lighthouse.storage
    - Return real CID (Qm... or bafy...)
    """
    reaction_json = reaction.to_dict()
    cid = reaction.generate_cid()
    
    print("📤 Uploading to IPFS...")
    time.sleep(0.5)  # Simulate network delay
    print(f"   ✓ Uploaded to IPFS: {cid}")
    print(f"   ✓ Pinned to Filecoin (simulated)")
    print(f"   ✓ Data size: {len(json.dumps(reaction_json))} bytes")
    
    return cid, reaction_json


def simulate_cardano_submission(cid: str, submitter: Submitter, chain: ChainState) -> dict:
    """
    Simulate Cardano transaction with Plutus validator
    
    In production:
    - Build transaction with CIP-68 metadata
    - Submit to Plutus validator (checkMassBalance, etc.)
    - Pay transaction fee (~0.2 ADA)
    - Return transaction hash
    """
    print()
    print("🔗 Submitting to Cardano (Plutus validator)...")
    time.sleep(0.3)
    
    tx_hash = f"cardano_tx_{chain.cardano_block}_{submitter.address[:8]}"
    
    print(f"   ✓ Transaction hash: {tx_hash}")
    print(f"   ✓ Block: {chain.cardano_block}")
    print(f"   ✓ Validator checked: Mass balance, charge balance, atom balance")
    print(f"   ✓ CIP-68 metadata: {cid}")
    print(f"   ✓ Fee: 0.17 ADA")
    
    chain.cardano_block += 1
    
    return {
        "chain": "cardano",
        "tx_hash": tx_hash,
        "block": chain.cardano_block - 1,
        "cid": cid,
        "submitter": submitter.address
    }


def simulate_monad_submission(cid: str, submitter: Submitter, chain: ChainState) -> dict:
    """
    Simulate Monad (EVM) submission
    
    In production:
    - Call PoXRegistry.submitExperiment(cid, experimentType, difficulty)
    - Stake 0.01 ETH (held in escrow for 15 days)
    - Emit ExperimentSubmitted event
    - Gas cost: ~50k gas @ 1 gwei = $0.10
    """
    print()
    print("⚡ Submitting to Monad (EVM contract)...")
    time.sleep(0.2)
    
    tx_hash = f"0xmonad{chain.monad_block:06d}{submitter.address[:8]}"
    experiment_id = chain.monad_block * 1000 + 42
    
    print(f"   ✓ Transaction hash: {tx_hash}")
    print(f"   ✓ Block: {chain.monad_block}")
    print(f"   ✓ Experiment ID: {experiment_id}")
    print(f"   ✓ Stake: {submitter.stake} ETH (refundable after challenge period)")
    print(f"   ✓ Gas used: 52,341 @ 1 gwei")
    print(f"   ✓ Challenge period: 15 days")
    
    chain.monad_block += 1
    
    return {
        "chain": "monad",
        "tx_hash": tx_hash,
        "block": chain.monad_block - 1,
        "experiment_id": experiment_id,
        "stake": submitter.stake,
        "challenge_end_block": chain.monad_block + (15 * 24 * 60 * 60 // 2)  # 15 days in 2s blocks
    }


def simulate_solana_reputation(submitter: Submitter, chain: ChainState) -> dict:
    """
    Simulate Solana reputation update
    
    In production:
    - Call reputation_program.update_reputation(submitter, +1)
    - Transaction cost: $0.0003 (Solana transaction fee)
    - Reputation capped at 100
    """
    print()
    print("🚀 Updating reputation on Solana...")
    time.sleep(0.1)
    
    tx_sig = f"solana_sig_{chain.solana_slot}_{submitter.address[:8]}"
    old_rep = submitter.reputation
    new_rep = min(old_rep + 1, 100)  # Cap at 100
    submitter.reputation = new_rep
    
    print(f"   ✓ Transaction signature: {tx_sig}")
    print(f"   ✓ Slot: {chain.solana_slot}")
    print(f"   ✓ Reputation: {old_rep} → {new_rep}")
    print(f"   ✓ Fee: $0.0003")
    
    chain.solana_slot += 1
    
    return {
        "chain": "solana",
        "tx_sig": tx_sig,
        "slot": chain.solana_slot - 1,
        "old_reputation": old_rep,
        "new_reputation": new_rep
    }


def simulate_filecoin_storage(cid: str, chain: ChainState) -> dict:
    """
    Simulate Filecoin deal creation
    
    In production:
    - Create storage deal via Lighthouse.storage API
    - Pay for storage (Filecoin Plus DataCap for subsidized storage)
    - 1 year storage: ~$0.10/GB
    """
    print()
    print("💾 Creating Filecoin storage deal...")
    time.sleep(0.4)
    
    deal_id = chain.filecoin_height * 10 + 7
    
    print(f"   ✓ Deal ID: {deal_id}")
    print(f"   ✓ CID: {cid}")
    print(f"   ✓ Duration: 1 year (525,600 epochs)")
    print(f"   ✓ Cost: $0.02 (subsidized via Filecoin Plus)")
    print(f"   ✓ Miner: f01234567 (simulated)")
    
    chain.filecoin_height += 1
    
    return {
        "chain": "filecoin",
        "deal_id": deal_id,
        "cid": cid,
        "duration_years": 1,
        "cost_usd": 0.02
    }


def print_summary(reaction: Reaction, submitter: Submitter, 
                 cardano_tx: dict, monad_tx: dict, 
                 solana_tx: dict, filecoin_tx: dict):
    """Print submission summary"""
    print()
    print("=" * 70)
    print("📊 SUBMISSION SUMMARY")
    print("=" * 70)
    print()
    print(f"Reaction: {reaction}")
    print(f"Submitter: {submitter.address}")
    print(f"Reputation: {submitter.reputation}")
    print()
    print("Multi-Chain Transactions:")
    print(f"  • Cardano (governance):  {cardano_tx['tx_hash']}")
    print(f"  • Monad (speed):         {monad_tx['tx_hash']}")
    print(f"  • Solana (state):        {solana_tx['tx_sig']}")
    print(f"  • Filecoin (storage):    Deal #{filecoin_tx['deal_id']}")
    print()
    print("Economics:")
    print(f"  • Stake locked:  {monad_tx['stake']} ETH (refundable)")
    print(f"  • Cardano fee:   0.17 ADA (~$0.08)")
    print(f"  • Monad gas:     52,341 gwei (~$0.10)")
    print(f"  • Solana fee:    $0.0003")
    print(f"  • Filecoin:      $0.02")
    print(f"  • TOTAL COST:    ~$0.20 (excluding refundable stake)")
    print()
    print("Challenge Period:")
    print(f"  • Duration: 15 days")
    print(f"  • End block: {monad_tx['challenge_end_block']} (Monad)")
    print(f"  • If unchallenged → reputation +1, stake refunded, ADA reward")
    print(f"  • If challenged → 3-of-5 multi-sig resolvers vote")
    print()
    print("=" * 70)


def main():
    """Run complete workflow demo"""
    print("=" * 70)
    print("ChimiaDAO Complete Workflow Demo")
    print("=" * 70)
    print()
    print("Simulating multi-chain reaction submission...")
    print()
    
    # Initialize chain state
    chain = ChainState(
        cardano_block=8_500_000,
        monad_block=1_234_567,
        solana_slot=250_000_000,
        filecoin_height=3_500_000
    )
    
    # Create submitter
    submitter = Submitter(
        address="0xChemist123456789abcdef",
        reputation=42,
        stake=0.01  # ETH
    )
    
    print(f"👤 Submitter: {submitter.address}")
    print(f"   Reputation: {submitter.reputation}")
    print(f"   Stake: {submitter.stake} ETH")
    print()
    
    # Step 1: Define reaction
    print("🧪 Step 1: Define Reaction")
    print("-" * 70)
    reaction = Reaction(
        reactants=[
            (1, create_hcl()),
            (1, create_naoh())
        ],
        products=[
            (1, create_nacl()),
            (1, create_water())
        ],
        conditions={
            "temperature": "25°C",
            "solvent": "aqueous",
            "type": "acid-base neutralization"
        }
    )
    print(f"Reaction: {reaction}")
    print()
    
    # Step 2: Validate
    print("✅ Step 2: Validate")
    print("-" * 70)
    is_valid, errors = reaction.validate()
    if not is_valid:
        print("❌ Validation failed:")
        for error in errors:
            print(f"   • {error}")
        return
    
    print("✓ Mass balanced")
    print("✓ Charge balanced")
    print("✓ Atoms balanced")
    print()
    
    # Step 3: Upload to IPFS
    print("📦 Step 3: IPFS Upload")
    print("-" * 70)
    cid, reaction_data = simulate_ipfs_upload(reaction)
    print()
    
    # Step 4: Multi-chain submission
    print("🌐 Step 4: Multi-Chain Submission")
    print("-" * 70)
    
    cardano_tx = simulate_cardano_submission(cid, submitter, chain)
    monad_tx = simulate_monad_submission(cid, submitter, chain)
    solana_tx = simulate_solana_reputation(submitter, chain)
    filecoin_tx = simulate_filecoin_storage(cid, chain)
    
    # Summary
    print_summary(reaction, submitter, cardano_tx, monad_tx, solana_tx, filecoin_tx)
    
    print("✅ Workflow complete!")
    print()
    print("What happens next:")
    print("  1. 15-day challenge period begins")
    print("  2. Validators check reaction correctness")
    print("  3. If no challenge → stake refunded, ADA reward issued")
    print("  4. If challenged → multi-sig resolvers vote")
    print("  5. Reaction added to queryable index (SQD/Subgraph)")
    print()
    print("=" * 70)


if __name__ == "__main__":
    main()
