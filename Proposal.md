# On-Chain Verifiable Chemistry (Concept, ₳100k)

## Problem
Chemistry powers a $5T industry, yet its data layer is centralized, opaque, and outdated (e.g., SMILES). (Retro)synthesis knowledge is siloed, slowing verification, reproducibility, and collaboration.

## Solution
Cardano-based proto-infrastructure for **on-chain verifiable chemistry**:
- **Type-safe Haskell** molecular representation (formally checkable; beyond SMILES/SELFIES).
- **Plutus** validators to record & verify synthesis/retrosynthesis steps on-chain.
- A **verifiable chemistry ledger** as a reusable public good.

## Why Cardano
Haskell + formal methods match chemistry’s need for rigor, which is something every professional synthetic chemist understands implicitly. 
Cardano lacks a flagship **DeSci** primitive; this establishes it.

## Scope (Concept, 6 months)
- **Library prototype:** serialize as many reactions as possible via a type-safe ADT (bare minimum target: ≥50 reactions across ≥3 common classes; ≥1 full retrosynthesis pathway).
- **Validator prototype:** minimal Plutus validator for a reaction step (inputs/outputs/stoichiometry present; CIP-68 state layout).
- **Demo UI:** submit/view a step; see validator outcome.
- **Docs + pilot:** examples + ≥5 external testers with physical Chemistry/Physics labs.

## Budget (₳100,000)
- Founder stipend (6 mo, lean): ₳20k
- Core dev (Haskell/Plutus, incl. UI build): ₳65k
- Infra & tools (testnets/hosting/tooling): ₳10k
- Docs/outreach/contingency: ₳5k

## Proof / Credibility
- **ETHGlobal SF 2024:** Polygon “Best Consumer App” Runner-Up — https://ethglobal.com/showcase/chimiadao-xsmjg  
- **Subgraph prototype (Polygon Amoy):** https://github.com/xchemtina/chimiadao-ethglobal-subgraph  
- **Etica.io:** on-chain research ecosystem — https://github.com/etica

## License & data
Apache-2.0 for libs/contracts; demo & docs open. Bulk data off-chain with on-chain hashes (CIP-68 layout).
