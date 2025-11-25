# WARP.md

This file provides guidance to WARP (warp.dev) when working with code in this repository.

## Project Overview

**ChimiaDAO** is a multi-chain verifiable chemistry infrastructure project seeking ₳200,000 from Cardano Catalyst Fund 15. This is a **concept/planning repository** (not a code implementation) containing proposal documentation, architectural designs, and roadmap planning.

### Core Concept
- **Multi-chain architecture**: Cardano (governance/formal verification) + Monad (speed) + Solana (state/payments) + Filecoin (storage)
- **Type-safe molecular representation**: Haskell ADT → Plutus validators for on-chain chemistry verification
- **Stake-based spam prevention**: Economic incentives for data quality
- **Challenge resolution**: Multi-sig → bisection → ZK proofs (progressive decentralization)

### Project Status
- ✅ ETHGlobal SF 2024 demo completed (Polygon prototype, Runner-Up award)
- ✅ Security audit insights documented (PoX hardening plan)
- ✅ Multi-chain architecture designed
- ⏳ Catalyst Fund 15 submission in progress

## Repository Structure

```
catalyst-usecase-concept/
├── README.md           # Main proposal document (multi-chain architecture, budget, roadmap)
├── Proposal.md         # Condensed 1-page proposal (₳100k original scope)
├── NEXT_STEPS.md       # 3-day sprint plan for proposal finalization
├── THOUGHTS.md         # Deep-dive: architectural decisions, security philosophy, open problems
├── Links.md            # Supporting references (ETHGlobal, ORD, Goldstein's work)
└── docs/               # Future: diagrams, technical specs, audit reports
```

## Key Documentation Files

### README.md
- **Audience**: Catalyst voters, potential collaborators
- **Content**: Problem statement, multi-chain architecture, technical foundation (Haskell ADT, Plutus validators), budget breakdown (₳200k), roadmap
- **Key sections**: Cross-chain communication diagram, CIP-68 metadata standard, security & economics, proof of capability

### THOUGHTS.md
- **Audience**: Technical reviewers, future development team
- **Content**: Philosophical rationale for multi-chain approach, detailed architectural decisions per chain, security lessons from PoX audit, type-safe molecular representation deep-dive
- **Key insights**: 
  - "Fortis est Veritas" principle (truth strengthened by multiple verification layers)
  - Input validation hierarchy (format → economic → stoichiometric → peer review)
  - Loosely coupled cross-chain strategy (periodic state commitments, not real-time sync)

### NEXT_STEPS.md
- **Audience**: Project lead (you), immediate team
- **Content**: Detailed 3-day sprint plan for Catalyst submission
- **Structure**: Day-by-day tasks (research → writing → finalization), success criteria, post-submission actions

## Core Technical Concepts

### 1. Type-Safe Molecular Representation
**Why it matters**: Traditional chemistry notation (SMILES/SELFIES) is string-based and error-prone. ChimiaDAO uses Haskell algebraic data types (ADTs) for compile-time verification.

**Example structure** (conceptual, not implemented here):
```haskell
data Molecule = Molecule
  { atoms :: [(Atom, Charge)]
  , bonds :: [(Int, Int, BondType)]
  }

checkMassBalance :: Reaction -> Either String ()
```

**Cross-language challenge**: Need to compile Haskell ADT to:
- Plutus (Cardano validators)
- Rust (Solana programs)  
- Solidity (Monad/EVM contracts)

### 2. Multi-Chain Coordination
**Design principle**: Each chain optimizes for different tradeoffs
- **Cardano**: Formal verification (Haskell → Plutus), governance anchor, eUTXO determinism
- **Monad**: High-speed execution (10k+ TPS), challenge resolution, sub-second finality
- **Solana**: Low-cost micro-transactions ($0.0003 per tx), real-time reputation updates
- **Filecoin**: Permanent storage (IPFS CIDs), content-addressed data integrity

**Bridge strategy**: Wormhole for cross-chain messaging, periodic state root synchronization (not real-time)

### 3. Economic Security Model
**Stake-based spam prevention**:
- Register experiment: 0.01 ETH stake (refunded after 15-day challenge period)
- Submit challenge: 0.01 ETH stake (2x return if valid; forfeit if invalid)
- Reputation range: 1-100 (capped; no overflow risk)

**Challenge resolution phases**:
- Phase 1 (current): 3 trusted multi-sig resolvers (2-of-3 quorum)
- Phase 2 (6-12mo): Interactive bisection game (MACI-style)
- Phase 3 (long-term): ZK proofs for reaction verification

## Budget & Team Structure

### Total: ₳200,000 (6 months)
- **Personnel (70%)**: ₳140k
  - Lead Architect (Cardano/Haskell/Plutus): ₳50k
  - Monad/EVM Engineer (Solidity, gas optimization): ₳40k
  - Solana/Rust Developer (Anchor, cross-chain): ₳30k
  - Filecoin/FVM Specialist (IPFS, deal automation): ₳20k

- **Infrastructure (15%)**: ₳30k
  - Multi-chain testnet operations, bridge infrastructure, IPFS/Filecoin storage

- **Security & Audits (10%)**: ₳20k
  - Smart contract audits (4 chains), penetration testing, bug bounty

- **Operations (5%)**: ₳10k
  - Documentation, community outreach, legal compliance

## Development Phases (If Funded)

### Month 1-2: Multi-Chain Architecture
- Deploy Cardano testnet validators (Plutus v2/v3)
- Deploy Monad testnet contracts (PoXRegistry, ChallengeManager, Reputation)
- Deploy Solana devnet programs (reputation_program, payment_escrow)
- Deploy Filecoin Calibration contracts (CID registry)
- Implement cross-chain bridges (Axelar/Wormhole)

### Month 3-4: Core Features
- Type-safe Haskell library (≥50 reactions, ≥3 reaction classes)
- Multi-sig challenge resolution system (3 resolvers, voting tested)
- Batch registration CLI for lab data (ORD integration prototype)
- Cross-chain bridge functional (Monad → Cardano testnet)

### Month 5-6: Testing & Deployment
- Security audit (all chains)
- Mainnet deployments (Cardano, Solana, Filecoin; Monad if launched, else Polygon zkEVM)
- Pilot program (≥5 labs, ≥100 experiments)
- Documentation, tutorials, community governance activation

## Important Context from Related Work

### ETHGlobal SF 2024 Demo
- **Achievement**: Polygon "Best Consumer App" Runner-Up
- **Demo link**: https://ethglobal.com/showcase/chimiadao-xsmjg
- **Components**: Experiment submission UI, Subgraph for ORD indexing, reputation prototype, basic challenge mechanism
- **GitHub**: https://github.com/xchemtina/chimiadao-ethglobal-subgraph

### PoX Security Audit Lessons
Referenced in THOUGHTS.md, key insights:
- **Reputation cap (1-100)**: Prevents integer overflow without complex checks
- **Economic alignment over cryptographic complexity**: Stakes + reputation more practical than commit-reveal schemes for early adoption
- **Input validation hierarchy**: Fail fast on format/economic checks; warn (don't revert) on stoichiometry; resolve chemical validity via multi-sig

### Open Reaction Database (ORD) Integration
- **Data source**: 4M+ reactions in protobuf format
- **Challenge**: Migrate ORD data to ChimiaDAO (parse protobuf → Haskell ADT → IPFS CIDs)
- **Open questions**: Who pays for Filecoin storage? Should ORD data skip challenge period (pre-verified)?
- **Links**: 
  - Schema: https://github.com/open-reaction-database/ord-schema
  - Data: https://github.com/open-reaction-database/ord-data

### Haskell Chemistry ADT Inspiration
- **Reference**: Oliver J. Goldstein's work on type-safe molecular representation
- **GitHub**: https://github.com/oliverjgoldstein
- **Key insight**: Molecules as typed structures (not strings) enable formal verification and stoichiometry checking at compile-time

## Working with This Repository

### Common Tasks

#### Update proposal documentation
```bash
# Edit main proposal
open README.md  # or your preferred editor

# Update technical architecture
open THOUGHTS.md

# Revise sprint plan
open NEXT_STEPS.md
```

#### Generate diagrams for proposal
The repository currently uses ASCII art in README.md. To create professional diagrams:
- Use Figma, Excalidraw, or Mermaid for visual diagrams
- Save to `docs/diagrams/` (create directory if needed)
- Update README.md with image references

Suggested diagrams (from NEXT_STEPS.md):
1. Multi-chain architecture (Cardano/Monad/Solana/Filecoin relationships)
2. Data flow (experiment submission → challenge → reward)
3. Challenge resolution phases (multi-sig → bisection → ZK)

#### Prepare for Catalyst submission
Follow the 3-day sprint in NEXT_STEPS.md:
- **Day 1**: Technical validation (CNS research, bridge research, team structure)
- **Day 2**: Proposal drafting (Ideascale template, competitor analysis, visuals)
- **Day 3**: Finalization (peer review, GitHub cleanup, submission)

### Key References

**Cardano Ecosystem**:
- Catalyst Forum: https://forum.cardano.org/
- Plutus Docs: https://plutus.readthedocs.io/
- CIP-68 (Datum Metadata Standard): Reference for experiment NFT structure

**Multi-Chain Bridges**:
- Wormhole: https://wormhole.com/ (Cardano ↔ Solana ↔ Monad)
- Axelar: Alternative cross-chain messaging protocol

**Partner Organizations**:
- Bio-Block: https://www.bio-block.org/ (advisory board)
- Open Reaction Database: https://github.com/open-reaction-database

**Previous Work**:
- Etica.io: https://github.com/etica (on-chain research ecosystem)

## Risk Mitigation Strategies

### Technical Risks
- **Monad mainnet delay**: Fallback to Polygon zkEVM (ZK proofs align with long-term vision)
- **Bridge failure**: Manual state sync via governance (slow but functional)
- **Haskell ADT complexity**: Start simple (C, H, O, N only), expand iteratively

### Adoption Risks
- **Lab reluctance**: Partner with open-science advocates (Bio-Block, ORD community)
- **UX friction**: Abstract wallet complexity (ENS/CNS, social login via Privy/Magic Link)
- **Data quality**: Bootstrap with peer-reviewed ORD data (pre-verified)

### Economic Risks
- **Challenge spam**: Adjust stake amounts via governance
- **Reputation inflation**: Dynamic update frequency (tune via on-chain parameters)
- **Cross-chain price volatility**: Batch ADA rewards monthly (reduce bridge costs)

## Philosophy & Principles

### "Fortis est Veritas" (Truth is Strong)
**Core premise**: Truth is strengthened by multiple, independent verification layers. A single-chain approach is a single point of failure for scientific consensus.

**Applied to architecture**:
- Multiple chains provide redundant verification (Cardano formal verification + Monad speed + Solana cost efficiency)
- Economic incentives (stakes, reputation) more practical than pure cryptographic complexity for early adoption
- Progressive decentralization (multi-sig → bisection → ZK) balances pragmatism with long-term trustlessness

### Security Philosophy
From PoX audit lessons:
- **Don't assume fragility**: Systems that over-engineer for fragility become brittle (e.g., unnecessary overflow checks for capped reputation)
- **Fail fast and explicitly**: Input validation hierarchy (format → economic → stoichiometric → peer review)
- **Economic mechanism design scales better than cryptographic complexity**: Stakes + reputation for Phase 1; ZK proofs for Phase 3 (not Phase 1)

### Data Architecture
**"Bulk Data Off-Chain" Principle**:
- Store only CIDs on-chain (not full reaction data; gas costs too high)
- IPFS ensures data integrity via content addressing (mutation = new CID)
- Subgraph indexes CIDs for queryability (full-text search on off-chain metadata)

## Success Metrics (6-Month Targets)

### Technical Milestones
- 50+ reactions in Haskell ADT library (≥3 reaction classes)
- 1 full retrosynthesis pathway (multi-step synthesis)
- Deployed contracts on 4 chains (all mainnets except Monad if delayed)
- Cross-chain bridge functional (Monad → Cardano, Cardano → Solana)
- Security audit completed

### Adoption Metrics
- 5+ pilot labs registered (physical chemistry labs)
- 100+ experiments submitted (historical + new data)
- 10+ challenges resolved (test economic incentives)
- 1,000+ CIDs stored on Filecoin (ORD data migration)

### Community Metrics
- 50+ GitHub stars
- 10+ external contributors (code, docs, data)
- 5+ academic citations (preprints/conference papers)
- 1,000+ Discord members

## Notes for Future Development

### When Transitioning to Implementation
1. **Create monorepo structure** (turborepo recommended):
   ```
   ChimiaDAO-PoX/
   ├── apps/
   │   ├── web/              # Next.js frontend
   │   ├── indexer/          # SQD indexer service
   │   └── analysis-api/     # AI analysis microservice
   ├── packages/
   │   ├── contracts/        # Solidity contracts + tests
   │   ├── shared-types/     # TypeScript type definitions
   │   ├── experiment-parser/# Universal data parsers
   │   └── ui-components/    # Shared React components
   ├── services/
   │   ├── bittensor-subnet/ # BT subnet integration (future)
   │   └── ipfs-gateway/     # Dedicated IPFS node
   └── docs/
   ```

2. **Smart contract repositories** (separate per chain):
   - `chimiadao-cardano`: Plutus validators, CIP-68 implementation
   - `chimiadao-monad`: Solidity contracts (PoXRegistry, ChallengeManager, Reputation)
   - `chimiadao-solana`: Anchor programs (reputation_program, payment_escrow, bridge_sync)
   - `chimiadao-filecoin`: FVM contracts (CID registry, deal automation)

3. **Haskell ADT library** (standalone package):
   - `chimia-core`: Type-safe molecular representation, reaction validation
   - Compile targets: Plutus, Rust (via FFI), Solidity (via codegen)

### Testing Strategy
- **Unit tests**: Haskell (HSpec), Solidity (Forge), Rust (cargo test), TypeScript (Jest)
- **Integration tests**: Cross-chain message passing (Wormhole testnet)
- **E2E tests**: Full experiment submission → challenge → reward flow
- **Security**: Slither, Mythril (Solidity), property-based testing (QuickCheck for Haskell)

### CI/CD Pipeline (Future)
```yaml
# .github/workflows/deploy.yml
on:
  push:
    branches: [main]
jobs:
  test:
    - run: pnpm test           # TypeScript/React
    - run: forge test          # Solidity
    - run: anchor test         # Solana
    - run: cabal test          # Haskell
  deploy-contracts:
    - run: forge script Deploy --broadcast  # Monad
    - run: anchor deploy                    # Solana
    - run: cardano-cli ...                  # Cardano (TBD)
  deploy-frontend:
    - run: vercel deploy --prod
```

## Contact & Community

- **Proposal**: Cardano Catalyst Fund 15 submission (Ideascale link TBD)
- **GitHub**: https://github.com/chimiadao (organization TBD)
- **Website**: https://chimiadao.github.io/catalyst-usecase-concept/ (GitHub Pages)
- **Discord**: [Community server to be created]
- **Twitter**: [@ChimiaDAO TBD]

---

**Last Updated**: 2024-11-25 (pre-Catalyst submission)

**Status**: Concept/Planning Phase → Awaiting Fund 15 approval

**License**: Apache-2.0 (core libraries, smart contracts), CC BY-SA 4.0 (documentation)
