# ChimiaDAO: On-Chain Verifiable Chemistry for Cardano

[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](LICENSE)
[![Catalyst Fund 15](https://img.shields.io/badge/Catalyst-Fund%2015-red)](https://projectcatalyst.io/)
[![ETHGlobal SF](https://img.shields.io/badge/ETHGlobal%20SF-Runner--Up-orange)](https://ethglobal.com/showcase/chimiadao-xsmjg)

## Vision

**ChimiaDAO** is building decentralized scientific infrastructure for verifiable chemistry on blockchain. We're creating a **multi-chain protocol** anchored on Cardano that enables chemists, researchers, and laboratories worldwide to register, verify, and monetize chemical synthesis data with cryptographic proof and reputation-based validation.

### The Core Problem

Chemistry powers a **$5 trillion industry**, yet:
- **Data is siloed**: Proprietary databases lock away crucial synthesis knowledge
- **Verification is centralized**: Reproducibility relies on trusted intermediaries
- **Representation is broken**: SMILES/SELFIES notation is insufficient for formal verification
- **Collaboration is limited**: No global, permissionless ledger for chemical knowledge

## Architecture Overview

### Multi-Chain Strategy

ChimiaDAO operates across **three blockchain ecosystems**, each serving a specific role:

#### 🔷 **Cardano (Primary Layer)**
- **Role**: Governance, reputation registry, and formal verification anchor
- **Why**: Haskell foundation enables type-safe molecular representation; eUTXO model ideal for deterministic validation
- **Components**:
  - Plutus validators for synthesis step verification
  - CIP-68 metadata standard for chemical structures
  - Native token rewards for verified contributions
  - On-chain governance for protocol parameters

#### ⚡ **Monad (High-Performance Layer)**
- **Role**: Experiment submission, challenge resolution, reputation system, and payments
- **Why**: 10,000+ TPS for instant feedback; parallel execution for complex validation; EVM compatibility simplifies architecture
- **Components**:
  - Rapid experiment registration (sub-second finality)
  - Multi-signature challenge resolution system
  - Real-time reputation scoring (every ~20 experiments)
  - Gas-optimized batch operations for lab data ingestion
  - Payment escrow and reward distribution
  - EVM compatibility for cross-chain bridges

#### 📦 **Filecoin (Storage Layer)**
- **Role**: Permanent, verifiable data storage
- **Why**: Content-addressed storage ensures data integrity; retrieval market for decentralized access
- **Components**:
  - IPFS/Filecoin CID registry (primary data store)
  - Deal verification and renewal automation
  - Integration with ORD (Open Reaction Database) schemas
  - Long-term preservation (5+ years minimum)

### Cross-Chain Communication

```
┌──────────────────────────────────────────────────────────┐
│              CARDANO (Governance Layer)              │
│  • Plutus validators (formal synthesis verification)  │
│  • Reputation registry (canonical truth)             │
│  • Token economics (₳DA rewards)                     │
│  • CIP-68 NFTs (experiment metadata)                 │
└────────────┬───────────────────────────┬─────────────┘
             │                           │
    ┌────────┴────────┐           ┌───────┴────────┐
    │  MONAD (Speed)  │           │ FILECOIN (Data)│
    │  • Experiments  │◄─────────►│  • IPFS CIDs   │
    │  • Challenges   │  CID sync │  • ORD schemas │
    │  • Reputation   │           │  • Long-term   │
    │  • Payments     │           │    storage     │
    └─────────────────┘           └─────────────────┘
         (10k+ TPS)                 (Content-addressed)
```

## Technical Foundation

### 1. Type-Safe Molecular Representation (Haskell)

Inspired by [Oliver J. Goldstein's work](https://github.com/oliverjgoldstein), we're building an algebraic data type (ADT) system that:

- **Formally verifiable**: Molecules as typed structures (no string parsing)
- **Composable**: Reactions as transformations between types
- **Stoichiometry-aware**: Balance checking at compile time
- **Extensible**: Support for catalysts, conditions, intermediates

**Example ADT structure** (conceptual):

```haskell
data Atom = H | C | N | O | F | ...
  deriving (Eq, Show, Enum, Bounded)

data Bond = Single | Double | Triple | Aromatic
  deriving (Eq, Show)

data Molecule = Molecule
  { atoms :: [(Atom, Charge)]
  , bonds :: [(AtomIndex, AtomIndex, Bond)]
  , geometry :: Maybe SpatialConfig
  } deriving (Eq, Show)

data Reaction = Reaction
  { reactants :: [Molecule]
  , products :: [Molecule]
  , catalyst :: Maybe Molecule
  , conditions :: ReactionConditions
  , mechanism :: Maybe MechanismPath
  } deriving (Eq, Show)

-- Type-level balance verification
checkStoichiometry :: Reaction -> Either BalanceError ()
```

### 2. Plutus Validators (Cardano Layer)

On-chain validation logic for synthesis steps:

```haskell
-- Conceptual validator structure
validateSynthesisStep :: SynthesisStep -> ScriptContext -> Bool
validateSynthesisStep step ctx =
  -- 1. Verify CID format (IPFS/Filecoin)
  validCID (stepDataHash step)
  -- 2. Check stoichiometric balance
  && balancedReaction (stepReaction step)
  -- 3. Verify submitter reputation threshold
  && sufficientReputation (stepSubmitter step)
  -- 4. Validate stake attached
  && hasRequiredStake ctx
  -- 5. Check challenge period not expired
  && withinChallengePeriod (stepTimestamp step) ctx
```

**CIP-68 Metadata Standard**:
- Reference NFT: immutable experiment metadata
- User token: transferable proof of contribution
- On-chain state: reputation score, challenge history

### 3. Multi-Chain Contract Suite

#### Monad (High-Speed Validation)
- **PoXRegistry.sol**: Experiment registration with gas-optimized storage
- **ChallengeManager.sol**: Multi-sig resolution with 15-day timeout
- **Reputation.sol**: Dynamic scoring with timelock authorization
- **PaymentEscrow.sol**: Reward distribution and stake management

#### Filecoin (Data Layer)
- **CID Registry Contract**: Map experiment IDs to IPFS CIDs
- **Deal Renewal Automation**: Perpetual storage via FVM
- **ORD Integration**: Bulk import from Open Reaction Database

## Security & Economics

### Stake-Based Spam Prevention

| Action | Required Stake | Outcome |
|--------|---------------|---------|
| **Register Experiment** | 0.01 ETH (Monad) | Refunded after challenge period |
| **Submit Challenge** | 0.01 ETH (Monad) | 2x return if valid; forfeit if invalid |
| **Batch Registration** | 0.001 ETH per exp | Discounted for labs with bulk data |

### Reputation System

- **Range**: 1-100 (capped; no overflow)
- **Update Frequency**: Every ~20 experiments
- **Decay**: None (experiments gain value when useful)
- **Challenge Impact**:
  - Valid challenge: -10 reputation to submitter
  - Invalid challenge: -5 reputation to challenger
- **Rewards**: Reputation points paid by ChimiaDAO for verified contributions

### Challenge Resolution (Multi-Sig → Bisection → ZK)

**Phase 1 (Current)**: 
- 3 trusted resolvers (2-of-3 quorum)
- 15-day challenge period
- Auto-resolve as invalid if timeout

**Phase 2 (6-12 months)**:
- Interactive bisection game (MACI-style)
- On-chain dispute resolution
- Reduced trust assumptions

**Phase 3 (Long-term)**:
- ZK proofs for reaction verification
- Fully trustless validation
- Integration with verifiable computation platforms

## Roadmap

### ✅ **Phase 0: Foundation (Completed)**
- ETHGlobal SF 2024 demo (Polygon)
- Subgraph prototype for ORD data ingestion
- Initial Solidity contracts (PoXRegistry, Reputation)

### 🔄 **Phase 1: Catalyst Fund 15 (6 months, ₳190,000)**

**Month 1-2: Multi-Chain Architecture**
- Deploy Cardano testnet validators (Plutus)
- Deploy Monad testnet contracts (experiments, challenges, reputation, payments)
- Deploy Filecoin Calibration contracts (CID registry)
- Implement cross-chain bridges (Wormhole)

**Month 3-4: Core Features**
- Type-safe Haskell library (≥50 reactions, ≥3 classes)
- Multi-sig challenge resolution system
- Batch registration for lab data (decades of backlog)
- ENS/Cardano Name Service integration
- Payment/reward distribution (cross-chain)

**Month 5-6: Testing & Deployment**
- Security audit (all chains)
- Mainnet deployments (Cardano → Monad → Filecoin)
- Pilot program with ≥5 physical labs
- Documentation & tutorials
- Community governance activation

### 🚀 **Phase 2: Ecosystem Growth (12 months)**
- ZK proof integration (reaction verification)
- DAO treasury management (multi-chain)
- Academic partnerships (universities, research institutes)
- Industry adoption (pharma, materials science)
- Grant program for builders (₳DA/MONA/FIL rewards)

## Budget (₳190,000)

### Personnel (₳110,000 / 58%)
- **Lead Architect** (Cardano/Haskell specialist): ₳50,000
- **Monad/EVM Engineer** (Solidity, gas optimization, reputation & payments): ₳40,000
- **Filecoin Integration Specialist** (FVM, IPFS, ORD migration): ₳20,000

### Infrastructure (₳40,000 / 21%)
- Multi-chain testnet operations (Cardano, Monad, Filecoin): ₳10,000
- Bridge infrastructure (Wormhole): ₳15,000
- IPFS/Filecoin storage deals (5-year commitment): ₳10,000
- Monitoring & alerting: ₳5,000

### Security & Audits (₳25,000 / 13%)
- Smart contract audits (3 chains: Cardano, Monad, Filecoin): ₳18,000
- Penetration testing: ₳4,000
- Bug bounty program: ₳3,000

### Operations (₳15,000 / 8%)
- Documentation & technical writing: ₳6,000
- Community outreach & partnerships: ₳4,000
- Legal (multi-jurisdiction): ₳3,000
- Contingency reserve: ₳2,000

## Why Cardano as Primary Chain?

### 1. **Formal Methods Alignment**
- Haskell → Plutus: seamless type-safe pipeline
- Chemistry requires formal verification (stoichiometry, reaction balance)
- eUTXO model: deterministic validation (no nonce issues)

### 2. **DeSci Flagship Opportunity**
- Cardano lacks a major decentralized science primitive
- First-mover advantage in on-chain chemistry
- Natural fit for academic/research community

### 3. **Long-Term Sustainability**
- Staking rewards fund ongoing development
- Treasury system for community grants
- Peer-reviewed protocol upgrades (no contentious forks)

### 4. **Economic Efficiency**
- Native tokens (no ERC-20 gas overhead)
- Predictable transaction costs
- Energy-efficient consensus (critical for scientific community)

## Proof of Capability

### ETHGlobal San Francisco 2024
- **Achievement**: Polygon "Best Consumer App" Runner-Up
- **Demo**: https://ethglobal.com/showcase/chimiadao-xsmjg
- **Highlights**:
  - Functional experiment submission UI
  - Subgraph for ORD data indexing
  - Reputation system prototype
  - Challenge mechanism (basic)

### Open Source Contributions
- **Subgraph**: https://github.com/xchemtina/chimiadao-ethglobal-subgraph
- **ORD Integration**: Automated ingestion from Open Reaction Database
- **Etica.io**: On-chain research ecosystem (prior work)

### Academic Grounding
- Inspired by Oliver J. Goldstein's work on molecular representation
- Collaboration with Bio-Block advisory board
- Integration with ORD schema standards

## Get Involved

### For Chemists & Researchers
1. **Pilot Program**: Register your lab for early access
2. **Data Contribution**: Upload historical synthesis data (earn reputation)
3. **Validation**: Participate in challenge resolution as a trusted expert

### For Developers
1. **Smart Contracts**: Contribute to Plutus validators or EVM contracts
2. **Haskell Library**: Extend molecular ADT system
3. **Cross-Chain Bridges**: Improve multi-chain synchronization

### For Investors & Supporters
1. **Catalyst Fund 15**: Vote for this proposal (₳200k)
2. **Direct Grants**: Support via Cardano Treasury
3. **Partnerships**: Pharma, biotech, academic institutions

## Contact & Links

- **Website**: https://chimiadao.github.io/catalyst-usecase-concept/
- **GitHub**: https://github.com/chimiadao
- **Proposal**: [Catalyst Fund 15 Submission](#) *(link pending)*
- **Email**: [contact info TBD]
- **Discord**: [community server TBD]

## License

- **Core Libraries**: Apache-2.0 (Haskell ADT, Plutus validators)
- **Smart Contracts**: Apache-2.0 (Monad, Solana, Filecoin)
- **Demo UI**: Apache-2.0
- **Documentation**: CC BY-SA 4.0

---

**Built with ❤️ for the scientific community. Fortis est Veritas.**
