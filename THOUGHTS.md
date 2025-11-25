# THOUGHTS: ChimiaDAO Multi-Chain Architecture

## Philosophical Foundation

### Why Multi-Chain? (The Recursive Argument)

The question isn't "Why use multiple chains?" but rather **"Why would a single chain suffice for verifiable chemistry?"**

Each blockchain optimizes for specific tradeoffs:
- **Cardano**: Formal verification at the cost of throughput
- **Monad**: High-speed execution at the cost of decentralization (early stage)
- **Solana**: Low fees at the cost of validator hardware requirements
- **Filecoin**: Permanent storage at the cost of retrieval complexity

Chemistry itself is multi-layered:
- **Molecular structure** (static, formal) → **Cardano** (Plutus validators)
- **Reaction kinetics** (dynamic, high-frequency) → **Monad/Solana** (real-time updates)
- **Experimental data** (large, permanent) → **Filecoin** (IPFS/FVM)

**Fortis est Veritas**: Truth is strengthened by multiple, independent verification layers. A single chain is a single point of failure for scientific consensus.

---

## Architectural Decisions (& Their Rationale)

### 1. Cardano as Governance Anchor

**Decision**: Make Cardano the "source of truth" for reputation and high-value experiments.

**Reasoning**:
- **Haskell → Plutus pipeline**: Type-safe molecular ADTs in Haskell compile directly to Plutus validators
- **eUTXO determinism**: Reactions are state transitions; eUTXO models them naturally (no nonce footguns)
- **Academic alignment**: Cardano's peer-reviewed ethos matches scientific culture
- **Long-term stability**: No VC-driven pivot risk; treasury-funded sustainability

**Tradeoffs**:
- ❌ Slower finality (~20 seconds vs <1 second on Monad)
- ❌ Lower throughput (~250 TPS theoretical vs 10,000+ on Monad)
- ✅ Formal verification tooling (Plutus → Coq/Agda proofs feasible)
- ✅ No gas wars (predictable fees critical for labs)

**Implementation**:
- CIP-68 for experiment metadata (reference NFT + user token)
- Native tokens for reputation (no ERC-20 gas overhead)
- Plutus validator for stoichiometry checking (inspired by [Goldstein's ADT work](https://github.com/oliverjgoldstein))

**Open Questions**:
- Can we compile Haskell ADTs to *both* Plutus and Rust/Anchor for cross-chain consistency?
- What's the latency for Cardano → Monad bridge messages? (Axelar vs. Wormhole benchmarks needed)

---

### 2. Monad for Challenge Resolution

**Decision**: Use Monad (not Polygon/Optimism/Arbitrum) for high-speed experiment submission and challenge workflows.

**Reasoning**:
- **MonadBFT consensus**: 1-second block time; sub-second finality
- **Parallel EVM execution**: Challenges can be resolved concurrently (no global state lock)
- **Native MEV resistance**: Timeboost auction reduces front-running risk (critical for challenge stakes)
- **EVM compatibility**: Reuse existing Solidity contracts from ETHGlobal prototype

**Tradeoffs**:
- ❌ Monad is pre-mainnet (testnet only as of Nov 2024)
- ❌ Centralization risk (fewer validators initially)
- ✅ 10,000+ TPS (handles lab batch uploads without congestion)
- ✅ Gas optimizations from Polygon demo transfer directly

**Implementation**:
- **PoXRegistry.sol**: Packed storage (26% gas savings via uint32 difficulty)
- **ChallengeManager.sol**: Multi-sig resolution (3 resolvers, 2-of-3 quorum)
- **ReentrancyGuard**: Protect payment functions (learned from PoX audit)

**Open Questions**:
- What's Monad's mainnet launch timeline? (Fallback: deploy to Polygon PoS if delayed)
- Can we use Monad's native account abstraction for ENS-like naming? (Avoid ENS integration complexity)

---

### 3. Solana for Reputation Microtransactions

**Decision**: Use Solana (not Cardano native tokens) for incremental reputation updates.

**Reasoning**:
- **400ms slot time**: Real-time feedback for chemists (vs 20s on Cardano)
- **$0.00025 per transaction**: Update reputation every ~20 experiments without budget blowout
- **Cross-program invocations**: Atomic updates across reputation/payment/bridge programs
- **Solana Pay integration**: Instant micropayments for data contributors

**Tradeoffs**:
- ❌ Validator hardware requirements (centralization pressure)
- ❌ Occasional congestion (2023 NFT mints, 2024 meme coin spam)
- ✅ Low latency critical for user experience (chemists expect instant feedback)
- ✅ Anchor framework maturity (battle-tested for DeFi)

**Implementation**:
- **reputation_program**: Rust/Anchor program with PDA-based state
- **bridge_sync**: Cross-chain state roots every 100 updates (Merkle proofs to Cardano)
- **Solana Pay**: QR code payments for labs without wallet setup

**Open Questions**:
- How to handle Solana congestion? (Priority fees? Transaction retries?)
- Can we use Solana's stake-weighted QoS for lab accounts? (Guarantee throughput)

---

### 4. Filecoin for Data Permanence

**Decision**: Use Filecoin (not Arweave/Ceramic) for experiment data storage.

**Reasoning**:
- **Content addressing**: CIDs (IPFS hashes) ensure data integrity (mutation = new CID)
- **Retrieval market**: Decentralized data access (no single gateway dependency)
- **FVM smart contracts**: On-chain deal renewal automation (no manual uploads)
- **ORD compatibility**: Open Reaction Database uses IPFS; direct integration path

**Tradeoffs**:
- ❌ Retrieval latency (seconds vs milliseconds on centralized CDN)
- ❌ Deal complexity (storage providers, collateral, proofs)
- ✅ Provable permanence (cryptoeconomic guarantees vs. Arweave's "probably forever")
- ✅ No platform risk (permissionless storage, no API key revocation)

**Implementation**:
- **CID Registry Contract**: Map experiment IDs to IPFS CIDs (FVM Solidity)
- **Deal Renewal Bot**: Off-chain service monitors deal expiry, renews via FVM
- **ORD Ingestion Pipeline**: Batch import from ord-data repo (millions of reactions)

**Open Questions**:
- What's the optimal deal duration? (90 days? 1 year? 5 years?)
- Can we use Filecoin Plus (DataCap) for subsidized storage? (Require governance vote?)

---

## Security Philosophy (Learned from PoX Audit)

### The "Fortis est Veritas" Principle

**Truth is not fragile. Systems that assume fragility introduce brittleness.**

From the PoX security review (Warp Notebook context), we identified:
- ❌ **Assumed fragility**: Overflow checks for reputation (billions) → brittle max values
- ✅ **Enforced constraints**: Cap reputation 1-100 → natural bounds, no overflow
- ❌ **Assumed fragility**: Front-running protection (commit-reveal) → UX friction
- ✅ **Economic alignment**: Stake requirements → incentive-compatible honesty

**Applied to ChimiaDAO**:
1. **Reputation cap** (1-100): No integer overflow, natural decay resistance
2. **Challenge stakes** (0.01 ETH): Economic spam filter, not cryptographic defense
3. **Multi-sig resolution** (Phase 1): Honest resolvers vs. adversarial proof games (Phase 2)
4. **Timelock authorization** (15 days): Prevent rug-pulls, not front-running

**Key Insight**: 
Security through **economic mechanism design** (stakes, reputation, multi-sig) scales better than cryptographic complexity (commit-reveal, ZK proofs) for early-stage adoption. ZK is Phase 3, not Phase 1.

---

### Input Validation Hierarchy

From the PoX audit, we learned **validation should fail fast and explicitly**:

```
1. Format validation (CID, address, uint ranges)
   ↓ FAIL: Reject transaction
2. Economic validation (stake, reputation threshold)
   ↓ FAIL: Reject transaction
3. Stoichiometric validation (Plutus/Haskell)
   ↓ FAIL: Emit warning event (don't block submission)
4. Chemical validity (peer review, challenge)
   ↓ FAIL: Resolve via multi-sig (Phase 1) or bisection (Phase 2)
```

**Why warnings, not reverts, for stoichiometry?**
- Chemistry is exploratory; novel reactions may appear unbalanced
- False negatives (rejecting valid work) worse than false positives (accepting questionable work)
- Challenge mechanism is the final arbiter, not the validator

**Implementation**:
- **Level 1-2**: Solidity `require()` statements (cheap, deterministic)
- **Level 3**: Plutus `traceError` or Solidity `emit Warning()` (log, don't revert)
- **Level 4**: Off-chain resolution → on-chain state update (multi-sig)

---

## Economic Model (Incentive Alignment)

### The Three Actors Problem

**Goal**: Align incentives for **submitters**, **challengers**, and **resolvers**.

#### Actor 1: Data Submitters (Chemists, Labs)
- **Motivation**: Recognition, monetization, open science ideals
- **Incentive**: Reputation points → governance weight → treasury grants
- **Risk**: Invalid experiments reduce reputation (social + economic loss)

#### Actor 2: Challengers (Peer Reviewers)
- **Motivation**: Maintain data quality, earn reputation
- **Incentive**: 2x stake return for valid challenges
- **Risk**: Invalid challenges lose stake + reputation penalty

#### Actor 3: Resolvers (Multi-Sig, then DAO)
- **Motivation**: Protocol health, long-term alignment
- **Incentive**: Governance tokens (vested), protocol fees (if implemented)
- **Risk**: Reputation loss for incorrect resolutions (slashing in Phase 2)

**Current Parameters** (subject to governance adjustment):
- **Challenge stake**: 0.01 ETH (~$20 at $2k ETH)
- **Challenge period**: 15 days (academic peer review timescale)
- **Reputation penalty**: -10 for invalid experiment, -5 for invalid challenge
- **Reward multiplier**: 2x stake for valid challengers

**Open Questions**:
- Should resolvers earn fees? (E.g., 10% of challenge stakes)
- Should reputation decay over time? (Current: no decay; experiments gain value)
- Should there be a reputation *floor*? (E.g., can't go below 10?)

---

### Payment Flows (Cross-Chain Complexity)

**Challenge**: Chemists submit on Monad, but want ADA rewards on Cardano.

**Solution**: Multi-chain escrow with bridge synchronization.

**Flow 1: Experiment Submission**
```
1. Submit experiment on Monad (fast UX)
   ↓
2. Emit event with experiment hash
   ↓
3. Bridge relayer posts CID to Filecoin
   ↓
4. After 15-day challenge period, trigger reward:
   a) Monad → Solana: Update reputation (fast)
   b) Solana → Cardano: Mint ADA reward (monthly batch)
```

**Flow 2: Challenge Resolution**
```
1. Challenge submitted on Monad (stake locked)
   ↓
2. Multi-sig resolvers vote (off-chain aggregation)
   ↓
3. Resolution posted on-chain:
   a) Valid: Redistribute stake (Monad), update reputation (Solana)
   b) Invalid: Refund stake, penalize challenger (reputation on Solana)
```

**Why not all on Cardano?**
- Latency: 20s finality unacceptable for real-time challenge submissions
- Cost: Cardano fees ~0.17 ADA (~$0.10); Solana ~$0.0003 (350x cheaper for micro-updates)

**Why not all on Monad?**
- Decentralization: Monad is early-stage; Cardano is proven
- Governance: ADA staking for DAO voting is native; ERC-20 staking is friction

---

## Data Architecture (IPFS + On-Chain Indexing)

### The "Bulk Data Off-Chain" Principle

**From PoX audit**: Don't store reaction data on-chain (gas explosion).

**Solution**: IPFS CIDs on-chain, reaction data off-chain.

**Example Experiment Structure**:
```json
{
  "experimentId": 12345,
  "submitter": "0xABC...789",
  "timestamp": 1699920000,
  "difficulty": 42,  // 1-100 scale
  "reputationStake": 85,  // Submitter's reputation at time of submission
  "dataCID": "bafybeigdyrzt5sfp7udm7hu76uh7y26nf3efuylqabf3oclgtqy55fbzdi",
  "challengeIds": [67, 89],  // References to Challenge structs
  "validationStatus": "unchallenged"  // unchallenged | challenged | validated | invalidated
}
```

**IPFS CID Points To** (off-chain):
```json
{
  "version": "ORD-2.0",
  "reaction": {
    "reactants": [...],  // Molecular structures (SMILES, InChI, or our Haskell ADT)
    "products": [...],
    "conditions": {...},
    "catalyst": {...}
  },
  "metadata": {
    "lab": "MIT Chemistry Dept",
    "researcher": "Alice Smith",
    "date": "2024-03-15",
    "notes": "Room temperature, 2 hours"
  },
  "rawData": {
    "nmr": "ipfs://...",  // Link to NMR spectra
    "gcms": "ipfs://...",  // Link to GC-MS data
    "photos": ["ipfs://...", "ipfs://..."]
  }
}
```

**Why not store reactants/products on-chain?**
- Gas cost: ~20,000 gas per SSTORE (reactants array would cost 100k+ gas)
- Immutability: IPFS CID already guarantees data integrity
- Queryability: Subgraph indexes CIDs, allows full-text search of off-chain data

**On-Chain Index** (Subgraph):
- **Entities**: Experiment, Challenge, Reputation
- **Relationships**: Experiment → Challenges (1:many), Submitter → Experiments (1:many)
- **Full-Text Search**: On CID metadata (lab name, researcher, reaction type)

---

## Type-Safe Molecular Representation (The Core Innovation)

### Why SMILES/SELFIES Fails

**SMILES Example**: `C1=CC=CC=C1` (benzene)

**Problems**:
1. **String parsing**: Requires regex, error-prone
2. **No balance checking**: Can't verify stoichiometry at compile-time
3. **Ambiguity**: Multiple SMILES for same molecule (canonicalization needed)
4. **No formal semantics**: Can't prove properties (e.g., "this reaction conserves mass")

**Our Approach** (Haskell ADT → Plutus):

```haskell
-- Atom with oxidation state
data Atom = H | C | N | O | F | Cl | Br | I
  deriving (Eq, Show, Enum, Bounded)

data Charge = Neutral | Plus Int | Minus Int
  deriving (Eq, Show)

-- Molecule as graph (atoms = nodes, bonds = edges)
data Molecule = Molecule
  { atoms :: [(Atom, Charge)]
  , bonds :: [(Int, Int, BondType)]  -- (fromAtom, toAtom, bondType)
  } deriving (Eq, Show)

data BondType = Single | Double | Triple | Aromatic
  deriving (Eq, Show)

-- Reaction as transformation
data Reaction = Reaction
  { reactants :: [Molecule]
  , products :: [Molecule]
  , catalyst :: Maybe Molecule
  } deriving (Eq, Show)

-- Compile-time balance check
checkMassBalance :: Reaction -> Either String ()
checkMassBalance rxn =
  let reactantAtoms = concatMap (\(Molecule as _) -> map fst as) (reactants rxn)
      productAtoms = concatMap (\(Molecule as _) -> map fst as) (products rxn)
      reactantCounts = countAtoms reactantAtoms
      productCounts = countAtoms productAtoms
  in if reactantCounts == productCounts
     then Right ()
     else Left "Reaction not balanced"

countAtoms :: [Atom] -> Map Atom Int
countAtoms = foldr (\a -> Map.insertWith (+) a 1) Map.empty
```

**Key Properties**:
1. **Type-safe**: Can't construct invalid molecules (e.g., 5-bond carbon)
2. **Composable**: Reactions are functions `[Molecule] → [Molecule]`
3. **Provably correct**: Can derive formal proofs (via Coq/Agda)
4. **Plutus-compatible**: Compiles to on-chain validator logic

**Example Usage**:
```haskell
-- Define benzene
benzene :: Molecule
benzene = Molecule
  { atoms = replicate 6 (C, Neutral) ++ replicate 6 (H, Neutral)
  , bonds = [(0,1,Aromatic), (1,2,Aromatic), ..., (5,0,Aromatic)]
            ++ [(0,6,Single), (1,7,Single), ..., (5,11,Single)]
  }

-- Define hydrogenation reaction (benzene + H2 → cyclohexane)
hydrogenation :: Reaction
hydrogenation = Reaction
  { reactants = [benzene, h2, h2, h2]  -- 3 H2 molecules
  , products = [cyclohexane]
  , catalyst = Just palladiumOnCarbon
  }

-- Check balance (compile-time or run-time)
main = case checkMassBalance hydrogenation of
  Left err -> putStrLn $ "Error: " ++ err
  Right () -> putStrLn "Reaction balanced!"
```

**Why This Matters**:
- **Formal verification**: Can prove "all registered reactions are balanced" (Plutus validator)
- **Auditability**: Chemists can inspect ADT code (Haskell) vs. opaque SMILES parser
- **Extensibility**: Add stereochemistry, isotopes, etc. as new ADT constructors

**Challenge**: How to bridge Haskell ADT ↔ IPFS JSON?
- **Option 1**: Aeson JSON serialization (Haskell → JSON → IPFS)
- **Option 2**: Custom binary format (more compact, less human-readable)
- **Current plan**: JSON for MVP, binary for v2 (after ORD integration)

---

## Cross-Chain Bridge Strategy

### The "Loosely Coupled" Principle

**Insight**: Don't try to synchronize all state in real-time. Instead, propagate **state commitments** periodically.

**Architecture**:
```
MONAD (Source of Truth for Submissions)
  ↓ Every 100 experiments
  Merkle root posted to CARDANO
  ↓
CARDANO (Source of Truth for Reputation)
  ↓ Every 1000 reputation updates
  State root posted to SOLANA
  ↓
SOLANA (Source of Truth for Payments)
  ↓ On-demand
  Payment receipts verified on CARDANO (via Merkle proof)
```

**Bridge Choices**:
- **Monad ↔ Cardano**: Wormhole (supports Cardano natively)
- **Cardano ↔ Solana**: Wormhole or custom relayer (Wormhole preferable)
- **Solana ↔ Filecoin**: No direct bridge; CID registry is write-once (no sync needed)

**Security Model**:
- **Trust assumption**: Wormhole guardians (19 validators, 13/19 threshold)
- **Fallback**: If bridge fails, manually post state roots (governance override)
- **Long-term**: Implement custom ZK bridge (prove state transitions off-chain)

**Latency Budget**:
- **Monad → Cardano**: ~30 seconds (Wormhole finality)
- **Cardano → Solana**: ~30 seconds
- **End-to-end** (experiment submission → ADA reward): ~15 days (challenge period) + 30 seconds (bridge)

**Cost Estimate**:
- Wormhole fee: ~$5 per message (Monad → Cardano)
- Batch 100 experiments per message: $0.05 per experiment
- Alternative: Custom relayer (free, but requires infrastructure)

---

## Open Problems & Research Directions

### 1. Haskell ADT Compilation to Multiple Targets

**Problem**: We want type-safe molecular representation in Haskell, but need to deploy to:
- Plutus (Cardano validators)
- Rust (Solana programs)
- Solidity (Monad/EVM contracts)

**Current approach**: Maintain three implementations, manually sync.

**Better approach**: 
- Use **dhall** or **nix** for cross-language configuration (data structure definitions)
- Generate bindings via **fficxx** (Haskell FFI to C++ → Rust/Solidity)
- Or: Compile Haskell to WASM, run on all chains (future: WASM support on Cardano?)

**Open question**: Can we prove equivalence across implementations? (Formal verification via Coq)

---

### 2. Challenge Resolution Decentralization Roadmap

**Phase 1 (Current)**: 3 trusted multi-sig resolvers
- Who are the resolvers? (Advisors from Bio-Block, ORD contributors, academic chemists)
- How to remove/add resolvers? (Governance vote on Cardano)

**Phase 2 (6-12 months)**: Interactive bisection game
- **Protocol**: MACI-like dispute resolution (binary search for disagreement point)
- **Incentive**: Loser pays gas (economic penalty for frivolous challenges)
- **Challenge**: Requires off-chain computation (reaction simulation)

**Phase 3 (Long-term)**: ZK proofs for reaction correctness
- **Protocol**: Prover generates ZK-SNARK of stoichiometric balance
- **Verification**: On-chain verifier (constant cost, ~200k gas)
- **Challenge**: How to encode chemistry in ZK circuits? (Novel research)

**Open question**: Can we use **verifiable computation platforms** (e.g., Risc0, SP1) to accelerate Phase 3?

---

### 3. ORD (Open Reaction Database) Integration

**Status**: 4M+ reactions in ord-data repo (protobuf format)

**Challenge**: How to migrate existing ORD data to ChimiaDAO?

**Approach**:
1. **Batch ingestion**: Parse protobuf → Haskell ADT → IPFS CIDs
2. **Reputation bootstrapping**: Assign reputation to original researchers (opt-in)
3. **Governance vote**: Should ORD data be "pre-verified" (skip challenge period)?

**Open questions**:
- Who pays for Filecoin storage? (ChimiaDAO treasury? ORD consortium?)
- How to handle ORD updates? (New CID = new experiment? Or version history?)

---

## Catalyst Fund 15 Alignment

### Why This Deserves ₳200k (Not ₳100k)

**Original proposal**: ₳100k for Cardano-only prototype

**Revised scope** (multi-chain):
- **4 chains** (Cardano, Monad, Solana, Filecoin) vs. 1
- **Cross-chain bridges** (Wormhole integration) vs. none
- **3 smart contract languages** (Plutus, Solidity, Rust) vs. 1
- **4 developer roles** (Haskell, Solidity, Rust, FVM) vs. 1-2

**Budget justification**:
- **Personnel**: 4 specialists × ₳35k avg = ₳140k (70%)
- **Infrastructure**: Testnet ops, bridges, storage = ₳30k (15%)
- **Security**: Multi-chain audits = ₳20k (10%)
- **Operations**: Docs, outreach, legal = ₳10k (5%)

**Comparison to similar projects**:
- **Ocean Protocol** (data marketplace): $50M+ raised (centralized architecture)
- **VitaDAO** (longevity research): $4M+ treasury (no on-chain verification)
- **Molecule** (IP-NFTs): $20M+ valuation (no reproducibility layer)

**ChimiaDAO advantage**: 
- Open-source from day one (Apache-2.0)
- Multi-chain from genesis (no vendor lock-in)
- Formal verification foundation (Haskell ADT → Plutus)

---

## Success Metrics (6-Month Targets)

### Technical Milestones
- [ ] **50+ reactions** in Haskell ADT library (≥3 reaction classes)
- [ ] **1 full retrosynthesis pathway** (multi-step synthesis)
- [ ] **Deployed contracts** on 4 chains (Cardano mainnet, Monad testnet, Solana devnet, Filecoin mainnet)
- [ ] **Cross-chain bridge** functional (Monad → Cardano, Cardano → Solana)
- [ ] **Security audit** completed (all chains)

### Adoption Metrics
- [ ] **5+ pilot labs** registered (physical chemistry labs)
- [ ] **100+ experiments** submitted (mix of historical + new data)
- [ ] **10+ challenges** resolved (test multi-sig + economic incentives)
- [ ] **1,000+ CIDs** stored on Filecoin (ORD data migration)

### Community Metrics
- [ ] **50+ GitHub stars** (visibility threshold)
- [ ] **10+ external contributors** (code, docs, or data)
- [ ] **5+ academic citations** (preprint or conference papers)
- [ ] **1,000+ Discord members** (community engagement)

---

## Risk Mitigation

### Technical Risks
- **Monad mainnet delay**: Fallback to Polygon PoS (existing contracts)
- **Bridge failure**: Manual state sync via governance (slow but functional)
- **Haskell ADT complexity**: Start with simple molecules (C, H, O, N only)

### Economic Risks
- **Insufficient challenge activity**: Subsidize early challengers (treasury funds)
- **Reputation inflation**: Adjust update frequency (from ~20 to ~50 experiments)
- **Cross-chain price volatility**: Batch ADA rewards monthly (reduce bridge costs)

### Adoption Risks
- **Lab reluctance**: Partner with open-science advocates (Bio-Block, ORD)
- **UX friction**: Abstract wallet complexity (ENS, social login via Privy)
- **Data quality concerns**: Require peer-reviewed data for initial bootstrap (ORD only)

---

## Closing Reflection

**Why ChimiaDAO matters beyond DeSci**:

This isn't just about chemistry. It's about proving that **multi-chain coordination can unlock value** that no single chain can provide alone.

- **Cardano** gives us formal verification.
- **Monad** gives us speed.
- **Solana** gives us cost efficiency.
- **Filecoin** gives us permanence.

Each chain is a tool. The question is: **Are we building with the right tools?**

**Fortis est Veritas** — Truth is made stronger by the systems that verify it. A multi-chain scientific ledger is stronger than any single-chain alternative.

---

**Next**: See `NEXT_STEPS.md` for immediate action items (3-day sprint).
