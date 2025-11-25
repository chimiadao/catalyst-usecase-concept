# ChimiaDAO Architecture Diagrams

This directory contains architectural diagrams for the Catalyst Fund 15 proposal.

## Rendering Instructions

### Mermaid Diagrams
These `.mmd` files can be rendered using:

1. **GitHub** (native support): View directly in GitHub
2. **Mermaid Live Editor**: https://mermaid.live
3. **VS Code**: Install "Mermaid Preview" extension
4. **CLI**: `mmdc -i diagram.mmd -o diagram.png` (requires mermaid-cli)

### Convert to PNG for Proposal

```bash
# Install mermaid-cli
npm install -g @mermaid-js/mermaid-cli

# Generate PNGs
mmdc -i multi-chain-architecture.mmd -o multi-chain-architecture.png -w 1920 -H 1080
mmdc -i data-flow.mmd -o data-flow.png -w 1920 -H 1080
mmdc -i challenge-resolution-phases.mmd -o challenge-resolution-phases.png -w 1920 -H 1080
```

## Diagrams

### 1. Multi-Chain Architecture
**File**: `multi-chain-architecture.mmd`

Shows the four-chain coordination strategy:
- **Cardano**: Governance anchor, Plutus validators, reputation registry
- **Monad**: High-speed experiment submission, challenge resolution
- **Solana**: Real-time reputation updates, payment escrow
- **Filecoin**: Permanent storage, IPFS CIDs, ORD integration

**Cross-chain bridges**: Wormhole for Cardano ↔ Monad ↔ Solana

### 2. Data Flow (Experiment Lifecycle)
**File**: `data-flow.mmd`

Sequence diagram showing:
1. Experiment submission (Monad, 0.01 ETH stake)
2. IPFS/Filecoin CID storage
3. 15-day challenge period (multi-sig resolution)
4. Reputation updates (Solana, every ~20 experiments)
5. ADA rewards (Cardano, monthly batch)

### 3. Challenge Resolution Phases
**File**: `challenge-resolution-phases.mmd`

Progressive decentralization roadmap:
- **Phase 1** (current): Multi-sig (3 resolvers, 2-of-3 quorum)
- **Phase 2** (6-12mo): Interactive bisection game (MACI-style)
- **Phase 3** (long-term): ZK proofs (fully trustless)

## ASCII Previews
See main proposal documents (README.md, THOUGHTS.md) for ASCII versions suitable for text-only viewing.

## Usage in Catalyst Proposal
1. Generate PNGs using mermaid-cli
2. Upload to Ideascale proposal
3. Link in README.md
4. Include in demo video

## License
CC BY-SA 4.0 (documentation)
