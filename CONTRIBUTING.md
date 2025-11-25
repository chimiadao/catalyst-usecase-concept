# Contributing to ChimiaDAO

Thank you for your interest in contributing to ChimiaDAO! We're building multi-chain verifiable chemistry infrastructure, and we welcome contributions from developers, chemists, and researchers.

## Ways to Contribute

### 1. Code Contributions
- **Haskell ADT Library** (`packages/chimia-core`): Molecular representation, reaction validation
- **Smart Contracts**: Plutus (Cardano), Solidity (Monad), Rust/Anchor (Solana), FVM (Filecoin)
- **Frontend**: Next.js web application
- **Indexer**: SQD/Subgraph development
- **Testing**: Unit tests, integration tests, E2E tests

### 2. Documentation
- Technical specs
- API documentation
- Tutorial content
- Translation (internationalization)

### 3. Data Contributions
- Historical experiment data (with proper permissions)
- Chemical reaction datasets
- ORD (Open Reaction Database) integration improvements

### 4. Research & Design
- Economic model refinement
- ZK proof circuit design
- Cross-chain bridge optimization
- Molecular representation extensions

## Getting Started

### Prerequisites
- **Haskell**: Stack (for `chimia-core` library)
- **Node.js**: >=18.0.0
- **pnpm**: >=8.0.0
- **Solidity**: Foundry (for smart contracts)
- **Rust**: For Solana/Anchor programs (if contributing to Solana layer)

### Setup

```bash
# Clone repository
git clone https://github.com/chimiadao/catalyst-usecase-concept.git
cd catalyst-usecase-concept

# Install dependencies
pnpm install

# Build Haskell library
cd packages/chimia-core
stack build
stack test

# Run tests
pnpm test
```

## Development Workflow

### 1. Fork & Branch
- Fork the repository to your GitHub account
- Create a feature branch: `git checkout -b feature/your-feature-name`
- Use descriptive branch names: `feat/add-nmr-support`, `fix/reputation-overflow`, `docs/update-readme`

### 2. Commit Messages
Follow [Conventional Commits](https://www.conventionalcommits.org/):

```
feat: add NMR spectrum parsing to chimia-core
fix: correct stoichiometry validation in Reaction module
docs: update multi-chain architecture diagram
test: add QuickCheck properties for Molecule ADT
```

### 3. Code Style
- **Haskell**: Follow [Haskell Style Guide](https://github.com/tibbe/haskell-style-guide)
- **TypeScript/JavaScript**: Use Prettier (run `pnpm format`)
- **Solidity**: Follow [Solidity Style Guide](https://docs.soliditylang.org/en/latest/style-guide.html)
- **Rust**: Use `rustfmt`

### 4. Testing
- **Haskell**: Add HSpec unit tests and QuickCheck properties
- **Solidity**: Write Foundry tests (Forge)
- **TypeScript**: Jest for unit tests
- Maintain >80% code coverage for new features

### 5. Pull Requests
- Ensure all tests pass: `pnpm test`
- Update documentation if necessary
- Reference related issues: `Closes #123` or `Relates to #456`
- Request review from maintainers

## Contribution Areas

### Haskell ADT Library (High Priority)
**Goal**: 50+ reactions across ≥3 reaction classes for Catalyst milestone

Current focus:
- Acid-base reactions
- Redox reactions
- Substitution reactions
- Stoichiometry validation
- JSON serialization (IPFS compatibility)

**How to help**:
- Add new `Atom` types (expand beyond C, H, O, N)
- Implement reaction classes (e.g., `AcidBase`, `Redox`)
- Write QuickCheck properties for validation logic
- Create example molecules (benzene, ethanol, etc.)

### Smart Contract Security
Based on PoX audit lessons (see `docs/security/`):
- Input validation (difficulty 1-100, CID format)
- Integer overflow protection (reputation cap 1-100)
- Reentrancy guards on payment functions
- Stake requirements (0.01 ETH for experiments/challenges)

### Cross-Chain Integration
- Wormhole bridge testing
- State root synchronization
- Merkle proof generation
- Gas optimization

## Code Review Process

1. **Automated Checks**: CI/CD runs tests, linting, type checking
2. **Maintainer Review**: Core team reviews design, security, correctness
3. **Community Feedback**: Open discussion on architecture decisions
4. **Approval**: 2 maintainer approvals required for merge

## Communication

- **GitHub Discussions**: General questions, feature proposals
- **Discord**: Real-time chat (link TBD after Catalyst submission)
- **Issues**: Bug reports, feature requests
- **Twitter/X**: Announcements (@ChimiaDAO TBD)

## Recognition

Contributors will be:
- Listed in CONTRIBUTORS.md
- Acknowledged in release notes
- Eligible for future DAO governance tokens
- Invited to pilot lab program (priority access)

## License

By contributing, you agree that your contributions will be licensed under:
- **Code**: Apache-2.0
- **Documentation**: CC BY-SA 4.0

## Questions?

- Open a GitHub Discussion
- Email: contact@chimiadao.org (TBD)
- Join Discord: [link TBD]

---

**Fortis est Veritas** — Truth is strengthened by multiple, independent verification layers. Thank you for helping build the verifiable chemistry layer the world needs.
