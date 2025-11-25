# NEXT_STEPS: 3-Day Sprint for Catalyst Fund 15 Submission

## Context

**Deadline**: 3 days to finalize ₳200,000 proposal for Catalyst Fund 15

**Current Status**:
- ✅ ETHGlobal SF demo (Polygon prototype)
- ✅ PoX security audit completed (hardening plan documented)
- ✅ Multi-chain architecture designed (README.md, THOUGHTS.md)
- ⏳ Catalyst proposal submission (this sprint)

**Goal**: Transform concept repository into **fundable, executable proposal** with:
1. Clear technical roadmap
2. Credible team structure
3. Measurable milestones
4. Risk mitigation strategy
5. Community buy-in mechanisms

---

## Day 1: Foundation & Research (8 hours)

### Morning (4 hours): Technical Validation

#### Task 1.1: Cardano Ecosystem Audit (2h)
**Owner**: Lead (you)

**Actions**:
- [ ] Research Cardano Name Service (CNS) status vs. ENS
  - Is CNS production-ready? (Alternative: unstoppabledomains.com/cardano)
  - Cost comparison: CNS registration vs. ENS
  - Integration complexity: CIP-68 metadata + CNS
- [ ] Validate Plutus → Haskell ADT tooling
  - Check `plutus-tx` plugin compatibility with GHC 9.x
  - Test Aeson serialization of Haskell ADT → JSON → IPFS
  - Verify `ctl` (Cardano Transaction Library) for frontend integration
- [ ] Wormhole Cardano support verification
  - Current status: mainnet or testnet only?
  - Latency benchmarks: Cardano → Solana (find existing data)
  - Cost: per-message fees (Wormhole docs)

**Deliverable**: 1-page technical feasibility report (add to `docs/` folder in repo)

---

#### Task 1.2: Multi-Chain Bridge Research (2h)
**Owner**: Lead (you) + AI research assistant (if available)

**Actions**:
- [ ] **Monad Research**:
  - Mainnet launch timeline (Q1 2025? Q2 2025?)
  - Testnet access: how to get whitelisted (Discord? Application?)
  - Bridge partners: Wormhole, Axelar, or LayerZero support?
  - Fallback plan: If Monad delayed, deploy to **Polygon zkEVM** (ZK proofs align with long-term vision)
- [ ] **Solana Bridge Options**:
  - Wormhole vs. Allbridge Core (cost, latency, security)
  - Solana → Cardano: native SPL → ADA conversion (Jupiter Aggregator?)
  - PDAs (Program Derived Addresses) for cross-chain state (Anchor best practices)
- [ ] **Filecoin Integration**:
  - FVM (Filecoin Virtual Machine) Solidity compatibility (EVM-equivalent?)
  - Lighthouse.storage API for automated IPFS/Filecoin uploads
  - Filecoin Plus (DataCap) application process (for subsidized storage)

**Deliverable**: Update `THOUGHTS.md` with bridge strategy + cost estimates

---

### Afternoon (4 hours): Team & Budget Refinement

#### Task 1.3: Team Structure Definition (1.5h)
**Owner**: Lead (you)

**Actions**:
- [ ] **Define Roles** (4 specialists):
  1. **Lead Architect (You?)**: Cardano/Haskell/Plutus, overall vision (₳50k / 6mo)
  2. **Monad/EVM Engineer**: Solidity, gas optimization, challenge system (₳40k / 6mo)
     - Ideal candidate: Previous Polygon/Optimism experience
     - Bonus: Participated in Monad testnet (early access)
  3. **Solana/Rust Developer**: Anchor, Solana Pay, reputation program (₳30k / 6mo)
     - Must have: Shipped ≥1 Solana mainnet program
     - Bonus: Experience with cross-chain bridges (Wormhole SDK)
  4. **Filecoin/FVM Specialist**: IPFS, FVM contracts, deal automation (₳20k / 6mo)
     - Must have: Deployed FVM smart contract
     - Bonus: ORD (Open Reaction Database) familiarity

- [ ] **Recruitment Strategy**:
  - Post on: Cardano Forum, Monad Discord, Solana Jobs board, ETHGlobal Talent
  - Timeline: Start recruiting in Week 1 (if funded), onboard by Week 3
  - Backup: If role unfilled, reallocate budget (e.g., ₳40k Monad → ₳60k Haskell + consultant)

**Deliverable**: Update `README.md` budget section with detailed role descriptions

---

#### Task 1.4: Budget Breakdown Justification (1.5h)
**Owner**: Lead (you)

**Actions**:
- [ ] **Personnel (₳140k / 70%)**:
  - Compare to market rates:
    - Haskell dev: $80-120/hr (₳50k for 6mo = ~₳8.3k/mo = ~$10k/mo at $1.2/ADA = $50/hr for 200hr/mo)
    - Solidity dev: $60-100/hr (₳40k for 6mo = ~$40/hr)
    - Rust/Solana dev: $70-110/hr (₳30k for 6mo = ~$35/hr)
    - FVM/IPFS specialist: $50-80/hr (₳20k for 6mo = ~$25/hr)
  - **Justification**: Below market rate (lean, mission-driven team)
  
- [ ] **Infrastructure (₳30k / 15%)**:
  - Testnet operations: ₳8k (Cardano testnet faucet, Monad testnet gas, Solana devnet)
  - Bridge infra: ₳10k (Wormhole guardian fees, relayer server costs)
  - IPFS/Filecoin storage: ₳6k (1TB storage for 6 months, ~₳1k/mo; ORD data ~500GB)
  - Monitoring: ₳6k (Grafana Cloud, Sentry error tracking, Alchemy/Helius RPC)
  - **Justification**: Conservative estimates; overage goes to contingency

- [ ] **Security & Audits (₳20k / 10%)**:
  - Multi-chain audit: ₳15k (₳5k per chain × 3 chains; Filecoin FVM audit free via Immunefi)
  - Penetration testing: ₳3k (frontend + API security; Hacken or CertiK)
  - Bug bounty: ₳2k (Immunefi program; tiered rewards $100-$1000)
  - **Justification**: Security-first approach (per PoX audit lessons)

- [ ] **Operations (₳10k / 5%)**:
  - Documentation: ₳4k (Technical writer, tutorial videos)
  - Outreach: ₳3k (Cardano Summit, ETHDenver, academic conferences)
  - Legal: ₳2k (Multi-jurisdiction compliance; DAO formation costs)
  - Contingency: ₳1k (buffer for unforeseen expenses)

**Deliverable**: Add "Budget Justification" section to proposal document

---

#### Task 1.5: Milestone Definition (1h)
**Owner**: Lead (you)

**Actions**:
- [ ] **Month 1-2 Milestones** (₳66k / 33%):
  - Cardano testnet validators deployed (Plutus v2 or v3?)
  - Monad testnet contracts deployed (PoXRegistry, ChallengeManager, Reputation)
  - Solana devnet programs deployed (reputation_program, payment_escrow)
  - Filecoin Calibration contracts deployed (CID registry, deal automation)
  - **Deliverable**: GitHub releases with deployment addresses + testnet demo video

- [ ] **Month 3-4 Milestones** (₳66k / 33%):
  - Haskell ADT library: ≥50 reactions (acid-base, redox, substitution classes)
  - Multi-sig challenge system: 3 resolvers identified, voting tested
  - Batch registration: CLI tool for lab data upload (ORD integration prototype)
  - Cross-chain bridge: Monad → Cardano functional (testnet)
  - **Deliverable**: Technical documentation + pilot lab onboarding guide

- [ ] **Month 5-6 Milestones** (₳68k / 34%):
  - Security audit completed (all chains)
  - Mainnet deployments: Cardano mainnet, Solana mainnet, Filecoin mainnet
  - Monad mainnet (if launched) or Polygon zkEVM (fallback)
  - Pilot program: ≥5 labs registered, ≥100 experiments submitted
  - **Deliverable**: Public mainnet launch announcement + governance activation

**Deliverable**: Add "Milestones & Deliverables" section to proposal

---

## Day 2: Proposal Writing & Visuals (8 hours)

### Morning (4 hours): Catalyst Proposal Drafting

#### Task 2.1: Catalyst Proposal Template (2h)
**Owner**: Lead (you)

**Actions**:
- [ ] Download Catalyst Fund 15 proposal template (Ideascale or Google Docs)
- [ ] Fill required sections:
  - **Title**: "ChimiaDAO: Multi-Chain Verifiable Chemistry Infrastructure"
  - **Category**: Use Cases (Concept) → DeSci / Infrastructure
  - **Budget**: ₳200,000 (justify vs. ₳100k original)
  - **Team**: 4 specialists (bios + LinkedIn/GitHub links)
  - **Problem Statement**: (from README.md, condense to 200 words)
  - **Solution Overview**: Multi-chain architecture (diagram + 300 words)
  - **Feasibility**: ETHGlobal demo, PoX audit, academic partnerships (Bio-Block, ORD)
  - **Impact**: 6-month targets (5 labs, 100 experiments, 50+ reactions in library)
  - **Roadmap**: Month-by-month breakdown (from Task 1.5)
  - **Open Source**: Apache-2.0 license, all code on GitHub

**Deliverable**: Draft proposal document (Google Docs or Markdown)

---

#### Task 2.2: Competitor Analysis (1h)
**Owner**: Lead (you) or AI assistant

**Actions**:
- [ ] Research similar Catalyst proposals:
  - DeSci projects: VitaDAO, Molecule, LabDAO
  - Multi-chain projects: Milkomeda, Wanchain, Nervos
  - Chemistry/science: Any previous chemistry-focused proposals?
- [ ] Identify gaps ChimiaDAO fills:
  - **Formal verification**: Haskell ADT (no competitor has this)
  - **Multi-chain from day 1**: Others pivot post-launch (we design for it)
  - **Reproducibility focus**: Not just data marketplace (verification layer)
- [ ] Highlight unique value propositions:
  - "Only project with type-safe molecular representation"
  - "First multi-chain DeSci protocol anchored on Cardano"
  - "Open Reaction Database integration (4M+ reactions)"

**Deliverable**: Add "Competitive Advantage" section to proposal

---

#### Task 2.3: Risk Mitigation Section (1h)
**Owner**: Lead (you)

**Actions**:
- [ ] Document technical risks + mitigations:
  - **Monad delay**: Fallback to Polygon zkEVM (ZK future-proofing)
  - **Bridge failure**: Manual state sync via governance (slow but functional)
  - **Haskell complexity**: Start simple (C, H, O, N only), expand later
- [ ] Document adoption risks + mitigations:
  - **Lab reluctance**: Partner with open-science orgs (Bio-Block, ORD)
  - **UX friction**: Abstract wallets (Privy, Magic Link) for chemists
  - **Data quality**: Bootstrap with peer-reviewed ORD data (pre-verified)
- [ ] Document economic risks + mitigations:
  - **Challenge spam**: Adjust stake amounts via governance
  - **Reputation inflation**: Dynamic update frequency (tune via on-chain params)
  - **Token price volatility**: Batch rewards monthly (reduce bridge costs)

**Deliverable**: Add "Risk Mitigation" section to proposal

---

### Afternoon (4 hours): Visuals & Community Prep

#### Task 2.4: Architecture Diagrams (2h)
**Owner**: Lead (you) + Design tool (Figma, Excalidraw, Mermaid)

**Actions**:
- [ ] **Diagram 1: Multi-Chain Architecture** (already in README.md, refine):
  - Clean up ASCII art → professional SVG/PNG
  - Color-code: Blue (Cardano), Purple (Monad), Green (Solana), Orange (Filecoin)
  - Add labels: "Governance", "Speed", "State", "Storage"
  
- [ ] **Diagram 2: Data Flow** (experiment submission → reward):
  1. Chemist submits experiment (Monad UI)
  2. CID posted to Filecoin (IPFS)
  3. Challenge period (15 days, Monad)
  4. Reputation update (Solana)
  5. ADA reward minted (Cardano, monthly batch)

- [ ] **Diagram 3: Challenge Resolution** (multi-sig → bisection → ZK):
  - Phase 1: 3 resolvers (2-of-3 quorum)
  - Phase 2: Bisection game (MACI-style)
  - Phase 3: ZK proofs (Risc0/SP1)

**Deliverable**: Add diagrams to proposal + `docs/` folder in repo

---

#### Task 2.5: Demo Video Script (1h)
**Owner**: Lead (you)

**Actions**:
- [ ] Write 3-minute video script:
  - **0:00-0:30**: Problem (siloed chemistry data, centralized verification)
  - **0:30-1:00**: Solution (multi-chain verifiable chemistry)
  - **1:00-1:30**: ETHGlobal demo walkthrough (experiment submission)
  - **1:30-2:00**: Multi-chain architecture (diagram explanation)
  - **2:00-2:30**: Roadmap + team (6 months to mainnet)
  - **2:30-3:00**: Call to action (vote on Catalyst, join Discord)
  
- [ ] Plan video production:
  - **Option 1**: Screen recording + voiceover (Loom, OBS)
  - **Option 2**: Animated explainer (Descript, Animaker)
  - **Option 3**: Live demo + slides (Zoom recording)
  - **Timeline**: Record Day 3, upload to YouTube + IPFS

**Deliverable**: Video script + production plan

---

#### Task 2.6: Community Engagement Prep (1h)
**Owner**: Lead (you)

**Actions**:
- [ ] **Set up communication channels**:
  - Discord server (or join existing Cardano DeSci Discord?)
  - Twitter/X account (@ChimiaDAO?)
  - Telegram group (for community updates)
  - GitHub Discussions (enable on repo)

- [ ] **Draft launch announcements**:
  - **Cardano Forum post**: "ChimiaDAO: Multi-Chain Verifiable Chemistry (Catalyst F15)"
  - **Reddit posts**: r/cardano, r/defi, r/cryptodevs
  - **Twitter thread**: 10-tweet summary of proposal (with diagrams)
  - **Bio-Block announcement**: Coordinate with advisory board (if possible)

- [ ] **Prepare FAQ document**:
  - Q: Why not single chain? A: Tradeoffs (speed vs. decentralization vs. cost)
  - Q: Why Cardano as primary? A: Formal verification, academic fit, eUTXO
  - Q: What if Monad delays? A: Fallback to Polygon zkEVM
  - Q: How to get involved? A: Pilot lab program, developer contributions, vote

**Deliverable**: Communication plan + draft posts (ready to publish Day 3)

---

## Day 3: Finalization & Submission (8 hours)

### Morning (4 hours): Proposal Polishing

#### Task 3.1: Internal Review & Editing (2h)
**Owner**: Lead (you) + Co-founder/Advisor (if available)

**Actions**:
- [ ] **Proofread proposal**:
  - Grammar/spelling check (Grammarly)
  - Technical accuracy (verify all claims)
  - Clarity (assume reader has no chemistry background)
  - Catalyst-specific requirements (word limits, formatting)

- [ ] **Peer review**:
  - Share with trusted advisors (Bio-Block, ORD contacts, Cardano devs)
  - Incorporate feedback (address concerns, clarify ambiguities)
  - Ensure alignment with Catalyst Fund 15 goals (use cases, impact metrics)

- [ ] **Budget double-check**:
  - Total = ₳200,000? (Personnel ₳140k + Infra ₳30k + Security ₳20k + Ops ₳10k)
  - Milestones add up to 100%? (33% + 33% + 34%)
  - Contingency buffer included? (₳1k explicit, ~₳5k implicit in conservative estimates)

**Deliverable**: Final proposal draft (v1.0)

---

#### Task 3.2: Visual Assets Finalization (1h)
**Owner**: Lead (you)

**Actions**:
- [ ] **Export diagrams** (high-res PNG + SVG):
  - Multi-chain architecture diagram
  - Data flow diagram
  - Challenge resolution diagram
- [ ] **Create proposal cover image**:
  - ChimiaDAO logo (if exists) or placeholder
  - Tagline: "Multi-Chain Verifiable Chemistry"
  - Catalyst Fund 15 branding (if required)
- [ ] **Record demo video** (if not done yesterday):
  - 3-minute walkthrough (see Task 2.5 script)
  - Upload to YouTube (unlisted or public)
  - Upload to IPFS (decentralized hosting)
  - Add video link to proposal

**Deliverable**: Final visual assets + demo video link

---

#### Task 3.3: GitHub Repository Cleanup (1h)
**Owner**: Lead (you)

**Actions**:
- [ ] **Update repository**:
  - Merge all new docs (README.md, THOUGHTS.md, NEXT_STEPS.md)
  - Add `docs/` folder: diagrams, feasibility report, FAQ
  - Add LICENSE file (Apache-2.0)
  - Add CONTRIBUTING.md (how to get involved)

- [ ] **Create release tag**:
  - Tag: `catalyst-fund15-submission` (v0.1.0)
  - Release notes: Link to proposal, highlight ETHGlobal demo, PoX audit
  - Attach proposal PDF + video link

- [ ] **Enable GitHub features**:
  - Turn on GitHub Discussions (community Q&A)
  - Create issue templates (bug reports, feature requests)
  - Add GitHub Actions (future: CI/CD for contracts)

**Deliverable**: Clean, professional GitHub repo ready for public scrutiny

---

### Afternoon (4 hours): Submission & Outreach

#### Task 3.4: Catalyst Ideascale Submission (1h)
**Owner**: Lead (you)

**Actions**:
- [ ] **Create Ideascale account** (if not already):
  - Link Cardano wallet (for voting participation)
  - Verify email
- [ ] **Submit proposal**:
  - Copy/paste from final draft
  - Upload diagrams, demo video link
  - Tag categories: DeSci, Infrastructure, Use Cases
  - Request ₳200,000
- [ ] **Publish to Cardano community**:
  - Share Ideascale link on Cardano Forum
  - Post on r/cardano
  - Announce on Twitter/X
  - Share in Cardano Discord servers (Catalyst channel)

**Deliverable**: Live Catalyst Fund 15 proposal (Ideascale link)

---

#### Task 3.5: Community Launch (2h)
**Owner**: Lead (you) + Team (if available)

**Actions**:
- [ ] **Publish announcements** (prepared on Day 2):
  - Cardano Forum: Detailed post with proposal link
  - Reddit: r/cardano, r/cryptodevs (cross-post to r/defi if relevant)
  - Twitter/X: Thread + demo video + proposal link
  - Bio-Block: Notify advisory board (request endorsement?)
  - ORD community: Reach out (email, GitHub issue?) about integration plans

- [ ] **Engage with comments**:
  - Monitor Ideascale comments (respond within 24 hours)
  - Answer questions on Reddit/Twitter
  - Address concerns transparently (e.g., "Why 4 chains?")

- [ ] **Activate pilot lab outreach**:
  - Email universities with chemistry departments (MIT, Stanford, ETH Zurich)
  - Reach out to Bio-Block network (academic contacts)
  - Offer early access to pilot program (free, in exchange for feedback)

**Deliverable**: Public proposal launch + community engagement started

---

#### Task 3.6: Backup Plan & Follow-Up (1h)
**Owner**: Lead (you)

**Actions**:
- [ ] **If proposal needs revision**:
  - Monitor Catalyst community feedback (first 48 hours critical)
  - Adjust budget, milestones, or scope if necessary
  - Resubmit updated version (if allowed by Catalyst rules)

- [ ] **Prepare for non-funding scenarios**:
  - **Plan B**: Apply to other grants (Cardano Foundation, Emurgo, dcSpark)
  - **Plan C**: Seek private investment (angels, VCs interested in DeSci)
  - **Plan D**: Bootstrap with ETHGlobal prize money + personal funds (lean MVP)

- [ ] **Set up follow-up schedule**:
  - **Week 1 post-submission**: Daily engagement (comments, questions)
  - **Week 2-3**: Weekly updates (progress on Haskell ADT, testnet prep)
  - **Week 4**: Voting period (rally community, request votes)
  - **Post-voting**: Thank voters, publish results analysis (win or lose)

**Deliverable**: Contingency plan + engagement schedule

---

## Success Criteria (End of Day 3)

### Must-Haves (Critical)
- [x] Catalyst Fund 15 proposal submitted on Ideascale (₳200k request)
- [x] GitHub repository updated (README, THOUGHTS, NEXT_STEPS, docs/)
- [x] Demo video recorded + uploaded (YouTube + IPFS)
- [x] Community channels active (Discord/Twitter/Forum)
- [x] At least 3 pilot lab contacts initiated

### Nice-to-Haves (Optional)
- [ ] Bio-Block endorsement secured (advisory board support)
- [ ] ORD team contacted (official integration partnership)
- [ ] 10+ GitHub stars (early visibility indicator)
- [ ] 50+ Ideascale views (community interest signal)
- [ ] 1+ co-founder recruited (team expansion started)

---

## Post-Sprint Actions (Week 1+)

### If Funded (₳200k Approved)
1. **Week 1**: Finalize team contracts, set up multi-sig treasury
2. **Week 2-3**: Onboard developers, establish dev environment (Cardano testnet, Monad testnet)
3. **Week 4**: Kick off Month 1 milestones (Plutus validators, Monad contracts)
4. **Month 2**: Weekly progress reports to Catalyst community (transparency)

### If Not Funded
1. **Week 1**: Analyze voting feedback, identify weaknesses
2. **Week 2**: Revise proposal for Catalyst Fund 16 (or other grants)
3. **Week 3**: Bootstrap MVP with personal funds (focus on Haskell ADT library)
4. **Month 2**: Apply to alternative funding (Cardano Foundation, Gitcoin)

---

## Key Contacts & Resources

### Cardano Ecosystem
- **Cardano Forum**: https://forum.cardano.org/
- **Catalyst Discord**: https://discord.gg/catalyst
- **Cardano Foundation**: grants@cardanofoundation.org

### Partner Organizations
- **Bio-Block**: https://www.bio-block.org/ (advisory board)
- **Open Reaction Database (ORD)**: https://github.com/open-reaction-database
- **Etica.io**: https://github.com/etica (prior work)

### Technical Resources
- **Plutus Docs**: https://plutus.readthedocs.io/
- **Monad Devnet**: https://docs.monad.xyz/ (testnet access)
- **Wormhole Bridge**: https://wormhole.com/
- **Filecoin FVM**: https://docs.filecoin.io/smart-contracts/fundamentals/the-fvm/

### Recruitment Channels
- **Cardano Jobs**: https://cardano.org/jobs/
- **ETHGlobal Talent**: https://ethglobal.com/talent
- **Solana Jobs**: https://jobs.solana.com/
- **Rust Jobs**: https://www.rustjobs.com/

---

## Final Reflection

**This is not just a proposal. It's a commitment to:**

1. **Truth**: Formal verification (Haskell ADT → Plutus)
2. **Transparency**: Open-source from day one (Apache-2.0)
3. **Collaboration**: Multi-chain (no maximalism)
4. **Impact**: Real science, real labs, real reproducibility

**Fortis est Veritas** — Let's build the verifiable chemistry layer the world needs.

---

**Questions? Feedback? Next Steps?**
- Open a GitHub Discussion: [https://github.com/chimiadao/catalyst-usecase-concept/discussions](https://github.com/chimiadao/catalyst-usecase-concept/discussions)
- Join our Discord: [link TBD after setup]
- Email: [contact TBD]

---

**End of Document**

*Last updated: 2024-11-25 (pre-submission)*
