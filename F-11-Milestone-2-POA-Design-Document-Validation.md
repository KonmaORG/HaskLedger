### Title:

**HaskLedger: A Phased Architecture for Cardano-Compatible Off-Chain/Sidechain Computation**

### **Abstract**

HaskLedger is an embedded domain-specific language (EDSL) in Haskell with a modular runtime for Cardano-compatible applications. Contracts that require on-chain validation are compiled to **Plutus Core**, while high-throughput logic executes off-chain or on sidechains via reproducible Linux worker images. This document presents the overall system design (generic and vendor-neutral) and distinguishes a **short-term** implementation track from **long-term** evolution. It clarifies architecture, compatibility, performance strategy and operational concerns. The design assumes a pragmatic toolchain on x86_64/aarch64 and validates riscv64 via emulation; physical RISC-V boards are optional for future validation. It informs layering and reproducibility; embedded-device SDKs such as MLabs’ **Embedano** are acknowledged as optional references, **not dependencies**.

**Keywords:** Cardano, Plutus Core, Haskell, EDSL, sidechains, off-chain compute, reproducible builds, RISC-V, Rust FFI

### Front-Page Facts

- **What HaskLedger is:** A Haskell EDSL + toolchain that emits **Plutus Core** for validators and orchestrates **off-chain/sidechain** execution.
- **Where things run:**
    - Developer & gateway hosts: **x86_64/aarch64 Linux**.
    - Workers: Linux images; **riscv64 validated via QEMU** (optional physical RV64 in future).
- **RISC-V baseline :** **RV64GC** (I, M, A, F, D + C) with **Zicsr** and **Zifencei**; ABI=**LP64D**; OS=**Linux**. (RVV/crypto/bitmanip in future scope)
- **Languages & roles:** **Haskell/GHC (AOT)** for EDSL, compiler, orchestration; **Rust** only for perf-critical libraries/tools via **FFI**. See Rust usage & Haskell FFI policy and EDSL→Plutus subset for details.
- **Cardano interop:** **EDSL → Plutus Core** for validators; **gateway service** handles L1 tx building/checkpoints.
- **Embedano:** referenced **only as optional related work** for embedded devices.

### Introduction

Cardano applications benefit from a clear separation of concerns: succinct, deterministic validators on Layer-1 (L1) and stateful, scalable logic off-chain. **HaskLedger** provides:

1. an EDSL embedded in Haskell that compiles validator logic to **Plutus Core**;
2. an off-chain/sidechain runtime with actor-based services;
3. a gateway for L1 interaction (transactions, checkpoints, attestations).

Design goals: type-level safety, reproducibility, predictable performance through structured concurrency and clear compatibility across architectures.

### Problem Statement

Developers face three recurring challenges:

- **Keep L1 minimal, express complex logic:** hard to maintain expressiveness without bloating on-chain validators.
- **Auditability across boundaries:** coordinating off-chain execution with on-chain settlement while preserving traceability.
- **Heterogeneous targets:** developer machines vs deployment targets (incl. RISC-V) without losing reproducibility.

**HaskLedger** addresses these with a typed EDSL → Plutus pipeline, a worker/gateway split and **build profiles** that make architecture variance operational rather than a programming burden.

### Proposed Solution

### Design tenets

- **Of Cardano, not on Cardano:** heavy computation off-chain/sidechain; L1 used for settlement/attestation.
- **Typed safety:** determinism and resource bounds encoded at the type level; validators generated from a restricted EDSL subset.
- **Reproducibility first:** pinned builds (Nix/Guix where appropriate); signed, traceable artifacts.
- **Performance via structure:** actor pipelines with bounded queues, back-pressure and sharding.
- **Pragmatic portability:** primary targets **x86_64/aarch64**; riscv64 validated via emulation; physical RV64 optional later.

### Roles of Haskell and Rust

- **Haskell (GHC, AOT):** EDSL, compiler, orchestration, most services.
- **Rust (via stable C-ABI FFI):** perf-critical hotspots (crypto, parsers, I/O) and select tools.
- Not a Haskell→Rust compiler; not a VM target for the EDSL.
- A full boundary contract for Haskell↔Rust is defined in Rust usage & Haskell FFI policy section, including determinism, ABI, memory ownership and test strategy.

### System architecture (end-to-end)

```
                          ┌───────────────────────────────┐
                          │           CLIENTS             │
                          │  (apps, services, users)      │
                          └───────────────┬───────────────┘
                                          │  job/tx intent (RPC/HTTP)
                                          v
                    ┌───────────────────────────────┐
                    │     HaskLedger Orchestrator   │
                    │  (Haskell app using the EDSL) │
                    └───────────────┬───────────────┘
                                    │ schedules work (per key/shard)
                                    v
      ┌─────────────────────────────────────────────────────────────────┐
      │                     WORKER CLUSTER (Linux)                      │
      │    x86_64 / aarch64  [riscv64 validated via QEMU; optional HW]  │
      │                                                                 │
      │  ┌──────────────┐   ┌──────────────┐   ┌──────────────┐         │
      │  │  Ingest      │→Q→│  Validate    │→Q→│   Execute    │-→Q─┐    │
      │  │  (stage A)   │   │  (stage B)   │   │  (stage C)   │    │    │
      │  └──────────────┘   └──────────────┘   └──────────────┘    │    │
      │         ^                     ^                    ^       │    │
      │         │ back-pressure via bounded MPSC queues    │       │    │
      │         └──────────────────────────────────────────┘       │    │
      │                                                            │    │
      │                                     ┌───────────────────┐  │    │
      │                                     │      Attest       │──┘    │
      │                                     │  (checkpoint/tx)  │       │
      │                                     └─────────┬─────────┘       │
      └───────────────────────────────────────────────│─────────────────┘
                                                      │ RPC (signed msgs)
                                                      v
                           ┌──────────────────────────────────────────┐
                           │              GATEWAY                     │
                           │  Builds L1 txs/checkpoints; applies      │
                           │  confirmation policy; returns receipts   │
                           └───────────────┬──────────────────────────┘
                                           │ submit tx / get receipts
                                           v
                           ┌──────────────────────────────────────────┐
                           │            CARDANO L1 (Settlement)       │
                           │  Nodes/CLI (supported hosts via Nix)     │
                           └──────────────────────────────────────────┘

            ┌────────────────────────────────────────────────────────┐
            │       EDSL → Plutus Core (validator generation)        │
            │   HaskLedger compiler emits validators used by L1      │
            └────────────────────────────────────────────────────────┘

```

**Legend**

- `Q` = bounded queue with back-pressure
- Orchestrator schedules per key/shard; Workers run staged actors; Gateway is the only L1 touchpoint
- EDSL compiler produces Plutus Core validators for any on-chain checks needed

![image.png](attachment:bf64333b-759c-46ff-b607-16d06e6d9ccd:image.png)

### Logical view (high-level)

- **HaskLedger EDSL** (Haskell) → **Plutus Core** validators (for L1 when needed).
- **Worker cluster** (Linux) runs actor pipelines for ingest → validate → execute → attest.
- **Gateway** (on supported hosts) builds & submits L1 transactions/checkpoints and returns receipts to workers.

### Components

- **EDSL Compiler:** Haskell AST → Plutus Core (subset); **golden** and **differential** tests ensure semantic faithfulness.
- **Runtime Services:** actor-based microservices with bounded MPSC queues; structured logs, metrics, traces.
- **Gateway:** tx/checkpoint builder for L1; confirmation policy; stable RPC for workers.
- **State/Storage:** content-addressed logs and snapshots; deterministic serialization for audits.

### Deployment view

- **Gateway & tooling:** run on **x86_64/aarch64** with Cardano node/CLI.
- **Workers:** Linux images with pinned toolchains. **riscv64** builds tested under **QEMU**; physical boards are optional in future phases.

### Interoperation with Cardano

- When on-chain proof/settlement is needed, workers request checkpoints/txs from the **gateway**.
- EDSL generates Plutus validators; gateway submits and returns **tx hash / slot** receipts to workers.

![image.png](attachment:30b0115e-1a67-4552-8723-daa3c5f33f3a:image.png)

### Worker actor pipeline (with sharding & back-pressure)

```
                 ┌──────────────────────────────────────────────────┐
                 │                SHARD ROUTER                      │
                 │  partition by {contract_id | account | key ...}  │
                 └───────────────┬──────────────────────────────────┘
                                 │
           ┌─────────────────────┼─────────────────────┐
           │                     │                     │
           v                     v                     v
   ┌─────────────────┐   ┌─────────────────┐   ┌─────────────────┐
   │  PIPELINE: S0   │   │  PIPELINE: S1   │   │  PIPELINE: S2   │   … (N)
   └───────┬─────────┘   └───────┬─────────┘   └───────┬─────────┘
           │                     │                     │
           v                     v                     v
   [A] INGEST ──Q₁──▶ [B] VALIDATE ──Q₂──▶ [C] EXECUTE ──Q₃──▶ [D] ATTEST ──RPC──▶ Gateway
        │                  │                     │                     │
        │                  │                     │                     └── receipts ←- Gateway
        │                  │                     │
        └─ back-pressure ◀─┴────── back-pressure ┴───────── back-pressure ────────┘

Stage details:
[A] INGEST    : parse/authorize request, enrich with context, enqueue
[B] VALIDATE  : schema/rules checks, referential lookups, preconditions
[C] EXECUTE   : run EDSL-driven business logic; mutate local state
[D] ATTEST    : create checkpoint/tx intent → call Gateway; persist outcome

Queues (Q₁..Q₃):
- Bounded MPSC; size tuned to keep p95 latency targets
- Droptail or token-credit scheme to signal upstream slowdowns

Concurrency & safety:
- Each pipeline handles one shard; pipelines run in parallel across shards
- Within a stage, use STM (where helpful) or lock-free structures
- Idempotent handlers enable safe retries on failure/reorgs

```

![image.png](attachment:6166cf6b-8452-4cbf-bd59-e394de29ba10:image.png)

### Full Implementation

### EDSL → Plutus Core backend

**Subset & determinism.**

Start with a well-scoped subset (no unrestricted recursion; bounded data; deterministic primitives). Document resource metering assumptions alongside semantics. The exact validator subset is tabulated to make inclusions/exclusions auditable. Golden/round-trip/differential testing is unchanged.

**Testing.**

- **Golden tests:** EDSL snippet → expected Plutus Core.
- **Round-trip tests:** EDSL → Plutus → interpret → compare semantics.
- **Differential tests:** generated vs hand-written reference validators (escrow, multisig, timelock).

**Tooling.**

`hlc` CLI: build, check, transpile, simulate. CI runs golden/differential tests on primary targets.

### EDSL → Plutus Core (UPLC) Subset Table

The EDSL intentionally restricts constructs to ensure deterministic, meterable validators. This table lists what’s **in** vs **out**, with rationale and examples you can turn into golden tests.

| Construct | Allowed? | Rationale | Minimal Example (EDSL sketch) | Metering/Notes |
| --- | --- | --- | --- | --- |
| Algebraic data (finite) | **Yes** | Bounded, serializable | `data Choice = A | B` |
| Pattern match (total) | **Yes** | Totality → determinism | `case c of A -> …; B -> …` | Exhaustive only |
| Pure functions | **Yes** | Referential transparency | `f x = x + 1` | No hidden I/O |
| Recursion (unbounded) | **No** | Gas safety | - | Use folds/structural recursions only |
| Structural folds | **Yes** | Provable termination | `foldl go z xs` | Bound by input size |
| Effects (I/O, time, RNG) | **No** | Determinism | - | Must live off-chain |
| ByteString ops | **Yes** | Needed for crypto | `blake2b bs` | Limited set, costed |
| Maps/Sets | **Yes** (bounded) | Common patterns | `insert k v m` | Size limit → cost model |
| Higher-order | **Yes** (safe subset) | EDSL ergonomics | `map f xs` | No escaping closures |
| FFI intrinsics | **Yes** (gated) | Perf-critical ops | `ed25519_sign msg sk` | Must satisfy Rust Usage & Haskell FFI Policy |

### Concurrency & performance strategy

**Execution model.**

Staged **actor pipelines**: ingest → validate → execute → attest; sharded by job/contract key.

**Back-pressure** and bounded queues maintain p95 latency; STM is used inside services where it simplifies correctness; otherwise use lock-free or coarse-grain structures.

**Benchmarks plan (initial).**

- **Boot-to-ready** time.
- **p95 latency** on I/O-bound operation.
- **Throughput** of the pipeline under saturation.
- **Crypto ops/sec** (e.g., ed25519) via Rust FFI vs pure Haskell baseline.
- **STM abort rate** at N threads.
    
    (Conservative targets; each metric paired with a reproducible harness.)
    

### Build profiles & toolchains

- **Primary dev:** x86_64/aarch64 Linux (Nix + Cabal/Stack + Rust).
- **riscv64 (validation path):** cross-compile + **QEMU** test; static link as needed; **no dependency** on GHCi/dylibs.
- **Rust on riscv64:** prefer crates with riscv64 targets; use `no_std` or stubs if needed; riscv64 Rust usage optional.

### Rust Usage & Haskell FFI Policy

**Scope.** Rust is used **selectively** in worker services for performance-critical libraries (crypto, parsing, I/O) and some tools. The **EDSL compiler and core semantics remain in Haskell**. No Haskell→Rust transpilation or VM translation is implied. (This reinforces the “Roles of Haskell and Rust” already stated.)

**Boundary contract (C-ABI).** Every Haskell↔Rust crossing MUST satisfy:

1. **ABI & Types**
    - `foreign import/export ccall` (Haskell) and `extern "C"` (Rust).
    - Only FFI-safe, fixed-layout types (fixed-width ints, length-delimited byte spans, enums with fixed repr). No ad-hoc structs without documentation.
2. **Determinism**
    - Functions callable from EDSL-visible code must be **pure** from the caller’s perspective: no global mutable state, no time, RNG, filesystem, or network. Side-effects belong in worker stages, not validator semantics.
3. **Error handling**
    - Return explicit status codes (OK/ERR) plus result length; no panics across the boundary. Timeouts handled at the call site with bounded waits to protect p95 latency goals (see performance section).
4. **Memory & Ownership**
    - Either (a) caller-allocated buffers (capacity provided; callee writes exact length), or (b) callee-allocated with paired `_free`. No ambiguous ownership.
5. **Threading & Safety**
    - Reentrant by default; if not, the call sites serialize access. No thread-local state that could vary outcomes across runs.
6. **Versioning**
    - Exported symbols include an explicit semver (e.g., `_v1`) or are gated via versioned headers to prevent silent ABI drift.

**Testing & verification.**

- **Golden FFI tests:** Canonical input→output vectors diffed in CI across x86_64, aarch64, and riscv64 (QEMU).
- **Property tests:** QuickCheck-style properties at the Haskell boundary (e.g., parse/serialize round-trips, crypto invariants).
- **Fault injection:** Simulate error codes, short reads/writes, and timeouts; assert correct caller behavior.
- **Determinism checks:** identical vectors on all targets → identical digests; divergence blocks release.

**Illustrative example (signing):**

```haskell
-- Haskell
foreign import ccall unsafe "ed25519_sign_v1"
  c_ed25519_sign_v1 :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> Ptr Word8 -> IO CInt

sign :: ByteString -> ByteString -> Either Err ByteString
sign sk msg = withBS sk $ \psk lsk -> withBS msg $ \pmsg lmsg ->
  allocaBytes 64 $ \psig -> do
    rc <- c_ed25519_sign_v1 pmsg (fromIntegral lmsg) psk (fromIntegral lsk) psig
    if rc == 0 then Right <$> packPtr psig 64 else pure (Left ErrSign)

```

```rust
// Rust
#[no_mangle]
pub extern "C" fn ed25519_sign_v1(
    msg_ptr: *const u8, msg_len: usize,
    sk_ptr:  *const u8, sk_len:  usize,
    sig_out: *mut u8,
) -> i32 {
    if msg_ptr.is_null() || sk_ptr.is_null() || sig_out.is_null() || sk_len != 32 { return -1; }
    let msg = unsafe { core::slice::from_raw_parts(msg_ptr, msg_len) };
    let sk  = unsafe { core::slice::from_raw_parts(sk_ptr,  sk_len)  };
    let sig = ed25519::sign(sk, msg); // deterministic
    unsafe { core::ptr::copy_nonoverlapping(sig.as_ptr(), sig_out, 64); }
    0
}

```

### Competitive & Related Work

### **Purpose**

Position HaskLedger among Cardano-compatible languages/DSLs and execution patterns by comparing (i) authoring model, (ii) determinism/auditability, (iii) off-chain/runtime story and (iv) testing/ops posture. This clarifies what HaskLedger optimizes for: a **typed EDSL + explicit off-chain runtime** with an auditable boundary to L1.

### Survey

- **PlutusTx (Haskell → Plutus Core):** platform-native; flexible; off-chain is app-defined.
- **Plutarch (Haskell EDSL):** strong typing and predictable codegen; off-chain out of scope.
- **Aiken (stand-alone language):** fast toolchain, ergonomic; neutral on runtime/orchestration.
- **Helios (JS/TS-friendly DSL):** approachable; relies on discipline for determinism/testing rigor.
- **Covenant-style DSLs:** minimalist, pattern-specific; excellent auditability; limited expressiveness; external runtime.
- **HaskLedger (this work):** **Haskell-embedded EDSL** with **restricted validator subset** + **first-class off-chain runtime** (actor pipelines + gateway), with **golden/round-trip/differential tests** and **reproducible ops** (pinned builds, SBOM, signed releases).

### Comparison

| Aspect | PlutusTx | Plutarch | Aiken | Helios | Covenant-style DSLs | **HaskLedger** |
| --- | --- | --- | --- | --- | --- | --- |
| **Authoring model** | Haskell → Plutus | Haskell EDSL | Dedicated lang | JS/TS DSL | Minimal pattern DSL | **Haskell EDSL** |
| **Validator subset** | Platform-native | Typed & explicit | Language-defined | Library-defined | Very restricted | **Restricted + tabled EDSL→Plutus subset** |
| **Determinism stance** | Platform + discipline | Type rigor + templates | Language design + tooling | Discipline + tests | Simplicity = determinism | **Types + codegen rules + tests** |
| **Off-chain/runtime** | App-specific | Out of scope | Non-prescriptive | Non-prescriptive | External | **Actor pipelines + gateway (System Architecture, Concurrency & performance strategy)** |
| **Testing emphasis** | Unit/property | Golden patterns | Built-in focus | Example-driven | Pattern proofs | **Golden + round-trip + differential (EDSL → Plutus Core backend)** |
| **Ops/reproducibility** | App-specific | App-specific | Tooling-centric | Tooling-light | External | **Pinned builds, SBOM, signed releases (Reproducibility, Security, and Ops)** |
| **Primary trade-off** | Flexibility ↔ exposure | Rigor ↔ learning curve | Ergonomics ↔ new lang | Approachability ↔ rigor | Simplicity ↔ expressiveness | **End-to-end rigor ↔ more moving parts** |

**Why HaskLedger?** 

Teams that want a **single, typed source of truth** for validator semantics and a specified, testable **off-chain execution model** get a clearer audit trail and more predictable operations than assembling these concerns piecemeal.

### Phasing (short-term vs long-term)

### Short-term (next 5–6 months)

- Deliver **EDSL backend** for a concrete Plutus subset with golden/round-trip tests.
- Implement **actor pipelines** with minimal service set and end-to-end tracing.
- Stand up the **gateway** with tx/checkpoint flows on supported hosts.
- Establish **CI profiles** (x86_64/aarch64 native; riscv64 via QEMU) and publish signed artifacts/SBOMs.

### Long-term (6+ months)

- Extend EDSL subset, libraries and static analysis; add formal methods where impactful.
- Add **sidechain protocol options** (state commitments, dispute windows, alternative finality gadgets).
- Evaluate **physical RISC-V** boards/accelerators; tune Rust FFI hot paths.
- Harden ops: SLOs, autoscaling, admission control, richer observability and governance hooks.

### Reproducibility, Security and Ops

- **Reproducible builds:** Nix for Cardano-side tooling; Guix/Nix for worker images when needed; publish lockfiles & build hashes.
- **Supply chain:** pin compiler/ABI/ISA; generate **SBOM** per artifact; signed releases.
- **Isolation & least privilege:** slim worker images; gateway principle-of-least-privilege; encrypted secrets.
- **Observability & SLOs:** trace IDs across gateway↔worker; p95/p99 latency targets and error budgets; structured logs.

### Future-proofing & governance hooks

- **Versioned Plutus backend:** explicit language/version gates and migration path.
- **Policy controls:** configurable checkpoint frequency/fees; audit trails for regulator/customer needs.
- **Portability:** clean separation between gateway, workers and EDSL toolchain to allow alternative chains or execution environments later, without rewriting business logic.

### Glossary

- **Of Cardano:** off-chain/sidechain system that consumes L1 context and settles/attests on L1.
- **Gateway:** service that builds/submits on-chain transactions and returns receipts.
- **Worker:** off-chain compute node executing EDSL-defined logic.
- **Checkpoint:** on-chain commitment to off-chain state for integrity/finality.
- **Golden test:** fixed “expected output” test for codegen correctness.

### Conclusion

HaskLedger separates concerns cleanly: a typed Haskell EDSL that emits Plutus Core for on-chain validators and a scalable, observable off-chain runtime with straightforward interop to Cardano through a gateway. The plan is generic and phased, minimizing lock-in, preserving portability and reproducibility and aiming for measurable performance with a simple, testable concurrency model.
