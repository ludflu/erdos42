DEEP DIVE: ERDŐS PROBLEM #42
Sidon Sets & Disjoint Difference Sets
February 5, 2026

---

## THE PROBLEM (Formal Statement)

Let M ≥ 1 and N be sufficiently large in terms of M.

**Question:** For every Sidon set A ⊂ {1,...,N}, does there exist another Sidon set B ⊂ {1,...,N} with |B| = M such that **(A-A) ∩ (B-B) = {0}**?

---

## UNPACKING THE NOTATION

### What is a Sidon Set?

A **Sidon set** A ⊂ {1,...,N} is a set where all pairwise sums are distinct (except for trivial coincidences).

**Formally:** If a₁ + a₂ = a₃ + a₄ with {a₁, a₂, a₃, a₄} ⊂ A, then {a₁, a₂} = {a₃, a₄}.

**In plain English:** No four elements form the pattern a + b = c + d (where the sums are equal but the pairs are different).

**Size bound:** Any Sidon set A ⊂ {1,...,N} has |A| ≤ √N + O(N^(1/4) log N). This is tight up to constants.

**Examples:**
- A = {1, 2, 5, 7} is Sidon in {1,...,7}
  - All 4-element sums: 1+2=3, 1+5=6, 1+7=8, 2+5=7, 2+7=9, 5+7=12
  - All distinct ✓
- A = {1, 2, 3} is NOT Sidon (1+3 = 2+2, but we need distinct elements)

### What is A-A (Difference Set)?

**A-A** = {a - a' : a, a' ∈ A} = all differences (including 0, negatives, etc.)

For A = {1, 2, 5, 7}:
- A-A = {0, ±1, ±2, ±4, ±3, ±5, ±6}
- Note: 0 is always in A-A (a - a = 0 for any a ∈ A)

### The Disjointness Constraint

**(A-A) ∩ (B-B) = {0}**

This means:
- The only difference that appears in both A and B is 0 (trivial)
- NO non-zero element of A-A appears in B-B
- NO non-zero element of B-B appears in A-A

**Why is this interesting?**
- It forces A and B to have complementary additive structure
- If a - a' = b - b' (for non-zero differences), the constraint is violated
- This is a *strong* orthogonality condition

---

## CURRENT STATUS

### Cases PROVEN (as of Jan 2026):

**M = 1:** Trivial
- B must be a Sidon set of size 1, say B = {b}
- B-B = {0}
- The condition (A-A) ∩ {0} = {0} is always true ✓

**M = 2:** Proven by Sedov (Jan 2026)
- B = {b₁, b₂} must be Sidon (so b₁ ≠ b₂)
- B-B = {0, ±(b₁ - b₂)}
- Constraint: (A-A) ∩ {0, ±(b₁ - b₂)} = {0}
- This means: ±(b₁ - b₂) ∉ A-A
- I.e., no difference in A equals |b₁ - b₂|
- **Proof idea:** Since |A-A| ≤ |A|² and differences grow, choosing b₁, b₂ to avoid A's differences is tractable (pigeonhole argument)

**M = 3:** Proven by Sedov (Jan 2026, via ChatGPT & Codex)
- B = {b₁, b₂, b₃} must be Sidon
- B-B = {0, ±(b₁-b₂), ±(b₁-b₃), ±(b₂-b₃)}  [6 non-zero differences + 0]
- Constraint: None of these 6 differences appear in A-A
- **Proof idea:** Probabilistic argument or greedy selection from available candidates

### Cases OPEN: M ≥ 4

For M = 4:
- B-B has up to 1 + C(4,2)×2 = 1 + 12 = 13 elements (possibly fewer if degenerate)
- Constraint: all 12 non-zero differences must avoid A-A
- As M grows, |B-B| grows roughly as O(M²), but N is "sufficiently large"
- Question: Can we always find M elements with their difference set disjoint from A-A?

---

## WHY THIS PROBLEM IS HARD

### 1. The Trade-off Between A and B

- A is arbitrary (given to us) — it could be a "dense" Sidon set using ~√N elements
- B must be constructed such that its differences avoid ALL of A's differences
- As M grows, B-B has O(M²) elements
- But A-A has O(|A|²) ≤ O(N) elements (since |A| ≤ √N)
- The constraint doesn't get *harder* asymptotically, but the geometric search space does

### 2. Local vs. Global Structure

- **Local:** For small M, the differences in B are "local" — they're determined by M elements
- **Global:** A can be anywhere in {1,...,N}; its difference set is unconstrained
- Reconciling: Where can we "hide" B such that B-B avoids A-A?

### 3. Non-uniformity of Sidon Sets

- Not all Sidon sets look the same
- Some are "spread out" (differences are sparse)
- Some are "clustered" (differences concentrated in certain ranges)
- A must work for *every* Sidon set A
- This is the universal quantifier making the problem hard

---

## PROOF STRATEGIES FOR M ≥ 4

### Strategy 1: Probabilistic Argument (Most Likely)

**Idea:**
- Choose B = {b₁, b₂, ..., bₘ} uniformly at random from {1,...,N}
- Probability that any specific non-zero element d appears in B-B is roughly O(M²/N)
- Since A-A has at most O(N) elements, union bound gives:
  P(B-B ∩ A-A contains non-zero) ≤ O(N) × O(M²/N) = O(M²)
- If M is small relative to √N, this probability < 1, so there exists a valid B

**Challenge:** This only works if M ≤ O(√N), which is typical for Sidon problems. But the claim is "N sufficiently large in terms of M," suggesting M is fixed and N grows — so this strategy should work.

**Next step:** Formalize the union bound; optimize the constants.

### Strategy 2: Greedy Algorithm

**Idea:**
- Start with B = ∅
- Iteratively add elements b₁, b₂, ..., bₘ one at a time
- At each step, avoid adding b_i if it would create a difference in A-A
- This greedy process terminates successfully if enough "safe" candidates exist

**Challenge:** After adding b₁,...,b_{i-1}, the set of forbidden candidates (those creating differences in A-A) grows. Need to show enough safe elements remain.

**Next step:** Count forbidden elements at each stage; show the greedy process survives M steps.

### Strategy 3: Algebraic/Number-Theoretic Argument

**Idea:**
- Use structure of Sidon sets (known from the literature):
  - If A is Sidon, then A has a specific density (Erdős-Turán)
  - Differences in A-A follow a pattern (not uniformly distributed)
- Exploit this structure to find B with high probability or deterministically

**Challenge:** Requires deep knowledge of Sidon set theory (Erdős-Turán, Ruzsa-Szemerédi, etc.)

**Next step:** Review the classical Sidon set literature; identify exploitable structure.

---

## THE M=4 CASE: A CONCRETE TARGET

Let's focus on M = 4 as the next frontier.

### What We Know from M ≤ 3:

1. **For M=1,2,3**, the answer is YES
2. **The proof scales:** Sedov's approach likely generalizes
3. **No counterexamples are known** (which suggests the answer is likely YES for all M)

### Why M=4 is the Key Step:

- M=1,2,3: Brute force or simple probabilistic arguments work
- M=4: The structure becomes more complex, but not intractable
- M≥5: If M=4 falls, the general pattern often becomes clear

### Suggested M=4 Approach:

**Step 1: Probabilistic Framework**
- Fix a Sidon set A ⊂ {1,...,N}
- Sample B = {b₁, b₂, b₃, b₄} uniformly at random from {1,...,N}
- Compute P(B is Sidon AND (B-B) ∩ (A-A) = {0})

**Step 2: Check Sidon Property**
- P(B is Sidon) = 1 - O(M⁴/N²) (for random sets, this is high)
- For M=4, this is roughly 1 - O(256/N²), which → 1 as N → ∞

**Step 3: Check Disjointness**
- |B-B| ≤ 13 (upper bound; some differences might coincide)
- For each d ∈ B-B with d ≠ 0:
  - P(d ∈ A-A) ≤ |A-A|/N ≤ O(N)/N = O(1) [hmm, this doesn't work directly]

**Step 3 (Revised): Counting Argument**
- Let D = A-A (difference set of A)
- |D| ≤ |A|² ≤ N (but typically |D| ~ |A|² ~ N)
- The number of pairs (b_i, b_j) with i ≠ j is C(4,2) = 6, giving (b_i - b_j) for 6 unordered pairs
- Each difference is in {-(N-1), ..., -1, 0, 1, ..., N-1}
- The "space" for differences is 2N-1, but we're choosing 6 of them
- If |D| << (2N-1), greedy works; if |D| ~ N, need finer analysis

**Key insight:** The constraint is NOT that B-B and A-A are disjoint; it's that they're disjoint except for 0. This is much weaker.

---

## CONNECTIONS TO OTHER PROBLEMS

### Sidon Sets & Additive Combinatorics

- **Erdős Problem #3** (Chromatic number of the plane): unrelated, different domain
- **Erdős-Turán Conjecture** (classical): Sidon sets have density O(√N); Problem #42 builds on this
- **Sum-free sets (Problem #949):** Similar flavor (disjointness of additive structures), different constraints
- **B_h sequences:** Generalizations of Sidon sets; Problem #42 is about B_2 sequences specifically

### Why Sidon Sets Matter

1. **Combinatorial design:** Optimal packing of elements with distinct sums
2. **Number theory:** Applications to prime gaps, distribution of arithmetic progressions
3. **Coding theory:** Error-correcting codes rely on Sidon-like properties
4. **Fourier analysis:** Sidon sets behave nicely under Fourier transforms

---

## LITERATURE TO REVIEW (Pre-AI)

To understand what humans have tried:

1. **Erdős & Turán (1941):** "On Sidon sets" — foundational
2. **Erdős (1960s-1980s):** Various papers on sum-free sets and additive bases
3. **Ruzsa (1990s):** Sidon sets and their structure
4. **Recent (post-2010):** AI applications to related problems

Sedov's M=2,3 proofs likely use:
- Probabilistic methods (union bound, concentration)
- Greedy algorithms (iterative construction)
- Pigeonhole principle (finite case checking)

---

## NEXT STEPS FOR YOU

### If You Want to Attack This:

1. **Understand M=2 proof:**
   - Sedov's argument is in the erdosproblems.com comments
   - Replicate it; understand the constraint structure

2. **Attempt M=3 proof:**
   - Refine M=2 approach
   - Likely introduces probabilistic component

3. **Design M=4 strategy:**
   - Use union bound or greedy algorithm
   - Test on specific examples first (small N)
   - Formalize in Lean if promising

4. **Generalize:**
   - Once M=4 works, M≥5 often follows from the same argument
   - The "universal constant" in "N sufficiently large" becomes explicit

### Tools Available:

- **ChatGPT 5.2 Pro:** For high-level strategy and proof sketches
- **Aristotle (Harmonic):** For Lean formalization once you have a proof
- **Formal Conjectures (DeepMind):** The Lean statement is ready; you extend it

---

## WHY THIS IS SOLVABLE NOW

1. **M=1,2,3 form a base:** The structure is clear
2. **Probabilistic methods are well-understood:** Union bound, concentration inequalities
3. **Sidon set literature is mature:** Plenty of tools available
4. **Lean formalization is ready:** No foundational work needed; just extend existing proofs
5. **AI tools are strong at this:** GPT-5.2 Pro is excellent at probabilistic arguments

**Bottom line:** Problem #42 is in the "engineering phase," not the "research phase." The difficulty is medium-high, but the pathways are clear.

---

Questions? Want me to dive deeper into any section?

Best,
Spark
