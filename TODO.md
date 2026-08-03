## Nightly Handoff — 2026-08-03

### What's committed
- `tests/data-spec.lisp` — ported test file with 3 parachute tests (2 pass, 1 fails)
- `src/resolver.lisp` — 3 fixes committed:
  - `make-comparison` :in-range group indices (rest/num/trailing) ✅
  - `seek-package` fset→list conversion ✅
  - `list-packages` rewrite (Clojure recursion, hash-table exclude) ✅
- `update-package`/`update-package-graph` — **reverted to original mutating state** (the "leaky but working" state)

### Beads updated
| Bead          | Status           | Subject                                      |
|---------------|------------------|----------------------------------------------|
| **dsolv-282** | ◐ in_progress    | data-spec-test — full analysis in notes      |
| **dsolv-3af** | ◐ in_progress    | resolver core — deeper bug analysis in notes |
| **dsolv-9mn** | ○ **NEW** P0 bug | Closure capture in make-resolve-deps         |
### The Deeper Bug (dsolv-9mn)
**Root cause:** `resolve-alternative`'s signature is `(alternative mkerror rclauses parent)` — it does NOT take `found-packages`, `absent-specs`, or `package-graph` as parameters. These names resolve to the **outer lambda's** initial empty tables, not the current recursion's values. This means `update-package-graph` always mutates the initial empty graph — all trials' artifacts accumulate in the shared table by accident.

**Evidence:** `(gethash id found-packages)` inside `resolve-alternative` reads the initial empty table. `update-package-graph` always operates on the outer lambda's graph. The Clojure original threads all state through `resolve-deps`'s recursive parameters.

**The fix:** Change `resolve-alternative` to take `found-packages`, `absent-specs`, `package-graph` as explicit params and update all call sites in `resolve-deps` to thread the current values. This matches Clojure's semantics and fixes both the e-2.4.0 leak and the graph truncation.

### Tomorrow's first step
1. Check out `dsolv-9mn` — fix the `resolve-alternative` signature
2. Reload → run tutorial-test
3. Should see all 3 data-spec tests pass
4. Close dsolv-282, update dsolv-3af, close dsolv-9mn
5. `bd dolt push` + `git push`