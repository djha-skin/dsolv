# Agent Instructions

## Beads Issue Tracking

This project uses **bd (beads)** for issue tracking. See [bd prime] for
full workflow context.

The `djha-skin-common-lisp` skill lives in
`.agents/skills/djha-skin-common-lisp/` and covers project setup, development
workflow, and style guidelines for Common Lisp code in this repo. Run `bd prime` for full workflow context.

> **Architecture in one line:** Issues live in a local Dolt database
> (`.beads/dolt/`); cross-machine sync uses `bd dolt push/pull` (a
> git-compatible protocol), stored under `refs/dolt/data` on your git
> remote — separate from `refs/heads/*` where your code lives.
> `.beads/issues.jsonl` is a passive export, not the wire protocol.
>
> See [SYNC_CONCEPTS.md](https://github.com/gastownhall/beads/blob/main/docs/SYNC_CONCEPTS.md)
> for the one-screen overview and anti-patterns (don't treat JSONL as the
> source of truth; don't `bd import` during normal operation; don't
> reach for third-party Dolt hosting before trying the default).

## Quick Reference

```bash
bd ready              # Find available work
bd show <id>          # View issue details
bd update <id> --claim  # Claim work atomically
bd close <id>         # Complete work
bd dolt push          # Push beads data to remote
```

## Non-Interactive Shell Commands

**ALWAYS use non-interactive flags** with file operations to avoid hanging on
confirmation prompts.

```bash
cp -f source dest           # NOT: cp source dest
mv -f source dest           # NOT: mv source dest
rm -f file                  # NOT: rm file
rm -rf directory            # NOT: rm -r directory
cp -rf source dest          # NOT: cp -r source dest
```

## Beads Rules

- Use `bd` for ALL task tracking — do NOT use TodoWrite, TaskCreate, or markdown TODO lists
- Run `bd prime` for detailed command reference and session close protocol
- Use `bd remember` for persistent knowledge — do NOT use MEMORY.md files
- Beads must be granular — one per test file ported
- Each bead: port tests, make them pass, keep the app running the whole time
- Keep beads up to date; edit them if they disagree with guidelines

## Porting Guide: degasolv (Clojure) → dsolv (Common Lisp)

This project ports the **degasolv** dependency resolver from Clojure to Common Lisp as **dsolv**.

### Key Libraries

| Library | Purpose | Notes |
|---------|---------|-------|
| `com.djhaskin.cliff` (CLIFF) | CLI argument parsing, subcommand dispatch, config files, env vars, data-slurp | Use CLIFF's `data-slurp` and `base-slurp` instead of porting from degasolv |
| `com.djhaskin.nrdl` (NRDL) | Structured data serialization (replaces Clojure EDN) | Has `to-fset` to convert CL hash tables to fset data structures |
| `fset` | Persistent/functional data structures (replaces Clojure's immutable collections) | Use throughout for maps, seqs, sets |
| `alexandria` | General utility functions | Use for `copy-hash-table`, `alist-hash-table`, `hash-table-alist`, etc. |
| `serapeum` | Modern utility library | Use for missing CL idioms (e.g., `->`, `->>`, `assoc`, `dissoc`) |
| `cl-ppcre` | Regular expressions | Use for version spec parsing, URL matching |
| `parachute` | Unit testing framework | Use instead of Clojure's `deftest` |

### fset: Mapping Clojure to Common Lisp

Clojure ONLY has immutable (persistent) data structures. **fset** provides the same in CL.
Use fset everywhere the Clojure source uses Clojure's native data structures.

| Clojure | fset |
|---------|------|
| `{}` (empty map) | `(fset:empty-map)` |
| `[]` (empty vector) | `(fset:empty-seq)` |
| `#{}` (empty set) | `(fset:empty-set)` |
| `(assoc m k v)` | `(fset:with m k v)` |
| `(get m k)` | `(fset:lookup m k)` |
| `(dissoc m k)` | `(fset:less m k)` |
| `(conj coll v)` | `(fset:push-last coll v)` (seqs) or `(fset:with coll v)` (sets) |
| `(into coll1 coll2)` | `(fset:union coll1 coll2)` |
| `(empty? coll)` | `(fset:empty? coll)` |
| `(first coll)` | `(fset:first coll)` |
| `(rest coll)` | `(fset:tail coll)` |
| `(map f coll)` | `(fset:map f coll)` |
| `(reduce f init coll)` | `(fset:reduce f init coll)` |
| `(filter f coll)` | `(fset:filter f coll)` |
| `(some f coll)` | `(fset:some f coll)` |
| `(every f coll)` | `(fset:every f coll)` |
| `(set coll)` | `(fset:convert 'fset:set coll)` |
| `(into [] coll)` | `(fset:convert 'fset:seq coll)` |
| `(select f set)` | `(fset:filter f set)` |
| `(clojure.set/intersection a b)` | `(fset:intersection a b)` |
| `(merge-with f a b)` | Manual loop with `fset:do-map` |
| `(update-in m [k] f v)` | `(fset:with m k (f v (fset:lookup m k)))` |

### Clojure → CL Idiom Mapping

| Clojure | Common Lisp |
|---------|-------------|
| `(defn f [args] body)` | `(defun f (args) body)` |
| `(def x val)` | `(defparameter x val)` |
| `(let [a 1 b 2] body)` | `(let ((a 1) (b 2)) body)` |
| `(fn [x] body)` | `(lambda (x) body)` |
| `(fn name [x] ... (name ...))` | `(labels ((name (x) ... (name ...))) ...)` |
| `(-> x f1 f2)` | `(f2 (f1 x))` (use serapeum's `->` if desired) |
| `(partial f a b)` | `(lambda (x) (funcall f a b x))` |
| `(comp f g)` | `(lambda (x) (funcall f (funcall g x)))` |
| `(into {} coll)` | `(fset:convert 'fset:map coll)` |
| `(mapv f coll)` | `(fset:map f coll)` |
| `(filterv f coll)` | `(fset:filter f coll)` |
| `(some identity coll)` | `(fset:some #'identity coll)` |
| `(defprotocol P (f [this]))` | `(defgeneric f (obj))` with `defmethod` or just use lambdas |
| `(extend-protocol P nil ...)` | Handle nil in the lambda body |
| `(fnil f default)` | `(lambda (x) (funcall f (or x default)))` |

### Structs for Records (not CLOS classes)

Use `defstruct` for all data records (like Clojure's `defrecord`). This matches
Clojure's struct semantics and is fast in all CL implementations:

```lisp
(defstruct (version-predicate (:conc-name vp-))
  (relation nil :type (or null keyword))
  (version "" :type string))
```

Use `defgeneric` + `defmethod` only when methods dispatch on multiple types
(Clojure's `defprotocol`). Otherwise, use lambdas for simple polymorphism.

### Writing Tests with parachute

```lisp
(define-test my-test
  :parent nil
  (is = 1 1)
  (is string= "hello" "hello")
  (true (evenp 2))
  (false (oddp 2)))
```

- Each test file gets its own package: `com.djhaskin.dsolv/tests/<name>`
- Import `define-test`, `true`, `false`, `is`, `isnt`, `finish`, `test`
- Tests should be `:parent nil` (standalone) unless explicitly hierarchical
- See `~/Code/third/parachute` for examples

### Script Tests

- Script tests call `./dsolv` (the local binary)
- CLIFF-compatible argument format: `--set-*`, `--add-*`, `--enable-*`/`--disable-*`
- Helper scripts live in `test/resources/scripts/`
- Test data lives in `test/resources/data/`
- Scripts use `DSOLV_` env vars (not `DEGASOLV_`)

### Porting Order (Chronological by creation date)

**Unit tests:**

1. `core_test.clj` (2016-02-02) — basic resolver, version specs, present packages, retrieval
2. `util/core_test.clj` (2017-01-11) — utility functions
3. `data_spec_test.clj` (2017-01-14) — version ranges, comparisons
4. `interesting_cases_test.clj` (2017-01-16) — managed deps, implied deps
5. `string_to_req_test.clj` (2017-01-28) — string-to-requirement parsing
6. `repo_aggregation_test.clj` (2017-01-31) — priority-repo, global-repo
7. `apt_test.clj` (2017-03-25) — deb-to-degasolv-requirements, apt-repo
8. `conflict_strat_test.clj` (2017-06-10) — inclusive, prioritized conflict strategies
9. `disable_alternatives_test.clj` (2017-06-20) — allow-alternatives option
10. `auxiliary_funcs_test.clj` (2017-06-23) — hoisting, repo query count
11. `performance_test.clj` (2017-06-23) — pruning, repo query count
12. `unsuccessful_test.clj` (2017-07-22) — unsuccessful resolution cases
13. `search_strat_test.clj` (2017-08-16) — depth-first, breadth-first
14. `subproc_test.clj` (2018-01-06) — subprocess package system
15. `list_packages_test.clj` (2018-01-17) — list-packages function
16. `version_suggestions.clj` (2019-08-28) — version suggestion

**Script tests:**

1. `test-apt` (2017-05-11)
2. `test-option-packs` (2017-07-03)
3. `test-env-vars` (2017-08-17)
4. `test-output-format` (2017-08-17)
5. `test-query-output-format` (2017-08-17)
6. `test-search-strat` (2017-08-17)
7. `test-version-comparison` (2017-08-17)
8. `test-meta` (2017-11-15)
9. `test-all` (2017-12-23)
10. `test-subproc` (2018-01-04)
11. `test-list-strat` (2018-01-17)
12. `test-json-config` (2018-01-18)
13. `test-index-sort-order` (2019-10-29)
14. `test-install-graph` (2020-06-23)

### Process for Each Test

1. Create a bead for the test
2. Port the Clojure test → parachute (unit) or bash (script)
3. Have a subagent audit the port for correctness
4. Get the test to pass
5. Close the bead
6. `bd dolt push` to sync progress

### Build & Run

* **Building the executable**:

```bash
# Build executable
ros build com.djhaskin.dsolv.ros
```

* **Running tests**: Use `cl-mcp` mcp server to run `(asdf:test-system ...)`.
  NEVER use bash (`ros` or `sbcl`) to call lisp stuff (EXCEPT to call `ros
  build`). It often ends with the process dropping into the debugger and hosing
  the session.

### Critical Rules

- Use `cl-mcp` for ALL Lisp interaction — loading, editing, paren-checking, evaling
- Rewrite from scratch with fset rather than patching hash-table code
- `defstruct` for records, `defgeneric` only when needed for type dispatch
- CLIFF has `data-slurp`/`base-slurp` — don't port those from degasolv
- Use alexandria and serapeum for utility functions
- fset operations are your primary data structure tools

## Session Completion

When ending a work session, you MUST complete ALL steps below:

1. **File issues** for remaining work
2. **Run quality gates** — tests, linters, builds
3. **Update issue status** — close finished, update in-progress
4. **PUSH TO REMOTE** — mandatory:
   ```bash
   git pull --rebase
   git push
   git status  # MUST show "up to date with origin"
   ```
5. **Clean up** — clear stashes, prune remote branches
6. **Verify** — all changes committed AND pushed
7. **Hand off** — provide context for next session

**CRITICAL:** Work is NOT complete until `git push` succeeds. NEVER stop before pushing.