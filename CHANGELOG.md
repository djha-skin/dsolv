# Changelog

All notable changes to this project will be documented in this file.

## [0.2.0] - 2026-08-09

### Added

- Install-graph output fidelity: JSON and NRDL install-graph entries (and the
  JSON packages list) now include each package's `requirements` and `metadata`
  fields, matching legacy degasolv. Present packages serialize `requirements`
  as JSON `null` while resolved packages with no dependencies serialize `[]`
  (degasolv's Clojure distinguishes `nil` from `[]`; dsolv uses the
  `"already present"` location to tell them apart, since the empty list is
  `nil` in Common Lisp). The `test-install-graph` fixtures were regenerated
  and verified equal to degasolv's expected output (modulo map iteration
  order).
- `scripts/build`: builds the executable with `dynamic-space-size=8192` and
  `control-stack-size=64` baked into the dumped image (overridable via
  `DYNAMIC_SPACE_SIZE`/`CONTROL_STACK_SIZE`). Replaces the ad-hoc `ros build`
  invocations that previously lost the heap/stack settings.
- No-regex APT parser: `slurp-apt-repo` now parses `Packages.gz` with simple
  line-based operations — records cut at blank lines, key/value pairs split at
  the first colon — instead of regular expressions. This fixes heap exhaustion
  on the 19 MB Ubuntu index (regex-heavy parsing was why the original degasolv
  took ~40 minutes and moved to the same string-based approach). Also adds
  `file://` repository URL support so local index files work.
- Implement the `subproc` package system slurper (`make-slurper` in
  `src/pkgsys/subproc.lisp`): runs an external executable, parses its JSON or
  NRDL output, and converts it into resolver query functions. Includes tests
  for JSON/NRDL parsing, non-zero exit handling, and unknown output formats.
- Regression tests for the `generate-repo-index` → `slurp-degasolv-repo` round
  trip (`tests/pkgsys-core.lisp`): generated indexes decode into sorted
  `package-info` structs with requirements intact.

### Fixed

- Fix config-file merge precedence in `setup-function`: user-specified
  `-c`/`-j` config files now override one another (later files win, matching
  degasolv's `reduce merge`) while CLI/env values still take precedence.
  Previously a value set by an earlier config file appeared "CLI-set" and
  blocked later config files from overriding it (test-env-vars).
- Parsed packages with no requirements now carry an empty requirement list
  (legacy degasolv `PackageInfo` defaults to `[]`), so JSON output can
  distinguish present packages (`null`) from resolved packages without
  dependencies (`[]`).
- `test-meta` now uses `set -e` so a failed assertion fails the script
  instead of being masked by the final command's exit status. `test-apt`
  keeps the original's `set -x`-only semantics: its middle
  `--disable-alternatives` invocations are expected to fail because
  `update-manager-gnome` is absent from the fixture indexes (the original
  degasolv behaves identically, and the script's exit status is the last
  invocation's).
- Fix `generate-repo-index` card decoding (dsolv-ixz): generated repository

- Fix `generate-repo-index` card decoding (dsolv-ixz): generated repository
  indexes now store `package-info` structs instead of raw fset maps, so index
  sort orders and downstream decoders (`slurp-degasolv-repo`) no longer crash
  on non-`package-info` entries. Index entries with no requirements serialize
  as `requirements []` (and requirement specs as `spec []`) rather than
  `false`, keeping list-typed fields type-stable in the NRDL.
- Fix `query-repo` exit status: the subcommand always returned `:successful`;
  a query with no results now exits non-zero (65, `:data-format-error`) as the
  legacy degasolv did, and the error-format output includes a `packages` field.
- Validate strategy options in `resolve-locations-fn`: `--search-strat`,
  `--conflict-strat`, `--list-strat`, `--resolve-strat`, and `--index-strat`
  reject values other than the legacy degasolv choices with a clean
  `:general-error` message (CLIFF has no validation hook, so the checks run in
  the subcommand).
- Fix `resolve-locations` exit status: misbalanced parens in
  `resolve-locations-fn` left the success result map orphaned, so every
  successful resolution fell through to the error path and exited with status
  71 (`:system-error`). Successful resolutions now exit 0.
- Register the `subproc` package system's required argument under the keyword
  `:subproc-exe` (was the string `"subproc-exe"`), so the CLI's
  required-argument check works.

## [0.1.0] - 2025-07-31

### Added

- Initial port of the degasolv dependency resolver from Clojure to Common Lisp.
- Core resolver data structures and algorithm: `package-info`, `requirement`,
  `version-predicate`, `decorated-requirement` structs, string-to-requirement
  parser, version comparison, spec-calling, and the full
  `resolve-dependencies-deluxe` algorithm with all strategies (thorough/fast,
  exclusive/inclusive/prioritized, breadth-first/depth-first).
- CLI interface using CLIFF with subcommands: `display-config`,
  `generate-card`, `generate-repo-index`, `resolve-locations`, `query-repo`.
- Utility functions: `data-slurp` (HTTP/file/stdin), `base-slurp`,
  `default-spit`, `pretty-spit`, `map-query`, `priority-repo`, `global-repo`,
  `aggregator`.
- Package system modules:
  - Core: `read-card`, `generate-repo-index`, `slurp-degasolv-repo`.
  - APT: `slurp-apt-repo` for Debian-style Packages.gz parsing.
  - Git: stub for future git-based repository support.
  - Subproc: `make-slurper` for external executable integration.
- ASDF system definition (`com.djhaskin.dsolv.asd`) with all dependencies.
- Roswell script (`com.djhaskin.dsolv.ros`) for executable building.
- Quicklisp dependency management via `qlfile` and `qlot`.