# Changelog

All notable changes to this project will be documented in this file.

## [Unreleased]

### Added

- Implement the `subproc` package system slurper (`make-slurper` in
  `src/pkgsys/subproc.lisp`): runs an external executable, parses its JSON or
  NRDL output, and converts it into resolver query functions. Includes tests
  for JSON/NRDL parsing, non-zero exit handling, and unknown output formats.
- Regression tests for the `generate-repo-index` → `slurp-degasolv-repo` round
  trip (`tests/pkgsys-core.lisp`): generated indexes decode into sorted
  `package-info` structs with requirements intact.

### Fixed

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