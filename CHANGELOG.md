# Changelog

All notable changes to this project will be documented in this file.

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