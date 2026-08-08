# Agent Instructions

## Beads Issue Tracking

This project uses **bd (beads)** for issue tracking. See [bd prime] for
full workflow context.

The `djha-skin-common-lisp` skill lives in
`.agents/skills/djha-skin-common-lisp/` and covers project setup, development
workflow, and style guidelines for Common Lisp code in this repo. Run `bd prime` for full workflow context.

> Issues live in the local Dolt database (`.beads/dolt/`); sync Beads with `bd dolt push`.

## Quick Reference

```bash
bd ready
bd show <id>
bd update <id> --claim
bd close <id>
bd dolt push
```

## Non-Interactive File Operations

Always use non-interactive flags with shell file operations.

## Common Lisp Porting Rules

### FSet and gmap

Use FSet persistent data structures for resolver data. **Do not use `fset:map`
for ordinary iteration over sequences**. `fset:map` operates on map/pair data;
use the `gmap` generalized mapping facility for sequence mapping and collection
transforms.

Read the authoritative documentation before changing mapping code:
https://fset.common-lisp.dev/Modern-CL/Top_html/GMap.html

Examples:

```lisp
(gmap (:result fset:seq) #'function-to-call
      (:arg fset:seq input-seq))

(gmap (:result list) #'function-to-call
      (:arg fset:seq input-seq))

(gmap (:result fset:seq) #'cons
      (:arg fset:seq seq1)
      (:arg fset:seq seq2))

(gmap (:result list) #'list
      (:arg fset:map input-map))
```

`fset:do-map` is appropriate for direct traversal of an FSet map when the
operation is not a generalized mapping transform. Use `gmap` for mapping
values, mapping sequences, and converting collection results.

### Other rules

- Use `defstruct` for data records.
- Use `cl-mcp` for Lisp interaction, loading, editing, testing, and builds.
- Use CLIFF `data-slurp`/`base-slurp`; do not reimplement them.
- Use Parachute for tests.

## CL-MCP Inclusions

@~/common-lisp/cl-mcp/prompts/repl-driven-development.md


## Build and Test

Run Lisp through cl-mcp. Build the executable with:

```bash
ros build com.djhaskin.dsolv.ros
```

## Session Completion

Before ending a session, update Beads, run quality gates, commit changes, push
to the remote, and verify the working tree is synchronized.
