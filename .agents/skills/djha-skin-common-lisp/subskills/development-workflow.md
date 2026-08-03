---
name: development-workflow
sub-of: djha-skin-common-lisp
description: >
  Skill that describes a typical development working session in
  Common Lisp, Dan Haskin style.
---

# Development Workflow

These steps assume you have the repository cloned out and that you are in that
folder.

## Steps

1. Ask the user for instructions on what to build.

2. Ask the user several questions, at least five, about specifics of what is
   needed. Try to make sure you're not guessing at features.

3. Write a bunch of beads using the beads MCP server to capture what the user
   said.

4. Pick up each bead and work it in turn. As you work each bead, heavily employ
   TDD. Check your work often by spinning up subagents to audit both your work,
   your tests, and your commits.

5. Spin up a subagent to validate your commits against the brief in the beads,
   and offer any helpful critiques. Have them point out any critical bugs or how
   the code could break.

6. Address the concerns.

7. For each bead, commit and push work done per the above workflow.

## Tooling

Bear in mind, we use the following tools:

* **cl-mcp MCP server** — Use the `cl-mcp` MCP server for ALL Lisp operations:
  loading systems, running tests, editing forms, checking parens, searching
  code. Do NOT use one-off `sbcl` or `ros` commands — the MCP server manages
  the Lisp image and provides structure-aware tools (`lisp-edit-form`,
  `lisp-patch-form`, `lisp-read-file`, `repl-eval`, `run-tests`,
  `lisp-check-parens`, `code-find`, `code-describe`, `code-find-references`).

* **OCICL** for package management. Run `ocicl install` to install all systems
  listed in `ocicl.csv`. Systems are downloaded project-locally. Do NOT use
  Qlot (`qlot`) or Quicklisp (`ql:quickload`).

* Roswell. `ros init` to make roswell scripts, `ros build` to build executables.
  Dependencies are resolved via OCICL, not Qlot.

* For testing, use `run-tests` from the cl-mcp server (which calls
  `(asdf:test-system "com.djhaskin.<name-of-the-repository>")` internally).