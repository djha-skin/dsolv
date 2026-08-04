* Always use the `cl-mcp` mcp server to interact with common lisp. Never run `ros`
  or `sbcl` directly. That server might show up as named `127_0_0_1_12345_mcp` in
  the current session.

* Always track your work with beads. Create them prolifically, claim them, work
  them, update them, and push them using `bd dolt` to the same remote as the
  current repository.

* We're using the included `djha-skin-common-lisp` skill in the `.agents`
 folder to port degasolv from Clojure to Common Lisp and naming the port
 `dsolv`.

* If you get stuck, use the included `debbuging-9-rules` skill to debug your way
  out.