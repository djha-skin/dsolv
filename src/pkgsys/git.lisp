;;;; src/pkgsys/git.lisp
;;;;
;;;; Git package system: cloning git repos and extracting package info.
;;;;
;;;; Ported from degasolv's cli-src/degasolv/pkgsys/git.clj

(defpackage #:com.djhaskin.dsolv/pkgsys/git
  (:use #:cl)
  (:export
    #:make-query))

(in-package #:com.djhaskin.dsolv/pkgsys/git)

(defun make-query (options)
  "Return NIL for the intentionally unimplemented Git package system.

OPTIONS is accepted for compatibility with package-system query
constructors.  The corresponding Clojure function has no body, so this
port preserves its no-op, NIL-returning behavior rather than inventing
Git repository semantics."
  (declare (ignore options))
  nil)
