;;;; src/pkgsys/git.lisp
;;;;
;;;; Git package system integration. Currently a stub for future
;;;; implementation.

(defpackage #:com.djhaskin.dsolv/pkgsys/git
  (:use #:cl)
  (:import-from #:com.djhaskin.dsolv/util)
  (:import-from #:com.djhaskin.dsolv/resolver)
  (:local-nicknames
    (#:util #:com.djhaskin.dsolv/util)
    (#:resolver #:com.djhaskin.dsolv/resolver))
  (:export
    #:make-query))

(in-package #:com.djhaskin.dsolv/pkgsys/git)

(defun make-query (options)
  "Create a git-based repository query function.

  Currently unimplemented. Will clone git repositories and extract
  package metadata."
  (declare (ignore options))
  nil)
