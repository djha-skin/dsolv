;;;; src/pkgsys/git.lisp
;;;;
;;;; Git package system: cloning git repos and extracting package info.
;;;;
;;;; Ported from degasolv's cli-src/degasolv/pkgsys/git.clj

(defpackage #:com.djhaskin.dsolv/pkgsys/git
  (:use #:cl)
  (:import-from #:com.djhaskin.dsolv/resolver)
  (:import-from #:fset)
  (:local-nicknames
    (#:f #:fset)
    (#:resolver #:com.djhaskin.dsolv/resolver))
  (:export
    #:make-query))

(in-package #:com.djhaskin.dsolv/pkgsys/git)

(defun make-query (options)
  "Create a repository query function from git options."
  (declare (ignore options))
  (error "make-query not yet implemented"))