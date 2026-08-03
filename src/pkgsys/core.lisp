;;;; src/pkgsys/core.lisp
;;;;
;;;; Degasolv package system: reading .dscard files and generating
;;;; repository indices.
;;;;
;;;; Ported from degasolv's cli-src/degasolv/pkgsys/core.clj

(defpackage #:com.djhaskin.dsolv/pkgsys/core
  (:use #:cl)
  (:import-from #:com.djhaskin.dsolv/resolver
    #:package-info #:pi-id #:pi-version #:pi-location #:pi-requirements
    #:make-package-info)
  (:import-from #:com.djhaskin.dsolv/util
    #:map-query)
  (:import-from #:com.djhaskin.cliff
    #:data-slurp
    #:base-slurp)
  (:import-from #:com.djhaskin.nrdl)
  (:import-from #:fset)
  (:local-nicknames
    (#:f #:fset)
    (#:nrdl #:com.djhaskin.nrdl)
    (#:resolver #:com.djhaskin.dsolv/resolver))
  (:export
    #:generate-repo-index!
    #:slurp-degasolv-repo))

(in-package #:com.djhaskin.dsolv/pkgsys/core)

(defun generate-repo-index! (search-directory index-file &key add-to sortindex)
  "Generate a repository index from .dscard files in SEARCH-DIRECTORY."
  (declare (ignore search-directory index-file add-to sortindex))
  (error "generate-repo-index! not yet implemented"))

(defun slurp-degasolv-repo (url)
  "Read a degasolv repository from URL, returning a list of query functions."
  (declare (ignore url))
  (error "slurp-degasolv-repo not yet implemented"))