;;;; src/pkgsys/subproc.lisp
;;;;
;;;; Subprocess package system: calling external executables to get
;;;; package data.
;;;;
;;;; Ported from degasolv's cli-src/degasolv/pkgsys/subproc.clj

(defpackage #:com.djhaskin.dsolv/pkgsys/subproc
  (:use #:cl)
  (:import-from #:com.djhaskin.dsolv/resolver
    #:package-info #:pi-id #:pi-version #:pi-location #:pi-requirements
    #:make-package-info #:string-to-requirement)
  (:import-from #:com.djhaskin.dsolv/util
    #:map-query)
  (:import-from #:fset)
  (:local-nicknames
    (#:f #:fset)
    (#:resolver #:com.djhaskin.dsolv/resolver))
  (:export
    #:convert-input
    #:make-slurper))

(in-package #:com.djhaskin.dsolv/pkgsys/subproc)

(defun convert-input (raw-repo-info)
  "Convert raw repository info (from external executable) to an fset map."
  (declare (ignore raw-repo-info))
  (error "convert-input not yet implemented"))

(defun make-slurper (options)
  "Create a repository slurper function that calls an external executable."
  (declare (ignore options))
  (error "make-slurper not yet implemented"))