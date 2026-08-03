;;;; src/main.lisp
;;;;
;;;; CLI entry point for dsolv using CLIFF for argument parsing,
;;;; subcommand dispatch, config files, and environment variables.
;;;;
;;;; Ported from degasolv's cli-src/degasolv/cli.clj

(defpackage #:com.djhaskin.dsolv
  (:use #:cl)
  (:import-from #:com.djhaskin.cliff)
  (:import-from #:com.djhaskin.dsolv/resolver)
  (:import-from #:com.djhaskin.dsolv/pkgsys/core)
  (:import-from #:com.djhaskin.dsolv/pkgsys/apt)
  (:import-from #:com.djhaskin.dsolv/pkgsys/git)
  (:import-from #:com.djhaskin.dsolv/pkgsys/subproc)
  (:export #:main))

(in-package #:com.djhaskin.dsolv)

(defun main ()
  "Entry point for the dsolv CLI tool."
  (format t "dsolv: dependency resolver~%")
  (format t "CLI not yet implemented. Use CLIFF for argument parsing.~%"))