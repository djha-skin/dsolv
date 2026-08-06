;;;; tests/git.lisp
;;;;
;;;; Tests for the intentionally stubbed Git package system.
;;;;
;;;; The corresponding Clojure make-query function has no body.  This port
;;;; preserves that behavior by accepting package-system options and returning
;;;; NIL instead of inventing Git repository semantics.

(defpackage #:com.djhaskin.dsolv/tests/git
  (:use #:cl)
  (:import-from #:com.djhaskin.dsolv/pkgsys/git
    #:make-query)
  (:import-from #:parachute
    #:define-test
    #:true))

(in-package #:com.djhaskin.dsolv/tests/git)

(define-test make-query-is-an-intentional-no-op
  :parent nil
  "The upstream Git constructor is a stub and therefore returns NIL."
  (true (null (make-query nil)))
  (let ((options (make-hash-table :test 'equal)))
    (setf (gethash "clone-folder" options) "/tmp/dsolv-git-test")
    (true (null (make-query options)))))
