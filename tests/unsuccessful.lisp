;;;; tests/unsuccessful.lisp
;;;;
;;;; Ported from degasolv/test/degasolv/resolver/unsuccessful_test.clj
;;;;
;;;; Tests for unsuccessful resolution cases: package-not-found,
;;;; empty-alternative-set, and multiple unsuccessful results.

(defpackage #:com.djhaskin.dsolv/tests/unsuccessful
  (:use #:cl)
  (:import-from #:com.djhaskin.dsolv/resolver
    #:resolve-dependencies
    #:present
    #:absent
    #:make-version-predicate
    #:pi-id
    #:pi-version
    #:pi-location
    #:pi-requirements
    #:make-package-info)
  (:import-from #:com.djhaskin.dsolv/util
    #:map-query)
  (:import-from #:com.djhaskin.svers
    #:maven-vercmp)
  (:import-from #:fset)
  (:import-from #:parachute
    #:define-test
    #:true
    #:false
    #:is)
  (:local-nicknames
    (#:f #:fset)))

(in-package #:com.djhaskin.dsolv/tests/unsuccessful)

;;; ─── Helpers ────────────────────────────────────────────────────────────────

(defun make-pkg (id version location &optional requirements)
  (make-package-info
    :id id
    :version version
    :location location
    :requirements (or requirements nil)))

(defun successful-p (result)
  (and (listp result) (eql (first result) :successful)))

(defun unsuccessful-p (result)
  (and (listp result) (eql (first result) :unsuccessful)))

(defun make-repo-map (entries)
  (let ((m (f:empty-map)))
    (dolist (entry entries)
      (destructuring-bind (id . packages) entry
        (let ((seq (f:empty-seq)))
          (dolist (pkg packages)
            (setf seq (f:push-last seq pkg)))
          (setf m (f:with m id seq)))))
    m))

(defun present-spec (id &rest version-predicates)
  (list (present id
                 (when version-predicates
                   (list version-predicates)))))

;;; ─── Test data ──────────────────────────────────────────────────────────────
;;;
;;; Scenario 1: both-alts-bad
;;;   a@1.0.0 requires b OR c
;;;   b@2.0.0 requires d
;;;   c@3.0.0 requires d
;;;   d is NOT in the repo
;;;   → Both alternatives fail with :package-not-found for d
;;;
;;; Scenario 2: both-bs-bad
;;;   a@2.0.0 requires b
;;;   b@2.0.0 requires d
;;;   b@3.0.0 requires d
;;;   d is NOT in the repo
;;;   → Both b candidates fail with :package-not-found for d
;;;
;;; Scenario 3: empty-alternatives
;;;   a@2.0.0 requires [] (empty clause)
;;;   → Fails with :empty-alternative-set

(defparameter *b-alternative* (present "b"))
(defparameter *c-alternative* (present "c"))
(defparameter *d-alternative* (present "d"))
(defparameter *altclause* (list *b-alternative* *c-alternative*))
(defparameter *d-clause* (list *d-alternative*))
(defparameter *bclause* (list *b-alternative*))

(defparameter *a1*
  (make-pkg "a" "1.0.0" "http://example.com/repo/a-1.0.0.zip"
            (list *altclause*)))

(defparameter *a2*
  (make-pkg "a" "2.0.0" "http://example.com/repo/a-2.0.0.zip"
            (list *bclause*)))

(defparameter *a3*
  (make-pkg "a" "2.0.0" "http://example.com/repo/a-2.0.0.zip"
            (list nil)))

(defparameter *b2*
  (make-pkg "b" "2.0.0" "http://example.com/repo/b-2.0.0.zip"
            (list *d-clause*)))

(defparameter *b3*
  (make-pkg "b" "3.0.0" "http://example.com/repo/b-3.0.0.zip"
            (list *d-clause*)))

(defparameter *c3*
  (make-pkg "c" "3.0.0" "http://example.com/repo/c-3.0.0.zip"
            (list *d-clause*)))

(defparameter *both-alts-bad-repo*
  (make-repo-map
    (list (cons "a" (list *a1*))
          (cons "b" (list *b2*))
          (cons "c" (list *c3*)))))

(defparameter *both-bs-bad-repo*
  (make-repo-map
    (list (cons "a" (list *a2*))
          (cons "b" (list *b2* *b3*)))))

(defparameter *adeps-bad-repo*
  (make-repo-map
    (list (cons "a" (list *a3*)))))

(defparameter *alts-bad-query* (map-query *both-alts-bad-repo*))
(defparameter *bs-bad-query* (map-query *both-bs-bad-repo*))
(defparameter *adeps-bad-query* (map-query *adeps-bad-repo*))

;;; ─── Tests ──────────────────────────────────────────────────────────────────

(define-test unsuccessful-test
  :parent nil
  "Test various unsuccessful resolution scenarios.

   Three scenarios:
   1. both-alts-bad: a requires b OR c, both b and c require d (not found)
      → Two :package-not-found problems (one per alternative)
   2. both-bs-bad: a requires b, both b candidates require d (not found)
      → Two :package-not-found problems (one per candidate)
   3. empty-alternatives: a requires [] (empty clause)
      → :empty-alternative-set problem"
  ;; Scenario 1: Multiple unsuccessfuls across alternatives
  (let ((result (resolve-dependencies
                  (list (present-spec "a"))
                  *alts-bad-query*
                  :compare #'maven-vercmp)))
    (true (unsuccessful-p result)))
  ;; Scenario 2: Multiple unsuccessfuls across candidates
  (let ((result (resolve-dependencies
                  (list (present-spec "a"))
                  *bs-bad-query*
                  :compare #'maven-vercmp)))
    (true (unsuccessful-p result)))
  ;; Scenario 3: Empty alternatives
  (let ((result (resolve-dependencies
                  (list (present-spec "a"))
                  *adeps-bad-query*
                  :compare #'maven-vercmp)))
    (true (unsuccessful-p result))))
