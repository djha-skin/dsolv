;;;; tests/conflict-strat.lisp
;;;;
;;;; Ported from degasolv/test/degasolv/resolver/conflict_strat_test.clj
;;;;
;;;; Tests for the :conflict-strat keyword of resolve-dependencies:
;;;;   :inclusive   → include ALL matching versions (no conflict rejection)
;;;;   :prioritized → prefer already-found packages, reject alternatives
;;;;
;;;; Two scenarios:
;;;;   1. conflict-strats-simple — a requires (b==0.6.0) OR (c==0.2.3);
;;;;      c requires b==0.5.0.  :inclusive includes both b versions,
;;;;      :prioritized picks one.
;;;;   2. inclusive-should-use-previously-found-stuff — same setup but
;;;;      c requires b>=0.5.0; :inclusive reuses the already-found b@0.6.0.

(defpackage #:com.djhaskin.dsolv/tests/conflict-strat
  (:use #:cl)
  (:import-from #:com.djhaskin.dsolv/resolver
    #:resolve-dependencies
    #:present
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
    #:false)
  (:local-nicknames
    (#:f #:fset)))

(in-package #:com.djhaskin.dsolv/tests/conflict-strat)

;;; ─── Helpers ────────────────────────────────────────────────────────────────

(defun make-pkg (id version location &optional requirements)
  "Create a package-info struct."
  (make-package-info
    :id id
    :version version
    :location location
    :requirements (or requirements nil)))

(defun successful-result (result)
  "Extract the packages list from a successful result."
  (when (and (listp result) (eql (first result) :successful))
    (second result)))

(defun unsuccessful-p (result)
  "Check if the result is unsuccessful."
  (and (listp result) (eql (first result) :unsuccessful)))

(defun make-repo-map (entries)
  "Create an fset map from a list of (id . packages) associations."
  (let ((m (f:empty-map)))
    (dolist (entry entries)
      (destructuring-bind (id . packages) entry
        (let ((seq (f:empty-seq)))
          (dolist (pkg packages)
            (setf seq (f:push-last seq pkg)))
          (setf m (f:with m id seq)))))
    m))

(defun present-spec (id &rest version-predicates)
  "Create a present clause with optional version predicates.
  Usage: (present-spec \"a\" vp1 vp2)  → clause requiring 'a' matching ALL predicates
         (present-spec \"a\")           → clause requiring 'a' at any version"
  (list (present id
                 (when version-predicates
                   (list version-predicates)))))

(defun locations-set (packages)
  "Extract the set of locations from a list of packages."
  (fset:convert 'fset:set
                (mapcar #'pi-location packages)))

;;; ─── Test data ──────────────────────────────────────────────────────────────
;;;
;;; Package a@1.0 requires (b==0.6.0) OR (c==0.2.3) — two alternatives.
;;; Package b@0.5.0 has no requirements.
;;; Package b@0.6.0 has no requirements.
;;; Package c@0.2.3 requires b==0.5.0 (test 1) or b>=0.5.0 (test 2).

(defparameter *pkg-a*
  (make-pkg "a" "1.0" "http://example.com/repo/a-1.0.zip"
            (list (present-spec "b"
                    (make-version-predicate :relation :equal-to :version "0.6.0"))
                  (present-spec "c"
                    (make-version-predicate :relation :equal-to :version "0.2.3")))))

(defparameter *pkg-b05*
  (make-pkg "b" "0.5.0" "http://example.com/repo/b-0.5.0.zip" nil))

(defparameter *pkg-b06*
  (make-pkg "b" "0.6.0" "http://example.com/repo/b-0.6.0.zip" nil))

(defparameter *pkg-c-equal*
  (make-pkg "c" "0.2.3" "http://example.com/repo/c-0.2.3.zip"
            (list (present-spec "b"
                    (make-version-predicate :relation :equal-to :version "0.5.0")))))

(defparameter *pkg-c-gte*
  (make-pkg "c" "0.2.3" "http://example.com/repo/c-0.2.3.zip"
            (list (present-spec "b"
                    (make-version-predicate :relation :greater-equal :version "0.5.0")))))

(defparameter *repo-equal*
  (make-repo-map
    (list (cons "a" (list *pkg-a*))
          (cons "b" (list *pkg-b05* *pkg-b06*))
          (cons "c" (list *pkg-c-equal*)))))

(defparameter *repo-gte*
  (make-repo-map
    (list (cons "a" (list *pkg-a*))
          (cons "b" (list *pkg-b05* *pkg-b06*))
          (cons "c" (list *pkg-c-gte*)))))

(defparameter *query-equal* (map-query *repo-equal*))
(defparameter *query-gte* (map-query *repo-gte*))

;;; ─── Tests: Conflict strategies ─────────────────────────────────────────────

(define-test conflict-strats-simple
  :parent nil
  "A simple test of the inclusive and prioritized conflict strategies.

   With :conflict-strat :inclusive, the resolver includes ALL matching
   versions of packages it finds, even across alternatives.  Here a@1.0
   requires (b==0.6.0) OR (c==0.2.3), and c@0.2.3 requires b==0.5.0.
   The inclusive result should include a, b@0.6.0, b@0.5.0, and c@0.2.3.

   With :conflict-strat :prioritized, the resolver picks the first
   alternative and rejects others.  The result should include a, b@0.6.0,
   and c@0.2.3 but NOT b@0.5.0."
  (let ((inclusive-result (resolve-dependencies
                            (list (present-spec "a"))
                            *query-equal*
                            :compare #'maven-vercmp
                            :conflict-strat :inclusive))
        (prioritized-result (resolve-dependencies
                              (list (present-spec "a"))
                              *query-equal*
                              :compare #'maven-vercmp
                              :conflict-strat :prioritized)))
    ;; :inclusive should include all 4 packages
    (true (successful-result inclusive-result))
    (let ((inclusive-locs (locations-set (successful-result inclusive-result))))
      (true (fset:member?
              "http://example.com/repo/a-1.0.zip" inclusive-locs))
      (true (fset:member?
              "http://example.com/repo/b-0.6.0.zip" inclusive-locs))
      (true (fset:member?
              "http://example.com/repo/b-0.5.0.zip" inclusive-locs))
      (true (fset:member?
              "http://example.com/repo/c-0.2.3.zip" inclusive-locs)))
    ;; :prioritized should include 3 packages (no b@0.5.0)
    (true (successful-result prioritized-result))
    (let ((prioritized-locs (locations-set (successful-result prioritized-result))))
      (true (fset:member?
              "http://example.com/repo/a-1.0.zip" prioritized-locs))
      (true (fset:member?
              "http://example.com/repo/b-0.6.0.zip" prioritized-locs))
      (true (fset:member?
              "http://example.com/repo/c-0.2.3.zip" prioritized-locs))
      ;; b@0.5.0 should NOT be in the prioritized result
      (true (not (fset:member?
                   "http://example.com/repo/b-0.5.0.zip" prioritized-locs))))))

;;; ─── Tests: Inclusive reuses previously found packages ──────────────────────

(define-test inclusive-should-use-previously-found-stuff
  :parent nil
  "With :conflict-strat :inclusive, if a package matching criteria is
   already found, reuse it rather than fetching a new version.

   Here c@0.2.3 requires b>=0.5.0.  The resolver first finds b@0.6.0
   for the first alternative (b==0.6.0).  When it explores the second
   alternative (c@0.2.3) and c requires b>=0.5.0, the already-found
   b@0.6.0 satisfies this requirement, so b@0.5.0 is not needed.

   Result should include a, b@0.6.0, and c@0.2.3 (3 packages)."
  (let ((result (resolve-dependencies
                  (list (present-spec "a"))
                  *query-gte*
                  :compare #'maven-vercmp
                  :conflict-strat :inclusive)))
    (true (successful-result result))
    (let ((locs (locations-set (successful-result result))))
      (true (fset:member?
              "http://example.com/repo/a-1.0.zip" locs))
      (true (fset:member?
              "http://example.com/repo/b-0.6.0.zip" locs))
      (true (fset:member?
              "http://example.com/repo/c-0.2.3.zip" locs))
      ;; b@0.5.0 should NOT be in the result because b@0.6.0
      ;; (already found) satisfies c's b>=0.5.0 requirement
      (true (not (fset:member?
                   "http://example.com/repo/b-0.5.0.zip" locs))))))