;;;; tests/auxiliary-funcs.lisp
;;;;
;;;; Ported from degasolv/test/degasolv/resolver/auxiliary_funcs_test.clj
;;;;
;;;; Tests for the :strategy keyword of resolve-dependencies, which controls
;;;; candidate culling:
;;;;   :strategy :thorough  → identity (try all candidates)   = cull-nothing
;;;;   :strategy :fast      → keep only first candidate        = cull-all-but-first
;;;;
;;;; In the Clojure original, these were separate private functions
;;;; (cull-nothing and cull-all-but-first). In the CL port, they are
;;;; implemented as lambda expressions in resolve-dependencies-deluxe
;;;; controlled by the :strategy keyword.

(defpackage #:com.djhaskin.dsolv/tests/auxiliary-funcs
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

(in-package #:com.djhaskin.dsolv/tests/auxiliary-funcs)

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
  "Create a present clause with optional version predicates."
  (list (present id
                 (when version-predicates
                   (list version-predicates)))))

;;; ─── Test data ──────────────────────────────────────────────────────────────
;;;
;;; Package "a" has two versions:
;;;   - version "30" requires "c"
;;;   - version "20" has no requirements (clean)
;;;
;;; Package "c" has version "10" with no requirements.
;;; Package "d" has version "22" with no requirements.
;;;
;;; "d" and "c" conflict (via :conflicts).
;;;
;;; When requesting both "a" and "d":
;;;   - :strategy :thorough → tries a@30 (fails: needs c, conflicts with d)
;;;                           then backtracks to a@20 → succeeds
;;;   - :strategy :fast     → tries a@30 only → fails immediately

(defparameter *pkg-a30*
  (make-pkg "a" "30" "a_loc30"
            (list (present-spec "c"))))

(defparameter *pkg-a20*
  (make-pkg "a" "20" "a_loc20" nil))

(defparameter *pkg-c10*
  (make-pkg "c" "10" "c_loc10" nil))

(defparameter *pkg-d22*
  (make-pkg "d" "22" "d_loc22" nil))

(defparameter *repo-info*
  (make-repo-map
    (list (cons "a" (list *pkg-a30* *pkg-a20*))
          (cons "c" (list *pkg-c10*))
          (cons "d" (list *pkg-d22*)))))

(defparameter *query* (map-query *repo-info*))

;;; ─── Tests: :strategy :thorough (cull-nothing) ──────────────────────────────

(define-test strategy-thorough
  :parent nil
  "With :strategy :thorough, the resolver tries ALL candidates.
   When a@30 (requires c) conflicts with d, it backtracks to a@20.
   Equivalent to cull-nothing in the Clojure original."
  (let ((result (resolve-dependencies
                  (list (present-spec "a")
                        (present-spec "d"))
                  *query*
                  :strategy :thorough
                  :conflicts (f:with (f:empty-map) "c" (list nil))
                  :compare #'maven-vercmp)))
    ;; Should succeed by choosing a@20 (which has no requirements)
    (true (successful-result result))
    (true (find *pkg-a20* (successful-result result) :test #'equal))
    (true (find *pkg-d22* (successful-result result) :test #'equal))
    ;; a@30 should NOT be in the result (it requires c, which conflicts with d)
    (true (not (find *pkg-a30* (successful-result result) :test #'equal)))))

;;; ─── Tests: :strategy :fast (cull-all-but-first) ────────────────────────────

(define-test strategy-fast
  :parent nil
  "With :strategy :fast, the resolver tries ONLY the first candidate.
   When a@30 (requires c) conflicts with d, it fails immediately without
   trying a@20.
   Equivalent to cull-all-but-first in the Clojure original."
  (let ((result (resolve-dependencies
                  (list (present-spec "a")
                        (present-spec "d"))
                  *query*
                  :strategy :fast
                  :conflicts (f:with (f:empty-map) "c" (list nil))
                  :compare #'maven-vercmp)))
    ;; Should fail because a@30 is tried first and conflicts
    (true (unsuccessful-p result))))

;;; ─── Tests: Both strategies succeed when no conflict ────────────────────────

(define-test both-strategies-succeed-without-conflict
  :parent nil
  "When there is no conflict, both strategies produce the same result:
   a@30 (highest version) is chosen."
  (let ((thorough-result (resolve-dependencies
                           (list (present-spec "a"))
                           *query*
                           :strategy :thorough
                           :compare #'maven-vercmp))
        (fast-result (resolve-dependencies
                       (list (present-spec "a"))
                       *query*
                       :strategy :fast
                       :compare #'maven-vercmp)))
    (true (successful-result thorough-result))
    (true (successful-result fast-result))
    ;; Both should pick a@30 (first/highest version)
    (true (find *pkg-a30* (successful-result thorough-result) :test #'equal))
    (true (find *pkg-a30* (successful-result fast-result) :test #'equal))))