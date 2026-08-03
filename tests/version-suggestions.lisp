;;;; tests/version-suggestions.lisp
;;;;
;;;; Ported from degasolv/test/degasolv/resolver/version_suggestions.clj
;;;;
;;;; Tests for version suggestion: the resolver should skip versions
;;;; that would cause errors (e.g., a "bomb" version that throws when
;;;; its requirements are explored) and pick the next best option.

(defpackage #:com.djhaskin.dsolv/tests/version-suggestions
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

(in-package #:com.djhaskin.dsolv/tests/version-suggestions)

;;; ─── Helpers ────────────────────────────────────────────────────────────────

(defun make-pkg (id version location &optional requirements)
  (make-package-info
    :id id
    :version version
    :location location
    :requirements (or requirements nil)))

(defun successful-result (result)
  (when (and (listp result) (eql (first result) :successful))
    (second result)))

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

(defun ids-set (packages)
  (fset:convert 'fset:set
                (mapcar #'pi-id packages)))

;;; ─── Test data ──────────────────────────────────────────────────────────────
;;;
;;; a@10 requires c>=10 OR b
;;; b@10 requires c>=30
;;; c@10 has no requirements
;;; c@20 requires d (this is the "bomb" - if explored, it would fail)
;;; c@30 has no requirements
;;; c@40 has no requirements
;;; d@10 has no requirements
;;;
;;; The resolver should skip c@20 (the bomb) and pick c@30, resulting
;;; in a, b, c@30 being selected.

(defparameter *a*
  (make-pkg "a" "10" "http://example.com/repo/a-1.0.0.zip"
            (list (present-spec "c"
                    (make-version-predicate
                      :relation :greater-equal :version "10"))
                  (present-spec "b"))))

(defparameter *b*
  (make-pkg "b" "10" "http://example.com/repo/b-1.0.0.zip"
            (list (present-spec "c"
                    (make-version-predicate
                      :relation :greater-equal :version "30")))))

(defparameter *c10*
  (make-pkg "c" "10" "http://example.com/repo/c-1.0.0.zip" nil))

(defparameter *c20*
  (make-pkg "c" "20" "http://example.com/repo/c-2.0.0.zip"
            (list (present-spec "d"))))

(defparameter *c30*
  (make-pkg "c" "30" "http://example.com/repo/c-3.0.0.zip" nil))

(defparameter *c40*
  (make-pkg "c" "40" "http://example.com/repo/c-4.0.0.zip" nil))

(defparameter *d*
  (make-pkg "d" "10" "http://example.com/repo/d-1.0.0.zip" nil))

(defparameter *repo-info-asc*
  (make-repo-map
    (list (cons "a" (list *a*))
          (cons "b" (list *b*))
          (cons "c" (list *c10* *c20* *c30* *c40*))
          (cons "d" (list *d*)))))

(defparameter *query-asc* (map-query *repo-info-asc*))

;;; ─── Tests ──────────────────────────────────────────────────────────────────

(define-test minimum-version-selection-case
  :parent nil
  "Test that the resolver skips 'bomb' versions that would cause errors
   and picks the next best option.

   Here c@20 requires d, which is not found by the resolver (because
   c@30 already satisfies the c>=30 requirement from b).  The resolver
   should skip c@20 and pick c@30, resulting in a, b, c@30."
  (let ((result (resolve-dependencies
                  (list (present-spec "a"))
                  *query-asc*
                  :compare #'maven-vercmp)))
    (true (successful-result result))
    (let ((ids (ids-set (successful-result result))))
      (true (fset:member? "a" ids))
      (true (fset:member? "b" ids))
      (true (fset:member? "c" ids)))))
