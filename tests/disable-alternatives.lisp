;;;; tests/disable-alternatives.lisp
;;;;
;;;; Ported from degasolv/test/degasolv/resolver/disable_alternatives_test.clj
;;;;
;;;; Tests for the :allow-alternatives option of resolve-dependencies.
;;;; When :allow-alternatives is false (nil), the resolver must not use
;;;; alternative packages to satisfy requirements.

(defpackage #:com.djhaskin.dsolv/tests/disable-alternatives
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

(in-package #:com.djhaskin.dsolv/tests/disable-alternatives)

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

(defun locations-set (packages)
  (fset:convert 'fset:set
                (mapcar #'pi-location packages)))

;;; ─── Test data ──────────────────────────────────────────────────────────────
;;;
;;; a@1.3.0 requires (d>=0.8.0) OR (c>=3.5.0)
;;; b@2.3.0 requires (c>=3.5.0) OR (d>=0.8.0)
;;; c@2.4.7 has no requirements
;;; d@0.8.0 has no requirements
;;;
;;; With :allow-alternatives false:
;;;   - Resolving "b>2.0.0" should fail because b requires c OR d,
;;;     but neither satisfies b>2.0.0 (only one version of b exists)
;;;   - Resolving "a>1.0.0" should succeed with a@1.3.0, d@0.8.0

(defparameter *pkg-a*
  (make-pkg "a" "1.3.0" "http://example.com/repo/a-1.3.0.zip"
            ;; One clause with two alternatives: (d>=0.8.0) OR (c>=3.5.0)
            (list (append (present-spec "d"
                          (make-version-predicate :relation :greater-equal :version "0.8.0"))
                        (present-spec "c"
                          (make-version-predicate :relation :greater-equal :version "3.5.0"))))))

(defparameter *pkg-b*
  (make-pkg "b" "2.3.0" "http://example.com/repo/b-2.3.0.zip"
            ;; One clause with two alternatives: (c>=3.5.0) OR (d>=0.8.0)
            (list (append (present-spec "c"
                          (make-version-predicate :relation :greater-equal :version "3.5.0"))
                        (present-spec "d"
                          (make-version-predicate :relation :greater-equal :version "0.8.0"))))))

(defparameter *pkg-c*
  (make-pkg "c" "2.4.7" "http://example.com/repo/c-2.4.7.zip" nil))

(defparameter *pkg-d*
  (make-pkg "d" "0.8.0" "http://example.com/repo/d-0.8.0.zip" nil))

(defparameter *repo-info*
  (make-repo-map
    (list (cons "a" (list *pkg-a*))
          (cons "b" (list *pkg-b*))
          (cons "c" (list *pkg-c*))
          (cons "d" (list *pkg-d*)))))

(defparameter *query* (map-query *repo-info*))

;;; ─── Tests ──────────────────────────────────────────────────────────────────

(define-test disable-alternatives-test
  :parent nil
  "Test the :allow-alternatives option of resolve-dependencies.

   When :allow-alternatives is false, the resolver must not use
   alternative packages to satisfy requirements.  This means:
   - b>2.0.0 should be :unsuccessful because b@2.3.0 requires c>=3.5.0
     OR d>=0.8.0, and with alternatives disabled, only one alternative
     is explored.
   - a>1.0.0 should be :successful because a@1.3.0 requires d>=0.8.0
     OR c>=3.5.0, and d is available."
  (let ((b-result (resolve-dependencies
                    (list (present-spec "b"
                            (make-version-predicate
                              :relation :greater-than :version "2.0.0")))
                    *query*
                    :compare #'maven-vercmp
                    :allow-alternatives nil)))
    (true (unsuccessful-p b-result)))
  (let ((a-result (resolve-dependencies
                    (list (present-spec "a"
                            (make-version-predicate
                              :relation :greater-than :version "1.0.0")))
                    *query*
                    :compare #'maven-vercmp
                    :allow-alternatives nil)))
    (true (successful-result a-result))
    (let ((locs (locations-set (successful-result a-result))))
      (true (fset:member?
              "http://example.com/repo/a-1.3.0.zip" locs))
      (true (fset:member?
              "http://example.com/repo/d-0.8.0.zip" locs)))))
