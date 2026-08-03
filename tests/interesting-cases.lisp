;;;; tests/interesting-cases.lisp
;;;;
;;;; Ported from degasolv/test/degasolv/resolver/interesting_cases_test.clj
;;;;
;;;; Tests for managed dependencies and implied dependencies.

(defpackage #:com.djhaskin.dsolv/tests/interesting-cases
  (:use #:cl)
  (:import-from #:com.djhaskin.dsolv/resolver
    #:resolve-dependencies
    #:present
    #:absent
    #:make-version-predicate
    #:make-package-info
    #:pi-id
    #:pi-version
    #:pi-location
    #:pi-requirements)
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

(in-package #:com.djhaskin.dsolv/tests/interesting-cases)

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

(defun absent-spec (id)
  "Create an absent clause."
  (list (absent id)))

;;; ─── Tests: Managed dependencies ────────────────────────────────────────────

(define-test managed-dependencies-case
  :parent nil
  ;; Managed dep: "absent b OR present b < 2.2.0" with descending repo (b23, b20).
  ;; Result should be a1 and b20 (b20 satisfies < 2.2.0).
  (let* ((a1 (make-pkg "a" "1.0.0" "http://example.com/repo/a-1.0.0.zip"
                       (list (present-spec "b"))))
         (b23 (make-pkg "b" "2.3.0" "http://example.com/repo/b-2.3.0.zip"))
         (b20 (make-pkg "b" "2.0.0" "http://example.com/repo/b-2.0.0.zip"))
         (repo-info-dsc (make-repo-map
                          (list (cons "a" (list a1))
                                (cons "b" (list b23 b20)))))
         (query-dsc (map-query repo-info-dsc))
         (result (resolve-dependencies
                   (list (present-spec "a")
                         (list (absent "b")
                               (first (present-spec "b"
                                      (make-version-predicate
                                        :relation :less-than :version "2.2.0")))))
                   query-dsc
                   :compare #'maven-vercmp)))
    (true (successful-result result))
    (true (find a1 (successful-result result) :test #'equal))
    (true (find b20 (successful-result result) :test #'equal)))
  ;; Managed dep: "absent b OR present b > 2.2.0" with ascending repo (b20, b23).
  ;; Result should be a1 and b23 (b23 satisfies > 2.2.0).
  (let* ((a1 (make-pkg "a" "1.0.0" "http://example.com/repo/a-1.0.0.zip"
                       (list (present-spec "b"))))
         (b23 (make-pkg "b" "2.3.0" "http://example.com/repo/b-2.3.0.zip"))
         (b20 (make-pkg "b" "2.0.0" "http://example.com/repo/b-2.0.0.zip"))
         (repo-info-asc (make-repo-map
                          (list (cons "a" (list a1))
                                (cons "b" (list b20 b23)))))
         (query-asc (map-query repo-info-asc))
         (result (resolve-dependencies
                   (list (present-spec "a")
                         (list (absent "b")
                               (first (present-spec "b"
                                      (make-version-predicate
                                        :relation :greater-than :version "2.2.0")))))
                   query-asc
                   :compare #'maven-vercmp)))
    (true (successful-result result))
    (true (find a1 (successful-result result) :test #'equal))
    (true (find b23 (successful-result result) :test #'equal))))

;;; ─── Tests: Implied dependencies ────────────────────────────────────────────

(define-test implied-dependencies-case
  :parent nil
  ;; Clause: present a, absent b OR present c.
  ;; a depends on b, so b must be present. Since b is present,
  ;; the "absent b" alternative fails, so "present c" is required.
  ;; Result should be a1, b23, and c353.
  (let* ((a1 (make-pkg "a" "1.0.0" "http://example.com/repo/a-1.0.0.zip"
                       (list (present-spec "b"))))
         (b23 (make-pkg "b" "2.3.0" "http://example.com/repo/b-2.3.0.zip"))
         (c353 (make-pkg "c" "3.5.3" "http://example.com/repo/c-3.5.3.zip"))
         (repo-info (make-repo-map
                      (list (cons "a" (list a1))
                            (cons "b" (list b23))
                            (cons "c" (list c353)))))
         (query (map-query repo-info))
         (result (resolve-dependencies
                   (list (present-spec "a")
                         (list (absent "b")
                               (first (present-spec "c"))))
                   query
                   :compare #'maven-vercmp)))
    (true (successful-result result))
    (true (find a1 (successful-result result) :test #'equal))
    (true (find b23 (successful-result result) :test #'equal))
    (true (find c353 (successful-result result) :test #'equal))))