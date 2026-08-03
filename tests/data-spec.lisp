;;;; tests/data-spec.lisp
;;;;
;;;; Ported from degasolv/test/degasolv/resolver/data_spec_test.clj
;;;;
;;;; Tests for resolve-dependencies with various version spec cases:
;;;; version ranges, comparison operators, pessimistic greater, matches,
;;;; and compound requirements.

(defpackage #:com.djhaskin.dsolv/tests/data-spec
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

(in-package #:com.djhaskin.dsolv/tests/data-spec)

;;; ─── Helpers ────────────────────────────────────────────────────────────────

(defun make-pkg (id version location &optional requirements)
  "Create a package-info struct."
  (make-package-info
    :id id
    :version version
    :location location
    :requirements (or requirements nil)))

(defun pkg-loc (pkg)
  "Get the location of a package."
  (pi-location pkg))

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

;;; ─── Tutorial test ──────────────────────────────────────────────────────────

(define-test tutorial-test
  :parent nil
  (let* ((b-1-7 (make-pkg "b" "1.7.0" "http://example.com/repo/b-1.7.0.zip"))
         (b-2-3 (make-pkg "b" "2.3.0" "http://example.com/repo/b-2.3.0.zip"
                          (list (list (present "c"
                                     (list (list (make-version-predicate
                                                  :relation :greater-equal
                                                  :version "3.5.0")))))
                                (list (present "d")))))
         (c-2-4 (make-pkg "c" "2.4.7" "http://example.com/repo/c-2.4.7.zip"))
         (c-3-5 (make-pkg "c" "3.5.0" "http://example.com/repo/c-3.5.0.zip"
                          (list (list (present "e"
                                     (list (list (make-version-predicate
                                                  :relation :greater-equal
                                                  :version "1.8.0"))))))))
         (d-0-8 (make-pkg "d" "0.8.0" "http://example.com/repo/d-0.8.0.zip"
                          (list (list (present "e"
                                     (list (list (make-version-predicate
                                                  :relation :greater-equal
                                                  :version "1.1.0")
                                                 (make-version-predicate
                                                  :relation :less-than
                                                  :version "2.0.0"))))))))
         (e-2-4 (make-pkg "e" "2.4.0" "http://exmaple.com/repo/e-2.4.0.zip"))
         (e-2-1 (make-pkg "e" "2.1.0" "http://exmaple.com/repo/e-2.1.0.zip"))
         (e-1-8 (make-pkg "e" "1.8.0" "http://exmaple.com/repo/e-1.8.0.zip"))
         (repo-info (make-repo-map
                     (list (cons "b" (list b-1-7 b-2-3))
                           (cons "c" (list c-2-4 c-3-5))
                           (cons "d" (list d-0-8))
                           (cons "e" (list e-2-4 e-2-1 e-1-8)))))
         (query (map-query repo-info))
         (result (resolve-dependencies
                  (list (list (present "b"
                           (list (list (make-version-predicate
                                        :relation :greater-than
                                        :version "2.0.0"))))))
                  query
                  :compare #'maven-vercmp))
         (expected-locations
           (f:convert 'f:set
                      (list "http://example.com/repo/b-2.3.0.zip"
                            "http://example.com/repo/c-3.5.0.zip"
                            "http://example.com/repo/d-0.8.0.zip"
                            "http://exmaple.com/repo/e-1.8.0.zip"))))
    (true (successful-result result))
    (let* ((packages (successful-result result))
           (locations (f:convert 'f:set (mapcar #'pkg-loc packages))))
      (true (f:empty? (f:set-difference locations expected-locations))))))

;;; ─── Range spec cases ───────────────────────────────────────────────────────

(define-test range-spec-cases
  :parent nil
  (let* ((b3 (make-pkg "b" "3.0.0" "http://example.com/repo/b-3.0.0.zip"))
         (b35 (make-pkg "b" "3.5.0" "http://example.com/repo/b-3.5.0.zip"))
         (b4 (make-pkg "b" "4.0.0" "http://example.com/repo/b-4.0.0.zip"))
         (repo-info-asc (make-repo-map (list (cons "b" (list b3 b35 b4)))))
         (query-asc (map-query repo-info-asc))
         (repo-info-desc (make-repo-map (list (cons "b" (list b4 b35 b3)))))
         (query-desc (map-query repo-info-desc))
         (range-spec (list (list (make-version-predicate
                                  :relation :in-range :version "3.x"))))
         (sub-range-spec (list (list (make-version-predicate
                                      :relation :in-range :version "3.5.x")))))
    ;; range start inclusive
    (let ((result (resolve-dependencies
                   (list (list (present "b" range-spec)))
                   query-asc :compare #'maven-vercmp)))
      (true (successful-result result))
      (true (find b3 (successful-result result) :test #'equal)))
    ;; range end exclusive
    (let ((result (resolve-dependencies
                   (list (list (present "b" range-spec)))
                   query-desc :compare #'maven-vercmp)))
      (true (successful-result result))
      (true (find b35 (successful-result result) :test #'equal)))
    ;; sub range
    (let ((result (resolve-dependencies
                   (list (list (present "b" sub-range-spec)))
                   query-asc :compare #'maven-vercmp)))
      (true (successful-result result))
      (true (find b35 (successful-result result) :test #'equal)))))

;;; ─── Data spec cases ────────────────────────────────────────────────────────

(define-test data-spec-cases
  :parent nil
  (let* ((b1 (make-pkg "b" "1.0.0" "http://example.com/repo/b-1.0.0.zip"))
         (b23 (make-pkg "b" "2.3.0" "http://example.com/repo/b-2.3.0.zip"))
         (b20 (make-pkg "b" "2.0.0" "http://example.com/repo/b-2.0.0.zip"))
         (b31 (make-pkg "b" "3.1.0" "http://example.com/repo/b-3.1.0.zip"))
         (repo-info-mixed (make-repo-map
                           (list (cons "b" (list b1 b23 b20 b31)))))
         (query-mixed (map-query repo-info-mixed))
         (repo-info-desc (make-repo-map
                          (list (cons "b" (list b31 b23 b20 b1)))))
         (query-desc (map-query repo-info-desc))
         (repo-info-asc (make-repo-map
                         (list (cons "b" (list b1 b20 b23 b31)))))
         (query-asc (map-query repo-info-asc)))
    ;; greater-than
    (let ((result (resolve-dependencies
                   (list (list (present "b"
                            (list (list (make-version-predicate
                                         :relation :greater-than
                                         :version "2.0.0"))))))
                   query-asc :compare #'maven-vercmp)))
      (true (successful-result result))
      (true (find b23 (successful-result result) :test #'equal)))
    ;; greater-equal, less-than
    (let ((result (resolve-dependencies
                   (list (list (present "b"
                            (list (list (make-version-predicate
                                         :relation :greater-equal
                                         :version "2.0.0")
                                        (make-version-predicate
                                         :relation :less-than
                                         :version "2.3.0"))))))
                   query-mixed :compare #'maven-vercmp)))
      (true (successful-result result))
      (true (find b20 (successful-result result) :test #'equal)))
    ;; less-equal, greater-than
    (let ((result (resolve-dependencies
                   (list (list (present "b"
                            (list (list (make-version-predicate
                                         :relation :less-equal
                                         :version "2.3.0")
                                        (make-version-predicate
                                         :relation :greater-than
                                         :version "2.0.0"))))))
                   query-asc :compare #'maven-vercmp)))
      (true (successful-result result))
      (true (find b23 (successful-result result) :test #'equal)))
    ;; equal-to
    (let ((result (resolve-dependencies
                   (list (list (present "b"
                            (list (list (make-version-predicate
                                         :relation :equal-to
                                         :version "2.0.0"))))))
                   query-mixed :compare #'maven-vercmp)))
      (true (successful-result result))
      (true (find b20 (successful-result result) :test #'equal)))
    ;; not-equal
    (let ((result (resolve-dependencies
                   (list (list (present "b"
                            (list (list (make-version-predicate
                                         :relation :not-equal
                                         :version "3.1.0")
                                        (make-version-predicate
                                         :relation :not-equal
                                         :version "2.3.0")
                                        (make-version-predicate
                                         :relation :not-equal
                                         :version "2.0.0"))))))
                   query-desc :compare #'maven-vercmp)))
      (true (successful-result result))
      (true (find b1 (successful-result result) :test #'equal)))
    ;; range spec case 1
    (let ((result (resolve-dependencies
                   (list (list (present "b"
                            (list (list (make-version-predicate
                                         :relation :in-range
                                         :version "01"))))))
                   query-desc :compare #'maven-vercmp)))
      (true (successful-result result))
      (true (find b1 (successful-result result) :test #'equal)))
    ;; range spec case 2
    (let ((result (resolve-dependencies
                   (list (list (present "b"
                            (list (list (make-version-predicate
                                         :relation :in-range
                                         :version "2.003"))))))
                   query-asc :compare #'maven-vercmp)))
      (true (successful-result result))
      (true (find b23 (successful-result result) :test #'equal)))
    ;; range spec case 3
    (let ((result (resolve-dependencies
                   (list (list (present "b"
                            (list (list (make-version-predicate
                                         :relation :in-range
                                         :version "2.3.x"))))))
                   query-asc :compare #'maven-vercmp)))
      (true (successful-result result))
      (true (find b23 (successful-result result) :test #'equal)))
    ;; pessimistic greater case 1
    (let ((result (resolve-dependencies
                   (list (list (present "b"
                            (list (list (make-version-predicate
                                         :relation :pess-greater
                                         :version "1.0.0"))))))
                   query-desc :compare #'maven-vercmp)))
      (true (successful-result result))
      (true (find b1 (successful-result result) :test #'equal)))
    ;; pessimistic greater case 2
    (let ((result (resolve-dependencies
                   (list (list (present "b"
                            (list (list (make-version-predicate
                                         :relation :pess-greater
                                         :version "2"))))))
                   query-asc :compare #'maven-vercmp)))
      (true (successful-result result))
      (true (find b20 (successful-result result) :test #'equal)))
    ;; pessimistic greater case 3
    (let ((result (resolve-dependencies
                   (list (list (present "b"
                            (list (list (make-version-predicate
                                         :relation :pess-greater
                                         :version "2.3.0"))))))
                   query-asc :compare #'maven-vercmp)))
      (true (successful-result result))
      (true (find b23 (successful-result result) :test #'equal)))
    ;; pessimistic greater case 4
    (let ((result (resolve-dependencies
                   (list (list (present "b"
                            (list (list (make-version-predicate
                                         :relation :pess-greater
                                         :version "3.0.0"))))))
                   query-asc :compare #'maven-vercmp)))
      (true (successful-result result))
      (true (find b31 (successful-result result) :test #'equal)))
    ;; pessimistic greater case 5
    (let ((result (resolve-dependencies
                   (list (list (present "b"
                            (list (list (make-version-predicate
                                         :relation :pess-greater
                                         :version "2.0.1-alpha0"))))))
                   query-asc :compare #'maven-vercmp)))
      (true (successful-result result))
      (true (find b23 (successful-result result) :test #'equal)))
    ;; regex case
    (let ((result (resolve-dependencies
                   (list (list (present "b"
                            (list (list (make-version-predicate
                                         :relation :matches
                                         :version "^[0-9][.][0-9][.][0-9]"))))))
                   query-desc :compare #'maven-vercmp)))
      (true (successful-result result))
      (true (find b31 (successful-result result) :test #'equal)))
    ;; bad regex case
    (let ((result (resolve-dependencies
                   (list (list (present "b"
                            (list (list (make-version-predicate
                                         :relation :matches
                                         :version "^["))))))
                   query-desc :compare #'maven-vercmp)))
      (true (unsuccessful-p result)))
    ;; dual ranges case
    (let ((result (resolve-dependencies
                   (list (list (present "b"
                            (list (list (make-version-predicate
                                         :relation :greater-than
                                         :version "2.0.0"))
                                  (list (make-version-predicate
                                         :relation :less-than
                                         :version "1.7.0"))))))
                   query-desc :compare #'maven-vercmp)))
      (true (successful-result result))
      (true (find b31 (successful-result result) :test #'equal)))
    (let ((result (resolve-dependencies
                   (list (list (present "b"
                            (list (list (make-version-predicate
                                         :relation :greater-than
                                         :version "2.0.0"))
                                  (list (make-version-predicate
                                         :relation :less-than
                                         :version "1.7.0"))))))
                   query-asc :compare #'maven-vercmp)))
      (true (successful-result result))
      (true (find b1 (successful-result result) :test #'equal)))))