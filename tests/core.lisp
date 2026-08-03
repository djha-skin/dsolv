;;;; tests/core.lisp
;;;;
;;;; Ported from degasolv/test/degasolv/resolver/core_test.clj
;;;;
;;;; Comprehensive tests for the core resolver: retrieval, present packages,
;;;; conflicts, transitive requires, disjunctive clauses, diamond problems,
;;;; hoisting, and circular dependencies.

(defpackage #:com.djhaskin.dsolv/tests/core
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
    #:false)
  (:local-nicknames
    (#:f #:fset)))

(in-package #:com.djhaskin.dsolv/tests/core)

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

(defun absent-spec (id)
  "Create an absent clause."
  (list (absent id)))

;;; ─── Test data ──────────────────────────────────────────────────────────────

(defparameter *pkg-a30*
  (make-pkg "a" "30" "a_loc30"
            (list (present-spec "c"))))

(defparameter *pkg-a20*
  (make-pkg "a" "20" "a_loc20" nil))

(defparameter *pkg-c10*
  (make-pkg "c" "10" "c_loc10" nil))

(defparameter *pkg-d22*
  (make-pkg "d" "22" "d_loc22" nil))

(defparameter *pkg-e18*
  (make-pkg "e" "18" "e_loc18"
            (list (absent-spec "d"))))

(defparameter *repo-info*
  (make-repo-map
    (list (cons "a" (list *pkg-a30* *pkg-a20*))
          (cons "c" (list *pkg-c10*))
          (cons "d" (list *pkg-d22*)))))

(defparameter *query* (map-query *repo-info*))

;;; ─── Tests: Retrieval ───────────────────────────────────────────────────────

(define-test retrieval
  :parent nil
  ;; Asking for a present package succeeds.
  (let ((result (resolve-dependencies
                  (list (present-spec "d"))
                  *query*
                  :compare #'maven-vercmp)))
    (true (successful-result result))
    (true (find *pkg-d22* (successful-result result) :test #'equal)))
  (let ((result (resolve-dependencies
                  (list (present-spec "c"))
                  *query*
                  :compare #'maven-vercmp)))
    (true (successful-result result))
    (true (find *pkg-c10* (successful-result result) :test #'equal)))
  ;; Asking for a nonexistent package fails.
  (let ((result (resolve-dependencies
                  (list (present-spec "b"))
                  *query*
                  :compare #'maven-vercmp)))
    (true (unsuccessful-p result)))
  ;; Package present but no suitable version.
  (let ((result (resolve-dependencies
                  (list (present-spec "a"
                         (make-version-predicate
                           :relation :greater-equal :version "40")))
                  *query*
                  :compare #'maven-vercmp)))
    (true (unsuccessful-p result)))
  ;; Package present but with impossible constraints (pess-greater beyond max).
  (let ((result (resolve-dependencies
                  (list (present-spec "a"
                         (make-version-predicate
                           :relation :pess-greater :version "200")))
                  *query*
                  :compare #'maven-vercmp)))
    (true (unsuccessful-p result)))
  ;; Package present with version range that fits.
  (let ((result (resolve-dependencies
                  (list (present-spec "a"
                         (make-version-predicate
                           :relation :greater-equal :version "15")
                         (make-version-predicate
                           :relation :less-equal :version "25")))
                  *query*
                  :compare #'maven-vercmp)))
    (true (successful-result result))
    (true (find *pkg-a20* (successful-result result) :test #'equal)))
  (let ((result (resolve-dependencies
                  (list (present-spec "d"
                         (make-version-predicate
                           :relation :greater-equal :version "20")))
                  *query*
                  :compare #'maven-vercmp)))
    (true (successful-result result))
    (true (find *pkg-d22* (successful-result result) :test #'equal))))

;;; ─── Tests: Present packages ────────────────────────────────────────────────

(define-test present-packages
  :parent nil
  ;; Asking to install a package twice (duplicate clauses) succeeds.
  (let ((result (resolve-dependencies
                  (list (present-spec "c") (present-spec "c"))
                  *query*
                  :compare #'maven-vercmp)))
    (true (successful-result result))
    (true (find *pkg-c10* (successful-result result) :test #'equal)))
  ;; Installing an already-present package returns empty (nothing new to install).
  (let ((result (resolve-dependencies
                  (list (present-spec "c"))
                  *query*
                  :present-packages (f:with (f:empty-map) "c" (list *pkg-c10*))
                  :compare #'maven-vercmp)))
    (true (eql :successful (first result)))
    (true (null (second result))))
  ;; Installing a package provided as already installed, even if not in repo.
  (let* ((pkg-b (make-pkg "b" "10" "b-loc10" nil))
         (result (resolve-dependencies
                   (list (present-spec "b"))
                   *query*
                   :present-packages (f:with (f:empty-map) "b" (list pkg-b))
                   :compare #'maven-vercmp)))
    (true (eql :successful (first result)))
    (true (null (second result))))
  ;; Already installed but the installed version doesn't suit.
  (let ((result (resolve-dependencies
                  (list (present-spec "a"
                         (make-version-predicate
                           :relation :greater-equal :version "25")))
                  *query*
                  :present-packages (f:with (f:empty-map) "a" (list *pkg-a20*))
                  :compare #'maven-vercmp)))
    (true (unsuccessful-p result)))
  ;; Already installed and the installed version suits.
  (let ((result (resolve-dependencies
                  (list (present-spec "a"
                         (make-version-predicate
                           :relation :greater-equal :version "20")))
                  *query*
                  :present-packages (f:with (f:empty-map) "a" (list *pkg-a20*))
                  :compare #'maven-vercmp)))
    (true (eql :successful (first result)))
    (true (null (second result)))))

;;; ─── Tests: Conflicts ───────────────────────────────────────────────────────

(define-test conflicts
  :parent nil
  (let* ((d-pkg (present-spec "d"))
         (result (resolve-dependencies
                   (list d-pkg (present-spec "e"))
                   *query*
                   :compare #'maven-vercmp)))
    ;; Package 'e' has (absent "d"), so 'd' and 'e' conflict.
    (true (unsuccessful-p result)))
  ;; Conflict with a priori conflicting package.
  (let ((result (resolve-dependencies
                  (list (present-spec "d")
                        (present-spec "a"
                          (make-version-predicate
                            :relation :less-equal :version "25")))
                  *query*
                  :conflicts (f:with (f:empty-map) "d" (list nil))
                  :compare #'maven-vercmp)))
    (true (unsuccessful-p result)))
  ;; Conflict version-specific: 'd' conflicts with packages < version 22
  ;; The conflicts map value is a list of specs, where each spec is a list of
  ;; disjunctions (each disjunction being a list of conjunctions of version predicates).
  ;; A single vp like (make-version-predicate :less-than "22") becomes:
  ;;   (((make-version-predicate ...)))  → one disjunction, one conjunction, one vp
  (let ((result (resolve-dependencies
                  (list (present-spec "d"))
                  *query*
                  :conflicts (f:with (f:empty-map) "d"
                                (list (list (list (make-version-predicate
                                                    :relation :less-than
                                                    :version "22")))))
                  :compare #'maven-vercmp)))
    (true (successful-result result))
    (true (find *pkg-d22* (successful-result result) :test #'equal))))

;;; ─── Tests: Requires (transitive dependencies) ──────────────────────────────

(define-test requires
  :parent nil
  ;; One package requires another, both should be found.
  (let* ((pkg-a (make-pkg "a" "30" "a_loc30"
                          (list (present-spec "b"))))
         (pkg-b (make-pkg "b" "20" "b_loc20" nil))
         (repo (make-repo-map
                 (list (cons "a" (list pkg-a))
                       (cons "b" (list pkg-b)))))
         (query (map-query repo))
         (result (resolve-dependencies
                   (list (present-spec "a"))
                   query
                   :compare #'maven-vercmp)))
    (true (successful-result result))
    (true (find pkg-a (successful-result result) :test #'equal))
    (true (find pkg-b (successful-result result) :test #'equal)))
  ;; One package requires another, but it's already installed.
  (let* ((pkg-a (make-pkg "a" "30" "a_loc30"
                          (list (present-spec "b"))))
         (pkg-b (make-pkg "b" "20" "b_loc20" nil))
         (repo (make-repo-map
                 (list (cons "a" (list pkg-a))
                       (cons "b" (list pkg-b)))))
         (query (map-query repo))
         (result (resolve-dependencies
                   (list (present-spec "a"))
                   query
                   :present-packages (f:with (f:empty-map) "b" (list pkg-b))
                   :compare #'maven-vercmp)))
    (true (successful-result result))
    (true (find pkg-a (successful-result result) :test #'equal))
    ;; pkg-b is already installed, should not be in the result.
    (true (not (find pkg-b (successful-result result) :test #'equal))))
  ;; Package 'b' depends on 'c', 'c' has (absent "a"): conflict.
  (let* ((pkg-a (make-pkg "a" "10" "a_loc10" nil))
         (pkg-b (make-pkg "b" "20" "b_loc20"
                          (list (present-spec "c"))))
         (pkg-c (make-pkg "c" "10" "c_loc10"
                          (list (absent-spec "a"))))
         (repo (make-repo-map
                 (list (cons "a" (list pkg-a))
                       (cons "b" (list pkg-b))
                       (cons "c" (list pkg-c)))))
         (query (map-query repo))
         (result (resolve-dependencies
                   (list (present-spec "a")
                         (present-spec "b"))
                   query
                   :compare #'maven-vercmp)))
    (true (unsuccessful-p result)))
  ;; Package 'b' depends on 'c', 'c' is marked as a priori conflict.
  (let* ((pkg-a (make-pkg "a" "10" "a_loc10" nil))
         (pkg-b (make-pkg "b" "10" "b_loc10"
                          (list (present-spec "c"))))
         (pkg-c (make-pkg "c" "10" "c_loc10" nil))
         (repo (make-repo-map
                 (list (cons "a" (list pkg-a))
                       (cons "b" (list pkg-b))
                       (cons "c" (list pkg-c)))))
         (query (map-query repo))
         (result (resolve-dependencies
                   (list (present-spec "a")
                         (present-spec "b"))
                   query
                   :conflicts (f:with (f:empty-map) "c" (list nil))
                   :compare #'maven-vercmp)))
    (true (unsuccessful-p result))))

;;; ─── Tests: Disjunctive clauses ─────────────────────────────────────────────

(define-test disjunctive-clauses
  :parent nil
  ;; Disjunction tautology: (absent "c") OR (present "b") with empty repo.
  (let ((result (resolve-dependencies
                  (list (list (absent "c") (present "b")))
                  (map-query (f:empty-map))
                  :compare #'maven-vercmp)))
    (true (eql :successful (first result)))
    (true (null (second result))))
  ;; Skip past a conflict: (absent "c") is false (c is present),
  ;; so (present "b") must be resolved.
  (let* ((pkg-a (make-pkg "a" "30" "a_loc30"
                          (list (present-spec "b"))))
         (pkg-b (make-pkg "b" "20" "b_loc20" nil))
         (pkg-c (make-pkg "c" "10" "c_loc10" nil))
         (repo (make-repo-map
                 (list (cons "a" (list pkg-a))
                       (cons "b" (list pkg-b)))))
         (query (map-query repo))
         (result (resolve-dependencies
                   (list (list (absent "c") (present "b")))
                   query
                   :present-packages (f:with (f:empty-map) "c" (list pkg-c))
                   :compare #'maven-vercmp)))
    (true (successful-result result))
    (true (find pkg-b (successful-result result) :test #'equal))))

;;; ─── Tests: No-locking (diamond problems) ───────────────────────────────────

(define-test no-locking
  :parent nil
  ;; Find two packages, even when the preferred version of one conflicts.
  (let* ((pkg-a30 (make-pkg "a" "30" "a_loc30"
                            (list (absent-spec "c"))))
         (pkg-a20 (make-pkg "a" "20" "a_loc20" nil))
         (pkg-c10 (make-pkg "c" "10" "c_loc10" nil))
         (repo (make-repo-map
                 (list (cons "a" (list pkg-a30 pkg-a20))
                       (cons "c" (list pkg-c10)))))
         (query (map-query repo))
         (result (resolve-dependencies
                   (list (present-spec "a")
                         (present-spec "c"))
                   query
                   :compare #'maven-vercmp)))
    (true (successful-result result))
    (true (find pkg-a20 (successful-result result) :test #'equal))
    (true (find pkg-c10 (successful-result result) :test #'equal)))
  ;; Diamond problem: a requires (b OR c), b requires d >= 2, c requires d < 4.
  (let* ((pkg-a (make-pkg "a" "1" "a_loc1"
                          (list (present-spec "b")
                                (present-spec "c"))))
         (pkg-b (make-pkg "b" "1" "b_loc1"
                          (list (present-spec "d"
                                 (make-version-predicate
                                   :relation :greater-equal :version "2")))))
         (pkg-c (make-pkg "c" "1" "c_loc1"
                          (list (present-spec "d"
                                 (make-version-predicate
                                   :relation :less-than :version "4")))))
         (pkg-d3 (make-pkg "d" "3" "d_loc3" nil))
         (pkg-d4 (make-pkg "d" "4" "d_loc4" nil))
         (repo (make-repo-map
                 (list (cons "a" (list pkg-a))
                       (cons "b" (list pkg-b))
                       (cons "c" (list pkg-c))
                       (cons "d" (list pkg-d4 pkg-d3)))))
         (query (map-query repo))
         (result (resolve-dependencies
                   (list (present-spec "a"))
                   query
                   :compare #'maven-vercmp)))
    (true (successful-result result))
    (true (find pkg-a (successful-result result) :test #'equal))
    (true (find pkg-b (successful-result result) :test #'equal))
    (true (find pkg-c (successful-result result) :test #'equal))
    (true (find pkg-d3 (successful-result result) :test #'equal)))
  ;; Inter-locking diamond: additional cross-constraints between b and c.
  (let* ((pkg-a (make-pkg "a" "1" "a_loc1"
                          (list (present-spec "b")
                                (present-spec "c"))))
         (pkg-b (make-pkg "b" "1" "b_loc1"
                          (list (present-spec "d"
                                 (make-version-predicate
                                   :relation :greater-equal :version "2"))
                                (present-spec "e"
                                 (make-version-predicate
                                   :relation :equal-to :version "5")))))
         (pkg-c (make-pkg "c" "1" "c_loc1"
                          (list (present-spec "e"
                                 (make-version-predicate
                                   :relation :greater-equal :version "1"))
                                (present-spec "d"
                                 (make-version-predicate
                                   :relation :less-than :version "4")))))
         (pkg-d3 (make-pkg "d" "3" "d_loc3" nil))
         (pkg-d4 (make-pkg "d" "4" "d_loc4" nil))
         (pkg-e6 (make-pkg "e" "6" "e_loc6" nil))
         (pkg-e5 (make-pkg "e" "5" "e_loc5" nil))
         (repo (make-repo-map
                 (list (cons "a" (list pkg-a))
                       (cons "b" (list pkg-b))
                       (cons "c" (list pkg-c))
                       (cons "d" (list pkg-d4 pkg-d3))
                       (cons "e" (list pkg-e6 pkg-e5)))))
         (query (map-query repo))
         (result (resolve-dependencies
                   (list (present-spec "a"))
                   query
                   :compare #'maven-vercmp)))
    (true (successful-result result))
    (true (find pkg-a (successful-result result) :test #'equal))
    (true (find pkg-b (successful-result result) :test #'equal))
    (true (find pkg-c (successful-result result) :test #'equal))
    (true (find pkg-d3 (successful-result result) :test #'equal))
    (true (find pkg-e5 (successful-result result) :test #'equal)))
  ;; The puzzle: nested diamond with deep chains.
  (let* ((pkg-a (make-pkg "a" "1" "a_loc1"
                          (list (present-spec "b")
                                (present-spec "c"))))
         (pkg-b (make-pkg "b" "1" "b_loc1"
                          (list (present-spec "d"
                                 (make-version-predicate
                                   :relation :greater-equal :version "1")))))
         (pkg-c (make-pkg "c" "1" "c_loc1"
                          (list (present-spec "d"
                                 (make-version-predicate
                                   :relation :less-than :version "4")))))
         (pkg-d1 (make-pkg "d" "1" "d_loc1"
                           (list (present-spec "e"
                                  (make-version-predicate
                                    :relation :equal-to :version "4")))))
         (pkg-d2 (make-pkg "d" "2" "d_loc2"
                           (list (present-spec "e"
                                  (make-version-predicate
                                    :relation :equal-to :version "3")))))
         (pkg-e4 (make-pkg "e" "4" "e_loc4" nil))
         (pkg-e3 (make-pkg "e" "3" "e_loc3" nil))
         (repo (make-repo-map
                 (list (cons "a" (list pkg-a))
                       (cons "b" (list pkg-b))
                       (cons "c" (list pkg-c))
                       (cons "d" (list pkg-d2 pkg-d1))
                       (cons "e" (list pkg-e4 pkg-e3)))))
         (query (map-query repo))
         (result (resolve-dependencies
                   (list (present-spec "a"))
                   query
                   :compare #'maven-vercmp)))
    (true (successful-result result))
    (true (find pkg-a (successful-result result) :test #'equal))
    (true (find pkg-b (successful-result result) :test #'equal))
    (true (find pkg-c (successful-result result) :test #'equal))
    (true (find pkg-d2 (successful-result result) :test #'equal))
    (true (find pkg-e3 (successful-result result) :test #'equal)))
  ;; Double diamond: a requires (b OR d >= 1), b requires (c OR d < 4),
  ;; c requires d == 2.
  (let* ((pkg-a (make-pkg "a" "1" "a_loc1"
                          (list (present-spec "b")
                                (present-spec "d"
                                 (make-version-predicate
                                   :relation :greater-equal :version "1")))))
         (pkg-b (make-pkg "b" "1" "b_loc1"
                          (list (present-spec "c")
                                (present-spec "d"
                                 (make-version-predicate
                                   :relation :less-than :version "4")))))
         (pkg-c (make-pkg "c" "1" "c_loc1"
                          (list (present-spec "d"
                                 (make-version-predicate
                                   :relation :equal-to :version "2")))))
         (pkg-d4 (make-pkg "d" "4" "d_loc4" nil))
         (pkg-d3 (make-pkg "d" "3" "d_loc3" nil))
         (pkg-d2 (make-pkg "d" "2" "d_loc2" nil))
         (repo (make-repo-map
                 (list (cons "a" (list pkg-a))
                       (cons "b" (list pkg-b))
                       (cons "c" (list pkg-c))
                       (cons "d" (list pkg-d4 pkg-d3 pkg-d2)))))
         (query (map-query repo))
         (result (resolve-dependencies
                   (list (present-spec "a"))
                   query
                   :compare #'maven-vercmp)))
    (true (successful-result result))
    (true (find pkg-a (successful-result result) :test #'equal))
    (true (find pkg-b (successful-result result) :test #'equal))
    (true (find pkg-c (successful-result result) :test #'equal))
    (true (find pkg-d2 (successful-result result) :test #'equal))))

;;; ─── Tests: Hoisting ────────────────────────────────────────────────────────

(define-test hoisting
  :parent nil
  (let* ((pkg-a (make-pkg "a" "1" "a_loc1"
                          (list (list (present "b") (present "c")))))
         (pkg-b (make-pkg "b" "1" "b_loc1"
                          (list (present-spec "c")
                                (present-spec "d"
                                 (make-version-predicate
                                   :relation :less-than :version "4")))))
         (pkg-c (make-pkg "c" "1" "c_loc1" nil))
         (pkg-d (make-pkg "d" "1" "d_loc1"
                          (list (list (present "b") (absent "e")))))
         (repo (make-repo-map
                 (list (cons "a" (list pkg-a))
                       (cons "b" (list pkg-b))
                       (cons "d" (list pkg-d)))))
         (query (map-query repo)))
    ;; Prefer what's installed: 'c' is already installed, so 'a' resolves alone.
    (let ((result (resolve-dependencies
                    (list (present-spec "a"))
                    query
                    :present-packages (f:with (f:empty-map) "c" (list pkg-c))
                    :compare #'maven-vercmp)))
      (true (successful-result result))
      (true (find pkg-a (successful-result result) :test #'equal)))
    ;; Prefer conflicts over installs: 'd' would require 'b' but 'e' is
    ;; conflicting; with 'c' installed, 'b' can choose the c-alternative instead.
    (let ((result (resolve-dependencies
                    (list (present-spec "d"))
                    query
                    :present-packages (f:with (f:empty-map) "c" (list pkg-c))
                    :conflicts (f:with (f:empty-map) "e" (list nil))
                    :compare #'maven-vercmp)))
      (true (successful-result result))
      (true (find pkg-d (successful-result result) :test #'equal)))))

;;; ─── Tests: Circular dependencies ───────────────────────────────────────────

(define-test circular-dependencies
  :parent nil
  (let* ((pkg-a (make-pkg "a" "30" "a_loc30"
                          (list (present-spec "b"))))
         (pkg-b (make-pkg "b" "20" "b_loc20"
                          (list (present-spec "a"))))
         (repo (make-repo-map
                 (list (cons "a" (list pkg-a))
                       (cons "b" (list pkg-b)))))
         (query (map-query repo)))
    (let ((result (resolve-dependencies
                    (list (present-spec "a"))
                    query
                    :compare #'maven-vercmp)))
      (true (successful-result result))
      (true (find pkg-a (successful-result result) :test #'equal))
      (true (find pkg-b (successful-result result) :test #'equal)))
    (let ((result (resolve-dependencies
                    (list (present-spec "b"))
                    query
                    :compare #'maven-vercmp)))
      (true (successful-result result))
      (true (find pkg-a (successful-result result) :test #'equal))
      (true (find pkg-b (successful-result result) :test #'equal)))))