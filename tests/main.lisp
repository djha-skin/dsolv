;;;; tests/main.lisp
;;;;
;;;; Unit tests for the dsolv dependency resolver, utility functions,
;;;; and package system modules.

(defpackage #:com.djhaskin.dsolv/tests
  (:use #:cl)
  (:import-from
      #:org.shirakumo.parachute
    #:define-test
    #:true
    #:false
    #:fail
    #:is
    #:isnt
    #:finish
    #:test)
  (:import-from
      #:com.djhaskin.dsolv
    #:main)
  (:import-from
      #:com.djhaskin.dsolv/resolver
    #:make-version-predicate
    #:make-requirement
    #:make-package-info
    #:make-decorated-requirement
    #:version-predicate
    #:vp-relation
    #:vp-version
    #:requirement
    #:req-status
    #:req-id
    #:req-spec
    #:package-info
    #:pi-id
    #:pi-version
    #:pi-location
    #:pi-requirements
    #:decorated-requirement
    #:dr-clause
    #:dr-parent
    #:string-to-requirement
    #:make-spec-call
    #:present
    #:absent
    #:explain-package
    #:list-packages
    #:resolve-dependencies
    #:resolve-dependencies-deluxe
    #:*version-comparators*
    #:*package-systems*
    #:*relation-strings*)
  (:import-from
      #:com.djhaskin.dsolv/util
    #:data-slurp
    #:base-slurp
    #:default-spit
    #:pretty-spit
    #:map-query
    #:priority-repo
    #:global-repo
    #:aggregator)
  (:import-from
      #:com.djhaskin.dsolv/pkgsys/core
    #:read-card
    #:generate-repo-index
    #:slurp-degasolv-repo)
  (:import-from
      #:com.djhaskin.dsolv/pkgsys/apt
    #:slurp-apt-repo)
  (:local-nicknames
    (#:parachute #:org.shirakumo.parachute)
    (#:dsolv #:com.djhaskin.dsolv)
    (#:resolver #:com.djhaskin.dsolv/resolver)
    (#:util #:com.djhaskin.dsolv/util)
    (#:pkgsys/core #:com.djhaskin.dsolv/pkgsys/core)
    (#:pkgsys/apt #:com.djhaskin.dsolv/pkgsys/apt)))

(in-package #:com.djhaskin.dsolv/tests)

;;; ─── Version predicate tests ───────────────────────────────────────────────

(define-test version-predicate-basics
             :parent nil
             (let ((vp (make-version-predicate :relation :greater-than
                                               :version "1.0.0")))
               (is eq :greater-than (vp-relation vp))
               (is string= "1.0.0" (vp-version vp))))

(define-test version-predicate-defaults
             :parent nil
             (let ((vp (make-version-predicate)))
               (is eq nil (vp-relation vp))
               (is string= "" (vp-version vp))))

;;; ─── Requirement struct tests ──────────────────────────────────────────────

(define-test requirement-basics
             :parent nil
             (let ((r (make-requirement :status :present :id "foo" :spec nil)))
               (is eq :present (req-status r))
               (is string= "foo" (req-id r))
               (is eq nil (req-spec r))))

(define-test requirement-absent
             :parent nil
             (let ((r (make-requirement :status :absent :id "bar" :spec nil)))
               (is eq :absent (req-status r))
               (is string= "bar" (req-id r))))

(define-test present-helper
             :parent nil
             (let ((r (present "foo")))
               (is eq :present (req-status r))
               (is string= "foo" (req-id r))
               (is eq nil (req-spec r))))

(define-test present-helper-with-spec
             :parent nil
             (let* ((spec '(((make-version-predicate :relation :greater-than
                              :version "1.0.0"))))
                    (r (present "foo" spec)))
               (is eq :present (req-status r))
               (is string= "foo" (req-id r))
               (is eq spec (req-spec r))))

(define-test absent-helper
             :parent nil
             (let ((r (absent "bar")))
               (is eq :absent (req-status r))
               (is string= "bar" (req-id r))))

;;; ─── Package info tests ────────────────────────────────────────────────────

(define-test package-info-basics
             :parent nil
             (let ((pkg (make-package-info :id "test-pkg"
                                           :version "2.0.0"
                                           :location "http://example.com"
                                           :requirements nil)))
               (is string= "test-pkg" (pi-id pkg))
               (is string= "2.0.0" (pi-version pkg))
               (is string= "http://example.com" (pi-location pkg))
               (is eq nil (pi-requirements pkg))))

(define-test package-info-with-requirements
             :parent nil
             (let* ((reqs (list (present "dep-a")))
                    (pkg (make-package-info :id "parent"
                                            :version "1.0.0"
                                            :location "http://example.com"
                                            :requirements reqs)))
               (is eq reqs (pi-requirements pkg))
               (is string= "parent" (pi-id pkg))))

;;; ─── Decorated requirement tests ───────────────────────────────────────────

(define-test decorated-requirement-basics
             :parent nil
             (let* ((req (present "leaf"))
                    (pkg (make-package-info :id "parent" :version "1.0.0"
                                            :location "loc" :requirements nil))
                    (dr (make-decorated-requirement :clause (list req) :parent pkg)))
               (is eq (first (dr-clause dr)) req)
               (is eq pkg (dr-parent dr))))

;;; ─── String-to-requirement tests ───────────────────────────────────────────

(define-test string-to-requirement-simple
             :parent nil
             (let ((reqs (string-to-requirement "a")))
               (is = 1 (length reqs))
               (let ((r (first reqs)))
                 (is eq :present (req-status r))
                 (is string= "a" (req-id r))
                 (is eq nil (req-spec r)))))

(define-test string-to-requirement-absent
             :parent nil
             (let ((reqs (string-to-requirement "!a")))
               (is = 1 (length reqs))
               (is eq :absent (req-status (first reqs)))
               (is string= "a" (req-id (first reqs)))))

(define-test string-to-requirement-alternatives
             :parent nil
             (let ((reqs (string-to-requirement "a|b")))
               (is = 2 (length reqs))
               (is string= "a" (req-id (first reqs)))
               (is string= "b" (req-id (second reqs)))))

(define-test string-to-requirement-with-version
             :parent nil
             (let ((reqs (string-to-requirement "a>2.0")))
               (is = 1 (length reqs))
               (let* ((r (first reqs))
                      (spec (req-spec r)))
                 (true (not (null spec)))
                 (is = 1 (length spec))
                 (let* ((disjunction (first spec))
                        (vp (first disjunction)))
                   (is eq :greater-than (vp-relation vp))
                   (is string= "2.0" (vp-version vp))))))

(define-test string-to-requirement-empty
             :parent nil
             (let ((reqs (string-to-requirement "")))
               (is eq nil reqs)))

(define-test string-to-requirement-version-range
             :parent nil
             (let ((reqs (string-to-requirement "a>1.0,<=2.0")))
               (is = 1 (length reqs))
               (let* ((r (first reqs))
                      (spec (req-spec r)))
                 (is = 1 (length spec))
                 (let ((disjunction (first spec)))
                   (is = 2 (length disjunction))
                   (is eq :greater-than (vp-relation (first disjunction)))
                   (is eq :less-equal (vp-relation (second disjunction)))))))

(define-test string-to-requirement-disjunction
             :parent nil
             (let ((reqs (string-to-requirement "a>1.0;<=2.0")))
               (is = 1 (length reqs))
               (let* ((r (first reqs))
                      (spec (req-spec r)))
                 (is = 2 (length spec)))))

;;; ─── Explain-package tests ─────────────────────────────────────────────────

(define-test explain-package-basic
             :parent nil
             (let ((pkg (make-package-info :id "pkg" :version "1.0"
                                           :location "https://example.com")))
               (is string= "pkg==1.0 @ https://example.com"
                   (explain-package pkg))))

;;; ─── Spec-call tests ───────────────────────────────────────────────────────

(define-test spec-call-equal-to
             :parent nil
             (let* ((cmp (lambda (a b)
                           (cond ((string< a b) -1)
                                 ((string> a b) 1)
                                 (t 0))))
                    (spec-call (make-spec-call cmp))
                    (spec (list (list (make-version-predicate :relation :equal-to
                                                                 :version "1.0.0"))))
                    (pkg (make-package-info :id "test" :version "1.0.0"
                                            :location "loc" :requirements nil)))
               (is eq t (funcall spec-call spec pkg))))

(define-test spec-call-greater-than
             :parent nil
             (let* ((cmp (lambda (a b)
                           (cond ((string< a b) -1)
                                 ((string> a b) 1)
                                 (t 0))))
                    (spec-call (make-spec-call cmp))
                    (spec (list (list (make-version-predicate :relation :greater-than
                                                                 :version "1.0.0"))))
                    (pkg (make-package-info :id "test" :version "2.0.0"
                                            :location "loc" :requirements nil)))
               (is eq t (funcall spec-call spec pkg))))

(define-test spec-call-less-than
             :parent nil
             (let* ((cmp (lambda (a b)
                           (cond ((string< a b) -1)
                                 ((string> a b) 1)
                                 (t 0))))
                    (spec-call (make-spec-call cmp))
                    (spec (list (list (make-version-predicate :relation :less-than
                                                                 :version "2.0.0"))))
                    (pkg (make-package-info :id "test" :version "1.0.0"
                                            :location "loc" :requirements nil)))
               (is eq t (funcall spec-call spec pkg))))

(define-test spec-call-null-spec
             :parent nil
             (let* ((cmp (lambda (a b) (declare (ignore a b)) 0))
                    (spec-call (make-spec-call cmp))
                    (pkg (make-package-info :id "test" :version "1.0.0"
                                            :location "loc" :requirements nil)))
               (is eq t (funcall spec-call nil pkg))))

;;; ─── List-packages tests ───────────────────────────────────────────────────

(define-test list-packages-empty
             :parent nil
             (let ((graph (make-hash-table :test 'equal)))
               (setf (gethash :root graph) nil)
               (is eq nil (list-packages graph :list-strat :lazy))))

(define-test list-packages-simple
             :parent nil
             (let* ((pkg-a (make-package-info :id "a" :version "1.0"
                                              :location "loc" :requirements nil))
                    (graph (make-hash-table :test 'equal)))
               (setf (gethash :root graph) (list pkg-a))
               (setf (gethash pkg-a graph) nil)
               (let ((result (list-packages graph :list-strat :lazy)))
                 (is = 1 (length result))
                 (is string= "a" (pi-id (first result))))))

;;; ─── Resolver tests ────────────────────────────────────────────────────────

(define-test resolve-dependencies-simple
             :parent nil
             (let* ((repo-data (make-hash-table :test 'equal))
                    (pkg (make-package-info :id "a" :version "1.0"
                                            :location "loc" :requirements nil))
                    (query-fn (util:map-query repo-data)))
               (setf (gethash "a" repo-data) (list pkg))
               (let ((result (resolve-dependencies
                               (list (present "a"))
                               query-fn
                               :compare (lambda (a b)
                                          (cond ((string< a b) -1)
                                                ((string> a b) 1)
                                                (t 0))))))
                 (is eq :successful (first result))
                 (is = 1 (length (second result)))
                 (is string= "a" (pi-id (first (second result)))))))

(define-test resolve-dependencies-not-found
             :parent nil
             (let* ((repo-data (make-hash-table :test 'equal))
                    (query-fn (util:map-query repo-data)))
               (let ((result (resolve-dependencies
                               (list (present "nonexistent"))
                               query-fn
                               :compare (lambda (a b)
                                          (cond ((string< a b) -1)
                                                ((string> a b) 1)
                                                (t 0))))))
                 (is eq :unsuccessful (first result)))))

(define-test resolve-dependencies-transitive
             :parent nil
             (let* ((repo-data (make-hash-table :test 'equal))
                    (dep-b (make-package-info :id "b" :version "1.0"
                                              :location "loc" :requirements nil))
                    (dep-a (make-package-info :id "a" :version "1.0"
                                              :location "loc"
                                              :requirements (list (present "b")))))
               (setf (gethash "a" repo-data) (list dep-a))
               (setf (gethash "b" repo-data) (list dep-b))
               (let ((result (resolve-dependencies
                               (list (present "a"))
                               (util:map-query repo-data)
                               :compare (lambda (a b)
                                          (cond ((string< a b) -1)
                                                ((string> a b) 1)
                                                (t 0))))))
                 (is eq :successful (first result))
                 (let ((pkgs (second result)))
                   (is = 2 (length pkgs))
                   (is string= "a" (pi-id (first pkgs)))
                   (is string= "b" (pi-id (second pkgs)))))))

;;; ─── Version comparison tests ──────────────────────────────────────────────

(define-test version-comparator-naive
             :parent nil
             (let ((cmp (gethash "naive" *version-comparators*)))
               (true (not (null cmp)))
               (is eq t (funcall cmp "1.0.0" "1.0.0"))
               (is eq nil (funcall cmp "1.0.0" "2.0.0"))))

;;; ─── Package system definitions tests ──────────────────────────────────────

(define-test package-systems-defined
             :parent nil
             (true (not (null (gethash "degasolv" *package-systems*))))
             (true (not (null (gethash "apt" *package-systems*))))
             (true (not (null (gethash "subproc" *package-systems*)))))

(define-test degasolv-package-system-config
             :parent nil
             (let ((config (gethash "degasolv" *package-systems*)))
               (true (not (null (getf config :genrepo))))
               (is string= "semver" (getf config :version-comparison))))

;;; ─── Utility function tests ────────────────────────────────────────────────

(define-test map-query-found
             :parent nil
             (let* ((table (make-hash-table :test 'equal))
                    (fn (util:map-query table)))
               (setf (gethash "key" table) "value")
               (is string= "value" (funcall fn "key"))))

(define-test map-query-not-found
             :parent nil
             (let* ((table (make-hash-table :test 'equal))
                    (fn (util:map-query table)))
               (is eq nil (funcall fn "nonexistent"))))

(define-test base-slurp-stdin
             :parent nil
             (let ((result (base-slurp "-")))
               (true (stringp result)))

(define-test aggregator-priority
             :parent nil
             (let* ((cmp (lambda (a b) (string< a b)))
                    (agg (aggregator "priority" cmp)))
               (true (not (null agg)))
               (true (functionp agg))))

(define-test aggregator-global
             :parent nil
             (let* ((cmp (lambda (a b) (string< a b)))
                    (agg (aggregator "global" cmp)))
               (true (not (null agg)))
               (true (functionp agg))))

(define-test priority-repo-basics
             :parent nil
             (let* ((table (make-hash-table :test 'equal))
                    (q1 (util:map-query table))
                    (pr (priority-repo (list q1))))
               (setf (gethash "a" table) (list (make-package-info :id "a" :version "1.0"
                                                                  :location "loc"
                                                                  :requirements nil)))
               (let ((result (funcall pr "a")))
                 (true (not (null result)))
                 (is string= "a" (pi-id (first result))))))

;;; ─── Relation strings tests ────────────────────────────────────────────────

(define-test relation-strings-defined
             :parent nil
             (true (not (null (cdr (assoc :greater-than *relation-strings*)))))
             (true (not (null (cdr (assoc :less-than *relation-strings*)))))
             (true (not (null (cdr (assoc :equal-to *relation-strings*)))))
             (true (not (null (cdr (assoc :not-equal *relation-strings*)))))
             (true (not (null (cdr (assoc :greater-equal *relation-strings*)))))
             (true (not (null (cdr (assoc :less-equal *relation-strings*))))))

(define-test relation-strings-values
             :parent nil
             (is string= ">" (cdr (assoc :greater-than *relation-strings*)))
             (is string= "<" (cdr (assoc :less-than *relation-strings*)))
             (is string= "==" (cdr (assoc :equal-to *relation-strings*)))
             (is string= "!=" (cdr (assoc :not-equal *relation-strings*)))
             (is string= ">=" (cdr (assoc :greater-equal *relation-strings*)))
             (is string= "<=" (cdr (assoc :less-equal *relation-strings*))))

;;; ─── Resolve-dependencies-deluxe strategy tests ────────────────────────────

(define-test resolve-deluxe-thorough
             :parent nil
             (let* ((repo-data (make-hash-table :test 'equal))
                    (pkg-a (make-package-info :id "a" :version "2.0"
                                              :location "loc" :requirements nil))
                    (pkg-a-old (make-package-info :id "a" :version "1.0"
                                                  :location "loc" :requirements nil))
                    (query-fn (util:map-query repo-data)))
               (setf (gethash "a" repo-data) (list pkg-a pkg-a-old))
               (let ((result (resolve-dependencies-deluxe
                               (list (present "a"))
                               query-fn
                               :compare (lambda (a b)
                                          (cond ((string< a b) -1)
                                                ((string> a b) 1)
                                                (t 0)))
                               :strategy :thorough)))
                 (is eq :successful (getf result :result))
                 (let ((pkgs (getf result :packages)))
                   (is = 1 (length pkgs))))))

(define-test resolve-deluxe-fast
             :parent nil
             (let* ((repo-data (make-hash-table :test 'equal))
                    (pkg-a (make-package-info :id "a" :version "2.0"
                                              :location "loc" :requirements nil))
                    (pkg-a-old (make-package-info :id "a" :version "1.0"
                                                  :location "loc" :requirements nil))
                    (query-fn (util:map-query repo-data)))
               (setf (gethash "a" repo-data) (list pkg-a pkg-a-old))
               (let ((result (resolve-dependencies-deluxe
                               (list (present "a"))
                               query-fn
                               :compare (lambda (a b)
                                          (cond ((string< a b) -1)
                                                ((string> a b) 1)
                                                (t 0)))
                               :strategy :fast)))
                 (is eq :successful (getf result :result))
                 (let ((pkgs (getf result :packages)))
                   (is = 1 (length pkgs))))))

(define-test resolve-deluxe-present-packages
             :parent nil
             (let* ((repo-data (make-hash-table :test 'equal))
                    (pkg-a (make-package-info :id "a" :version "1.0"
                                              :location "loc" :requirements nil))
                    (present-pkg (make-package-info :id "a" :version "1.0"
                                                    :location "already present"
                                                    :requirements nil))
                    (present-pkgs (make-hash-table :test 'equal))
                    (query-fn (util:map-query repo-data)))
               (setf (gethash "a" repo-data) (list pkg-a))
               (setf (gethash "a" present-pkgs) (list present-pkg))
               (let ((result (resolve-dependencies-deluxe
                               (list (present "a"))
                               query-fn
                               :present-packages present-pkgs
                               :compare (lambda (a b)
                                          (cond ((string< a b) -1)
                                                ((string> a b) 1)
                                                (t 0))))))
                 (is eq :successful (getf result :result)))))

;;; ─── Resolve-dependencies-deluxe absent tests ──────────────────────────────

(define-test resolve-deluxe-absent-requirement
             :parent nil
             (let* ((repo-data (make-hash-table :test 'equal))
                    (pkg-b (make-package-info :id "b" :version "1.0"
                                              :location "loc" :requirements nil))
                    (query-fn (util:map-query repo-data)))
               (setf (gethash "b" repo-data) (list pkg-b))
               (let ((result (resolve-dependencies-deluxe
                               (list (absent "b"))
                               query-fn
                               :compare (lambda (a b)
                                          (cond ((string< a b) -1)
                                                ((string> a b) 1)
                                                (t 0))))))
                 (is eq :successful (getf result :result))
                 (is eq nil (getf result :packages)))))

;;; ─── Resolve-dependencies-deluxe conflict strategy tests ──────────────────

(define-test resolve-deluxe-exclusive-conflict
             :parent nil
             (let* ((repo-data (make-hash-table :test 'equal))
                    (pkg-a1 (make-package-info :id "a" :version "1.0"
                                               :location "loc" :requirements nil))
                    (pkg-a2 (make-package-info :id "a" :version "2.0"
                                               :location "loc" :requirements nil))
                    (query-fn (util:map-query repo-data)))
               (setf (gethash "a" repo-data) (list pkg-a1 pkg-a2))
               (let ((result (resolve-dependencies-deluxe
                               (list (present "a") (present "a"))
                               query-fn
                               :compare (lambda (a b)
                                          (cond ((string< a b) -1)
                                                ((string> a b) 1)
                                                (t 0)))
                               :conflict-strat :exclusive)))
                 ;; With exclusive, having two requirements for "a" should succeed
                 ;; (it picks the same package for both)
                 (is eq :successful (getf result :result)))))

;;; ─── Resolve-dependencies-deluxe list-strat tests ──────────────────────────

(define-test resolve-deluxe-list-strat-as-set
             :parent nil
             (let* ((repo-data (make-hash-table :test 'equal))
                    (pkg-a (make-package-info :id "a" :version "1.0"
                                              :location "loc" :requirements nil))
                    (query-fn (util:map-query repo-data)))
               (setf (gethash "a" repo-data) (list pkg-a))
               (let ((result (resolve-dependencies-deluxe
                               (list (present "a"))
                               query-fn
                               :compare (lambda (a b)
                                          (cond ((string< a b) -1)
                                                ((string> a b) 1)
                                                (t 0)))
                               :list-strat :as-set)))
                 (is eq :successful (getf result :result))
                 (is = 1 (length (getf result :packages)))))
)
)
