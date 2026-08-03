;;;; tests/search-strat.lisp
;;;;
;;;; Ported from degasolv/test/degasolv/resolver/search_strat_test.clj
;;;;
;;;; Tests for the :search-strat keyword of resolve-dependencies:
;;;;   :breadth-first → explore alternatives breadth-first
;;;;   :depth-first   → explore alternatives depth-first
;;;;
;;;; These tests verify that different search strategies produce
;;;; different (but valid) resolution results.

(defpackage #:com.djhaskin.dsolv/tests/search-strat
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

(in-package #:com.djhaskin.dsolv/tests/search-strat)

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

(defun absent-spec (id)
  (list (absent id)))

(defun ids-set (packages)
  "Extract the set of package IDs from a list of packages."
  (fset:convert 'fset:set
                (mapcar #'pi-id packages)))

;;; ─── Test: basic DFS puzzle ─────────────────────────────────────────────────
;;;
;;; a requires (b) OR (c)
;;; b requires d>=1
;;; c requires d<4
;;; d@1 requires e==4
;;; d@2 requires e==3
;;; e@4 has no requirements
;;; e@3 has no requirements (named "3" in Clojure original)
;;;
;;; Depth-first should find the solution path that works.

(define-test basic-dfs
  :parent nil
  (let* ((package-a (make-pkg "a" "1" "a_loc1"
                      (list (present-spec "b")
                            (present-spec "c"))))
         (package-b (make-pkg "b" "1" "b_loc1"
                      (list (present-spec "d"
                              (make-version-predicate
                                :relation :greater-equal :version "1")))))
         (package-c (make-pkg "c" "1" "c_loc1"
                      (list (present-spec "d"
                              (make-version-predicate
                                :relation :less-than :version "4")))))
         (package-d1 (make-pkg "d" "1" "d_loc1"
                       (list (present-spec "e"
                               (make-version-predicate
                                 :relation :equal-to :version "4")))))
         (package-d2 (make-pkg "d" "2" "d_loc2"
                       (list (present-spec "e"
                               (make-version-predicate
                                 :relation :equal-to :version "3")))))
         (package-e4 (make-pkg "e" "4" "e_loc4" nil))
         (package-e3 (make-pkg "3" "3" "e_loc3" nil))
         (repo-info
           (make-repo-map
             (list (cons "a" (list package-a))
                   (cons "b" (list package-b))
                   (cons "c" (list package-c))
                   (cons "d" (list package-d2 package-d1))
                   (cons "e" (list package-e4 package-e3)))))
         (query (map-query repo-info))
         (result (resolve-dependencies
                   (list (present-spec "a"))
                   query
                   :search-strat :depth-first
                   :compare #'maven-vercmp)))
    (true (successful-result result))
    (let ((ids (ids-set (successful-result result))))
      (true (fset:member? "a" ids))
      (true (fset:member? "b" ids))
      (true (fset:member? "c" ids))
      (true (fset:member? "d" ids))
      (true (fset:member? "3" ids)))))

;;; ─── Test: breadth-first vs depth-first ─────────────────────────────────────
;;;
;;; a requires (b) OR (c OR d)
;;; b requires (d) OR (c)
;;; c@2.7.0 has no requirements
;;; d@4.0.0 has no requirements
;;;
;;; Breadth-first should pick the b→c path (shorter).
;;; Depth-first should pick the b→d path (deeper first).

(define-test search-strat-comparison
  :parent nil
  (let* ((d (make-pkg "d" "4.0.0" "http://example.com/repo/d-4.0.0.zip" nil))
         (c (make-pkg "c" "2.7.0" "http://example.com/repo/c-2.7.0.zip" nil))
         (b (make-pkg "b" "1.0.0" "http://example.com/repo/b-1.0.0.zip"
              ;; One clause with two alternatives: (d) OR (c)
              (list (append (present-spec "d")
                          (present-spec "c")))))
         (a (make-pkg "a" "1.0.0" "http://example.com/repo/a-1.0.0.zip"
              ;; Two clauses: (b) OR (c OR d)
              (list (present-spec "b")
                    (append (present-spec "c")
                          (present-spec "d")))))
         (repo-info
           (make-repo-map
             (list (cons "a" (list a))
                   (cons "b" (list b))
                   (cons "c" (list c))
                   (cons "d" (list d)))))
         (query (map-query repo-info)))
    ;; Breadth-first should include c, b, a
    (let ((bf-result (resolve-dependencies
                       (list (present-spec "a"))
                       query
                       :search-strat :breadth-first
                       :compare #'maven-vercmp)))
      (true (successful-result bf-result))
      (let ((ids (ids-set (successful-result bf-result))))
        (true (fset:member? "a" ids))
        (true (fset:member? "b" ids))
        (true (fset:member? "c" ids))))
    ;; Depth-first should include d, b, a
    (let ((df-result (resolve-dependencies
                       (list (present-spec "a"))
                       query
                       :search-strat :depth-first
                       :compare #'maven-vercmp)))
      (true (successful-result df-result))
      (let ((ids (ids-set (successful-result df-result))))
        (true (fset:member? "a" ids))
        (true (fset:member? "b" ids))
        (true (fset:member? "d" ids))))))

;;; ─── Test: absent dependencies with search strategies ────────────────────────
;;;
;;; a requires (b) OR (d OR c)
;;; b requires (absent d) OR (present e)
;;; c@2.7.0 has no requirements
;;; d@4.0.0 has no requirements
;;; e@7.0.0 has no requirements
;;;
;;; Breadth-first with absent: b→e (absent d fails), so a, b, d, e
;;; Depth-first with absent: b→d (absent d succeeds), so a, b, c

(define-test search-strat-absent
  :parent nil
  (let* ((e (make-pkg "e" "7.0.0" "http://example.com/repo/e-7.0.0.zip" nil))
         (d (make-pkg "d" "4.0.0" "http://example.com/repo/d-4.0.0.zip" nil))
         (c (make-pkg "c" "2.7.0" "http://example.com/repo/c-2.7.0.zip" nil))
         (b (make-pkg "b" "1.0.0" "http://example.com/repo/b-1.0.0.zip"
              ;; One clause with two alternatives: (absent d) OR (present e)
              (list (append (absent-spec "d")
                          (present-spec "e")))))
         (a (make-pkg "a" "1.0.0" "http://example.com/repo/a-1.0.0.zip"
              ;; Two clauses: (b) OR (d OR c)
              (list (present-spec "b")
                    (append (present-spec "d")
                          (present-spec "c")))))
         (repo-info
           (make-repo-map
             (list (cons "a" (list a))
                   (cons "b" (list b))
                   (cons "c" (list c))
                   (cons "d" (list d))
                   (cons "e" (list e)))))
         (query (map-query repo-info)))
    ;; Breadth-first: b→e (absent d fails, e works), so a, b, d, e
    (let ((bf-result (resolve-dependencies
                       (list (present-spec "a"))
                       query
                       :search-strat :breadth-first
                       :compare #'maven-vercmp)))
      (true (successful-result bf-result))
      (let ((ids (ids-set (successful-result bf-result))))
        (true (fset:member? "a" ids))
        (true (fset:member? "b" ids))
        (true (fset:member? "d" ids))
        (true (fset:member? "e" ids))))
    ;; Depth-first: b→d (absent d succeeds, so no e), so a, b, c
    (let ((df-result (resolve-dependencies
                       (list (present-spec "a"))
                       query
                       :search-strat :depth-first
                       :compare #'maven-vercmp)))
      (true (successful-result df-result))
      (let ((ids (ids-set (successful-result df-result))))
        (true (fset:member? "a" ids))
        (true (fset:member? "b" ids))
        (true (fset:member? "c" ids))))))

;;; ─── Test: search strategies with conflict-strat prioritized ────────────────
;;;
;;; a requires (b) OR (c==4.0.0)
;;; b requires c==2.7.0
;;; c@4.0.0 has no requirements
;;; c@2.7.0 has no requirements
;;;
;;; With :conflict-strat :prioritized:
;;;   Breadth-first: explores a→b first, b needs c==2.7.0, succeeds → a, b, c@4.0.0
;;;   Depth-first: explores a→c==4.0.0 first, succeeds → a, b, c@2.7.0

(define-test search-strat-prioritized
  :parent nil
  (let* ((c40 (make-pkg "c" "4.0.0" "http://example.com/repo/c-4.0.0.zip" nil))
         (c27 (make-pkg "c" "2.7.0" "http://example.com/repo/c-2.7.0.zip" nil))
         (b (make-pkg "b" "1.0.0" "http://example.com/repo/b-1.0.0.zip"
              (list (present-spec "c"
                      (make-version-predicate
                        :relation :equal-to :version "2.7.0")))))
         (a (make-pkg "a" "1.0.0" "http://example.com/repo/a-1.0.0.zip"
              ;; Two clauses: (b) OR (c==4.0.0)
              (list (present-spec "b")
                    (present-spec "c"
                      (make-version-predicate
                        :relation :equal-to :version "4.0.0")))))
         (repo-info
           (make-repo-map
             (list (cons "a" (list a))
                   (cons "b" (list b))
                   (cons "c" (list c40 c27)))))
         (query (map-query repo-info)))
    ;; Breadth-first with prioritized: a, b, c@4.0.0
    (let ((bf-result (resolve-dependencies
                       (list (present-spec "a"))
                       query
                       :conflict-strat :prioritized
                       :search-strat :breadth-first
                       :compare #'maven-vercmp)))
      (true (successful-result bf-result))
      (let ((ids (ids-set (successful-result bf-result))))
        (true (fset:member? "a" ids))
        (true (fset:member? "b" ids))
        (true (fset:member? "c" ids))))
    ;; Depth-first with prioritized: a, b, c@2.7.0
    (let ((df-result (resolve-dependencies
                       (list (present-spec "a"))
                       query
                       :conflict-strat :prioritized
                       :search-strat :depth-first
                       :compare #'maven-vercmp)))
      (true (successful-result df-result))
      (let ((ids (ids-set (successful-result df-result))))
        (true (fset:member? "a" ids))
        (true (fset:member? "b" ids))
        (true (fset:member? "c" ids))))))
