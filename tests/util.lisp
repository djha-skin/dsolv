;;;; tests/util.lisp
;;;;
;;;; Ported (in spirit) from degasolv/test/degasolv/util/core_test.clj
;;;;
;;;; The original Clojure test tested `default-slurp` with HTTP authentication
;;;; using a wiremock server. In dsolv we use CLIFF's `data-slurp` and
;;;; `base-slurp` instead, so that test is not applicable (see AGENTS.md:
;;;; "CLIFF has data-slurp/base-slurp — don't port those from degasolv").
;;;;
;;;; This file tests the remaining utility functions from src/util.lisp
;;;; that are not already covered by tests/repo-aggregation.lisp:
;;;;   - map-query
;;;;   - aggregator

(defpackage #:com.djhaskin.dsolv/tests/util
  (:use #:cl)
  (:import-from #:com.djhaskin.dsolv/util
    #:map-query
    #:priority-repo
    #:global-repo
    #:aggregator)
  (:import-from #:fset)
  (:import-from #:parachute
    #:define-test
    #:true
    #:is
    #:false)
  (:local-nicknames
    (#:f #:fset)))

(in-package #:com.djhaskin.dsolv/tests/util)

;;; ─── Helpers ────────────────────────────────────────────────────────────────

(defun make-pkg (id version location)
  "Create an fset map representing a package entry."
  (f:with (f:with (f:with (f:empty-map) :id id) :version version) :location location))

;;; ─── map-query tests ────────────────────────────────────────────────────────

(define-test map-query-empty-map
  :parent nil
  (true (f:empty? (funcall (map-query (f:empty-map)) "a")))
  (true (f:empty? (funcall (map-query (f:empty-map)) "")))
  (true (f:empty? (funcall (map-query (f:empty-map)) nil))))

(define-test map-query-single-entry
  :parent nil
  (let* ((pkg-a (make-pkg "a" "1.0" "loc_a"))
         (m (f:with (f:empty-map) "a" (f:with-last (f:empty-seq) pkg-a)))
         (q (map-query m)))
    (true (f:equal? (funcall q "a")
                    (f:with-last (f:empty-seq) pkg-a)))
    (true (f:empty? (funcall q "b")))
    (true (f:empty? (funcall q "")))))

(define-test map-query-multiple-entries
  :parent nil
  (let* ((pkg-a (make-pkg "a" "1.0" "loc_a"))
         (pkg-b (make-pkg "b" "2.0" "loc_b"))
         (pkg-c (make-pkg "c" "3.0" "loc_c"))
         (m (f:with (f:with (f:with (f:empty-map)
                                    "a" (f:with-last (f:empty-seq) pkg-a))
                            "b" (f:with-last (f:empty-seq) pkg-b))
                    "c" (f:with-last (f:empty-seq) pkg-c)))
         (q (map-query m)))
    (true (f:equal? (funcall q "a")
                    (f:with-last (f:empty-seq) pkg-a)))
    (true (f:equal? (funcall q "b")
                    (f:with-last (f:empty-seq) pkg-b)))
    (true (f:equal? (funcall q "c")
                    (f:with-last (f:empty-seq) pkg-c)))
    (true (f:empty? (funcall q "d")))))

(define-test map-query-multiple-versions
  :parent nil
  (let* ((pkg-a1 (make-pkg "a" "1.0" "loc_a1"))
         (pkg-a2 (make-pkg "a" "2.0" "loc_a2"))
         (m (f:with (f:empty-map) "a"
                    (f:with-last (f:with-last (f:empty-seq) pkg-a1) pkg-a2)))
         (q (map-query m))
         (result (funcall q "a")))
    (true (not (f:empty? result)))
    (true (f:equal? (f:first result) pkg-a1))
    (true (f:equal? (f:last result) pkg-a2))))

;;; ─── aggregator tests ───────────────────────────────────────────────────────

(define-test aggregator-priority
  :parent nil
  (let* ((pkg-a (make-pkg "a" "1.0" "loc_a"))
         (repo1 (f:with (f:empty-map) "a"
                        (f:with-last (f:empty-seq) pkg-a)))
         (agg (aggregator "priority" nil))
         (combined (funcall agg (list (map-query (f:empty-map))
                                      (map-query repo1))))
         (result (funcall combined "a")))
    (true (not (f:empty? result)))
    (true (f:equal? (f:first result) pkg-a))))

(define-test aggregator-priority-prefers-first
  :parent nil
  (let* ((pkg-a1 (make-pkg "a" "1.0" "loc_a1"))
         (pkg-a2 (make-pkg "a" "2.0" "loc_a2"))
         (repo1 (f:with (f:empty-map) "a"
                        (f:with-last (f:empty-seq) pkg-a1)))
         (repo2 (f:with (f:empty-map) "a"
                        (f:with-last (f:empty-seq) pkg-a2)))
         (agg (aggregator "priority" nil))
         (combined (funcall agg (list (map-query repo1)
                                      (map-query repo2))))
         (result (funcall combined "a")))
    (true (not (f:empty? result)))
    (true (f:equal? (f:first result) pkg-a1))
    (true (string= (f:lookup (f:first result) :version) "1.0"))))

(define-test aggregator-global
  :parent nil
  (let* ((pkg-a1 (make-pkg "a" "1.0" "loc_a1"))
         (pkg-a2 (make-pkg "a" "2.0" "loc_a2"))
         (repo1 (f:with (f:empty-map) "a"
                        (f:with-last (f:empty-seq) pkg-a1)))
         (repo2 (f:with (f:empty-map) "a"
                        (f:with-last (f:empty-seq) pkg-a2)))
         (agg (aggregator "global" #'string-lessp))
         (combined (funcall agg (list (map-query repo1)
                                      (map-query repo2))))
         (result (funcall combined "a")))
    (true (not (f:empty? result)))
    ;; With global-repo, results are sorted by version.
    ;; String-lessp sorts "1.0" before "2.0"
    (true (f:equal? (f:first result) pkg-a1))
    (true (f:equal? (f:last result) pkg-a2))))

(define-test aggregator-defaults-to-priority
  :parent nil
  (let* ((pkg-a (make-pkg "a" "1.0" "loc_a"))
         (repo1 (f:with (f:empty-map) "a"
                        (f:with-last (f:empty-seq) pkg-a)))
         (agg (aggregator "unknown-strategy" nil))
         (combined (funcall agg (list (map-query (f:empty-map))
                                      (map-query repo1))))
         (result (funcall combined "a")))
    ;; Unknown strategy defaults to priority
    (true (not (f:empty? result)))
    (true (f:equal? (f:first result) pkg-a))))