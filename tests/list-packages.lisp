;;;; tests/list-packages.lisp
;;;;
;;;; Ported from degasolv/test/degasolv/resolver/list_packages_test.clj
;;;;
;;;; Tests for the list-packages function that flattens a dependency graph
;;;; into a linear list of packages.

(defpackage #:com.djhaskin.dsolv/tests/list-packages
  (:use #:cl)
  (:import-from #:com.djhaskin.dsolv/resolver
    #:list-packages)
  (:import-from #:fset)
  (:import-from #:parachute
    #:define-test
    #:true
    #:false
    #:is)
  (:local-nicknames
    (#:f #:fset)))

(in-package #:com.djhaskin.dsolv/tests/list-packages)

;;; ─── Helper ─────────────────────────────────────────────────────────────────

(defun make-graph (&rest entries)
  "Build an fset map package graph from alternating key/value pairs.
  Each value should be a list of keyword package identifiers.
  Example: (make-graph :root '(:a :b) :a '(:c))"
  (let ((m (f:empty-map)))
    (loop for (key val) on entries by #'cddr
          do (let ((seq (f:empty-seq)))
               (dolist (item val)
                 (setf seq (f:push-last seq item)))
               (setf m (f:with m key seq))))
    m))

;;; ─── Tests ──────────────────────────────────────────────────────────────────

(define-test list-packages-function
  :parent nil

  ;; List an empty graph.
  (is equal nil (list-packages (f:empty-map)))
  (is equal nil (list-packages (f:with (f:empty-map) :root (f:empty-seq))))

  ;; Simple example: root [:a :x :b] with various children.
  (let ((simple-example
          (make-graph
            :root '(:a :x :b)
            :a   '(:c :d)
            :c   nil
            :d   nil
            :x   '(:y :z)
            :y   nil
            :z   nil
            :b   '(:e :a)
            :e   '(:a))))
    (is equal '(:c :d :y :z :a :e :x :b)
        (list-packages simple-example :list-strat :lazy))
    (is equal '(:c :d :a :y :z :x :e :b)
        (list-packages simple-example :list-strat :eager)))

  ;; Circular dependency: :a depends on :b, :b depends on :a.
  (let ((circular-example
          (make-graph
            :root '(:a)
            :a   '(:b)
            :b   '(:a))))
    (is equal '(:b :a)
        (list-packages circular-example :list-strat :lazy))
    (is equal '(:b :a)
        (list-packages circular-example :list-strat :eager)))

  ;; Circular triangle: more complex cycle with multiple roots.
  ;; NOTE: The Clojure original only tests :eager here; the :lazy value in the
  ;; original is just a message string, not an assertion. Both strategies
  ;; produce the same order for this graph due to circular dependencies.
  (let ((circular-triangle
          (make-graph
            :root '(:a :b)
            :a   '(:c)
            :b   '(:c :d)
            :d   '(:a :x)
            :x   nil
            :c   '(:a))))
    (is equal '(:c :a :x :d :b)
        (list-packages circular-triangle :list-strat :lazy))
    (is equal '(:c :a :x :d :b)
        (list-packages circular-triangle :list-strat :eager)))

  ;; Self-reliance: a package that depends on itself.
  (let ((awful-example
          (make-graph
            :root '(:a)
            :a   '(:a))))
    (is equal '(:a)
        (list-packages awful-example :list-strat :lazy))
    (is equal '(:a)
        (list-packages awful-example :list-strat :eager))))