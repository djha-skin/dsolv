;;;; src/util.lisp
;;;;
;;;; Utility functions for dsolv: repository aggregation and map-query.
;;;; Data-slurp and base-slurp are provided by CLIFF; we use them here.

(defpackage #:com.djhaskin.dsolv/util
  (:use #:cl)
  (:import-from #:com.djhaskin.cliff
    #:data-slurp
    #:base-slurp)
  (:import-from #:com.djhaskin.nrdl)
  (:import-from #:fset)
  (:import-from #:alexandria)
  (:local-nicknames
    (#:nrdl #:com.djhaskin.nrdl)
    (#:f #:fset)
    (#:dsolv/util #:com.djhaskin.dsolv/util))
  (:export
    #:map-query
    #:priority-repo
    #:global-repo
    #:aggregator
    #:data-slurp
    #:base-slurp))

(in-package #:com.djhaskin.dsolv/util)

(defun map-query (m)
  "Create a lookup function from an fset map."
  (lambda (nm)
    (f:lookup m nm)))

(defun priority-repo (repos)
  "Create a priority-ordered repository query function.
  Returns the first non-empty result from the list of repo query functions."
  (lambda (id)
    (some (lambda (r)
            (let ((result (funcall r id)))
              (when (and result (not (f:empty? result)))
                result)))
          repos)))

(defun global-repo (repos &key (comparator #'string-lessp))
  "Create a globally-ordered repository query function.
  Combines results from all repos and sorts them by COMPARATOR."
  (lambda (id)
    (let* ((all-results
             (reduce #'f:concat
                     (mapcar (lambda (r) (funcall r id)) repos)
                     :initial-value (f:empty-seq)))
           (sorted (f:sort
                     all-results
                     (lambda (a b)
                       (funcall comparator
                                (f:lookup a :version)
                                (f:lookup b :version))))))
      sorted)))

(defun aggregator (index-strat comparator)
  "Return a repository aggregation function based on INDEX-STRAT.
  When INDEX-STRAT is \"priority\", use PRIORITY-REPO.
  When INDEX-STRAT is \"global\", use GLOBAL-REPO."
  (cond
    ((string= index-strat "priority")
     (function priority-repo))
    ((string= index-strat "global")
     (lambda (repos)
       (global-repo repos :comparator comparator)))
    (t
     (function priority-repo))))