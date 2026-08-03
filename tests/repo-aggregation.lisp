;;;; tests/repo-aggregation.lisp
;;;;
;;;; Ported from degasolv/test/degasolv/resolver/repo_aggregation_test.clj
;;;; Tests priority-repo and global-repo aggregation strategies.

(defpackage #:com.djhaskin.dsolv/tests/repo-aggregation
  (:use #:cl)
  (:import-from #:com.djhaskin.dsolv/util
    #:map-query
    #:priority-repo
    #:global-repo)
  (:import-from #:fset)
  (:import-from #:parachute
    #:define-test
    #:true
    #:is)
  (:local-nicknames
    (#:f #:fset)))

(in-package #:com.djhaskin.dsolv/tests/repo-aggregation)

(defun make-pkg (id version location)
  "Create an fset map representing a package entry."
  (f:with (f:with (f:with (f:empty-map) :id id) :version version) :location location))

(defun make-single-repo (key pkg)
  "Create an fset map with one key mapped to a seq containing one package."
  (f:with (f:empty-map) key
    (f:with-last (f:empty-seq) pkg)))

(defun make-repo-two (key pkg1 pkg2)
  "Create an fset map with one key mapped to a seq containing two packages."
  (f:with (f:empty-map) key
    (f:with-last (f:with-last (f:empty-seq) pkg1) pkg2)))

(define-test priority-repo-test
  :parent nil
  (true (f:empty?
         (funcall (priority-repo
                   (list (map-query (f:empty-map))
                         (map-query (f:empty-map))))
                  "a")))
  (true (f:empty?
         (funcall (priority-repo
                   (list (map-query
                          (make-single-repo "b" (make-pkg "b" "1" "loc_b1")))
                         (map-query
                          (make-single-repo "c" (make-pkg "c" "2" "loc_c2")))))
                  "a")))
  (true (f:equal?
         (funcall (priority-repo
                   (list (map-query (f:empty-map))
                         (map-query
                          (make-single-repo "a" (make-pkg "a" "10" "loc_a")))))
                  "a")
         (f:convert 'f:seq (list (make-pkg "a" "10" "loc_a")))))
  (true (f:equal?
         (funcall (priority-repo
                   (list (map-query (f:empty-map))
                         (map-query
                          (make-single-repo "a" (make-pkg "a" "10" "loc_a10")))
                         (map-query
                          (make-repo-two "a"
                           (make-pkg "a" "20" "loc_a20")
                           (make-pkg "a" "30" "loc_a30")))))
                  "a")
         (f:convert 'f:seq (list (make-pkg "a" "10" "loc_a10"))))))

(define-test global-repo-test
  :parent nil
  (true (f:empty?
         (funcall (global-repo
                   (list (map-query (f:empty-map))
                         (map-query (f:empty-map))))
                  "a")))
  (true (f:empty?
         (funcall (global-repo
                   (list (map-query
                          (make-single-repo "b" (make-pkg "b" "1" "loc_b1")))
                         (map-query
                          (make-single-repo "c" (make-pkg "c" "2" "loc_c2")))))
                  "a")))
  (true (f:equal?
         (funcall (global-repo
                   (list (map-query (f:empty-map))
                         (map-query
                          (make-single-repo "a" (make-pkg "a" "10" "loc_a")))))
                  "a")
         (f:convert 'f:seq (list (make-pkg "a" "10" "loc_a")))))
  (true (f:equal?
         (funcall (global-repo
                   (list (map-query (f:empty-map))
                         (map-query
                          (make-single-repo "a" (make-pkg "a" "10" "loc_a10")))
                         (map-query
                          (make-repo-two "a"
                           (make-pkg "a" "20" "loc_a20")
                           (make-pkg "a" "30" "loc_a30")))))
                  "a")
         (f:convert 'f:seq
          (list (make-pkg "a" "10" "loc_a10")
                (make-pkg "a" "20" "loc_a20")
                (make-pkg "a" "30" "loc_a30"))))))
