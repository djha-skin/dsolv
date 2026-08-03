;;;; tests/performance.lisp
;;;;
;;;; Ported from degasolv/test/degasolv/resolver/performance_test.clj
;;;;
;;;; Tests for query pruning: ensure the resolver doesn't make unnecessary
;;;; repo queries during dependency resolution.  Uses a sensing-query wrapper
;;;; that counts calls to the underlying query function.

(defpackage #:com.djhaskin.dsolv/tests/performance
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
    #:false
    #:is)
  (:local-nicknames
    (#:f #:fset)))

(in-package #:com.djhaskin.dsolv/tests/performance)

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

(defun make-sensing-query (raw-query counter)
  "Create a query function that counts calls via COUNTER.
   COUNTER should be a hash table with :repo-query-count key."
  (lambda (id)
    (incf (gethash :repo-query-count counter 0))
    (funcall raw-query id)))

;;; ─── Test: pruning candidates ───────────────────────────────────────────────
;;;
;;; a@1.2.0 requires b
;;; a@1.1.0 requires b OR c
;;; b@2.3.0 has no requirements
;;; c@2.4.7 has no requirements
;;;
;;; Resolving a>1.0.0 should pick a@1.2.0 (requires only b), so only
;;; 2 repo queries (a, b) should be needed.

(define-test pruning-candidates-test
  :parent nil
  (let* ((repo-info
           (make-repo-map
             (list (cons "a"
                         (list (make-pkg "a" "1.2.0" "http://example.com/repo/a-1.2.0.zip"
                                (list (present-spec "b")))
                               (make-pkg "a" "1.1.0" "http://example.com/repo/a-1.1.0.zip"
                                ;; One clause with two alternatives: (b) OR (c)
                                (list (append (present-spec "b")
                                            (present-spec "c"))))))
                   (cons "b"
                         (list (make-pkg "b" "2.3.0" "http://example.com/repo/b-2.3.0.zip" nil)))
                   (cons "c"
                         (list (make-pkg "c" "2.4.7" "http://example.com/repo/c-2.4.7.zip" nil))))))
         (raw-query (map-query repo-info))
         (counter (make-hash-table))
         (sensing-query (make-sensing-query raw-query counter))
         (result (resolve-dependencies
                   (list (present-spec "a"
                           (make-version-predicate
                             :relation :greater-than :version "1.0.0")))
                   sensing-query
                   :compare #'maven-vercmp)))
    (true (successful-result result))
    (let ((locs (fset:convert 'fset:set
                              (mapcar #'pi-location (successful-result result)))))
      (true (fset:member? "http://example.com/repo/a-1.2.0.zip" locs))
      (true (fset:member? "http://example.com/repo/b-2.3.0.zip" locs)))
    (is = 2 (gethash :repo-query-count counter 0))))

;;; ─── Test: pruning circular dependencies ────────────────────────────────────
;;;
;;; a@1.2.0 requires b
;;; b@2.3.0 requires a
;;;
;;; Circular dependency: a needs b, b needs a.  Resolver should handle
;;; this with only 2 repo queries (a, b).

(define-test pruning-circular-test
  :parent nil
  (let* ((repo-info
           (make-repo-map
             (list (cons "a"
                         (list (make-pkg "a" "1.2.0" "http://example.com/repo/a-1.2.0.zip"
                                (list (present-spec "b")))))
                   (cons "b"
                         (list (make-pkg "b" "2.3.0" "http://example.com/repo/b-2.3.0.zip"
                                (list (present-spec "a"))))))))
         (raw-query (map-query repo-info))
         (counter (make-hash-table))
         (sensing-query (make-sensing-query raw-query counter))
         (result (resolve-dependencies
                   (list (present-spec "a"
                           (make-version-predicate
                             :relation :greater-than :version "1.0.0")))
                   sensing-query
                   :compare #'maven-vercmp)))
    (true (successful-result result))
    (let ((locs (fset:convert 'fset:set
                              (mapcar #'pi-location (successful-result result)))))
      (true (fset:member? "http://example.com/repo/a-1.2.0.zip" locs))
      (true (fset:member? "http://example.com/repo/b-2.3.0.zip" locs)))
    (is = 2 (gethash :repo-query-count counter 0))))

;;; ─── Test: pruning circular self-dependency ─────────────────────────────────
;;;
;;; a@1.2.0 requires a (self-dependency)
;;;
;;; Resolver should handle self-dependency with only 1 repo query.

(define-test pruning-circular-second-test
  :parent nil
  (let* ((repo-info
           (make-repo-map
             (list (cons "a"
                         (list (make-pkg "a" "1.2.0" "http://example.com/repo/a-1.2.0.zip"
                                (list (present-spec "a"))))))))
         (raw-query (map-query repo-info))
         (counter (make-hash-table))
         (sensing-query (make-sensing-query raw-query counter))
         (result (resolve-dependencies
                   (list (present-spec "a"
                           (make-version-predicate
                             :relation :greater-than :version "1.0.0")))
                   sensing-query
                   :compare #'maven-vercmp)))
    (true (successful-result result))
    (let ((locs (fset:convert 'fset:set
                              (mapcar #'pi-location (successful-result result)))))
      (true (fset:member? "http://example.com/repo/a-1.2.0.zip" locs)))
    (is = 1 (gethash :repo-query-count counter 0))))

;;; ─── Test: pruning alternatives ─────────────────────────────────────────────
;;;
;;; a@1.2.0 requires c OR b
;;; b@2.3.0 has no requirements
;;; c@2.4.7 has no requirements
;;;
;;; With alternatives, resolver explores c first (picks it), then doesn't
;;; need to query b.  So only 2 queries (a, c).

(define-test pruning-alternatives-test
  :parent nil
  (let* ((repo-info
           (make-repo-map
             (list (cons "a"
                         (list (make-pkg "a" "1.2.0" "http://example.com/repo/a-1.2.0.zip"
                                ;; One clause with two alternatives: (c) OR (b)
                                (list (append (present-spec "c")
                                            (present-spec "b"))))))
                   (cons "b"
                         (list (make-pkg "b" "2.3.0" "http://example.com/repo/b-2.3.0.zip" nil)))
                   (cons "c"
                         (list (make-pkg "c" "2.4.7" "http://example.com/repo/c-2.4.7.zip" nil))))))
         (raw-query (map-query repo-info))
         (counter (make-hash-table))
         (sensing-query (make-sensing-query raw-query counter))
         (result (resolve-dependencies
                   (list (present-spec "a"
                           (make-version-predicate
                             :relation :greater-than :version "1.0.0")))
                   sensing-query
                   :compare #'maven-vercmp)))
    (true (successful-result result))
    (let ((locs (fset:convert 'fset:set
                              (mapcar #'pi-location (successful-result result)))))
      (true (fset:member? "http://example.com/repo/a-1.2.0.zip" locs))
      (true (fset:member? "http://example.com/repo/c-2.4.7.zip" locs)))
    (is = 2 (gethash :repo-query-count counter 0))))

;;; ─── Test: diamond pruning ──────────────────────────────────────────────────
;;;
;;; a@1.2.0 requires b OR c
;;; b@2.3.0 requires c
;;; c@2.4.7 has no requirements
;;;
;;; Diamond dependency: a needs b or c, b needs c.  Resolver should
;;; make 3 queries (a, b, c).

(define-test diamond-pruning-test
  :parent nil
  (let* ((repo-info
           (make-repo-map
             (list (cons "a"
                         (list (make-pkg "a" "1.2.0" "http://example.com/repo/a-1.2.0.zip"
                                ;; One clause with two alternatives: (b) OR (c)
                                (list (append (present-spec "b")
                                            (present-spec "c"))))))
                   (cons "b"
                         (list (make-pkg "b" "2.3.0" "http://example.com/repo/b-2.3.0.zip"
                                (list (present-spec "c")))))
                   (cons "c"
                         (list (make-pkg "c" "2.4.7" "http://example.com/repo/c-2.4.7.zip" nil))))))
         (raw-query (map-query repo-info))
         (counter (make-hash-table))
         (sensing-query (make-sensing-query raw-query counter))
         (result (resolve-dependencies
                   (list (present-spec "a"
                           (make-version-predicate
                             :relation :greater-than :version "1.0.0")))
                   sensing-query
                   :compare #'maven-vercmp)))
    (true (successful-result result))
    (let ((locs (fset:convert 'fset:set
                              (mapcar #'pi-location (successful-result result)))))
      (true (fset:member? "http://example.com/repo/a-1.2.0.zip" locs))
      (true (fset:member? "http://example.com/repo/c-2.4.7.zip" locs))
      (true (fset:member? "http://example.com/repo/b-2.3.0.zip" locs)))
    (is = 3 (gethash :repo-query-count counter 0))))
