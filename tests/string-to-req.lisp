;;;; tests/string-to-req.lisp
;;;;
;;;; Ported from degasolv/test/degasolv/resolver/string_to_req_test.clj
;;;;
;;;; Tests string-to-requirement parsing: basic cases, comparison operators,
;;;; matches, pessimistic greater, ranges, and compound requirements.

(defpackage #:com.djhaskin.dsolv/tests/string-to-req
  (:use #:cl)
  (:import-from #:com.djhaskin.dsolv/resolver
    #:string-to-requirement
    #:present
    #:absent
    #:req-status
    #:req-id
    #:req-spec
    #:vp-relation
    #:vp-version
    #:requirement
    #:version-predicate
    #:make-requirement
    #:make-version-predicate)
  (:import-from #:parachute
    #:define-test
    #:true
    #:is
    #:false))

(in-package #:com.djhaskin.dsolv/tests/string-to-req)

;;; ─── Helpers ────────────────────────────────────────────────────────────────

(defun req= (a b)
  "Compare two requirement structs structurally."
  (and (eql (req-status a) (req-status b))
       (string= (req-id a) (req-id b))
       (spec= (req-spec a) (req-spec b))))

(defun spec= (a b)
  "Compare two spec lists (disjunctions of conjunctions of version-predicates)."
  (if (and (null a) (null b))
      t
      (and (= (length a) (length b))
           (every #'disj= a b))))

(defun disj= (a b)
  "Compare two disjunction lists (conjunctions of version-predicates)."
  (if (and (null a) (null b))
      t
      (and (= (length a) (length b))
           (every #'vp= a b))))

(defun vp= (a b)
  "Compare two version-predicate structs."
  (and (eql (vp-relation a) (vp-relation b))
       (string= (vp-version a) (vp-version b))))

(defun reqlist= (list-a list-b)
  "Compare two lists of requirement structs."
  (and (= (length list-a) (length list-b))
       (every #'req= list-a list-b)))

(defmacro is-req= (expected-str form)
  "Assert that STRING-TO-REQUIREMENT on EXPECTED-STR yields the given FORM."
  `(true (reqlist= (string-to-requirement ,expected-str) ,form)))

;;; ─── Tests ──────────────────────────────────────────────────────────────────

(define-test test-string-to-requirement-basic-cases
  :parent nil
  ;; Basic case
  (is-req= "a" (list (present "a")))
  ;; Empty case
  (true (null (string-to-requirement "")))
  ;; Comparative cases
  (is-req= "a<1.0.0"
    (list (present "a" (list (list (make-version-predicate
                                    :relation :less-than
                                    :version "1.0.0"))))))
  (is-req= "a<=whatever"
    (list (present "a" (list (list (make-version-predicate
                                    :relation :less-equal
                                    :version "whatever"))))))
  (is-req= "a!=notvalidated"
    (list (present "a" (list (list (make-version-predicate
                                    :relation :not-equal
                                    :version "notvalidated"))))))
  (is-req= "!z!=0000"
    (list (absent "z" (list (list (make-version-predicate
                                   :relation :not-equal
                                   :version "0000"))))))
  (is-req= "!z==alakazam"
    (list (absent "z" (list (list (make-version-predicate
                                   :relation :equal-to
                                   :version "alakazam"))))))
  (is-req= "!z>=barbar"
    (list (absent "z" (list (list (make-version-predicate
                                   :relation :greater-equal
                                   :version "barbar"))))))
  (is-req= "x>2.3.3"
    (list (present "x" (list (list (make-version-predicate
                                    :relation :greater-than
                                    :version "2.3.3")))))))

(define-test test-string-to-requirement-matches
  :parent nil
  (is-req= "a<>f[ea]{2}ture"
    (list (present "a" (list (list (make-version-predicate
                                    :relation :matches
                                    :version "f[ea]{2}ture")))))))

(define-test test-string-to-requirement-pess-greater
  :parent nil
  (is-req= "a><3.2.1"
    (list (present "a" (list (list (make-version-predicate
                                    :relation :pess-greater
                                    :version "3.2.1")))))))

(define-test test-string-to-requirement-range
  :parent nil
  (is-req= "a=>3"
    (list (present "a" (list (list (make-version-predicate
                                    :relation :in-range
                                    :version "3")))))))

(define-test test-string-to-requirement-illustrations
  :parent nil
  ;; Illustrative example: compound requirement with multiple alternatives
  (is-req= "a>=3.0.0,<4.0.0;>=2.0.0,<2.5.1|b>=1.0.0,!=1.5.0"
    (list
      (present "a"
        (list
          (list (make-version-predicate :relation :greater-equal :version "3.0.0")
                (make-version-predicate :relation :less-than :version "4.0.0"))
          (list (make-version-predicate :relation :greater-equal :version "2.0.0")
                (make-version-predicate :relation :less-than :version "2.5.1"))))
      (present "b"
        (list
          (list (make-version-predicate :relation :greater-equal :version "1.0.0")
                (make-version-predicate :relation :not-equal :version "1.5.0"))))))
  ;; Managed dependencies: absent + present with range
  (is-req= "!a|a>1.0.0,<=4.0.0;>=6.0.0,<7.0.0"
    (list
      (absent "a" nil)
      (present "a"
        (list
          (list (make-version-predicate :relation :greater-than :version "1.0.0")
                (make-version-predicate :relation :less-equal :version "4.0.0"))
          (list (make-version-predicate :relation :greater-equal :version "6.0.0")
                (make-version-predicate :relation :less-than :version "7.0.0")))))))

(define-test test-string-to-requirement-prints
  :parent nil
  ;; Matching prints
  (is string= "a<>f[ea]{2}ture"
    (princ-to-string (present "a"
      (list (list (make-version-predicate
                    :relation :matches
                    :version "f[ea]{2}ture"))))))
  ;; Range prints
  (is string= "a=>3"
    (princ-to-string (present "a"
      (list (list (make-version-predicate
                    :relation :in-range
                    :version "3")))))))
