;;;; tests/subproc.lisp
;;;;
;;;; Ported from degasolv/test/degasolv/pkgsys/subproc_test.clj
;;;;
;;;; Tests for the subprocess package system: convert-input function
;;;; that converts raw repository info to package info structures.

(defpackage #:com.djhaskin.dsolv/tests/subproc
  (:use #:cl)
  (:import-from #:com.djhaskin.dsolv/resolver
    #:present
    #:make-version-predicate
    #:pi-id
    #:pi-version
    #:pi-location
    #:pi-requirements
    #:make-package-info)
  (:import-from #:com.djhaskin.dsolv/pkgsys/subproc
    #:convert-input)
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

(in-package #:com.djhaskin.dsolv/tests/subproc)

;;; ─── Helper: compare package-info structures ────────────────────────────────

(defun pkg= (a b)
  "Compare two package-info structures for equality."
  (and (string= (pi-id a) (pi-id b))
       (string= (pi-version a) (pi-version b))
       (string= (pi-location a) (pi-location b))))

;;; ─── Tests: convert-input ───────────────────────────────────────────────────

(define-test convert-input-test
  :parent nil
  "Test the convert-input function that converts raw repository info
   from external executables to fset maps of package-info structures."
  ;; Empty cases
  (true (fset:empty? (convert-input (f:empty-map))))
  (let ((result (convert-input
                  (fset:convert 'fset:map
                    (list (cons "a" (f:empty-seq))
                          (cons "b" (f:empty-seq)))))))
    (true (not (null (fset:lookup result "a"))))
    (true (not (null (fset:lookup result "b"))))
    (true (fset:empty? (fset:lookup result "a")))
    (true (fset:empty? (fset:lookup result "b"))))
  ;; Basic case
  (let* ((raw (fset:convert 'fset:map
                (list (cons "a"
                        (fset:convert 'fset:seq
                          (list (fset:convert 'fset:map
                                  (list (cons :id "a")
                                        (cons :version "1.0.0")
                                        (cons :location "yurt")))))))))
         (result (convert-input raw))
         (pkgs (fset:lookup result "a")))
    (true (not (null pkgs)))
    (let ((pkg (fset:first pkgs)))
      (is string= "a" (pi-id pkg))
      (is string= "1.0.0" (pi-version pkg))
      (is string= "yurt" (pi-location pkg))))
  ;; Metadata case
  (let* ((raw (fset:convert 'fset:map
                (list (cons "a"
                        (fset:convert 'fset:seq
                          (list (fset:convert 'fset:map
                                  (list (cons :id "a")
                                        (cons :version "1.0.0")
                                        (cons :location "yurt")
                                        (cons :foo "bar")))))))))
         (result (convert-input raw))
         (pkgs (fset:lookup result "a")))
    (true (not (null pkgs)))
    (let ((pkg (fset:first pkgs)))
      (is string= "a" (pi-id pkg))
      (is string= "1.0.0" (pi-version pkg))
      (is string= "yurt" (pi-location pkg))))
  ;; Requirements case
  (let* ((raw (fset:convert 'fset:map
                (list (cons "a"
                        (fset:convert 'fset:seq
                          (list (fset:convert 'fset:map
                                  (list (cons :id "a")
                                        (cons :version "1.0.0")
                                        (cons :location "yurt")
                                        (cons :requirements
                                          (fset:convert 'fset:seq
                                            (list "b>=2.0,<3.0|c")))))))))))
         (result (convert-input raw))
         (pkgs (fset:lookup result "a")))
    (true (not (null pkgs)))
    (let ((pkg (fset:first pkgs)))
      (is string= "a" (pi-id pkg))
      (is string= "1.0.0" (pi-version pkg))
      (is string= "yurt" (pi-location pkg))
      ;; Should have parsed requirements
      (true (not (null (pi-requirements pkg)))))))
