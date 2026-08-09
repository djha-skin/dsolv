;;;; tests/pkgsys-core.lisp
;;;;
;;;; Regression tests for the degasolv package system core: the
;;;; generate-repo-index -> slurp-degasolv-repo round trip. Guards
;;;; dsolv-ixz: generated indexes must decode into package-info
;;;; structs (not raw fset maps) and sort correctly.

(defpackage #:com.djhaskin.dsolv/tests/pkgsys-core
  (:use #:cl)
  (:import-from #:com.djhaskin.dsolv
    #:main)
  (:import-from #:com.djhaskin.dsolv/resolver
    #:pi-id
    #:pi-version
    #:pi-location
    #:pi-requirements)
  (:import-from #:com.djhaskin.dsolv/pkgsys/core
    #:generate-repo-index
    #:slurp-degasolv-repo)
  (:import-from #:com.djhaskin.svers
    #:semver-vercmp)
  (:import-from #:fset)
  (:import-from #:uiop)
  (:import-from #:parachute
    #:define-test
    #:true
    #:is)
  (:local-nicknames
    (#:f #:fset)))

(in-package #:com.djhaskin.dsolv/tests/pkgsys-core)

(defun make-test-dir ()
  "Create and return a fresh temporary directory for index tests."
  (let ((dir (uiop:ensure-directory-pathname
               (merge-pathnames
                 (make-pathname
                   :directory (list :relative
                                    (format nil "dsolv-ixz-test-~a"
                                            (random 100000))))
                 (uiop:temporary-directory)))))
    (ensure-directories-exist dir)
    dir))

(defun make-card (dir id version location &optional requirement)
  "Run the generate-card CLI subcommand to write a card for
   (ID VERSION LOCATION) into DIR, optionally requiring REQUIREMENT.
   Matches what the POSIX shell scripts do."
  (apply #'main "generate-card"
         "-i" id
         "-v" version
         "-l" location
         "-C" (namestring
                (merge-pathnames
                  (make-pathname :name (format nil "~a-~a.zip" id version)
                                 :type "dscard")
                  dir))
         (when requirement (list "-r" requirement))))

(defun ascending-sortindex (packages)
  "Sort PACKAGES ascending by semantic version."
  (f:sort packages
          (lambda (a b)
            (minusp (funcall #'semver-vercmp
                             (pi-version a)
                             (pi-version b))))))

(defun query-packages (index-file id)
  "Slurp INDEX-FILE and return the packages for ID as a list of
   package-info structs."
  (let* ((queries (slurp-degasolv-repo index-file))
         (query (first queries))
         (result (funcall query id)))
    (when result
      (f:convert 'list result))))

(define-test generate-repo-index-round-trips-sorted-packages
             :parent nil
             "An index generated from cards decodes back into sorted
   package-info structs, not raw fset maps."
             (let ((dir (make-test-dir)))
               (unwind-protect
                   (let* ((index-file (merge-pathnames "index.dsrepo" dir)))
                     (make-card dir "a" "1.0.0" "https://example.com/repo/a-1.0.0.zip")
                     (make-card dir "a" "2.0.0" "https://example.com/repo/a-2.0.0.zip")
                     (generate-repo-index dir index-file
                                          :sortindex #'ascending-sortindex)
                     (let ((pkgs (query-packages index-file "a")))
                       (is = 2 (length pkgs))
                       (is string= "a" (pi-id (first pkgs)))
                       (is string= "1.0.0" (pi-version (first pkgs)))
                       (is string= "2.0.0" (pi-version (second pkgs)))))
                 (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore))))

(define-test generate-repo-index-requirements-round-trip
             :parent nil
             "A card with a requirement serializes into the index and reads back
   with its requirement intact."
             (let ((dir (make-test-dir)))
               (unwind-protect
                   (let* ((index-file (merge-pathnames "index.dsrepo" dir)))
                     (make-card dir "a" "1.0.0" "https://example.com/repo/a-1.0.0.zip"
                                "b")
                     (generate-repo-index dir index-file)
                     (let ((pkgs (query-packages index-file "a")))
                       (is = 1 (length pkgs))
                       (is string= "a" (pi-id (first pkgs)))
                       (is string= "1.0.0" (pi-version (first pkgs)))
                       (is string= "https://example.com/repo/a-1.0.0.zip"
                           (pi-location (first pkgs)))
                       (true (not (null (pi-requirements (first pkgs)))))))
                 (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore))))
