;;;; src/pkgsys/core.lisp
;;;;
;;;; Degasolv package system: reading .dscard files and generating
;;;; repository indices.
;;;;
;;;; Ported from degasolv's cli-src/degasolv/pkgsys/core.clj

(defpackage #:com.djhaskin.dsolv/pkgsys/core
  (:use #:cl)
  (:import-from #:com.djhaskin.dsolv/resolver
    #:package-info #:pi-id #:pi-version #:pi-location #:pi-requirements
    #:make-package-info)
  (:import-from #:com.djhaskin.dsolv/util
    #:map-query)
  (:import-from #:com.djhaskin.cliff
    #:data-slurp)
  (:import-from #:com.djhaskin.nrdl
    #:parse-from
    #:generate-to
    #:to-fset)
  (:import-from #:fset)
  (:import-from #:uiop)
  (:local-nicknames
    (#:f #:fset)
    (#:nrdl #:com.djhaskin.nrdl)
    (#:resolver #:com.djhaskin.dsolv/resolver))
  (:export
    #:generate-repo-index
    #:slurp-degasolv-repo))

(in-package #:com.djhaskin.dsolv/pkgsys/core)

(defun read-card (card-path)
  "Read a .dscard file and return the package data as an fset map.
   Uses NRDL to parse the tagged data format."
  (let* ((card-string (data-slurp card-path))
         (card-data (nrdl:parse-from
                      (make-string-input-stream card-string)))
         ;; Convert hash tables to fset maps for consistency
         (fset-data (to-fset card-data)))
    fset-data))

(defun generate-repo-index (search-directory index-file
                             &key add-to sortindex)
  "Generate a repository index from .dscard files in SEARCH-DIRECTORY.
   
   Walks SEARCH-DIRECTORY recursively, finds all .dscard files,
   reads them, and writes a consolidated index to INDEX-FILE.
   
   If ADD-TO is provided, the existing index at that path is used
   as the base (new entries are merged in).
   
   SORTINDEX is a function that takes a sequence of packages and
   returns a sorted sequence. If not provided, packages are kept
   in the order they are encountered."
  (let* ((initial-repository
           (if add-to
               (let* ((add-string (data-slurp add-to))
                      (add-data (nrdl:parse-from
                                  (make-string-input-stream add-string))))
                 (to-fset add-data))
               (f:empty-map)))
         (dscard-files
           (uiop:directory-files search-directory "*.dscard"))
         (merged-repository
           (reduce
             (lambda (acc card-path)
               (let* ((card-data (read-card card-path))
                      (pkg-id (fset:lookup card-data :id)))
                 (if pkg-id
                     (fset:with acc pkg-id
                       (let ((existing (fset:lookup acc pkg-id)))
                         (if existing
                             (fset:push-last existing card-data)
                             (fset:with (f:empty-seq) card-data))))
                     acc)))
             dscard-files
             :initial-value initial-repository))
         ;; Apply sortindex if provided
         (sorted-repository
           (if sortindex
               (let ((result (f:empty-map)))
                 (fset:do-map (id packages merged-repository)
                   (setf result (f:with result id
                                         (funcall sortindex packages))))
                 result)
               merged-repository)))
    ;; Write the index file
    (with-open-file (stream index-file
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (nrdl:generate-to stream sorted-repository
                         :pretty-indent 2))
    sorted-repository))

(defun slurp-degasolv-repo (url)
  "Read a degasolv repository from URL, returning a list of query functions.
   
   The repository file is expected to be in NRDL format, containing
   an fset map from package IDs to sequences of package data.
   
   Returns a list containing a single memoized query function, matching
   the Clojure semantics where slurp-degasolv-repo returns a vector of
   query functions."
  (let* ((repo-string (data-slurp url))
         (repo-data (nrdl:parse-from
                      (make-string-input-stream repo-string)))
         (fset-repo (to-fset repo-data)))
    ;; Convert the repo data to a query function via map-query
    (list (map-query fset-repo))))