;;;; src/pkgsys/core.lisp
;;;;
;;;; Core package system functions for reading degasolv repositories and
;;;; generating repo indices.

(defpackage #:com.djhaskin.dsolv/pkgsys/core
  (:use #:cl)
  (:import-from #:com.djhaskin.dsolv/util)
  (:import-from #:com.djhaskin.dsolv/resolver)
  (:import-from #:com.djhaskin.nrdl)
  (:local-nicknames
    (#:util #:com.djhaskin.dsolv/util)
    (#:resolver #:com.djhaskin.dsolv/resolver)
    (#:nrdl #:com.djhaskin.nrdl))
  (:export
    #:generate-repo-index
    #:slurp-degasolv-repo
    #:read-card))

(in-package #:com.djhaskin.dsolv/pkgsys/core)

(defun read-card (card-path)
  "Read a .dscard file and return a package-info struct."
  (let* ((card-data (nrdl:parse-from
                      (make-string-input-stream
                        (util:data-slurp card-path))))
         (id (getf card-data :id))
         (version (getf card-data :version))
         (location (getf card-data :location))
         (requirements (getf card-data :requirements)))
    (resolver:make-package-info
      :id id
      :version version
      :location location
      :requirements
      (when requirements
        (mapcar #'resolver:string-to-requirement requirements)))))

(defun generate-repo-index (search-directory index-file
                            &key add-to sortindex)
  "Generate a repository index file from .dscard files found in
  SEARCH-DIRECTORY."
  (let* ((output-file index-file)
         (initial-repo (if add-to
                           (nrdl:parse-from
                             (make-string-input-stream
                               (util:data-slurp add-to)))
                           (make-hash-table :test 'equal)))
         (cards ()))
    ;; Find all .dscard files
    (labels ((walk-dir (dir)
               (let ((dir-path (uiop:ensure-directory-pathname dir)))
                 (loop for entry in (uiop:directory-files dir-path)
                       for name = (pathname-name entry)
                       for type = (pathname-type entry)
                       when (and name type (string-equal type "dscard"))
                       do (push (uiop:truename* entry) cards)
                       when (uiop:directory-pathname-p entry)
                       do (walk-dir entry)))))
      (walk-dir search-directory))
    ;; Read each card and add to repo
    (loop for card-path in cards
          for pkg = (read-card card-path)
          do (let ((id (resolver:pi-id pkg)))
               (push pkg (gethash id initial-repo))))
    ;; Sort each package list
    (when sortindex
      (loop for key being each hash-key of initial-repo
            do (setf (gethash key initial-repo)
                     (funcall sortindex (gethash key initial-repo)))))
    ;; Write output
    (util:default-spit output-file initial-repo)))

(defun slurp-degasolv-repo (url)
  "Read a degasolv repository from URL and return a list of query functions."
  (let* ((repo-data (nrdl:parse-from
                      (make-string-input-stream
                        (util:data-slurp url))))
         (query-fn (util:map-query repo-data)))
    (list query-fn)))
