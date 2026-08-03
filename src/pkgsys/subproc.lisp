;;;; src/pkgsys/subproc.lisp
;;;;
;;;; Subprocess package system: calling external executables to get
;;;; package data.
;;;;
;;;; Ported from degasolv's cli-src/degasolv/pkgsys/subproc.clj

(defpackage #:com.djhaskin.dsolv/pkgsys/subproc
  (:use #:cl)
  (:import-from #:com.djhaskin.dsolv/resolver
    #:package-info #:pi-id #:pi-version #:pi-location #:pi-requirements
    #:make-package-info #:string-to-requirement)
  (:import-from #:com.djhaskin.dsolv/util
    #:map-query)
  (:import-from #:fset)
  (:import-from #:misc-extensions.gmap
    #:gmap)
  (:local-nicknames
    (#:f #:fset)
    (#:resolver #:com.djhaskin.dsolv/resolver))
  (:export
    #:convert-input
    #:make-slurper))

(in-package #:com.djhaskin.dsolv/pkgsys/subproc)

(defun convert-input (raw-repo-info)
  "Convert raw repository info (from external executable) to an fset map
   of package-id -> fset seq of package-info structs.

   RAW-REPO-INFO is an fset map where keys are package ID strings and
   values are fset seqs of entry maps.  Each entry map may have keys:
     :id           - package ID string (defaults to the map key)
     :version      - version string
     :location     - location URL string
     :requirements - optional fset seq of requirement strings, each
                     parsed via string-to-requirement into a clause

   Returns an fset map of package-id -> fset seq of package-info structs."
  (fset:reduce
    (lambda (acc id)
      (let* ((entries (fset:lookup raw-repo-info id))
             (converted
               (gmap (:result fset:seq)
                     (lambda (entry)
                       (let* ((entry-id (or (fset:lookup entry :id) id))
                              (version (fset:lookup entry :version))
                              (location (fset:lookup entry :location))
                              (raw-reqs (fset:lookup entry :requirements))
                              (reqs (when raw-reqs
                                      (loop for req-str
                                            in (fset:convert 'list raw-reqs)
                                            collect (string-to-requirement req-str)))))
                         (make-package-info
                           :id entry-id
                           :version version
                           :location location
                           :requirements reqs)))
                     (:arg fset:seq entries))))
        (fset:with acc id converted)))
    (fset:domain raw-repo-info)
    :initial-value (fset:empty-map)))

(defun make-slurper (options)
  "Create a repository slurper function that calls an external executable."
  (declare (ignore options))
  (error "make-slurper not yet implemented"))