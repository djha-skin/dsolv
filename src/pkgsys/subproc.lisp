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
  (:import-from #:com.djhaskin.nrdl
    #:parse-from
    #:to-fset)
  (:import-from #:uiop)
  (:import-from #:fset)
  (:import-from #:gmap
    #:gmap)
  (:local-nicknames
    (#:f #:fset)
    (#:nrdl #:com.djhaskin.nrdl)
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
               (gmap (:result :seq)
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
                     (:arg :seq entries))))
        (fset:with acc id converted)))
    (fset:domain raw-repo-info)
    :initial-value (fset:empty-map)))

(defun keywordize-keys (value)
  "Recursively convert string keys in fset maps and seqs to keywords.
   Non-map values (strings, numbers, etc.) are returned unchanged."
  (cond
    ((typep value 'fset:wb-map)
     (fset:reduce
       (lambda (acc k)
         (fset:with acc
                    (if (stringp k) (intern (string-upcase k) :keyword) k)
                    (keywordize-keys (fset:lookup value k))))
       (fset:domain value)
       :initial-value (fset:empty-map)))
    ((typep value 'fset:wb-seq)
     (gmap (:result :seq)
           #'keywordize-keys
           (:arg :seq value)))
    (t value)))

(defun keywordize-entry-keys (repo-map)
  "Convert string keys in the entry maps of REPO-MAP to keywords,
   leaving the top-level package-id keys as strings."
  (fset:reduce
    (lambda (acc id)
      (fset:with acc id
                 (gmap (:result :seq)
                       (lambda (entry) (keywordize-keys entry))
                       (:arg :seq (fset:lookup repo-map id)))))
    (fset:domain repo-map)
    :initial-value (fset:empty-map)))

(defun make-slurper (options)
  "Create a repository slurper function that calls an external executable.

   OPTIONS is a hash table that must contain :subproc-exe (the path to
   the executable to call) and may contain :subproc-output-format (one of
   \"json\" or \"nrdl\", defaulting to \"json\").

   The returned slurper takes a repo URL, runs the executable with that
   URL as its argument, parses its output, and returns a list containing
   a single query function, matching the repository generator convention."
  (let ((subproc-exe (gethash :subproc-exe options))
        (output-format (or (gethash :subproc-output-format options) "json")))
    (unless (or (string= output-format "json")
                (string= output-format "nrdl"))
      (error "Unknown subproc output format: `~a`" output-format))
    (lambda (repo)
      (multiple-value-bind (out err exit-code)
                           (uiop:run-program (list subproc-exe repo)
                                             :output :string
                                             :error-output :string
                                             :ignore-error-status t)
        (declare (ignore err))
        (unless (zerop exit-code)
          (error "Executable `~a` given argument `~a` exited with non-zero status `~a`."
                 subproc-exe repo exit-code))
        (let* ((parsed (nrdl:parse-from (make-string-input-stream out)))
               (raw-repo-info
                 (keywordize-entry-keys (nrdl:to-fset parsed)))
               (repo-map (convert-input raw-repo-info)))
          (list (map-query repo-map)))))))
