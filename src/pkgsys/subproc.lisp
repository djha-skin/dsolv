;;;; src/pkgsys/subproc.lisp
;;;;
;;;; Subprocess package system integration. Calls an external executable to
;;;; obtain package repository data.

(defpackage #:com.djhaskin.dsolv/pkgsys/subproc
  (:use #:cl)
  (:import-from #:com.djhaskin.dsolv/util)
  (:import-from #:com.djhaskin.dsolv/resolver)
  (:import-from #:com.djhaskin.cliff)
  (:local-nicknames
    (#:util #:com.djhaskin.dsolv/util)
    (#:resolver #:com.djhaskin.dsolv/resolver)
    (#:cliff #:com.djhaskin.cliff))
  (:export
    #:make-slurper))

(in-package #:com.djhaskin.dsolv/pkgsys/subproc)

(defun convert-input (raw-repo-info)
  "Convert raw repository info (a hash table from NRDL/JSON) into
  a hash table of package lists."
  (let ((result (make-hash-table :test 'equal)))
    (loop for package-name being each hash-key of raw-repo-info
          using (hash-value package-list)
          do
          (setf (gethash package-name result)
                (loop for pkg in package-list
                      collect
                      (let* ((id (getf pkg :id))
                             (version (getf pkg :version))
                             (location (getf pkg :location))
                             (requirements (getf pkg :requirements))
                             (converted-reqs (when requirements
                                               (mapcar
                                                 #'resolver:string-to-requirement
                                                 requirements))))
                        (resolver:make-package-info
                          :id id
                          :version version
                          :location location
                          :requirements converted-reqs))))))
  result)
