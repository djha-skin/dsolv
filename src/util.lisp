;;;; src/util.lisp
;;;;
;;;; Utility functions for reading resources across the network or from
;;;; the file system.

(defpackage #:com.djhaskin.dsolv/util
  (:use #:cl)
  (:import-from #:com.djhaskin.nrdl)
  (:import-from #:dexador)
  (:import-from #:quri)
  (:import-from #:alexandria)
  (:import-from #:cl-ppcre)
  (:local-nicknames
    (#:nrdl #:com.djhaskin.nrdl)
    (#:dsolv/util #:com.djhaskin.dsolv/util))
  (:export
    #:data-slurp
    #:base-slurp
    #:default-spit
    #:pretty-spit
    #:map-query
    #:priority-repo
    #:global-repo
    #:aggregator))

(in-package #:com.djhaskin.dsolv/util)

(defun base-slurp (loc)
  "Read the contents of LOC (a pathname or stream) as a UTF-8 string."
  (let ((input (if (equal loc "-")
                   *standard-input*
                   loc)))
    (with-open-file (in-stream input
                               :direction :input
                               :external-format :utf-8)
      (with-output-to-string (out)
        (loop for char = (read-char in-stream nil)
              while char
              do (write-char char out))))))

(defun data-slurp (resource)
  "Slurp a resource from a URL or file path.

  Supports:
    - http(s)://user:password@url  (basic auth)
    - http(s)://header=value@url   (custom header)
    - http(s)://token@url          (bearer token)
    - file://path                  (local file)
    - -                            (stdin)
    - anything else                (treated as file path)"
  (declare (type (or pathname string) resource))
  (or
    ;; Handle URLs with authentication
    (cl-ppcre:register-groups-bind
      (protocol username password rest-of-it)
      ("^(https?://)([^@:]+):([^@:]+)@(.+)$" (princ-to-string resource))
      (dexador:get (concatenate 'string protocol rest-of-it)
                   :basic-auth (cons (quri:url-decode username)
                                     (quri:url-decode password))
                   :force-string t))
    (cl-ppcre:register-groups-bind
      (protocol header headerval rest-of-it)
      ("^(https?://)([^@=]+)=([^@=]+)@(.+)$" (princ-to-string resource))
      (dexador:get (concatenate 'string protocol rest-of-it)
                   :headers (list (cons (quri:url-decode header)
                                        (quri:url-decode headerval)))
                   :force-string t))
    (cl-ppcre:register-groups-bind
      (protocol token rest-of-it)
      ("^(https?://)([^@]+)@(.+)$" (princ-to-string resource))
      (dexador:get (concatenate 'string protocol rest-of-it)
                   :headers (list (cons "Authorization"
                                        (format nil "Bearer ~a"
                                                (quri:url-decode token))))
                   :force-string t))
    (cl-ppcre:register-groups-bind
      ()
      ("^https?://.*$" (princ-to-string resource))
      (dexador:get resource :force-string t))
    ;; Fall through to file slurp
    (base-slurp resource)))

(defun default-spit (loc stuff)
  "Write the printed representation of STUFF to LOC as UTF-8."
  (with-open-file (out loc
                       :direction :output
                       :external-format :utf-8
                       :if-exists :supersede)
    (prin1 stuff out)))

(defun pretty-spit (loc stuff)
  "Write STUFF to LOC in a pretty-printed format."
  (with-open-file (out loc
                       :direction :output
                       :external-format :utf-8
                       :if-exists :supersede)
    (pprint stuff out)))

(defun map-query (m)
  "Create a lookup function for a hash table."
  (lambda (nm)
    (multiple-value-bind (result foundp)
                         (gethash nm m)
      (if foundp
          result
          nil))))

(defun priority-repo (repos)
  "Create a priority-ordered repository query function.

  Returns the first non-empty result from the list of repo query functions."
  (lambda (id)
    (some (lambda (r)
            (let ((result (funcall r id)))
              (when result result)))
          repos)))

(defun global-repo (repos &key (comparator #'string-lessp))
  "Create a globally-ordered repository query function.

  Combines results from all repos and sorts them by COMPARATOR."
  (lambda (id)
    (let* ((all-results (mapcan (lambda (r) (copy-list (funcall r id))) repos))
           (sorted (sort all-results comparator
                         :key (lambda (pkg)
                                (getf pkg :version)))))
      sorted)))

(defun aggregator (index-strat comparator)
  "Return a repository aggregation function based on INDEX-STRAT.

  When INDEX-STRAT is \"priority\", use PRIORITY-REPO.
  When INDEX-STRAT is \"global\", use GLOBAL-REPO."
  (cond
    ((string= index-strat "priority")
     (function priority-repo))
    ((string= index-strat "global")
     (lambda (repos)
       (global-repo repos :comparator comparator)))
    (t
     (function priority-repo))))
