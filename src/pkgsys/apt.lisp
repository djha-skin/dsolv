;;;; src/pkgsys/apt.lisp
;;;;
;;;; APT package system integration for reading Debian-style Packages files.

(defpackage #:com.djhaskin.dsolv/pkgsys/apt
  (:use #:cl)
  (:import-from #:com.djhaskin.dsolv/util)
  (:import-from #:com.djhaskin.dsolv/resolver)
  (:import-from #:com.djhaskin.cliff)
  (:import-from #:cl-ppcre)
  (:local-nicknames
    (#:util #:com.djhaskin.dsolv/util)
    (#:resolver #:com.djhaskin.dsolv/resolver)
    (#:cliff #:com.djhaskin.cliff))
  (:export
    #:slurp-apt-repo))

(in-package #:com.djhaskin.dsolv/pkgsys/apt)

(defun deb-to-dsolv-requirements (s)
  "Convert a Debian dependency string to dsolv requirements."
  (if (or (null s) (string= s ""))
      nil
      (let* ((cleaned (cl-ppcre:regex-replace-all ":(any|i386|amd64)" s ""))
             (no-parens (cl-ppcre:regex-replace-all "[ ()]" cleaned ""))
             (no-lt (cl-ppcre:regex-replace-all "<<" no-parens "<"))
             (no-gt (cl-ppcre:regex-replace-all ">>" no-lt ">"))
             (eq-form (cl-ppcre:regex-replace-all
                        "([^><=,]+)=([^><=|,]+)" no-gt "\\1==\\2")))
        (mapcar #'resolver:string-to-requirement
                (cl-ppcre:split "," eq-form)))))

(defun lines-to-map (lines)
  "Convert a list of 'Key: Value' lines to a property list."
  (loop for line in lines
        for match = (cl-ppcre:register-groups-bind
                      (k v) ("^([^:]+): +(.*)$" line)
                      (list k v))
        when match
        collect (intern (string-upcase (first match)) :keyword)
        and collect (second match)))

(defun convert-pkg-requirements (pkg)
  "Convert the :Depends field of a package entry to dsolv requirements."
  (let ((deps (getf pkg :depends)))
    (if deps
        (setf (getf pkg :depends)
              (deb-to-dsolv-requirements deps))
        pkg)
    pkg))

(defun add-pkg-location (pkg url)
  "Add the full download URL to a package entry."
  (let ((filename (getf pkg :filename)))
    (when filename
      (setf (getf pkg :location)
            (cl-ppcre:regex-replace-all "/+"
                                        (format nil "~a/~a" url filename)
                                        "/"))))
  pkg)

(defun deb-to-dsolv-provides (s)
  "Convert a Debian Provides string to a list of package names."
  (when s
    (cl-ppcre:split "\\s*,\\s*" (cl-ppcre:regex-replace-all "\\s" s ""))))

(defun expand-provides (pkg)
  "Expand a package entry into potentially multiple entries (one per provide)."
  (let* ((id (getf pkg :package))
         (version (getf pkg :version))
         (location (getf pkg :location))
         (depends (getf pkg :depends))
         (provides (getf pkg :provides))
         (base-pkg (resolver:make-package-info
                     :id (cl-ppcre:regex-replace ":any$" id "")
                     :version version
                     :location location
                     :requirements depends))
         (extra-pkgs (when provides
                       (loop for provided in (deb-to-dsolv-provides provides)
                             collect (resolver:make-package-info
                                       :id provided
                                       :version "0"
                                       :location location
                                       :requirements depends)))))
    (cons base-pkg extra-pkgs)))

(defun maybe-decompress (raw loc)
  "If LOC ends with .gz, decompress the data via gzip -dc."
  (if (cl-ppcre:scan "(?i)\\.gz$" loc)
      (multiple-value-bind (output exit-code)
                           (uiop:run-program (list "gzip" "-dc")
                                             :input raw
                                             :output :string
                                             :ignore-error-status t)
        (if (zerop exit-code)
            output
            raw))
      raw))

(defun apt-repo (url info)
  "Parse the contents of a Packages file into a hash table query function."
  (let ((repo (make-hash-table :test 'equal)))
    (loop for pkg-text in (cl-ppcre:split "\\n\\n" info)
          for lines = (cl-ppcre:split "\\n" pkg-text)
          for relevant = (remove-if-not
                           (lambda (line)
                             (cl-ppcre:scan "^([:alnum:]+):.*" line))
                           lines)
          when relevant
          do (let* ((pkg (convert-pkg-requirements
                           (add-pkg-location
                             (lines-to-map relevant) url)))
                    (expanded (expand-provides pkg)))
               (loop for epkg in expanded
                     do (let ((id (resolver:pi-id epkg)))
                          (push epkg (gethash id repo))))))
    (util:map-query repo)))

(defun slurp-apt-repo (repospec)
  "Read an APT repository from the given repospec string.

  REPOSPEC format: '<pkgtype> <url> <dist> [pool ...]'
  Example: \"deb http://deb.debian.org/debian bookworm main contrib\""
  (let* ((parts (cl-ppcre:split "\\s+" repospec))
         (pkgtype (first parts))
         (url (second parts))
         (dist (third parts))
         (pools (nthcdr 3 parts))
         (locations ()))
    ;; Build location URLs
    (if (cl-ppcre:scan "/" dist)
        ;; dist is already a full path
        (push (format nil "~a/~a" url dist) locations)
        ;; Build from components
        (loop for pool in pools
              do (push (format nil "~a/dists/~a/~a/~a/Packages.gz"
                               url dist pool pkgtype)
                       locations)))
    ;; Fetch and parse each location
    (loop for loc in locations
          collect
          (let* ((raw (util:data-slurp loc))
                 (info (maybe-decompress raw loc)))
            (apt-repo url info)))))
