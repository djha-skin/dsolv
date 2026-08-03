;;;; src/pkgsys/apt.lisp
;;;;
;;;; APT package system: parsing Debian Packages.gz files and converting
;;;; to degasolv requirements.
;;;;
;;;; Ported from degasolv's cli-src/degasolv/pkgsys/apt.clj

(defpackage #:com.djhaskin.dsolv/pkgsys/apt
  (:use #:cl)
  (:import-from #:com.djhaskin.dsolv/resolver
    #:package-info #:pi-id #:pi-version #:pi-location #:pi-requirements
    #:make-package-info #:string-to-requirement)
  (:import-from #:com.djhaskin.dsolv/util
    #:map-query)
  (:import-from #:fset)
  (:import-from #:cl-ppcre)
  (:local-nicknames
    (#:f #:fset)
    (#:resolver #:com.djhaskin.dsolv/resolver))
  (:export
    #:deb-to-degasolv-requirements
    #:apt-repo
    #:slurp-apt-repo))

(in-package #:com.djhaskin.dsolv/pkgsys/apt)

(defun deb-to-degasolv-requirements (s)
  "Convert a Debian dependency string to degasolv requirements."
  (declare (ignore s))
  (error "deb-to-degasolv-requirements not yet implemented"))

(defun apt-repo (url info)
  "Parse APT repository info from a Packages string."
  (declare (ignore url info))
  (error "apt-repo not yet implemented"))

(defun slurp-apt-repo (repospec)
  "Read an APT repository from a repository specification string."
  (declare (ignore repospec))
  (error "slurp-apt-repo not yet implemented"))