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
  (:import-from #:dexador)
  (:import-from #:chipz)
  (:import-from #:babel)
  (:local-nicknames
    (#:f #:fset)
    (#:resolver #:com.djhaskin.dsolv/resolver))
  (:export
    #:deb-to-degasolv-requirements
    #:apt-repo
    #:slurp-apt-repo))

(in-package #:com.djhaskin.dsolv/pkgsys/apt)

(defun deb-to-degasolv-requirements (s)
  "Convert a Debian dependency string to degasolv requirements.

  Parses strings like \"a (>>5.0), b (>= 4.0)\" into a list of lists of
  requirement structs. Each comma-separated piece becomes one inner list,
  and pipe-separated alternatives within a piece become separate requirement
  structs in that inner list (handled by string-to-requirement).

  Handles:
  - :any, :i386, :amd64 architecture suffix removal
  - << -> <, >> -> >, = -> == version operator conversion
  - Parenthesis and space removal"
  (if (or (null s) (string= s ""))
      nil
      (let* ((no-arch (cl-ppcre:regex-replace-all ":(any|i386|amd64)" s ""))
             (no-parens (cl-ppcre:regex-replace-all "[ ()]" no-arch ""))
             (lt-conv (cl-ppcre:regex-replace-all "<<" no-parens "<"))
             (gt-conv (cl-ppcre:regex-replace-all ">>" lt-conv ">"))
             (eq-conv (cl-ppcre:regex-replace-all
                        "([^><=,]+)=([^><=|,]+)" gt-conv "\\1==\\2")))
        (loop for piece in (cl-ppcre:split "," eq-conv)
              collect (string-to-requirement piece)))))

(defun lines-to-map (lines)
  "Parse Debian package paragraph lines into an fset map.

  Each line of the form 'Key: value' becomes a keyword-value pair in the
  returned fset map. Keys are converted to uppercase keywords (e.g.,
  \"Package\" -> :PACKAGE, \"Version\" -> :VERSION)."
  (reduce (lambda (acc line)
            (multiple-value-bind (match groups)
                (cl-ppcre:scan-to-strings "^([^:]+): +(.*)$" line)
              (if match
                  (let ((k (intern (string-upcase (aref groups 0)) :keyword))
                        (v (aref groups 1)))
                    (fset:with acc k v))
                  acc)))
          lines
          :initial-value (fset:empty-map)))

(defun convert-pkg-requirements (pkg)
  "Convert the :DEPENDS field of a package map to degasolv requirements.

  Takes an fset map representing a Debian package paragraph, finds the
  :DEPENDS key, and replaces its string value with the parsed requirements
  list (via deb-to-degasolv-requirements). Returns a new fset map."
  (let ((deps (fset:lookup pkg :DEPENDS)))
    (if deps
        (fset:with pkg :DEPENDS (deb-to-degasolv-requirements deps))
        pkg)))

(defun add-pkg-location (pkg url)
  "Add a location URL to a package map.

  Constructs the full URL by concatenating URL with the package's :FILENAME
  field. Normalizes double slashes and fixes protocol-relative URLs
  (e.g., 'http:/' -> 'http://'). Returns a new fset map with :LOCATION set."
  (let* ((filename (fset:lookup pkg :FILENAME))
         (raw-location (if filename
                           (cl-ppcre:regex-replace-all "/+"
                             (concatenate 'string url "/" filename) "/")
                           url))
         (clean-location (cl-ppcre:regex-replace-all
                           "^([a-zA-Z]+:)/" raw-location "\\1//")))
    (fset:with pkg :LOCATION clean-location)))

(defun deb-to-degasolv-provides (s)
  "Convert a Debian Provides string to a list of provided package names.

  Removes all whitespace, splits on commas, and returns a list of
  package name strings."
  (if (or (null s) (string= s ""))
      nil
      (cl-ppcre:split ","
        (cl-ppcre:regex-replace-all "\\s" s ""))))

(defun expand-provides (pkg)
  "Expand the :PROVIDES field into separate PackageInfo structs.

  Creates one PackageInfo for the package itself (using its :PACKAGE name
  with :any suffix stripped), and one additional PackageInfo for each
  provided virtual package (with version \"0\" and the same location and
  dependencies). Returns a list of package-info structs."
  (let* ((pkg-name (cl-ppcre:regex-replace-all "[:]any$"
                    (fset:lookup pkg :PACKAGE) ""))
         (version (fset:lookup pkg :VERSION))
         (location (fset:lookup pkg :LOCATION))
         (depends (fset:lookup pkg :DEPENDS))
         (new-package (make-package-info
                        :id pkg-name
                        :version version
                        :location location
                        :requirements depends)))
    (if (fset:lookup pkg :PROVIDES)
        (let ((provided-names
                (deb-to-degasolv-provides (fset:lookup pkg :PROVIDES))))
          (cons new-package
                (loop for name in provided-names
                      collect (make-package-info
                                :id name
                                :version "0"
                                :location location
                                :requirements depends))))
        (list new-package))))

(defun apt-repo (url info)
  "Parse APT repository content into a repo query function.

  URL is the base URL of the repository (e.g.,
  \"http://us.archive.ubuntu.com/ubuntu/\").

  INFO is the full text content of a Packages file (multiple paragraphs
  separated by blank lines, each paragraph describing one package).

  Returns a function that takes a package ID (string) and returns an
  fset seq of matching package-info structs, or nil if not found."
  (let* ((paragraphs (cl-ppcre:split "\\n\\n" info))
         (all-packages
           (loop for para in paragraphs
                 append
                 (let* ((lines (cl-ppcre:split "\\n" para))
                        (relevant-lines
                          (remove-if-not
                            (lambda (line)
                              (cl-ppcre:scan "^[a-zA-Z0-9]+:.*" line))
                            lines)))
                   (when relevant-lines
                     (let* ((pkg-map (lines-to-map relevant-lines))
                            (pkg-map (convert-pkg-requirements pkg-map))
                            (pkg-map (add-pkg-location pkg-map url)))
                       (expand-provides pkg-map))))))
         (repo-map
           (reduce (lambda (acc pkg)
                     (let ((id (pi-id pkg)))
                       (fset:with acc id
                         (fset:with-last
                           (or (fset:lookup acc id) (fset:empty-seq))
                           pkg))))
                   all-packages
                   :initial-value (fset:empty-map))))
    (map-query repo-map)))

(defun ensure-simple-octet-vector (value source)
  "Return VALUE as a simple octet vector, or explain why SOURCE is invalid."
  (unless (typep value '(array (unsigned-byte 8) (*)))
    (error "Expected gzip octets from APT repository ~a, got ~s."
           source (type-of value)))
  (if (typep value '(simple-array (unsigned-byte 8) (*)))
      value
      (let ((octets (make-array (length value)
                                :element-type '(unsigned-byte 8))))
        (replace octets value)
        octets)))

(defun slurp-apt-repo (repospec &key (fetcher #'dexador:get))
  "Read compressed APT package indexes specified by REPOSPEC.

REPOSPEC has the form: pkgtype url dist pool1 pool2. For each pool, retrieve
its Packages.gz index, decompress its UTF-8 contents, and return an APT
repository query function. FETCHER receives the index URL and :FORCE-BINARY T;
it defaults to DEXADOR:GET."
  (let* ((parts (cl-ppcre:split " +" repospec))
         (pkgtype (first parts))
         (url (second parts))
         (dist (third parts))
         (pools (nthcdr 3 parts)))
    (loop for loc in
          (if (cl-ppcre:scan "/" dist)
              (list (list url dist "Packages.gz"))
              (loop for pool in pools
                    collect (list url "dists" dist pool pkgtype "Packages.gz")))
          collect
          (let ((index-url (format nil "~{~a~^/~}" loc)))
            (handler-case
                (let* ((compressed
                         (ensure-simple-octet-vector
                          (funcall fetcher index-url :force-binary t)
                          index-url))
                       (contents
                         (babel:octets-to-string
                          (chipz:decompress nil 'chipz:gzip compressed)
                          :encoding :utf-8)))
                  (apt-repo url contents))
              (error (condition)
                (error "Could not read compressed APT index ~a: ~a"
                       index-url condition)))))))
