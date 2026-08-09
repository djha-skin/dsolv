;;;; src/pkgsys/apt.lisp
;;;;
;;;; APT package system: parsing Debian Packages.gz files and converting
;;;; to degasolv requirements.
;;;;
;;;; Ported from degasolv's cli-src/degasolv/pkgsys/apt.clj
;;;;
;;;; The Packages file parser deliberately avoids regular expressions:
;;;; records are cut at blank lines and key/value pairs are split at the
;;;; first colon using simple linear string operations.  Regular
;;;; expressions can be NP-hard to parse and made the original degasolv
;;;; run take ~40 minutes on the full Ubuntu package listing, so the
;;;; parsing here is kept linear and predictable.

(defpackage #:com.djhaskin.dsolv/pkgsys/apt
  (:use #:cl)
  (:import-from #:com.djhaskin.dsolv/resolver
    #:package-info #:pi-id #:pi-version #:pi-location #:pi-requirements
    #:make-package-info #:string-to-requirement)
  (:import-from #:com.djhaskin.dsolv/util
    #:map-query)
  (:import-from #:fset)
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

;;; ─── Simple string helpers (no regular expressions) ────────────────────────

(defun split-on-char (string char)
  "Split STRING on CHAR into a list of strings."
  (loop with start = 0
        for pos = (position char string :start start)
        collect (subseq string start pos)
        while pos
        do (setf start (1+ pos))))

(defun string-split-lines (string)
  "Split STRING on newline characters into a list of lines."
  (loop with start = 0
        for newline = (position #\Newline string :start start)
        collect (subseq string start newline)
        while newline
        do (setf start (1+ newline))))

(defun group-lines-into-records (lines)
  "Group LINES into records, each record separated by an empty line."
  (let ((records nil)
        (current nil))
    (dolist (line lines)
      (if (string= line "")
          (when current
            (push (nreverse current) records)
            (setf current nil))
          (push line current)))
    (when current
      (push (nreverse current) records))
    (nreverse records)))

(defun remove-substring (string pattern)
  "Remove every occurrence of PATTERN from STRING."
  (with-output-to-string (out)
    (loop with start = 0
          for pos = (search pattern string :start2 start)
          do (write-string string out :start start :end (or pos (length string)))
          while pos
          do (setf start (+ pos (length pattern))))))

(defun remove-substrings (string patterns)
  "Remove every occurrence of each string in PATTERNS from STRING."
  (reduce (lambda (acc pattern) (remove-substring acc pattern))
          patterns
          :initial-value string))

(defun remove-chars (string chars)
  "Remove every character in CHARS from STRING."
  (with-output-to-string (out)
    (loop for char across string
          unless (find char chars)
          do (write-char char out))))

(defun remove-whitespace (string)
  "Remove every whitespace character from STRING."
  (remove-chars string (list #\Space #\Tab #\Newline #\Return #\Page
                             (code-char 11))))

(defun replace-substring (string old new)
  "Replace every occurrence of OLD with NEW in STRING."
  (with-output-to-string (out)
    (loop with start = 0
          for pos = (search old string :start2 start)
          do (write-string string out :start start :end (or pos (length string)))
          while pos
          do (write-string new out)
          do (setf start (+ pos (length old))))))

(defun collapse-slashes (string)
  "Collapse runs of two or more '/' characters in STRING into one."
  (with-output-to-string (out)
    (loop with prev-slash = nil
          for char across string
          do (cond
               ((char= char #\/)
                (unless prev-slash (write-char char out))
                (setf prev-slash t))
               (t
                (write-char char out)
                (setf prev-slash nil))))))

(defun fix-scheme-slash (string)
  "Turn 'scheme:/' into 'scheme://' in STRING."
  (let ((colon (position #\: string)))
    (if (and colon (plusp colon)
             (loop for i below colon
                   always (alpha-char-p (char string i)))
             (< (1+ colon) (length string))
             (char= (char string (1+ colon)) #\/)
             (or (= (+ 2 colon) (length string))
                 (not (char= (char string (+ 2 colon)) #\/))))
        (concatenate 'string (subseq string 0 (1+ colon))
                     "/" (subseq string (1+ colon)))
        string)))

(defun convert-equals (string)
  "Convert single '=' operators to '==' in STRING.

  A single '=' is converted only when it is not adjacent to a
  comparison operator character ('<', '>', '=', '|', ','), matching
  the legacy regex ([^><=,]+)=([^><=|,]+) without regular
  expressions."
  (with-output-to-string (out)
    (loop for i from 0 below (length string)
          for char = (char string i)
          do (if (and (char= char #\=)
                      (plusp i)
                      (not (find (char string (1- i)) "><=,"))
                      (< (1+ i) (length string))
                      (not (find (char string (1+ i)) "><=|,")))
                 (write-string "==" out)
                 (write-char char out)))))

;;; ─── Requirement conversion ─────────────────────────────────────────────────

(defun deb-to-degasolv-requirements (s)
  "Convert a Debian dependency string to degasolv requirements.

  Parses strings like \"a (>>5.0), b (>= 4.0)\" into a list of lists of
  requirement structs. Each comma-separated piece becomes one inner list,
  and pipe-separated alternatives within a piece become separate
  requirement structs in that inner list (handled by
  string-to-requirement).

  Handles:
  - :any, :i386, :amd64 architecture suffix removal
  - << -> <, >> -> >, = -> == version operator conversion
  - Parenthesis and space removal

  Uses only simple string operations so that parsing large Packages
  files stays linear and predictable."
  (if (or (null s) (string= s ""))
      nil
      (let* ((no-arch (remove-substrings s '(":any" ":i386" ":amd64")))
             (no-parens (remove-chars no-arch '(#\Space #\( #\))))
             (lt-conv (replace-substring no-parens "<<" "<"))
             (gt-conv (replace-substring lt-conv ">>" ">"))
             (eq-conv (convert-equals gt-conv)))
        (loop for piece in (split-on-char eq-conv #\,)
              collect (string-to-requirement piece)))))

(defun deb-to-degasolv-provides (s)
  "Convert a Debian Provides string to a list of provided package names.

  Removes all whitespace, splits on commas, and returns a list of
  package name strings."
  (if (or (null s) (string= s ""))
      nil
      (split-on-char (remove-whitespace s) #\,)))

;;; ─── Packages file parsing ──────────────────────────────────────────────────

(defun parse-key-value-line (line)
  "Parse a 'Key: value' line, returning the key string and value string.

  Uses a linear search for the colon. Returns NIL for continuation
  lines and lines without an alphanumeric key followed by ': '."
  (let ((colon (position #\: line)))
    (when (and colon (plusp colon)
               (< (1+ colon) (length line))
               (eql (char line (1+ colon)) #\Space)
               (every #'alphanumericp (subseq line 0 colon)))
      (values (subseq line 0 colon)
              (string-left-trim " " (subseq line (1+ colon)))))))

(defun lines-to-map (lines)
  "Parse Debian package paragraph lines into an fset map.

  Each line of the form 'Key: value' becomes a keyword-value pair in the
  returned fset map. Keys are converted to uppercase keywords (e.g.,
  \"Package\" -> :PACKAGE, \"Version\" -> :VERSION)."
  (reduce (lambda (acc line)
            (multiple-value-bind (key value)
                                 (parse-key-value-line line)
              (if key
                  (fset:with acc (intern (string-upcase key) :keyword) value)
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
  field, collapsing runs of slashes and fixing protocol-relative URLs
  (e.g., 'http:/' -> 'http://'). Returns a new fset map with :LOCATION set."
  (let* ((filename (fset:lookup pkg :FILENAME))
         (raw-location (if filename
                           (collapse-slashes
                             (concatenate 'string url "/" filename))
                           url))
         (clean-location (fix-scheme-slash raw-location)))
    (fset:with pkg :LOCATION clean-location)))

(defun strip-any-suffix (name)
  "Remove a trailing ':any' qualifier from NAME."
  (if (and (>= (length name) 4)
           (string= name ":any" :start1 (- (length name) 4)))
      (subseq name 0 (- (length name) 4))
      name))

(defun expand-provides (pkg)
  "Expand the :PROVIDES field into separate PackageInfo structs.

  Creates one PackageInfo for the package itself (using its :PACKAGE name
  with :any suffix stripped), and one additional PackageInfo for each
  provided virtual package (with version \"0\" and the same location and
  dependencies). Returns a list of package-info structs."
  (let* ((pkg-name (strip-any-suffix (fset:lookup pkg :PACKAGE)))
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

(defun expand-record (url lines)
  "Parse one APT package record (a list of lines) into package-infos."
  (let ((pkg-map (lines-to-map lines)))
    (when (not (fset:empty? pkg-map))
      (expand-provides
        (add-pkg-location (convert-pkg-requirements pkg-map) url)))))

(defun apt-repo (url info)
  "Parse APT repository content into a repo query function.

  URL is the base URL of the repository (e.g.,
  \"http://us.archive.ubuntu.com/ubuntu/\").

  INFO is the full text content of a Packages file (multiple paragraphs
  separated by blank lines, each paragraph describing one package).

  Returns a function that takes a package ID (string) and returns an
  fset seq of matching package-info structs, or nil if not found.

  The file is parsed with simple line-based string operations: records
  are cut at blank lines and key/value pairs are split at the first
  colon, avoiding regular expressions."
  (let* ((records (group-lines-into-records (string-split-lines info)))
         (all-packages
           (loop for record in records
                 append (expand-record url record)))
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

;;; ─── Compressed index fetching ─────────────────────────────────────────────

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

(defun apt-fetch (url &key force-binary)
  "Fetch an APT index, reading local files for file:// URLs and
   delegating to DEXADOR:GET for network URLs.

   Local files are read as raw octets so that compressed Packages.gz
   indexes can be decompressed by the caller. Network requests use
   DEXADOR with :FORCE-BINARY T, matching the previous default."
  (declare (ignore force-binary))
  (if (and (>= (length url) 7) (string= url "file://" :end1 7))
      (let* ((path-str (subseq url 7))
             (clean-path (collapse-slashes path-str)))
        (with-open-file (stream clean-path :direction :input
                                :element-type '(unsigned-byte 8))
          (let ((octets (make-array (file-length stream)
                                    :element-type '(unsigned-byte 8))))
            (read-sequence octets stream)
            octets)))
      (dexador:get url :force-binary t)))

(defun slurp-apt-repo (repospec &key (fetcher #'apt-fetch))
  "Read compressed APT package indexes specified by REPOSPEC.

REPOSPEC has the form: pkgtype url dist pool1 pool2. For each pool, retrieve
its Packages.gz index, decompress its UTF-8 contents, and return an APT
repository query function. FETCHER receives the index URL and :FORCE-BINARY T;
it defaults to APT-FETCH, which reads file:// URLs from disk and delegates
network URLs to DEXADOR:GET."
  (let* ((parts (split-on-char repospec #\Space))
         (pkgtype (first parts))
         (url (second parts))
         (dist (third parts))
         (pools (nthcdr 3 parts)))
    (loop for loc in
          (if (find #\/ dist)
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
