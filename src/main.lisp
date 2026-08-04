;;;; src/main.lisp
;;;;
;;;; CLI entry point for dsolv using CLIFF for argument parsing,
;;;; subcommand dispatch, config files, and environment variables.
;;;;
;;;; Ported from degasolv's cli-src/degasolv/cli.clj

(defpackage #:com.djhaskin.dsolv
  (:use #:cl)
  (:import-from #:com.djhaskin.cliff
    #:execute-program
    #:data-slurp
    #:generate-string
    #:parse-string)
  (:import-from #:com.djhaskin.dsolv/resolver
    #:resolve-dependencies-deluxe
    #:string-to-requirement
    #:explain-package
    #:explain-problem
    #:make-spec-call
    #:make-package-info
    #:package-info
    #:pi-id
    #:pi-version
    #:pi-location
    #:pi-requirements
    #:req-id
    #:req-spec
    #:*version-comparators*
    #:*package-systems*
    #:*subcommand-option-defaults*
    #:*available-option-packs*)
  (:import-from #:com.djhaskin.dsolv/util
    #:aggregator)
  (:import-from #:com.djhaskin.dsolv/pkgsys/core
    #:generate-repo-index
    #:slurp-degasolv-repo)
  (:import-from #:com.djhaskin.dsolv/pkgsys/apt
    #:slurp-apt-repo)
  (:import-from #:com.djhaskin.dsolv/pkgsys/subproc
    #:make-slurper)
  (:import-from #:com.djhaskin.svers)
  (:import-from #:fset)
  (:import-from #:alexandria)
  (:import-from #:cl-ppcre)
  (:local-nicknames
    (#:f #:fset)
    (#:nrdl #:com.djhaskin.nrdl)
    (#:resolver #:com.djhaskin.dsolv/resolver)
    (#:util #:com.djhaskin.dsolv/util)
    (#:pkgsys/core #:com.djhaskin.dsolv/pkgsys/core))
  (:export #:main))

(in-package #:com.djhaskin.dsolv)

;;; ─── Helpers ────────────────────────────────────────────────────────────────

(defun exit-with (status-code msg)
  "Print MSG to stderr and exit with STATUS-CODE.
   STATUS-CODE is a keyword from CLIFF's *exit-codes* (e.g. :general-error).
   Returns a hash table suitable for CLIFF's execute-program."
  (format *error-output* "~a~%" msg)
  (alexandria:alist-hash-table
    `((:status . ,status-code)
      (:cliff-suppress-output . t))))

(defun out-exit-with (status-code msg)
  "Print MSG to stdout and exit with STATUS-CODE.
   STATUS-CODE is a keyword from CLIFF's *exit-codes* (e.g. :successful).
   Returns a hash table suitable for CLIFF's execute-program."
  (format t "~a~%" msg)
  (alexandria:alist-hash-table
    `((:status . ,status-code)
      (:cliff-suppress-output . t))))

(defun ht-get (ht key &optional default)
  "Get a value from a hash table with a default."
  (multiple-value-bind (val found) (gethash key ht)
    (if found val default)))

(defun ht-get-defaults (ht key)
  "Get a value from a hash table, falling back to *subcommand-option-defaults*."
  (multiple-value-bind (val found) (gethash key ht)
    (if found
        val
        (multiple-value-bind (dval dfound)
            (gethash key *subcommand-option-defaults*)
          (declare (ignore dfound))
          dval))))

(defun get-version-comparator (options)
  "Get the version comparator function from options, falling back to
   the package system's default."
  (let* ((pkg-system (ht-get options :package-system "degasolv"))
         (pkg-sys-entry (gethash pkg-system *package-systems*))
         (pkg-sys-vercmp (getf pkg-sys-entry :version-comparison))
         (vercmp-name (or (ht-get options :version-comparison)
                          pkg-sys-vercmp
                          "semver"))
         (comparator-fn (gethash vercmp-name *version-comparators*)))
    (unless comparator-fn
      (warn "Unknown version comparator: ~a, falling back to naive" vercmp-name)
      (setf comparator-fn (gethash "naive" *version-comparators*)))
    comparator-fn))

(defun expand-option-packs (options)
  "Expand option pack specifications in OPTIONS.
   Returns a new hash table with option pack values merged in."
  (let ((packs (ht-get options :option-packs)))
    (if (null packs)
        options
        (let ((result (alexandria:copy-hash-table options)))
          (dolist (pack-name packs result)
            (let ((pack (gethash pack-name *available-option-packs*)))
              (when pack
                (maphash (lambda (k v)
                           (setf (gethash k result) v))
                         pack))))
          (remhash :option-packs result)
          result))))

(defun aggregate-repositories (index-strat repositories genrepo version-comparator)
  "Aggregate repositories using the given strategy and generator function."
  (let ((agg-fn (util:aggregator index-strat version-comparator))
        (repos (loop for url in repositories
                     collect (funcall genrepo url))))
    (funcall agg-fn repos)))

;;; ─── Subcommand: resolve-locations ──────────────────────────────────────────

(defun resolve-locations-fn (options)
  "Resolve dependencies and print package locations."
  (let* ((alternatives (ht-get options :alternatives t))
         (conflict-strat (ht-get options :conflict-strat "exclusive"))
         (list-strat (ht-get options :list-strat "lazy"))
         (index-strat (ht-get options :index-strat "priority"))
         (output-format (ht-get options :output-format "plain"))
         (package-system (ht-get options :package-system "degasolv"))
         (present-pkgs (ht-get options :present-packages))
         (requirements (ht-get options :requirements))
         (resolve-strat (ht-get options :resolve-strat "thorough"))
         (search-strat (ht-get options :search-strat "breadth-first"))
         (version-comparison (ht-get options :version-comparison))
         (error-format (ht-get options :error-format t))
         (version-comparator (get-version-comparator options))
         (pkg-sys-entry (gethash package-system *package-systems*)))

    ;; Check required arguments
    (when (getf pkg-sys-entry :required-arguments)
      (let ((req-args (getf pkg-sys-entry :required-arguments)))
        (maphash (lambda (key val)
                   (declare (ignore val))
                   (unless (gethash key options)
                     (return-from resolve-locations-fn
                       (exit-with :general-error
                         (format nil "Missing required argument: ~a" key)))))
                 req-args)))

    ;; Check repositories requirement (if no query-constructor)
    (unless (getf pkg-sys-entry :query-constructor)
      (let ((repos (ht-get options :repositories)))
        (unless (and repos (listp repos) (not (null repos)))
          (return-from resolve-locations-fn
            (exit-with :general-error "Missing required argument: --set-repositories")))))

    ;; Parse requirements
    (let* ((requirement-data
             (loop for str-req in requirements
                   collect (string-to-requirement str-req)))
           (present-packages-map
             (let ((map (f:empty-map)))
               (loop for str-pkg in present-pkgs
                     for split = (cl-ppcre:split "==" str-pkg)
                     for id = (first split)
                     for version = (second split)
                     do (setf map (f:with map id
                                            (cons (make-package-info
                                                    :id id
                                                    :version version
                                                    :location "already present")
                                                  (f:lookup map id)))))
               map))
           (query
             (if (getf pkg-sys-entry :query-constructor)
                 (funcall (getf pkg-sys-entry :query-constructor) options)
                 (let* ((genrepo
                          (cond
                            ((getf pkg-sys-entry :repo-constructor)
                             (funcall (getf pkg-sys-entry :repo-constructor) options))
                            ((getf pkg-sys-entry :genrepo)
                             (getf pkg-sys-entry :genrepo))
                            (t (error "No repo generator for ~a" package-system))))
                        (repositories (ht-get options :repositories)))
                   (aggregate-repositories
                     index-strat
                     repositories
                     genrepo
                     version-comparator))))
           (result
             (resolve-dependencies-deluxe
               (apply #'append requirement-data)
               query
               :present-packages present-packages-map
               :strategy (intern (string-upcase resolve-strat) :keyword)
               :conflict-strat (intern (string-upcase conflict-strat) :keyword)
               :list-strat (intern (string-upcase list-strat) :keyword)
               :search-strat (intern (string-upcase search-strat) :keyword)
               :compare version-comparator
               :allow-alternatives alternatives))
           (result-status (getf result :result)))

      (if (eql result-status :successful)
          (let ((packages (getf result :packages)))
            (ecase (intern (string-upcase output-format) :keyword)
              (:json
               (format t "{\"result\":\"successful\",\"packages\":[~%")
               (loop for pkg in packages
                     do (format t "  {\"id\":\"~a\",\"version\":\"~a\",\"location\":\"~a\"}~%"
                                (pi-id pkg) (pi-version pkg) (pi-location pkg)))
               (format t "]}~%"))
              (:plain
               (loop for pkg in packages
                     do (format t "~a~%" (explain-package pkg))))
              (:edn
               (format t ":result :successful :packages (")
               (loop for pkg in packages
                     do (format t "#:package-info{:id \"~a\" :version \"~a\" :location \"~a\"} "
                                (pi-id pkg) (pi-version pkg) (pi-location pkg)))
               (format t ")~%")))
            (alexandria:alist-hash-table
              `((:status . :successful)
                (:cliff-suppress-output . t))))
          (let ((problems (getf result :problems)))
            (if error-format
                (out-exit-with :system-error
                  (ecase (intern (string-upcase output-format) :keyword)
                    (:json
                     (format nil "{\"result\":\"unsuccessful\",\"problems\":[...]}"))
                    (:plain
                     (format nil "~{~a~%~}" (mapcar #'explain-problem problems)))
                    (:edn
                     (format nil ":result :unsuccessful :problems ~a" problems))))
                (exit-with :system-error
                  (format nil "~{~a~%~}" (mapcar #'explain-problem problems)))))))))

;;; ─── Subcommand: generate-repo-index ────────────────────────────────────────

(defun generate-repo-index-fn (options)
  "Generate a repo index from .dscard files."
  (let* ((search-directory (ht-get options :search-directory "."))
         (index-file (ht-get options :index-file "index.dsrepo"))
         (add-to (ht-get options :add-to))
         (version-comparison (ht-get options :version-comparison "semver"))
         (index-sort-order (ht-get options :index-sort-order "descending"))
         (version-comparator (gethash version-comparison *version-comparators*
                                      (gethash "naive" *version-comparators*)))
         (sortindex
           (let ((vercmp version-comparator))
             (if (string= index-sort-order "ascending")
                 (lambda (x)
                   (f:sort x (lambda (a b)
                               (minusp (funcall vercmp
                                                (pi-version a)
                                                (pi-version b))))))
                 (lambda (x)
                   (f:sort x (lambda (a b)
                               (plusp (funcall vercmp
                                               (pi-version a)
                                               (pi-version b))))))))))
    (generate-repo-index search-directory index-file
                         :add-to add-to
                         :sortindex sortindex)
    (alexandria:alist-hash-table
      `((:status . :successful)
        (:cliff-suppress-output . t)))))

;;; ─── Subcommand: generate-card ──────────────────────────────────────────────

(defun generate-card-fn (options)
  "Generate a .dscard file."
  (let* ((id (ht-get options :id))
         (version (ht-get options :version))
         (location (ht-get options :location))
         (card-file (ht-get options :card-file "./out.dscard"))
         (requirements (ht-get options :requirements))
         (meta (ht-get options :meta)))
    (unless (and id version location)
      (return-from generate-card-fn
        (exit-with :general-error "Missing required arguments: --set-id, --set-version, --set-location")))
    (let* ((reqs (loop for r in requirements
                       collect (first (string-to-requirement r))))
           (pkg (make-package-info
                  :id id
                  :version version
                  :location location
                  :requirements reqs))
           (pkg-data
             (list :id (pi-id pkg)
                   :version (pi-version pkg)
                   :location (pi-location pkg)
                   :requirements (pi-requirements pkg))))
      ;; Merge in additional metadata
      (when meta
        (maphash (lambda (k v)
                   (push k pkg-data)
                   (push v pkg-data))
                 meta))
      ;; Write card file
      (with-open-file (stream card-file
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
        (nrdl:generate-to stream pkg-data :pretty-indent 2)))
    (alexandria:alist-hash-table
      `((:status . :successful)
        (:cliff-suppress-output . t)))))

;;; ─── Subcommand: query-repo ─────────────────────────────────────────────────

(defun query-repo-fn (options)
  "Query a repository for packages."
  (let* ((index-strat (ht-get options :index-strat "priority"))
         (output-format (ht-get options :output-format "plain"))
         (package-system (ht-get options :package-system "degasolv"))
         (repositories (ht-get options :repositories))
         (query (ht-get options :query))
         (version-comparison (ht-get options :version-comparison))
         (error-format (ht-get options :error-format t))
         (version-comparator (get-version-comparator options))
         (pkg-sys-entry (gethash package-system *package-systems*)))

    (unless (and repositories query)
      (return-from query-repo-fn
        (exit-with :general-error "Missing required arguments: --set-repositories, --set-query")))

    (let* ((genrepo (getf pkg-sys-entry :genrepo))
           (aggregate-repo
             (aggregate-repositories
               index-strat
               repositories
               genrepo
               version-comparator))
           (parsed-req (first (string-to-requirement query)))
           (id (req-id parsed-req))
           (spec (req-spec parsed-req))
           (spec-call (make-spec-call version-comparator))
           (results (fset:filter
                     (lambda (pkg)
                       (funcall spec-call spec pkg))
                     (funcall aggregate-repo id))))
      (if (fset:empty? results)
          (if error-format
              (out-exit-with :data-format-error
                (ecase (intern (string-upcase output-format) :keyword)
                  (:json "{\"result\":\"unsuccessful\",\"message\":\"No results returned from query\"}")
                  (:plain "No results returned from query")
                  (:edn ":result :unsuccessful :message \"No results returned from query\"")))
              (exit-with :data-format-error "No results returned from query"))
          (ecase (intern (string-upcase output-format) :keyword)
            (:json
             (format t "{\"packages\":[~%")
             (fset:do-seq (pkg results)
               (format t "  {\"id\":\"~a\",\"version\":\"~a\",\"location\":\"~a\"}~%"
                       (pi-id pkg) (pi-version pkg) (pi-location pkg)))
             (format t "]}~%"))
            (:plain
             (fset:do-seq (pkg results)
               (format t "~a~%" (explain-package pkg))))
            (:edn
             (fset:do-seq (pkg results)
               (format t "#:package-info{:id \"~a\" :version \"~a\" :location \"~a\"}~%"
                       (pi-id pkg) (pi-version pkg) (pi-location pkg))))))
      (alexandria:alist-hash-table
        `((:status . :successful)
          (:cliff-suppress-output . t))))))

;;; ─── Subcommand: display-config ─────────────────────────────────────────────

(defun display-config-fn (options)
  "Display the effective configuration."
  (let ((output-format (ht-get options :output-format "plain")))
    (ecase (intern (string-upcase output-format) :keyword)
      (:json
       (format t "{~%")
       (maphash (lambda (k v)
                  (format t "  ~s: ~s~%" k v))
                options)
       (format t "}~%"))
      (:plain
       (format t "Effective configuration:~%")
       (maphash (lambda (k v)
                  (format t "  ~s: ~s~%" k v))
                options))
      (:edn
       (format t ":effective-configuration~%")
       (maphash (lambda (k v)
                  (format t "  ~s ~s~%" k v))
                options))))
  (alexandria:alist-hash-table
    `((:status . :successful)
     (:cliff-suppress-output . t))))
;;; ─── Default function ───────────────────────────────────────────────────────

(defun default-fn (options)
  "Default function when no subcommand is given."
  (declare (ignore options))
  (format t "dsolv: dependency resolver~%")
  (format t "Usage: dsolv <subcommand> [options]~%")
  (format t "~%")
  (format t "Subcommands:~%")
  (format t "  resolve-locations   Resolve dependencies and print package locations~%")
  (format t "  generate-repo-index Generate a repo index from .dscard files~%")
  (format t "  generate-card       Generate a .dscard file~%")
  (format t "  query-repo          Query a repository for packages~%")
  (format t "  display-config      Display the effective configuration~%")
  (format t "~%")
  (format t "Run `dsolv <subcommand> --enable-help` for help on a specific subcommand.~%")
  (alexandria:alist-hash-table
    `((:status . :successful)
     (:cliff-suppress-output . t))))
;;; ─── Version comparators hash table ─────────────────────────────────────────

;; Populate *version-comparators* with all available comparators from svers
(eval-when (:load-toplevel :execute)
  (setf (gethash "debian" *version-comparators*)
        #'com.djhaskin.svers:debian-vercmp)
  (setf (gethash "maven" *version-comparators*)
        #'com.djhaskin.svers:maven-vercmp)
  (setf (gethash "naive" *version-comparators*)
        #'com.djhaskin.svers:naive-vercmp)
  (setf (gethash "python" *version-comparators*)
        #'com.djhaskin.svers:python-vercmp)
  (setf (gethash "rpm" *version-comparators*)
        #'com.djhaskin.svers:rpm-vercmp)
  (setf (gethash "rubygem" *version-comparators*)
        #'com.djhaskin.svers:rubygem-vercmp)
  (setf (gethash "semver" *version-comparators*)
        #'com.djhaskin.svers:semver-vercmp))

;;; ─── Package systems hash table ─────────────────────────────────────────────

;; Populate *package-systems* with real references
(eval-when (:load-toplevel :execute)
  (setf (gethash "degasolv" *package-systems*)
        (list :genrepo 'slurp-degasolv-repo
              :version-comparison "semver"))
  (setf (gethash "apt" *package-systems*)
        (list :genrepo 'slurp-apt-repo
              :version-comparison "debian"))
  (setf (gethash "git" *package-systems*)
        (list :query-constructor 'make-query
              :version-comparison "semver"
              :required-arguments
              (let ((h (make-hash-table :test 'equal)))
                (setf (gethash "clone-folder" h) "clone-folder")
                h)))
  (setf (gethash "subproc" *package-systems*)
        (list :repo-constructor 'make-slurper
              :required-arguments
              (let ((h (make-hash-table :test 'equal)))
                (setf (gethash "subproc-exe" h) "subproc-exe")
                h))))

;;; ─── Main entry point ───────────────────────────────────────────────────────

(defun main (&rest argv)
  "Entry point for the dsolv CLI tool.
   Uses CLIFF's execute-program for argument parsing, config file handling,
   environment variable processing, and subcommand dispatch."
  (declare (ignorable argv))
  (nth-value
    0
    (execute-program
      "dsolv"
      :subcommand-functions
      (list
        (cons '("resolve-locations") #'resolve-locations-fn)
        (cons '("generate-repo-index") #'generate-repo-index-fn)
        (cons '("generate-card") #'generate-card-fn)
        (cons '("query-repo") #'query-repo-fn)
        (cons '("display-config") #'display-config-fn))
      :default-function #'default-fn
      :defaults
      (alexandria:hash-table-alist *subcommand-option-defaults*)
      :cli-arguments (if argv
                         (coerce argv 'list)
                         t))))

;;; main.lisp ends here