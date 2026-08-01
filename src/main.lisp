;;;; src/main.lisp
;;;;
;;;; Main CLI entry point for the dsolv command-line tool. Uses CLIFF
;;;; for subcommand dispatch, option parsing, config file loading, and
;;;; environment variable handling.

(defpackage #:com.djhaskin.dsolv
  (:use #:cl)
  (:import-from #:com.djhaskin.dsolv/util)
  (:import-from #:com.djhaskin.dsolv/resolver)
  (:import-from #:com.djhaskin.dsolv/pkgsys/core)
  (:import-from #:com.djhaskin.dsolv/pkgsys/apt)
  (:import-from #:com.djhaskin.dsolv/pkgsys/git)
  (:import-from #:com.djhaskin.dsolv/pkgsys/subproc)
  (:import-from #:com.djhaskin.cliff)
  (:import-from #:com.djhaskin.nrdl)
  (:import-from #:alexandria)
  (:import-from #:cl-ppcre)
  (:local-nicknames
    (#:util #:com.djhaskin.dsolv/util)
    (#:resolver #:com.djhaskin.dsolv/resolver)
    (#:pkgsys/core #:com.djhaskin.dsolv/pkgsys/core)
    (#:pkgsys/apt #:com.djhaskin.dsolv/pkgsys/apt)
    (#:pkgsys/git #:com.djhaskin.dsolv/pkgsys/git)
    (#:pkgsys/subproc #:com.djhaskin.dsolv/pkgsys/subproc)
    (#:cliff #:com.djhaskin.cliff)
    (#:nrdl #:com.djhaskin.nrdl))
  (:export
    #:main))

(in-package #:com.djhaskin.dsolv)

;;; ─── Version comparators ────────────────────────────────────────────────────

(defparameter *version-comparators*
  (let ((table (make-hash-table :test 'equal)))
    (setf (gethash "naive" table) #'string=)
    table))

;;; ─── Helper functions ───────────────────────────────────────────────────────

(defun get-version-comparator (options)
  "Get the version comparator function from the options table."
  (let* ((comparison (or (gethash :version-comparison options) "semver"))
         (cmp (gethash comparison *version-comparators*)))
    (or cmp #'string=)))

(defun check-required-args (options required-keys)
  "Check that all REQUIRED-KEYS are present in OPTIONS."
  (loop for key in required-keys
        unless (gethash key options)
        do (error "Missing required option ~a" key)))

(defun aggregate-repositories (index-strat repositories genrepo
                               version-comparator)
  "Aggregate multiple repositories into a single query function."
  (let ((repo-fns (loop for url in repositories
                        append (funcall genrepo url))))
    (funcall (util:aggregator index-strat version-comparator)
             repo-fns)))

;;; ─── Subcommand: display-config ─────────────────────────────────────

(defun display-config-fn (options)
  "Display the effective combined configuration."
  (let* ((output-format (or (gethash :output-format options) "plain"))
         (arguments (gethash :arguments options nil))
         (result-info (list :command "display-config"
                            :options options
                            :arguments arguments)))
    (case (intern output-format :keyword)
      (:json
       (format t "~a~%" (cliff:generate-string result-info :pretty 2)))
      (:edn
       (format t "~a~%" (cliff:generate-string result-info :pretty 2)))
      (:plain
       (nrdl:generate-to t result-info :pretty-indent 4)
       (terpri)))
    (alexandria:alist-hash-table
      `((:status . :successful)))))

;;; ─── Subcommand: generate-card ──────────────────────────────────────

(defun generate-card-fn (options)
  "Generate a .dscard file based on the given options."
  (let* ((id (gethash :id options))
         (version (gethash :version options))
         (location (gethash :location options))
         (requirements (gethash :requirements options))
         (card-file (or (gethash :card-file options) "./out.dscard"))
         (meta (gethash :meta options nil)))
    (check-required-args options '(:id :version :location))
    (let* ((reqs (when requirements
                   (loop for str-req in requirements
                         collect (resolver:string-to-requirement str-req))))
           (pkg (resolver:make-package-info
                  :id id :version version :location location
                  :requirements reqs)))
      (util:default-spit card-file pkg)
      (alexandria:alist-hash-table
        `((:status . :successful)
          (:id . ,id)
          (:card-file . ,card-file))))))

;;; ─── Subcommand: generate-repo-index ────────────────────────────────

(defun generate-repo-index-fn (options)
  "Generate a repository index from .dscard files."
  (let* ((search-directory (or (gethash :search-directory options) "."))
         (index-file (or (gethash :index-file options) "index.dsrepo"))
         (add-to (gethash :add-to options nil))
         (version-comparison (or (gethash :version-comparison options)
                                 "semver"))
         (index-sort-order (or (gethash :index-sort-order options)
                               "descending"))
         (version-comparator (get-version-comparator options))
         (sortindex
           (if (string= index-sort-order "ascending")
               (lambda (pkgs)
                 (sort pkgs #'<
                       :key (lambda (p) (funcall version-comparator
                                                 (resolver:pi-version p)
                                                 ""))))
               (lambda (pkgs)
                 (sort pkgs #'>
                       :key (lambda (p) (funcall version-comparator
                                                 (resolver:pi-version p)
                                                 "")))))))
    (pkgsys/core:generate-repo-index search-directory index-file
                                     :add-to add-to
                                     :sortindex sortindex)
    (alexandria:alist-hash-table
      `((:status . :successful)
        (:index-file . ,index-file)))))

;;; ─── Subcommand: resolve-locations ──────────────────────────────────

(defun resolver-error-string (problems)
  "Format resolver problems into a human-readable string."
  (format nil "~%~%~a~%~%~%The resolver encountered the following ~
problems: ~%~{~a~^~%~}"
          "Could not resolve dependencies."
          (loop for prob in problems
                collect (resolver:explain-problem prob))))

(defun resolve-locations-fn (options)
  "Resolve dependencies and print package locations."
  (let* ((alternatives (gethash :alternatives options t))
         (conflict-strat-str (or (gethash :conflict-strat options) "exclusive"))
         (list-strat-str (or (gethash :list-strat options) "lazy"))
         (index-strat (or (gethash :index-strat options) "priority"))
         (error-format (gethash :error-format options t))
         (output-format (or (gethash :output-format options) "plain"))
         (package-system (or (gethash :package-system options) "degasolv"))
         (present-packages-strs (gethash :present-packages options))
         (requirements-strs (gethash :requirements options))
         (resolve-strat-str (or (gethash :resolve-strat options) "thorough"))
         (search-strat-str (or (gethash :search-strat options) "breadth-first"))
         (repositories (gethash :repositories options))
         (version-comparator (get-version-comparator options))
         ;; Convert keywords
         (conflict-strat (intern (string-upcase conflict-strat-str) :keyword))
         (list-strat (intern (string-upcase list-strat-str) :keyword))
         (resolve-strat (intern (string-upcase resolve-strat-str) :keyword))
         (search-strat (intern (string-upcase search-strat-str) :keyword)))
    ;; Check required args
    (check-required-args options '(:requirements))
    ;; Parse requirement strings
    (let* ((requirement-data
             (loop for str-req in requirements-strs
                   collect (resolver:string-to-requirement str-req)))
           ;; Parse present packages
           (present-packages
             (let ((ht (make-hash-table :test 'equal)))
               (loop for str-pkg in present-packages-strs
                     do (let* ((parts (cl-ppcre:split "==" str-pkg))
                               (id (first parts))
                               (version (second parts))
                               (pkg (resolver:make-package-info
                                      :id id :version version
                                      :location "already present"
                                      :requirements nil)))
                          (push pkg (gethash id ht))))
               ht))
           ;; Build the query function
           (query
             (let* ((pkg-sys-config (gethash package-system
                                             resolver:*package-systems*))
                    (genrepo (or (getf pkg-sys-config :repo-constructor)
                                 (getf pkg-sys-config :genrepo)
                                 ;; Fallback to degasolv
                                 #'pkgsys/core:slurp-degasolv-repo)))
               (aggregate-repositories index-strat
                                       repositories
                                       genrepo
                                       version-comparator)))
           ;; Resolve
           (result (resolver:resolve-dependencies-deluxe
                     requirement-data query
                     :present-packages present-packages
                     :strategy resolve-strat
                     :conflict-strat conflict-strat
                     :compare version-comparator
                     :search-strat search-strat
                     :allow-alternatives alternatives
                     :list-strat list-strat))
           (result-key (getf result :result))
           (base-result `(:command "dsolv"
                                   :subcommand "resolve-locations"
                                   :options ,options)))
      (if (eql result-key :successful)
          ;; Successful resolution
          (let* ((packages (getf result :packages))
                 (result-info (append base-result result)))
            (case (intern output-format :keyword)
              (:json
               (format t "~a~%"
                       (cliff:generate-string result-info :pretty 2)))
              (:edn
               (format t "~a~%"
                       (cliff:generate-string result-info :pretty 2)))
              (:plain
               (loop for pkg in packages
                     do (format t "~a~%" (resolver:explain-package pkg))))
              (t
               (format t "~a~%"
                       (cliff:generate-string result-info :pretty 2)))))
          ;; Unsuccessful resolution
          (let* ((problems (getf result :problems))
                 (result-info (append base-result result)))
            (if error-format
                (case (intern output-format :keyword)
                  (:json
                   (format t "~a~%"
                           (cliff:generate-string result-info :pretty 2)))
                  (:edn
                   (format t "~a~%"
                           (cliff:generate-string result-info :pretty 2)))
                  (:plain
                   (format t "~a~%" (resolver-error-string problems)))
                  (t
                   (format t "~a~%"
                           (cliff:generate-string result-info :pretty 2))))
                (format t "~a~%" (resolver-error-string problems)))
            (alexandria:alist-hash-table
              `((:status . :error)
                (:problems . ,problems))))))))

;;; ─── Subcommand: query-repo ─────────────────────────────────────────

(defun query-repo-fn (options)
  "Query a repository for packages matching a given requirement."
  (let* ((index-strat (or (gethash :index-strat options) "priority"))
         (error-format (gethash :error-format options t))
         (output-format (or (gethash :output-format options) "plain"))
         (package-system (or (gethash :package-system options) "degasolv"))
         (repositories (gethash :repositories options))
         (query-str (gethash :query options))
         (version-comparator (get-version-comparator options)))
    (check-required-args options '(:query :repositories))
    (let* ((pkg-sys-config (gethash package-system
                                    resolver:*package-systems*))
           (genrepo (or (getf pkg-sys-config :genrepo)
                        #'pkgsys/core:slurp-degasolv-repo))
           (aggregate-repo (aggregate-repositories
                             index-strat repositories
                             genrepo version-comparator))
           (req (first (resolver:string-to-requirement query-str)))
           (spec (resolver:req-spec req))
           (id (resolver:req-id req))
           (spec-call (resolver:make-spec-call version-comparator))
           (results (remove-if-not
                      (lambda (pkg) (funcall spec-call spec pkg))
                      (funcall aggregate-repo id)))
           (result-info `(:command "dsolv"
                                   :subcommand "query-repo"
                                   :options ,options
                                   :packages ,results)))
      (if (null results)
          (progn
            (if error-format
                (case (intern output-format :keyword)
                  (:json
                   (format t "~a~%"
                           (cliff:generate-string result-info :pretty 2)))
                  (:edn
                   (format t "~a~%"
                           (cliff:generate-string result-info :pretty 2)))
                  (:plain
                   (format t "No results returned from query~%"))
                  (t
                   (format t "No results returned from query~%")))
                (format t "No results returned from query~%"))
            (alexandria:alist-hash-table
              `((:status . :error)
                (:reason . "No results"))))
          (progn
            (case (intern output-format :keyword)
              (:json
               (format t "~a~%"
                       (cliff:generate-string result-info :pretty 2)))
              (:edn
               (format t "~a~%"
                       (cliff:generate-string result-info :pretty 2)))
              (:plain
               (loop for pkg in results
                     do (format t "~a~%" (resolver:explain-package pkg))))
              (t
               (format t "~a~%"
                       (cliff:generate-string result-info :pretty 2))))
            (alexandria:alist-hash-table
              `((:status . :successful)
                (:packages . ,results))))))))

;;; ─── Main entry point ───────────────────────────────────────────────────────

(defun main ()
  "Entry point for the dsolv CLI tool."
  (multiple-value-bind (exit-code result)
                       (cliff:execute-program
                         "dsolv"
                         :default-function
                         (lambda (options)
                           (declare (ignore options))
                           (alexandria:alist-hash-table
                             `((:status . :successful)
                               (:message . "Use 'dsolv help' for usage information."))))
                         :subcommand-functions
                         `((("display-config") . ,(function display-config-fn))
                           (("generate-card") . ,(function generate-card-fn))
                           (("generate-repo-index") . ,(function generate-repo-index-fn))
                           (("resolve-locations") . ,(function resolve-locations-fn))
                           (("query-repo") . ,(function query-repo-fn)))
                         :subcommand-helps
                         `((("display-config")
                            . "Print the effective combined configuration (and arguments) ~
of all the given config files.")
                           (("generate-card")
                            . "Generate a .dscard file based on arguments given.")
                           (("generate-repo-index")
                            . "Generate a repository index based on degasolv package cards.")
                           (("resolve-locations")
                            . "Print the locations of the packages which will resolve all ~
given dependencies.")
                           (("query-repo")
                            . "Query a repository for packages matching a given requirement."))
                         :defaults
                         `((:alternatives . t)
                           (:error-format . t)
                           (:card-file . "./out.dscard")
                           (:conflict-strat . "exclusive")
                           (:index-file . "index.dsrepo")
                           (:index-strat . "priority")
                           (:index-sort-order . "descending")
                           (:output-format . "plain")
                           (:subproc-output-format . "json")
                           (:package-system . "degasolv")
                           (:resolve-strat . "thorough")
                           (:search-directory . ".")
                           (:search-strat . "breadth-first")
                           (:list-strat . "lazy"))
                         :cli-aliases
                         `(("-h" . "--help")))
    (declare (ignore result))
    (uiop:quit exit-code)))
