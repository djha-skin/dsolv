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
    #:req-status
    #:vp-relation
    #:vp-version
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

(defparameter *already-present-location* "already present"
  "Location string carried by packages that are already present.

   Shared by the construction site (present-packages-map) and the JSON
   emission sites so the present-package marker cannot drift. It also
   disambiguates JSON serialization of an empty requirement list: in
   Common Lisp the empty list is NIL, so a resolved package with no
   dependencies (serialize as []) and a present package (serialize as
   null) can only be told apart by this marker.")

;;; ─── NRDL struct serialization methods ──────────────────────────────────────

(defmethod nrdl:emit-nrdl-struct (strm (val resolver:version-predicate)
                                       pretty-indent indented-at
                                       &key json-mode)
  "Serialize a VERSION-PREDICATE struct as an NRDL dictionary."
  (let ((ht (make-hash-table :test 'equal)))
    (setf (gethash :relation ht) (vp-relation val))
    (setf (gethash :version ht) (vp-version val))
    (nrdl:inject-object strm ht pretty-indent indented-at :json-mode json-mode)))

(defmethod nrdl:emit-nrdl-struct (strm (val resolver:requirement)
                                       pretty-indent indented-at
                                       &key json-mode)
  "Serialize a REQUIREMENT struct as an NRDL dictionary."
  (let ((ht (make-hash-table :test 'equal)))
    (setf (gethash :status ht) (req-status val))
    (setf (gethash :id ht) (req-id val))
    (setf (gethash :spec ht) (or (req-spec val) (f:empty-seq)))
    (nrdl:inject-object strm ht pretty-indent indented-at :json-mode json-mode)))

(defmethod nrdl:emit-nrdl-struct (strm (val resolver:package-info)
                                       pretty-indent indented-at
                                       &key json-mode)
  "Serialize a PACKAGE-INFO struct as an NRDL dictionary."
  (let ((ht (make-hash-table :test 'equal)))
    (setf (gethash :id ht) (pi-id val))
    (setf (gethash :version ht) (pi-version val))
    (setf (gethash :location ht) (pi-location val))
    (setf (gethash :requirements ht)
          (or (pi-requirements val) (f:empty-seq)))
    (nrdl:inject-object strm ht pretty-indent indented-at :json-mode json-mode)))

;;; ─── NRDL data conversion helpers ──────────────────────────────────────────

(defun problem-to-data (problem)
  "Convert a problem plist to a hash table for NRDL serialization."
  (let ((ht (make-hash-table :test 'equal)))
    (setf (gethash :term ht)
          (loop for req in (getf problem :term)
                collect req))
    (setf (gethash :found-packages ht) (make-hash-table :test 'equal))
    (setf (gethash :present-packages ht) (make-hash-table :test 'equal))
    (setf (gethash :absent-specs ht) (make-hash-table :test 'equal))
    (let ((reason (getf problem :reason)))
      (when reason
        (setf (gethash :reason ht) reason)))
    (let ((alternative (getf problem :alternative)))
      (when alternative
        (setf (gethash :alternative ht) alternative)))
    (let ((pkg-id (getf problem :package-id)))
      (when pkg-id
        (setf (gethash :package-id ht) pkg-id)))
    ht))

(defun problems-to-data (problems)
  "Convert a list of problem plists to a list of hash tables."
  (loop for problem in problems
        collect (problem-to-data problem)))

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

(defun validate-choice (value allowed message)
  "If VALUE is not one of the ALLOWED strings, throw an exit table for
   a :general-error printing MESSAGE. Mirrors the validation legacy
   degasolv performed at the CLI level (CLIFF has no hook)."
  (unless (member value allowed :test #'string=)
    (throw 'validation-exit (exit-with :general-error message)))
  nil)

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
                     append (funcall genrepo url))))
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

    ;; Validate strategy options (legacy degasolv rejected invalid values at
    ;; the CLI level; CLIFF has no validation hook, so check here)
    (catch 'validation-exit
      (validate-choice
        search-strat '("breadth-first" "depth-first")
        "Search strategy must either be 'breadth-first' or 'depth-first'.")
      (validate-choice
        conflict-strat '("exclusive" "inclusive" "prioritized")
        "Conflict strategy must either be 'exclusive', 'inclusive', or 'prioritized'.")
      (validate-choice
        list-strat '("as-set" "lazy" "eager")
        "List strategy must either be 'as-set', 'lazy', or 'eager'. Using the 'lazy' or 'eager' strategy is recommended.")
      (validate-choice
        resolve-strat '("thorough" "fast")
        "Resolve strategy must either be 'thorough' or 'fast'.")
      (validate-choice
        index-strat '("priority" "global")
        "Strategy must either be 'priority' or 'global'.")

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
                                                    :location *already-present-location*)
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
                 requirement-data
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
            (let ((packages (getf result :packages))
                  (install-graph (getf result :install-graph)))
              (ecase (intern (string-upcase output-format) :keyword)
                (:json
                 (format t "{\"result\":\"successful\",\"packages\":[")
                 (loop for pkg in packages
                       for first = t then nil
                       do (unless first (format t ","))                          (format t "{\"id\":\"~a\",\"version\":\"~a\",\"location\":\"~a\",\"requirements\":~a}"
                                                                                         (pi-id pkg) (pi-version pkg) (pi-location pkg)                                  (requirements-json
                                    (pi-requirements pkg)
                                    (string= (pi-location pkg)
                                             *already-present-location*))))
                 (format t "],\"install-graph\":{")
                 (let ((first t))
                   (fset:do-map (handle entry install-graph)
                                (unless first (format t ","))
                                (setf first nil)
                                (format t "\"~a\":{\"version\":\"~a\",\"location\":\"~a\",\"requirements\":~a,\"name\":\"~a\",\"metadata\":{},\"dependees\":["
                                        handle (getf entry :version)
                                        (getf entry :location)
                                        (requirements-json
                                          (getf entry :requirements)
                                          (string= (getf entry :location)
                                                   *already-present-location*))
                                        (getf entry :name))
                                (loop for dep in (getf entry :dependees)
                                      for first-dep = t then nil
                                      do (unless first-dep (format t ","))
                                      (format t "\"~a\"" dep))
                                (format t "]}")))
                 (format t "}}~%"))
                (:plain
                 (loop for pkg in packages
                       do (format t "~a~%" (explain-package pkg))))
                (:nrdl
                 (let ((ht (make-hash-table :test 'equal)))
                   (setf (gethash :result ht) :successful)
                   (setf (gethash :packages ht) (fset:convert 'list packages))
                   (setf (gethash :install-graph ht)
                         (install-graph-to-data install-graph))
                   (nrdl:generate-to t ht :pretty-indent 2))
                 (terpri)))
              (alexandria:alist-hash-table
                `((:status . :successful)
                  (:cliff-suppress-output . t))))
            (let ((problems (getf result :problems)))
              (if error-format
                  (out-exit-with :system-error
                                 (ecase (intern (string-upcase output-format) :keyword)
                                   (:json
                                    (let ((ht (make-hash-table :test 'equal)))
                                      (setf (gethash :result ht) :unsuccessful)
                                      (setf (gethash :problems ht) (problems-to-data problems))
                                      (with-output-to-string (s)
                                        (nrdl:generate-to s ht :json-mode t))))
                                   (:nrdl
                                    (let ((ht (make-hash-table :test 'equal)))
                                      (setf (gethash :result ht) :unsuccessful)
                                      (setf (gethash :problems ht) (problems-to-data problems))
                                      (with-output-to-string (s)
                                        (nrdl:generate-to s ht :pretty-indent 2))))
                                   (:plain
                                    (format nil "~{~a~%~}" (mapcar #'explain-problem problems)))))
                  (exit-with :system-error
                             (format nil "~{~a~%~}" (mapcar #'explain-problem problems))))))))))

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

;;; ─── NRDL serialization helpers ────────────────────────────────────────────

(defun requirement-to-data (req)
  "Convert a REQUIREMENT struct to a hash table for NRDL serialization."
  (alexandria:alist-hash-table
    (list (cons :status (req-status req))
          (cons :id (req-id req))
          (cons :spec (spec-to-data (req-spec req))))
    :test 'equal))

(defun spec-to-data (spec)
  "Convert a SPEC (list of disjunctions of version predicates) to NRDL data.
   Each disjunction is a list of version-predicate hash tables."
  (loop for disj in spec
        collect (loop for vp in disj
                      collect (alexandria:alist-hash-table
                                (list (cons :relation (vp-relation vp))
                                      (cons :version (vp-version vp)))
                                :test 'equal))))

(defun spec-json (spec)
  "Serialize a SPEC (list of disjunctions of version predicates) as a
   JSON string. NIL serializes as JSON null, matching legacy degasolv."
  (if (null spec)
      "null"
      (with-output-to-string (s)
        (format s "[")
        (loop for disj in spec
              for first-disj = t then nil
              do (unless first-disj (format s ","))
              (format s "[")
              (loop for vp in disj
                    for first-vp = t then nil
                    do (unless first-vp (format s ","))
                    (format s "{\"relation\":\"~a\",\"version\":\"~a\"}"
                            (string-downcase (symbol-name (vp-relation vp)))
                            (vp-version vp)))
              (format s "]"))
        (format s "]"))))

(defun requirements-json (requirements &optional (present-p nil))
  "Serialize REQUIREMENTS (a list of clauses of requirement structs) as
   a JSON string.

   In Common Lisp an empty requirement list is NIL, which is also the
   value used for present packages, so PRESENT-P (true when the package
   is already present, marked by the \"already present\" location)
   disambiguates the two: present packages serialize as JSON null,
   resolved packages with no requirements serialize as [], matching
   legacy degasolv output."
  (cond
    ((null requirements)
     (if present-p "null" "[]"))
    (t
     (with-output-to-string (s)
       (format s "[")
       (loop for clause in requirements
             for first-clause = t then nil
             do (unless first-clause (format s ","))
             (format s "[")
             (loop for req in clause
                   for first-req = t then nil
                   do (unless first-req (format s ","))
                   (format s "{\"status\":\"~a\",\"id\":\"~a\",\"spec\":~a}"
                           (string-downcase (symbol-name (req-status req)))
                           (req-id req)
                           (spec-json (req-spec req))))
             (format s "]"))
       (format s "]")))))

;;; ─── Subcommand: generate-card ──────────────────────────────────────────────

(defun generate-card-fn (options)
  "Generate a .dscard file."
  (let* ((id (ht-get options :id))
         (version (ht-get options :version))
         (location (ht-get options :location))
         (card-file (ht-get options :card-file "./out.dscard"))
         (requirements (or (ht-get options :requirements) (list)))
         (meta (ht-get options :meta)))
    (unless (and id version location)
      (return-from generate-card-fn
                   (exit-with :general-error "Missing required arguments: --set-id, --set-version, --set-location")))
    (let* ((reqs (loop for r in requirements
                       collect (loop for req in (string-to-requirement r)
                                     collect (requirement-to-data req))))
           (pkg-data (let ((ht (alexandria:alist-hash-table
                                 (list (cons :id id)
                                       (cons :version version)
                                       (cons :location location))
                                 :test 'equal)))
                       ;; Only add requirements key if there are any
                       (when reqs
                         (setf (gethash :requirements ht) reqs))
                       ht)))
      ;; Merge in additional metadata, but exclude keys that conflict
      ;; with PackageInfo fields (matching degasolv behavior)
      (when meta
        (maphash (lambda (k v)
                   (unless (member k '(:id :version :location :requirements))
                     (setf (gethash k pkg-data) v)))
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

;;; ─── NRDL serialization helpers ────────────────────────────────────────────

(defun install-graph-to-data (install-graph)
  "Convert an install-graph map to hash tables for NRDL serialization.

   INSTALL-GRAPH is an fset map of handle to plist entries with
   :NAME, :VERSION, :LOCATION, :REQUIREMENTS, and :DEPENDEES keys.
   The plists become hash tables so NRDL can emit them as objects."
  (let ((result (make-hash-table :test 'equal)))
    (fset:do-map (handle entry install-graph)
                 (let ((ht (make-hash-table :test 'equal)))
                   (setf (gethash :version ht) (getf entry :version))
                   (setf (gethash :location ht) (getf entry :location))
                   (when (getf entry :requirements)
                     (setf (gethash :requirements ht)
                           (loop for clause in (getf entry :requirements)
                                 collect (loop for req in clause
                                               collect (requirement-to-data req)))))
                   (setf (gethash :name ht) (getf entry :name))
                   (setf (gethash :metadata ht) (make-hash-table :test 'equal))
                   (setf (gethash :dependees ht) (getf entry :dependees))
                   (setf (gethash handle result) ht)))
    result))

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
              (out-exit-with
                :data-format-error
                (ecase (intern (string-upcase output-format) :keyword)
                  (:json
                   (let ((ht (make-hash-table :test 'equal)))
                     (setf (gethash :result ht) :unsuccessful)
                     (setf (gethash :message ht)
                           "No results returned from query")
                     (setf (gethash :packages ht) (f:empty-seq))
                     (with-output-to-string (s)
                       (nrdl:generate-to s ht :json-mode t))))
                  (:plain "No results returned from query")
                  (:nrdl
                   (let ((ht (make-hash-table :test 'equal)))
                     (setf (gethash :result ht) :unsuccessful)
                     (setf (gethash :message ht)
                           "No results returned from query")
                     (setf (gethash :packages ht) (f:empty-seq))
                     (with-output-to-string (s)
                       (nrdl:generate-to s ht :pretty-indent 2))))))
              (exit-with :data-format-error "No results returned from query"))
          (progn
            (ecase (intern (string-upcase output-format) :keyword)
              (:json
               (format t "{\"packages\":[")
               (let ((first t))
                 (fset:do-seq (pkg results)
                              (unless first (format t ","))
                              (setf first nil)
                              (format t "{\"id\":\"~a\",\"version\":\"~a\",\"location\":\"~a\"}"
                                      (pi-id pkg) (pi-version pkg) (pi-location pkg))))
               (format t "]}~%"))
              (:plain
               (fset:do-seq (pkg results)
                            (format t "~a~%" (explain-package pkg))))
              (:nrdl
               (let ((ht (make-hash-table :test 'equal)))
                 (setf (gethash :packages ht) (fset:convert 'list results))
                 (nrdl:generate-to t ht :pretty-indent 2))
               (terpri)))
            (alexandria:alist-hash-table
              `((:status . :successful)
                (:cliff-suppress-output . t))))))))

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
      (:nrdl
       (let ((ht (make-hash-table :test 'equal)))
         (setf (gethash :effective-configuration ht)
               (loop for k being the hash-key of options
                     using (hash-value v)
                     collect (cons k v)))
         (nrdl:generate-to t ht :pretty-indent 2))
       (terpri))))
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
        (list :query-constructor #'com.djhaskin.dsolv/pkgsys/git:make-query
              :version-comparison "semver"
              :required-arguments
              (let ((h (make-hash-table :test 'equal)))
                (setf (gethash :clone-folder h) "clone-folder")
                h)))
  (setf (gethash "subproc" *package-systems*)
        (list :repo-constructor 'make-slurper
              :required-arguments
              (let ((h (make-hash-table :test 'equal)))
                (setf (gethash :subproc-exe h) "subproc-exe")
                h))))

;;; ─── Main entry point ───────────────────────────────────────────────────────

(defun string-keys-to-keywords (ht)
  "Convert all string keys in a hash table to keywords."
  (let ((result (make-hash-table :test 'equal)))
    (maphash (lambda (k v)
               (setf (gethash (if (stringp k) (intern (string-upcase k) :keyword) k) result)
                     (if (hash-table-p v)
                         (string-keys-to-keywords v)
                         v)))
             ht)
    result))

(defun normalize-keys (options)
  "Normalize option keys by converting underscores to hyphens.
   This ensures env var keys (underscore) match CLI arg keys (hyphen)."
  (let ((keys-to-fix nil))
    (maphash (lambda (k v)
               (declare (ignore v))
               (when (and (keywordp k)
                          (find #\_ (string k)))
                 (push k keys-to-fix)))
             options)
    (dolist (k keys-to-fix)
      (let ((new-key (intern (substitute #\- #\_ (string k)) :keyword)))
        (setf (gethash new-key options) (gethash k options))
        (remhash k options))))
  options)(defun config-key-set-by-cli-or-env-p (options key)
            "Return true if KEY holds a non-default value in OPTIONS.

   CLIFF merges defaults, config files, environment variables, and
   command-line arguments before calling the setup function, so a key
   whose value differs from its default must have been set by the
   environment, the command line, or a standard-location config file.
   setup-function snapshots these keys before merging any user
   -c/--config-file or -j/--json-config values, so that user config
   files may override one another (later files win) without ever
   overriding values already decided by CLI/env."
            (let ((val (gethash key options)))
              (multiple-value-bind (dval present)
                                   (gethash key *subcommand-option-defaults*)
                (and val (not (and present (equal val dval)))))))

(defun merge-config-into-options (options parsed protected-keys)
  "Merge PARSED config hash table into OPTIONS without overriding
   the PROTECTED-KEYS already set by the environment or the command
   line. Among config files, later files override earlier ones."
  (maphash (lambda (k v)
             (unless (member k protected-keys)
               (setf (gethash k options) v)))
           parsed)
  options)

(defun setup-function (options)
  "Setup function for CLIFF's execute-program.
   Loads config files from the :config-files list and merges them into
   options, then expands any option packs. Environment and command-line
   values take precedence over config-file values; among config files,
   later files override earlier ones."
  ;; Normalize keys (underscore -> hyphen for env var compatibility)
  (normalize-keys options)
  ;; Snapshot the keys set by CLI/env (differing from defaults) BEFORE
  ;; merging any config files, so that later config files may override
  ;; earlier ones (matching degasolv's `reduce merge`) without ever
  ;; overriding CLI/env values.
  (let ((protected-keys
          (loop for k being the hash-keys of options
                when (config-key-set-by-cli-or-env-p options k)
                collect k)))
    ;; Load NRDL config files
    (let ((config-files (gethash :config-files options)))
      (when config-files
        (dolist (file config-files)
          (handler-case
              (let* ((raw (data-slurp file))
                     (parsed (parse-string raw)))
                (when (hash-table-p parsed)
                  (merge-config-into-options options parsed protected-keys)))
            (error (e)
              (format *error-output*
                      "Warning: Could not load config file ~a: ~a~%" file e))))))
    ;; Load JSON config files (string keys -> keywords)
    (let ((json-config-files (gethash :json-config-files options)))
      (when json-config-files
        (dolist (file json-config-files)
          (handler-case
              (let* ((raw (data-slurp file))
                     (parsed (parse-string raw)))
                (when (hash-table-p parsed)
                  (merge-config-into-options
                    options
                    (string-keys-to-keywords parsed)
                    protected-keys)))
            (error (e)
              (format *error-output*
                      "Warning: Could not load JSON config file ~a: ~a~%" file e)))))))
  ;; Apply option packs to the effective configuration
  (expand-option-packs options))

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
      :setup #'setup-function
      :environment-aliases
      '(;; Map DEGASOLV_* environment variables to CLIFF's DSOLV_* format
        ("DEGASOLV_ID" . "DSOLV_ITEM_ID")
        ("DEGASOLV_REQUIREMENTS" . "DSOLV_LIST_REQUIREMENTS")
        ("DEGASOLV_VERSION" . "DSOLV_ITEM_VERSION")
        ("DEGASOLV_LOCATION" . "DSOLV_ITEM_LOCATION")
        ("DEGASOLV_CARD_FILE" . "DSOLV_ITEM_CARD_FILE")
        ("DEGASOLV_CONFIG_FILES" . "DSOLV_LIST_CONFIG_FILES")
        ("DEGASOLV_JSON_CONFIG_FILES" . "DSOLV_LIST_JSON_CONFIG_FILES")
        ("DEGASOLV_REPOSITORIES" . "DSOLV_LIST_REPOSITORIES")
        ("DEGASOLV_PRESENT_PACKAGES" . "DSOLV_LIST_PRESENT_PACKAGES")
        ("DEGASOLV_OPTION_PACKS" . "DSOLV_LIST_OPTION_PACKS")
        ("DEGASOLV_SEARCH_DIRECTORY" . "DSOLV_ITEM_SEARCH_DIRECTORY")
        ("DEGASOLV_INDEX_FILE" . "DSOLV_ITEM_INDEX_FILE")
        ("DEGASOLV_OUTPUT_FORMAT" . "DSOLV_ITEM_OUTPUT_FORMAT")
        ("DEGASOLV_PACKAGE_SYSTEM" . "DSOLV_ITEM_PACKAGE_SYSTEM")
        ("DEGASOLV_SEARCH_STRAT" . "DSOLV_ITEM_SEARCH_STRAT")
        ("DEGASOLV_CONFLICT_STRAT" . "DSOLV_ITEM_CONFLICT_STRAT")
        ("DEGASOLV_RESOLVE_STRAT" . "DSOLV_ITEM_RESOLVE_STRAT")
        ("DEGASOLV_LIST_STRAT" . "DSOLV_ITEM_LIST_STRAT")
        ("DEGASOLV_INDEX_STRAT" . "DSOLV_ITEM_INDEX_STRAT")
        ("DEGASOLV_INDEX_SORT_ORDER" . "DSOLV_ITEM_INDEX_SORT_ORDER")
        ("DEGASOLV_VERSION_COMPARISON" . "DSOLV_ITEM_VERSION_COMPARISON")
        ("DEGASOLV_SUBPROC_EXE" . "DSOLV_ITEM_SUBPROC_EXE")
        ("DEGASOLV_CLONE_FOLDER" . "DSOLV_ITEM_CLONE_FOLDER")
        ("DEGASOLV_SUBPROC_OUTPUT_FORMAT" . "DSOLV_ITEM_SUBPROC_OUTPUT_FORMAT")
        ("DEGASOLV_QUERY" . "DSOLV_ITEM_QUERY")
        ("DEGASOLV_ADD_TO" . "DSOLV_ITEM_ADD_TO")
        ("DEGASOLV_ALTERNATIVES" . "DSOLV_FLAG_ALTERNATIVES")
        ("DEGASOLV_ERROR_FORMAT" . "DSOLV_FLAG_ERROR_FORMAT"))
      :cli-aliases
      '(;; Scalar options: --name -> --set-name
        ("--id" . "--set-id")
        ("--version" . "--set-version")
        ("--location" . "--set-location")
        ("--card-file" . "--set-card-file")
        ("--output-format" . "--set-output-format")
        ("--index-strat" . "--set-index-strat")
        ("--search-strat" . "--set-search-strat")
        ("--conflict-strat" . "--set-conflict-strat")
        ("--resolve-strat" . "--set-resolve-strat")
        ("--list-strat" . "--set-list-strat")
        ("--package-system" . "--set-package-system")
        ("--index-file" . "--set-index-file")
        ("--search-directory" . "--set-search-directory")
        ("--index-sort-order" . "--set-index-sort-order")
        ("--version-comparison" . "--set-version-comparison")
        ("--subproc-exe" . "--set-subproc-exe")
        ("--clone-folder" . "--set-clone-folder")
        ("--subproc-output-format" . "--set-subproc-output-format")
        ("--query" . "--set-query")
        ("--add-to" . "--set-add-to")
        ("--error-format" . "--set-error-format")
        ;; List options: --name -> --add-name (use plural form for CLIFF key)
        ("--requirement" . "--add-requirements")
        ("--repository" . "--add-repositories")
        ("--present-package" . "--add-present-packages")
        ("--option-pack" . "--add-option-packs")
        ("--config-file" . "--add-config-files")
        ("--json-config" . "--add-json-config-files")
        ;; Hash-table options: --name -> --join-name
        ("--meta" . "--join-meta")
        ;; Short options (for compatibility with degasolv scripts)
        ("-i" . "--set-id")
        ("-v" . "--set-version")
        ("-l" . "--set-location")
        ("-C" . "--set-card-file")
        ("-o" . "--set-output-format")
        ("-S" . "--set-index-strat")
        ("-e" . "--set-search-strat")
        ("-f" . "--set-conflict-strat")
        ("-s" . "--set-resolve-strat")
        ("-L" . "--set-list-strat")
        ("-t" . "--set-package-system")
        ("-I" . "--set-index-file")
        ("-d" . "--set-search-directory")
        ("-O" . "--set-index-sort-order")
        ("-V" . "--set-version-comparison")
        ("-x" . "--set-subproc-exe")
        ("-n" . "--set-clone-folder")
        ("-u" . "--set-subproc-output-format")
        ("-q" . "--set-query")
        ("-a" . "--set-add-to")
        ("-r" . "--add-requirements")
        ("-R" . "--add-repositories")
        ("-p" . "--add-present-packages")
        ("-k" . "--add-option-packs")
        ("-c" . "--add-config-files")
        ("-j" . "--add-json-config-files")
        ("-m" . "--join-meta")
        ;; help page
        ("-h" . "help")
        ("--help" . "help"))
      :cli-arguments (if argv
                         (coerce argv 'list)
                         t))))

;;; main.lisp ends here
