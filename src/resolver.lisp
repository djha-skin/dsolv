;;;; src/resolver.lisp
;;;;
;;;; Data structures and core functions for the dsolv dependency resolver.

(defpackage #:com.djhaskin.dsolv/resolver
  (:use #:cl)
  (:import-from #:com.djhaskin.dsolv/util)
  (:import-from #:alexandria)
  (:import-from #:cl-ppcre)
  (:import-from #:fset)
  (:local-nicknames
    (#:util #:com.djhaskin.dsolv/util)
    (#:resolver #:com.djhaskin.dsolv/resolver)
    (#:f #:fset))
  (:export
    ;; Records
    #:make-version-predicate
    #:version-predicate
    #:vp-relation
    #:vp-version
    #:make-requirement
    #:requirement
    #:requirement-status
    #:requirement-id
    #:requirement-spec
    #:req-id
    #:req-spec
    #:req-status
    #:make-decorated-requirement
    #:decorated-requirement
    #:decorated-requirement-clause
    #:decorated-requirement-parent
    #:make-package-info
    #:package-info
    #:package-info-id
    #:package-info-version
    #:package-info-location
    #:package-info-requirements
    #:pi-id
    #:pi-version
    #:pi-location
    #:pi-requirements
    ;; Functions
    #:present
    #:absent
    #:string-to-requirement
    #:explain-package
    #:explain-package-list
    #:explain-problem
    #:resolve-dependencies
    #:resolve-dependencies-deluxe
    #:list-packages
    #:make-install-graph
    #:make-spec-call
    #:explain-absent-spec
    ;; Constants
    #:*version-comparators*
    #:*package-systems*
    #:*subcommand-option-defaults*
    #:*available-option-packs*
    #:*relation-strings*
    ;; Regex
    #:*str-requirement-regex*
    #:*str-frozen-package-regex*
    #:*version-regex*
    ))

(in-package #:com.djhaskin.dsolv/resolver)

;;; ─── Records ────────────────────────────────────────────────────────────────

(defstruct (version-predicate (:conc-name vp-))
  (relation nil :type (or null keyword))
  (version "" :type string))

(defstruct (requirement (:conc-name req-))
  (status :present :type (member :present :absent))
  (id "" :type string)
  (spec nil :type (or null list)))

(defstruct (decorated-requirement (:conc-name dr-))
  (clause nil :type list)
  (parent nil :type (or null package-info keyword)))

(defstruct (package-info (:conc-name pi-))
  (id "" :type string)
  (version "" :type string)
  (location "" :type string)
  (requirements nil :type (or null list)))

;;; ─── Print methods for structs ───────────────────────────────────────────────

(defmethod print-object ((obj version-predicate) stream)
  (let ((relation-str (cdr (assoc (vp-relation obj) *relation-strings*))))
    (format stream "~a~a" relation-str (vp-version obj))))

(defmethod print-object ((obj requirement) stream)
  (when (eql (req-status obj) :absent)
    (write-char #\! stream))
  (princ (req-id obj) stream)
  (when (req-spec obj)
    (loop for conj in (req-spec obj)
          for first-disj = t then nil
          do (unless first-disj (write-char #\; stream))
          (loop for vp in conj
                for first-conj = t then nil
                do (unless first-conj (write-char #\, stream))
                (print-object vp stream)))))

;;; ─── Constants ──────────────────────────────────────────────────────────────

(defparameter *relation-strings*
  '((:greater-than . ">")
    (:greater-equal . ">=")
    (:equal-to . "==")
    (:not-equal . "!=")
    (:less-equal . "<=")
    (:less-than . "<")
    (:matches . "<>")
    (:in-range . "=>")
    (:pess-greater . "><")))

(defparameter *str-id-pattern* "[^>=<!;,|]+")
(defparameter *id-regex* (cl-ppcre:create-scanner
                           (format nil "^~a$" *str-id-pattern*)))
(defparameter *str-version-pattern* "(\\p{Alnum}|\\p{Punct})*")
(defparameter *version-regex* (cl-ppcre:create-scanner
                                (format nil "^~a$" *str-version-pattern*)))

(defparameter *str-relation-pattern* "(>=|==|!=|<=|<|>)")
(defparameter *str-version-predicate-pattern*
  (format nil "~a~a" *str-relation-pattern* *str-version-pattern*))
(defparameter *str-version-conj-predicate-pattern*
  (format nil "~a(,~a)*" *str-version-predicate-pattern*
          *str-version-predicate-pattern*))
(defparameter *str-spec-pattern*
  (format nil "~a(;~a)*" *str-version-conj-predicate-pattern*
          *str-version-conj-predicate-pattern*))
(defparameter *str-alternative-pattern*
  (format nil "!?~a(~a)?" *str-id-pattern* *str-spec-pattern*))
(defparameter *str-requirement-pattern*
  (format nil "~a([|]~a)*" *str-alternative-pattern*
          *str-alternative-pattern*))
(defparameter *str-requirement-regex*
  (cl-ppcre:create-scanner
    (format nil "^~a$" *str-requirement-pattern*)))
(defparameter *str-frozen-package-pattern*
  (format nil "~a==~a" *str-id-pattern* *str-version-pattern*))
(defparameter *str-frozen-package-regex*
  (cl-ppcre:create-scanner
    (format nil "^~a$" *str-frozen-package-pattern*)))

;;; ─── Version comparison ─────────────────────────────────────────────────────

(defparameter *version-comparators*
  (let ((table (make-hash-table :test 'equal)))
    (setf (gethash "naive" table) #'string=)
    table))

;;; ─── Package system definitions ─────────────────────────────────────────────

(defparameter *package-systems*
  (let ((table (make-hash-table :test 'equal)))
    (setf (gethash "degasolv" table)
          (list :genrepo 'slurp-degasolv-repo
                :version-comparison "semver"))
    (setf (gethash "apt" table)
          (list :genrepo 'slurp-apt-repo
                :version-comparison "debian"))
    (setf (gethash "subproc" table)
          (list :repo-constructor 'make-slurper
                :required-arguments
                (let ((h (make-hash-table :test 'equal)))
                  (setf (gethash "subproc-exe" h) "subproc-exe")
                  h)))
    table))

(defparameter *subcommand-option-defaults*
  (let ((table (make-hash-table :test 'equal)))
    (setf (gethash :alternatives table) t)
    (setf (gethash :error-format table) nil)
    (setf (gethash :card-file table) "./out.dscard")
    (setf (gethash :conflict-strat table) "exclusive")
    (setf (gethash :index-file table) "index.dsrepo")
    (setf (gethash :index-strat table) "priority")
    (setf (gethash :index-sort-order table) "descending")
    (setf (gethash :output-format table) "plain")
    (setf (gethash :subproc-output-format table) "json")
    (setf (gethash :package-system table) "degasolv")
    (setf (gethash :resolve-strat table) "thorough")
    (setf (gethash :search-directory table) ".")
    (setf (gethash :search-strat table) "breadth-first")
    (setf (gethash :list-strat table) "lazy")
    table))

(defparameter *available-option-packs*
  (let ((table (make-hash-table :test 'equal)))
    (setf (gethash "v1" table)
          (let ((h (make-hash-table :test 'equal)))
            (setf (gethash :error-format h) nil)
            (setf (gethash :list-strat h) "as-set")
            h))
    (setf (gethash "multi-version-mode" table)
          (let ((h (make-hash-table :test 'equal)))
            (setf (gethash :conflict-strat h) "inclusive")
            (setf (gethash :resolve-strat h) "fast")
            (setf (gethash :alternatives h) nil)
            h))
    (setf (gethash "firstfound-version-mode" table)
          (let ((h (make-hash-table :test 'equal)))
            (setf (gethash :conflict-strat h) "prioritized")
            (setf (gethash :resolve-strat h) "fast")
            (setf (gethash :alternatives h) nil)
            h))
    table))

;;; ─── Requirement parsing ────────────────────────────────────────────────────

(defun present (id &optional spec)
  "Create a :present requirement for ID with optional SPEC."
  (make-requirement :status :present :id id :spec spec))

(defun absent (id &optional spec)
  "Create an :absent requirement for ID with optional SPEC."
  (make-requirement :status :absent :id id :spec spec))

(defun string-to-requirement (str)
  "Parse a requirement string into a list of Requirement structs.

  Examples:
    \"a\"                    -> ((:present \"a\" nil))
    \"!a\"                   -> ((:absent \"a\" nil))
    \"a|b\"                  -> ((:present \"a\" nil) (:present \"b\" nil))
    \"a>2.0,<=3.0;>4.0\"    -> ((:present \"a\" (((:greater-than . \"2.0\")
                                                   (:less-equal . \"3.0\"))
                                                  ((:greater-than . \"4.0\")))))"
  (if (string= str "")
      nil
      (loop for piece in (cl-ppcre:split "\\|" str)
            collect
            (let* ((parsed (cl-ppcre:register-groups-bind
                             (status-piece name-piece spec-piece)
                             ("^(!?)([^!><=]+)(.*)$" piece)
                             (list status-piece name-piece spec-piece)))
                   (status-str (first parsed))
                   (name (second parsed))
                   (spec-str (third parsed))
                   (initial-term (if (string= status-str "!")
                                     (absent name)
                                     (present name))))
              (if (string= spec-str "")
                  initial-term
                  (let ((spec
                          (loop for disj in (cl-ppcre:split ";" spec-str)
                                collect
                                (loop for conj-piece
                                      in (cl-ppcre:split "," disj)
                                      collect
                                      (let* ((vp (cl-ppcre:register-groups-bind
                                                   (rel ver)
                                                   ("^(<>|><|=>|<=|!=|==|>=|<|>)(.+)"
                                                    conj-piece)
                                                   (list rel ver)))
                                             (rel-str (first vp))
                                             (ver (second vp))
                                             (rel-keyword
                                               (cond
                                                 ((string= rel-str "<")
                                                  :less-than)
                                                 ((string= rel-str "<=")
                                                  :less-equal)
                                                 ((string= rel-str "==")
                                                  :equal-to)
                                                 ((string= rel-str "!=")
                                                  :not-equal)
                                                 ((string= rel-str ">=")
                                                  :greater-equal)
                                                 ((string= rel-str ">")
                                                  :greater-than)
                                                 ((string= rel-str "=>")
                                                  :in-range)
                                                 ((string= rel-str "><")
                                                  :pess-greater)
                                                 ((string= rel-str "<>")
                                                  :matches)
                                                 (t :equal-to))))
                                        (make-version-predicate
                                          :relation rel-keyword
                                          :version ver))))))
                    (setf (req-spec initial-term) spec)
                    initial-term))))))

;;; ─── Display helpers ────────────────────────────────────────────────────────

(defun explain-package (pkg)
  "Return a string representation of a package."
  (format nil "~a==~a @ ~a"
          (pi-id pkg)
          (pi-version pkg)
          (pi-location pkg)))

(defun explain-absent-spec (entry)
  "Return a string representation of an absent spec entry."
  (destructuring-bind (id specs) entry
    (format nil "~a~@[ (~{~a~^ ~})~]" id specs)))

(defun explain-package-list (pkg-list label)
  "Return a string representation of a package list."
  (format nil "  - ~a:~@[~%~{~a~%~}~]"
          label
          (when pkg-list
            (loop for pkg in pkg-list
                  collect (format nil "    - ~a" (explain-package pkg))))))

(defparameter *reason-explanations*
  '((:uncovered-case . "Unknown Cause (uncovered case in conditional)")
    (:empty-alternative-set
     . "Empty alternative set (e.g., the requirement \"|\")")
    (:present-package-conflict
     . "Package in question conflicts with a previously selected package.")
    (:package-not-found
     . "Package in question is not present in the repository")
    (:package-rejected
     . "Package in question was found in the repository, but cannot be used.")))

(defun explain-problem (problem)
  "Return a detailed explanation string for a resolver problem."
  (with-output-to-string (out)
    (format out "  Clause: ~{~a~^|~}"
            (getf problem :term))
    (terpri out)
    (format out "~a"
            (let ((found (getf problem :found-packages)))
              (if found
                  (explain-package-list
                    (fset:reduce (lambda (acc key)
                                  (append acc (fset:lookup found key)))
                                (fset:domain found)
                                :initial-value '())
                    "Packages selected")
                  "")))
    (let ((present-pkgs (getf problem :present-packages)))
      (when present-pkgs
        (format out "~%~a"
                (explain-package-list
                  (fset:reduce (lambda (acc key)
                                (append acc (fset:lookup present-pkgs key)))
                              (fset:domain present-pkgs)
                              :initial-value '())
                  "Packages already present"))))
    (let ((alt (getf problem :alternative)))
      (when alt
        (format out "~%  - Alternative being considered: ~a" alt)))
    (let ((reason (getf problem :reason)))
      (when reason
        (format out "~%  - ~a"
                (or (cdr (assoc reason *reason-explanations*))
                    (princ-to-string reason)))))
    (let ((pkg-id (getf problem :package-id)))
      (when pkg-id
        (format out "~%  - Package ID in question: ~a" pkg-id)))))

;;; ─── Spec calling ───────────────────────────────────────────────────────────

(defun make-comparison (cmp pkg-ver relation version)
  "Compare PKG-VER with VERSION using the given RELATION and CMP function.

  RELATION is one of the version predicate relation keywords."
  (if (eql relation :matches)
      (handler-case
          (let ((pattern (cl-ppcre:create-scanner version)))
            (when pattern
              (cl-ppcre:scan pattern pkg-ver)))
        (error () nil))
      (let ((cmp-result (funcall cmp pkg-ver version)))
        (case relation
          (:in-range
           (multiple-value-bind (match groups)
                                (cl-ppcre:scan-to-strings "^(.*?)(\\d+)(\\D*)$" version)
             (if match
                 (let* ((rest-str (aref groups 0))
                        (re-num (aref groups 1))
                        (trailing (aref groups 2))
                        (num (parse-integer re-num))
                        (higher-version (format nil "~a~d~a" rest-str (1+ num) trailing))
                        (higher-result (funcall cmp pkg-ver higher-version)))
                   (and (>= cmp-result 0) (< higher-result 0)))
                 nil)))
          (:pess-greater
           (if (cl-ppcre:scan "\\d+" version)
               (let* ((split-on-nums (cl-ppcre:split "\\d+" version))
                      (non-nums (if (null split-on-nums)
                                    (list "")
                                    split-on-nums))
                      (nums (cl-ppcre:split "\\D+" version))
                      (strnum (car nums))
                      (intnum (parse-integer strnum))
                      (higher (format nil "~a~d" (car non-nums) (1+ intnum)))
                      (higher-result (funcall cmp pkg-ver higher)))
                 (and (>= cmp-result 0) (< higher-result 0)))
               nil))
          (:greater-than (plusp cmp-result))
          (:greater-equal (not (minusp cmp-result)))
          (:equal-to (zerop cmp-result))
          (:not-equal (not (zerop cmp-result)))
          (:less-equal (not (plusp cmp-result)))
          (:less-than (minusp cmp-result))
          (t nil)))))

(defun make-spec-call (cmp)
  "Create a spec-calling function using the given comparator CMP.
  When CMP is nil, all specs are satisfied (no comparison available)."
  (lambda (spec pkg)
    (if (null cmp)
        t
        (if (null spec)
            t
            (let ((pkg-ver (pi-version pkg)))
              (some (lambda (disjunction)
                      (every (lambda (vp)
                               (make-comparison cmp pkg-ver
                                                (vp-relation vp)
                                                (vp-version vp)))
                             disjunction))
                    spec))))))

;;; ─── Package listing ────────────────────────────────────────────────────────

(defun list-packages (package-graph &key (list-strat :lazy) (exclude nil))
  "List packages from a dependency graph.

  PACKAGE-GRAPH is an fset map mapping parent packages to children.
  :root is the special root key mapping to the list of top-level packages.
  EXCLUDE is an fset set of packages to skip.
  :lazy lists children before parents, :eager lists parents before children.

  Follows the Clojure semantics: recursively collects packages from the
  graph starting at :root, where children of a node are stored as the
  node's value in the map."
  (flet ((get-children (node)
           (fset:lookup package-graph node)))
    (labels ((list-pkgs-rec (already-visited parents children-of)
               (let* ((raw-children (get-children children-of))
                      (children (when raw-children
                                  (remove-if-not
                                    (lambda (p)
                                      (and (not (fset:member? p already-visited))
                                           (not (and exclude (fset:member? p exclude)))
                                           (not (fset:member? p parents))))
                                    (fset:convert 'list raw-children)))))
                 (if (null children)
                     (list nil (fset:empty-set))
                     (let ((result (reduce
                                     (lambda (acc v)
                                       (destructuring-bind (pkg-list visited) acc
                                         (let ((grandchildren-result
                                                 (list-pkgs-rec
                                                   visited
                                                   (fset:with parents v)
                                                   v)))
                                           (destructuring-bind (grandchildren-list grandchildren-visited)
                                               grandchildren-result
                                             (let ((base-pkg-list (append pkg-list grandchildren-list))
                                                   (base-visited (fset:union visited grandchildren-visited)))
                                               (if (and (eql list-strat :eager)
                                                        (not (fset:member? v base-visited)))
                                                   (list (append base-pkg-list (list v))
                                                         (fset:with base-visited v))
                                                   (list base-pkg-list base-visited)))))))
                                     children
                                     :initial-value (list nil already-visited))))
                         (if (eql list-strat :lazy)
                             (let ((visited-from-children (second result))
                                   (list-from-children (first result)))
                               (list (append list-from-children
                                             (remove-if (lambda (c) (fset:member? c visited-from-children))
                                                        children))
                                     (reduce (lambda (s c) (fset:with s c))
                                             children
                                             :initial-value visited-from-children)))
                             result))))))
      (first (list-pkgs-rec (fset:empty-set)
                            (fset:with (fset:empty-set) :root)
                            :root)))))

;;; ─── Resolver ───────────────────────────────────────────────────────────────

(defun vet-candidate (id-absent-specs safe-spec-call spec candidate)
  "Check if CANDIDATE satisfies the given spec and doesn't violate absent specs."
  (and (funcall safe-spec-call spec candidate)
       (loop for absent-spec in id-absent-specs
             always (not (funcall safe-spec-call absent-spec candidate)))))

(defun seek-package (query-results vet)
  "Seek a package from query results matching the VET predicate.
  QUERY-RESULTS may be an fset seq (from fset-based repo queries);
  it is converted to a plain list for CL sequence operations."
  (if (null query-results)
      (list :unsuccessful (list :problem :empty-query-results))
      (let* ((query-list (fset:convert 'list query-results))
             (filtered (remove-if-not vet query-list)))
        (if (null filtered)
            (list :unsuccessful (list :problem :unsatisfactory-query-results))
            (list :successful filtered)))))

(defun present-packages-satisfies-p (pkgs spec safe-spec-call status)
  "Check if any of the present packages already satisfy the spec."
  (some (lambda (pkg)
          (let ((test (funcall safe-spec-call spec pkg)))
            (when (or (and (eql status :absent) (not test))
                      (and (eql status :present) test))
              pkg)))
        pkgs))

(defun merge-failure-records (a b)
  "Merge two failure records together."
  (let ((result (copy-list a)))
    ;; Inline merge-into: (setf (getf base key) val) on a parameter doesn't
    ;; propagate back to the caller's variable, so operate on RESULT directly.
    (let ((existing (getf result :problems))
          (b-problems (getf b :problems)))
      (if existing
          (setf (getf result :problems) (append existing b-problems))
          (setf (getf result :problems) b-problems)))
    (let ((sug-a (getf a :suggestions))
          (sug-b (getf b :suggestions)))
      (if sug-a
          (if sug-b
              (setf (getf result :suggestions)
                    (fset:reduce
                     (lambda (merged key)
                       (let ((vb (fset:lookup sug-b key))
                             (va (fset:lookup sug-a key)))
                         (if va
                             (fset:with merged key
                                        (intersection va vb))
                             (fset:with merged key vb))))
                     (fset:domain sug-b)
                     :initial-value sug-a))
              (setf (getf result :suggestions) sug-a))
          (setf (getf result :suggestions) sug-b)))
    result))

(defun make-error (present-packages found-packages absent-specs
                   clause reason &key suggestions additional)
  "Create a resolver error result."
  (let ((problem (list :term clause
                       :found-packages found-packages
                       :present-packages present-packages
                       :absent-specs absent-specs
                       :reason reason)))
    (when additional
      (setf problem (append problem additional)))
    (list :unsuccessful
          (if suggestions
              (list :problems (list problem)
                    :suggestions suggestions)
              (list :problems (list problem))))))

(defun try-candidates (try-candidate vet candidates)
  "Try each candidate in order, returning the first successful result.

  Follows the Clojure semantics: on failure, checks for suggestions from
  the failure record and prepends relevant (not-yet-examined, vet-passing)
  suggested packages to the remaining candidates."
  (let ((remaining candidates)
        (failure-record nil)
        (examined (fset:empty-set)))
    (loop
      (if (null remaining)
          (return (list :unsuccessful failure-record))
          (let* ((fcand (first remaining))
                 (rcand (rest remaining))
                 (id (pi-id fcand))
                 (response (funcall try-candidate fcand)))
            (setf examined (fset:with examined fcand))
            (if (eql (first response) :successful)
                (return response)
                (let ((result (second response)))
                  (setf failure-record
                        (merge-failure-records failure-record result))
                  (setf remaining
                        (let ((suggestions (getf result :suggestions)))
                          (if suggestions
                              (let ((relevant (fset:lookup suggestions id)))
                                (if relevant
                                    (append
                                      (remove-if-not
                                        (lambda (y)
                                          (and (not (fset:member? y examined))
                                               (funcall vet y)))
                                        relevant)
                                      rcand)
                                    rcand))
                              rcand))))))))))

(defun hoist (alternatives absent-specs found-packages present-packages)
  "Reorder alternatives: absent first, then present, then unspecified."
  (if (= 1 (length alternatives))
      alternatives
      (let ((absent ())
            (present ())
            (unspecified ()))
        (loop for alt in alternatives
              do (let ((id (req-id alt)))
                   (cond
                     ((fset:lookup absent-specs id) (push alt absent))
                     ((or (fset:lookup found-packages id)
                          (fset:lookup present-packages id))
                      (push alt present))
                     (t (push alt unspecified)))))
        (append (reverse absent)
                (reverse present)
                (reverse unspecified)))))

(defun make-resolve-deps (conflict-strat concat-reqs safe-spec-call
                          cull cull-alternatives)
  "Create a resolver function with the given strategy.

  Returns a closure of 6 arguments (repo present-packages found-packages
  absent-specs clauses package-graph) that performs recursive dependency
  resolution.  Strategy arguments (conflict-strat, concat-reqs, safe-spec-call,
  cull, cull-alternatives) are captured by the closure, matching the Clojure
  pattern where make-resolve-deps returns resolve-deps with strategy baked in."
  (lambda (repo present-packages found-packages absent-specs
           clauses package-graph)
    (labels
        ((resolve-deps (found-packages absent-specs clauses package-graph)
           "Recursive resolver.  REPO and PRESENT-PACKAGES are captured
            by closure from the outer lambda.  FOUND-PACKAGES, ABSENT-SPECS,
            CLAUSES, PACKAGE-GRAPH change during recursion and are passed
            explicitly.  All strategy arguments (CONFLICT-STRAT, CONCAT-REQS,
            SAFE-SPEC-CALL, CULL, CULL-ALTERNATIVES) are captured by closure
            from MAKE-RESOLVE-DEPS."
           (if (null clauses)
               (list :successful package-graph)
               (let* ((fclause (first clauses))
                      (rclauses (rest clauses))
                      (clause (dr-clause fclause))
                      (parent (dr-parent fclause))
                      (mkerror (lambda (reason &key suggestions additional)
                                 (make-error present-packages
                                             found-packages absent-specs
                                             clause reason
                                             :suggestions suggestions
                                             :additional additional))))
                 (if (null clause)
                     (funcall mkerror :empty-alternative-set)
                     (let* ((hoisted (hoist
                                      (funcall cull-alternatives clause)
                                      absent-specs found-packages
                                      present-packages))
(clause-result
                              ;; Short-circuit: evaluate alternatives lazily,
                              ;; stopping at the first successful result.
                              ;; This mirrors Clojure's lazy `for` + `some`
                              ;; pattern, critical for NP-complete performance.
                              (let ((failures '()))
                                (or (loop for alternative in hoisted
                                          thereis
                                          (let ((result
                                                  (resolve-alternative
                                                    alternative mkerror rclauses
                                                    parent found-packages
                                                    absent-specs package-graph)))
                                            (if (eql (first result) :successful)
                                                result
                                                (progn
                                                  (push (second result) failures)
                                                  nil))))
                                    (list :unsuccessful
                                          (reduce #'merge-failure-records
                                                  (nreverse failures)
                                                  :initial-value nil))))))
                       clause-result)))))
         (resolve-alternative (alternative mkerror rclauses parent
                                 found-packages absent-specs package-graph)
           "Resolve a single alternative.  FOUND-PACKAGES, ABSENT-SPECS,
            and PACKAGE-GRAPH are passed explicitly so they reflect the
            current recursion state, not the initial values from the outer
            lambda's closure.  REPO and PRESENT-PACKAGES are captured by
            closure (they never change during recursion)."
           (let* ((status (req-status alternative))
                  (id (req-id alternative))
                  (spec (req-spec alternative))
                  (vet (lambda (candidate)
                         (vet-candidate
                           (fset:lookup absent-specs id)
                           safe-spec-call spec candidate)))
                  (present-id-packages (fset:lookup present-packages id))
                  (found-id-packages (fset:lookup found-packages id))
                  (present-package
                    (when present-id-packages
                      (if (eql conflict-strat :prioritized)
                          (car present-id-packages)
                          (present-packages-satisfies-p
                            present-id-packages spec safe-spec-call status))))
                  (found-package
                    (when found-id-packages
                      (if (eql conflict-strat :prioritized)
                          (car found-id-packages)
                          (present-packages-satisfies-p
                            found-id-packages spec safe-spec-call status)))))
             (cond
               (present-package
                (resolve-deps found-packages absent-specs rclauses
                              (update-package-graph package-graph parent
                                                    present-package)))
               (found-package
                (resolve-deps found-packages absent-specs rclauses
                              (update-package-graph package-graph parent
                                                    found-package)))
               ((and (not (eql conflict-strat :inclusive))
                     present-id-packages)
                (funcall mkerror :present-package-conflict
                         :additional
                         (list :alternative alternative
                               :package-present-by :given)))
               ((and (not (eql conflict-strat :inclusive))
                     found-id-packages)
                (let ((seek-result (seek-package (funcall repo id) vet)))
                  (if (eql (first seek-result) :successful)
                      (funcall mkerror :present-package-conflict
                               :additional
                               (list :alternative alternative
                                     :package-present-by :found
                                     :suggestion-attempt :successful)
                               :suggestions
                               (fset:with (fset:empty-map) id (second seek-result)))
                      (funcall mkerror :present-package-conflict
                               :additional
                               (list :alternative alternative
                                     :package-present-by :found
                                     :suggestion-attempt :unsuccessful)))))
               ((eql status :absent)
                (resolve-deps found-packages
                              (update-spec absent-specs id spec)
                              rclauses package-graph))
               ((eql status :present)
                (let* ((seek-result (seek-package (funcall repo id) vet)))
                  (if (eql (first seek-result) :successful)
                      (let* ((filtered (funcall cull (second seek-result))))
                        (try-candidates
                          (lambda (candidate)
                            (resolve-deps
                              (update-package found-packages id candidate)
                              absent-specs
                              (funcall concat-reqs rclauses
                                       (loop for clause in (pi-requirements candidate)
                                             collect (make-decorated-requirement
                                                       :clause clause
                                                       :parent candidate)))
                              (update-package-graph package-graph parent candidate)))
                          vet filtered))
                      (let* ((problem (getf (second seek-result) :problem))
                             (pkg-error (lambda (reason)
                                          (funcall mkerror reason
                                                   :additional
                                                   (list :alternative alternative
                                                         :package-id id)))))
                        (cond
                          ((eql problem :empty-query-results)
                           (funcall pkg-error :package-not-found))
                          ((eql problem :unsatisfactory-query-results)
                           (funcall pkg-error :package-rejected))
                          (t (funcall pkg-error :uncovered-case)))))))
               (t nil)))))
      (resolve-deps found-packages absent-specs clauses package-graph))))
                            (defun update-package-graph (graph parent child)
  "Add CHILD as a dependency of PARENT in the graph.
  Returns a new fset map; the original is unchanged."
  (fset:with graph parent
             (append (or (fset:lookup graph parent) '()) (list child))))

(defun update-spec (absent-specs id spec)
  "Add SPEC to the absent specs for ID.
  Returns a new fset map; the original is unchanged."
  (fset:with absent-specs id
             (append (or (fset:lookup absent-specs id) '()) (list spec))))

(defun update-package (found-packages id candidate)
  "Add CANDIDATE to the found packages for ID.
  Returns a new fset map; the original is unchanged."
  (fset:with found-packages id
             (append (or (fset:lookup found-packages id) '()) (list candidate))))

;;; ─── Install graph ──────────────────────────────────────────────────────────

(defun make-install-graph (package-graph handle)
  "Create an install graph from the package resolution graph.

  HANDLE is a function that maps a package to a string identifier."
  (fset:reduce (lambda (result key)
                 (let ((val (fset:lookup package-graph key)))
                   (if (and (not (eq key :root)) (pi-id key))
                       (let* ((handle-val (funcall handle key))
                              (entry (list :name (pi-id key)
                                           :version (pi-version key)
                                           :location (pi-location key)
                                           :dependees
                                           (loop for child in val
                                                 collect (funcall handle child)))))
                         (fset:with result handle-val entry))
                       result)))
               (fset:domain package-graph)
               :initial-value (fset:empty-map)))

;;; ─── Main resolver entry point ──────────────────────────────────────────────

(defun resolve-dependencies-deluxe
       (requirements query
        &key (present-packages (fset:empty-map))
        (conflicts (fset:empty-map))
        (strategy :thorough)
        (conflict-strat :exclusive)
        (compare nil)
        (search-strat :breadth-first)
        (allow-alternatives t)
        (list-strat :as-set))
  "Resolve dependencies with full control over strategy options.

  Returns a property list with :result key (either :successful or
  :unsuccessful) and other keys depending on the outcome."
  (let* ((concat-reqs (if (eql search-strat :depth-first)
                          (lambda (a b) (append b a))
                          (lambda (a b) (append a b))))
         (safe-spec-call (make-spec-call compare))
         (cull (case strategy
                 (:thorough #'identity)
                 (:fast (lambda (candidates)
                          (if (null candidates)
                              candidates
                              (list (car candidates)))))
                 (t (error "Invalid strategy: ~a" strategy))))
         (cull-alternatives (if allow-alternatives
                                #'identity
                                (lambda (alts)
                                  (if (null alts)
                                      alts
                                      (list (car alts))))))
         (resolve-deps (make-resolve-deps
                         conflict-strat concat-reqs safe-spec-call
                         cull cull-alternatives))
         (decorated-reqs (loop for clause in requirements
                               collect (make-decorated-requirement
                                         :clause clause :parent :root)))
         (result (funcall resolve-deps query
                          present-packages
                          (fset:empty-map)
                          conflicts
                          decorated-reqs
                          (fset:empty-map))))
    (if (eql (first result) :successful)
        (let ((graph (second result))
              (exclude-set (fset:empty-set)))
          ;; Build exclude set from present packages
          (fset:do-map (id pkgs present-packages)
                       (declare (ignore id))
                       (loop for pkg in pkgs
                             do (setf exclude-set (fset:with exclude-set pkg))))
          (list :result :successful
                :packages
                (if (eql list-strat :as-set)
                    (remove-duplicates
                      (list-packages graph
                                     :list-strat :lazy
                                     :exclude exclude-set)
                      :test #'equal)
                    (list-packages graph
                                   :list-strat list-strat
                                   :exclude exclude-set))
                :install-graph
                (make-install-graph
                  graph
                  (if (eql conflict-strat :inclusive)
                      (lambda (pkg)
                        (format nil "~a@~a" (pi-id pkg) (pi-version pkg)))
                      'pi-id))))
        (list* :result (first result)
               (second result)))))

(defun resolve-dependencies (requirements query &rest options &key
                             &allow-other-keys)
  "Simple resolver entry point.

  Returns (list :successful packages) or (list :unsuccessful problems)."
  (let ((r (apply #'resolve-dependencies-deluxe requirements query options)))
    (if (eql (getf r :result) :successful)
        (list :successful (getf r :packages))
        (list (getf r :result)
              (loop for key in '(:result)
                    do (remf r key)
                    finally (return r))))))
