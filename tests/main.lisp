;;;; tests/main.lisp
;;;;
;;;; Integration tests for dsolv CLI subcommand handlers.

(defpackage #:com.djhaskin.dsolv/tests/main
  (:use #:cl)
  (:import-from #:com.djhaskin.dsolv
    #:main)
  (:import-from #:com.djhaskin.dsolv/resolver
    #:*package-systems*)
  (:import-from #:com.djhaskin.dsolv/pkgsys/git
    #:make-query)
  (:import-from #:parachute
    #:define-test
    #:is
    #:true))

(in-package #:com.djhaskin.dsolv/tests/main)

(defun run-resolve-locations (&rest arguments)
  "Run the resolve-locations CLI handler and capture its status and output."
  (let ((stdout (make-string-output-stream))
        (stderr (make-string-output-stream)))
    (let ((*standard-output* stdout)
          (*error-output* stderr))
      (list (apply #'main "resolve-locations" arguments)
            (get-output-stream-string stdout)
            (get-output-stream-string stderr)))))

(defun run-display-config (&rest arguments)
  "Run the display-config CLI handler and capture its status and output."
  (let ((stdout (make-string-output-stream))
        (stderr (make-string-output-stream)))
    (let ((*standard-output* stdout)
          (*error-output* stderr))
      (list (apply #'main (append arguments (list "display-config")))
            (get-output-stream-string stdout)
            (get-output-stream-string stderr)))))

(define-test git-package-system-registers-the-exported-constructor
             :parent nil
             "Git registration refers to the exported Git constructor."
             (is eq #'make-query
                 (getf (gethash "git" *package-systems*) :query-constructor)))

(define-test git-package-system-requires-clone-folder
             :parent nil
             "Git resolution rejects an invocation without the required clone folder."
             (destructuring-bind (status stdout stderr)
                                 (run-resolve-locations
                                  "--package-system" "git"
                                  "--requirement" "example")
               (is = 1 status)
               (is string= "" stdout)
               (true (search "Missing required argument: CLONE-FOLDER" stderr))))

(define-test git-package-system-reaches-its-constructor
             :parent nil
             "Git resolution accepts clone-folder and reaches the NIL-returning stub."
             (destructuring-bind (status stdout stderr)
                                 (run-resolve-locations
                                  "--package-system" "git"
                                  "--clone-folder" "/tmp/dsolv-git-test"
                                  "--requirement" "example")
               (is = 70 status)
               (is string= "" stdout)
               (true (search "error-cell-name \"NIL\"" stderr))))

(defun write-option-pack-config (path pack-name)
  "Write an NRDL config file at PATH setting :option-packs to PACK-NAME."
  (with-open-file (stream path :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (format stream "~%{~%    :option-packs [\"~a\"]~%}~%" pack-name)))

(define-test option-pack-affects-display-config
             :parent nil
             "A multi-version-mode option pack sets the inclusive conflict strategy."
             (destructuring-bind (status stdout stderr)
                                 (run-display-config "-k" "multi-version-mode")
               (is = 0 status)
               (true (search ":CONFLICT-STRAT: \"inclusive\"" stdout))))

(define-test config-file-option-pack-affects-display-config
             :parent nil
             "An option pack from a config file sets the prioritized conflict strategy."
             (let ((config-path "/tmp/dsolv-test-option-packs.nrdl"))
               (write-option-pack-config config-path "firstfound-version-mode")
               (destructuring-bind (status stdout stderr)
                                   (run-display-config "-c" config-path)
                 (is = 0 status)
                 (true (search ":CONFLICT-STRAT: \"prioritized\"" stdout)))))

(define-test cli-option-pack-overrides-config-file
             :parent nil
             "A command-line option pack takes precedence over a config file's."
             (let ((config-path "/tmp/dsolv-test-option-packs-precedence.nrdl"))
               (write-option-pack-config config-path "firstfound-version-mode")
               (destructuring-bind (status stdout stderr)
                                   (run-display-config "-c" config-path "-k" "multi-version-mode")
                 (is = 0 status)
                 (true (search ":CONFLICT-STRAT: \"inclusive\"" stdout)))))

(defun write-later-config-files (dir)
  "Write NRDL and JSON config files in DIR where the JSON file's
   requirements list overrides the NRDL file's (later-file-wins)."
  (let ((nrdl-path (merge-pathnames "config.nrdl" dir))
        (json-path (merge-pathnames "config.json" dir)))
    (with-open-file (stream nrdl-path :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (format stream "~%{~%    :version \"2.3.0\"~%    :requirements [\"c\"]~%}~%"))
    (with-open-file (stream json-path :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (format stream "~%{~%    \"requirements\": [],~%    \"id\": \"b\",~%    \"location\": \"https://example.com/repo/b-2.3.0.zip\"~%}~%"))
    (values nrdl-path json-path)))

(defun run-generate-card (&rest arguments)
  "Run the generate-card CLI handler and capture its status and output."
  (let ((stdout (make-string-output-stream))
        (stderr (make-string-output-stream)))
    (let ((*standard-output* stdout)
          (*error-output* stderr))
      (list (apply #'main "generate-card" arguments)
            (get-output-stream-string stdout)
            (get-output-stream-string stderr)))))

(define-test later-json-config-file-overrides-earlier-nrdl
             :parent nil
             "A later JSON config file's requirements override an earlier NRDL file's."
             (let ((dir (uiop:ensure-directory-pathname "/tmp/dsolv-test-config-merge/"))
                   (card-path "/tmp/dsolv-test-config-merge/b.dscard"))
               (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore)
               (ensure-directories-exist dir)
               (multiple-value-bind (nrdl-path json-path)
                                    (write-later-config-files dir)
                 (destructuring-bind (status stdout stderr)
                                     (run-generate-card
                                      "-c" (namestring nrdl-path)
                                      "-j" (namestring json-path)
                                      "-C" card-path)
                   (is = 0 status)
                   (is string= "" stdout)
                   (is string= "" stderr)
                   (let ((card (alexandria:read-file-into-string card-path)))
                     (true (search "id \"b\"" card))
                     (true (search "version \"2.3.0\"" card))
                     (is equal nil (search "requirements" card)))))))

(define-test resolve-locations-json-includes-install-graph
             :parent nil
             "resolve-locations JSON output includes the install graph."
             (let ((index-path
                     (merge-pathnames
                       #P"test/resources/data/install-graph/index.dsrepo"
                       (asdf:system-source-directory "com.djhaskin.dsolv"))))
               (destructuring-bind (status stdout stderr)
                                   (run-resolve-locations
                                    "-R" (namestring index-path)
                                    "-r" "b"
                                    "-p" "e==1.8.0"
                                    "-o" "json")
                 (is = 0 status)
                 (true (search "\"install-graph\"" stdout))
                 (true (search "\"dependees\"" stdout))
                 (true (search "already present" stdout)))))
