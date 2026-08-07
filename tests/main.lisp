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
