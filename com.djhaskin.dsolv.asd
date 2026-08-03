(defsystem "com.djhaskin.dsolv"
  :version "0.1.0"
  :author "Daniel Jay Haskin"
  :license "MIT"
  :depends-on (
               "com.djhaskin.cliff"
               "com.djhaskin.nrdl"
               "alexandria"
               "cl-ppcre"
               "fset"
               "dexador"
               "quri"
               "com.djhaskin.svers"
               )
  :components ((:module "src"
                :components
                ((:file "util")
                 (:file "resolver")
                 (:module "pkgsys"
                  :components
                  ((:file "core")
                   (:file "apt")
                   (:file "git")
                   (:file "subproc")))
                 (:file "main"))))
  :description "Generic dependency resolver and CLI tool."
  :in-order-to ((test-op (test-op "com.djhaskin.dsolv/tests"))))

(defsystem "com.djhaskin.dsolv/tests"
  :version "0.1.0"
  :author "Daniel Jay Haskin"
  :license "MIT"
  :depends-on (
               "com.djhaskin.dsolv"
               "parachute"
               )
  :components ((:module "tests"
                :components
                ((:file "repo-aggregation")
                 (:file "util")
                 (:file "string-to-req")
               (:file "data-spec"))))
  :description "Test system for dsolv."
  :perform (asdf:test-op (op c)
                         (uiop:symbol-call :parachute :test
                                           (list
                                             '#:com.djhaskin.dsolv/tests/repo-aggregation
                                             '#:com.djhaskin.dsolv/tests/util
                                             '#:com.djhaskin.dsolv/tests/string-to-req
                                            '#:com.djhaskin.dsolv/tests/data-spec))))
