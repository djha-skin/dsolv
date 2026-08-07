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
               "chipz"
               "babel"
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
                 (:file "data-spec")
                 (:file "core")
                 (:file "auxiliary-funcs")
                 (:file "list-packages")
                 (:file "interesting-cases")
                 (:file "conflict-strat")
                 (:file "disable-alternatives")
                 (:file "performance")
                 (:file "search-strat")
                 (:file "unsuccessful")
                 (:file "version-suggestions")
                 (:file "apt")
                 (:file "git")
                 (:file "main")
                 (:file "subproc"))))
  :description "Test system for dsolv."
  :perform (asdf:test-op (op c)
                         (uiop:symbol-call :parachute :test
                                           (list
                                             '#:com.djhaskin.dsolv/tests/repo-aggregation
                                             '#:com.djhaskin.dsolv/tests/util
                                             '#:com.djhaskin.dsolv/tests/string-to-req
                                             '#:com.djhaskin.dsolv/tests/data-spec
                                             '#:com.djhaskin.dsolv/tests/core
                                             '#:com.djhaskin.dsolv/tests/auxiliary-funcs
                                             '#:com.djhaskin.dsolv/tests/list-packages
                                             '#:com.djhaskin.dsolv/tests/interesting-cases
                                             '#:com.djhaskin.dsolv/tests/conflict-strat
                                             '#:com.djhaskin.dsolv/tests/disable-alternatives
                                             '#:com.djhaskin.dsolv/tests/performance
                                             '#:com.djhaskin.dsolv/tests/search-strat
                                             '#:com.djhaskin.dsolv/tests/unsuccessful
                                             '#:com.djhaskin.dsolv/tests/version-suggestions
                                             '#:com.djhaskin.dsolv/tests/apt
                                             '#:com.djhaskin.dsolv/tests/git
                                             '#:com.djhaskin.dsolv/tests/main
                                             '#:com.djhaskin.dsolv/tests/subproc))))
