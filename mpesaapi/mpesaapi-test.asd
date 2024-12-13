(defsystem "mpesaapi-test"
  :defsystem-depends-on ("prove-asdf")
  :author "giture"
  :license ""
  :depends-on ("mpesaapi"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "mpesaapi"))))
  :description "Test system for mpesaapi"
  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
