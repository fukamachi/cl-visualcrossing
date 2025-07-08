(defsystem "visualcrossing"
  :description "A Common Lisp library for Visual Crossing's Weather API"
  :author "Eitaro Fukamachi"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("dexador"
               "yason"
               "str"
               "cl-ppcre")
  :components ((:module "src"
                :components
                ((:file "errors")
                 (:file "data")
                 (:file "params")
                 (:file "client" :depends-on ("errors"))
                 (:file "main" :depends-on ("errors" "data" "params" "client")))))
  :in-order-to ((test-op (test-op "visualcrossing/tests"))))

(defsystem "visualcrossing/tests"
  :description "Test system for visualcrossing"
  :depends-on ("visualcrossing"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "errors")
                 (:file "data")
                 (:file "params")
                 (:file "client")
                 (:file "main"))))
  :perform (test-op (op c) (symbol-call :rove :run c)))
