#|
  This file is a part of cl-sat project.
  Copyright (c) 2016 Masataro Asai (guicho2.71828@gmail.com)
|#


(in-package :cl-user)
(defpackage cl-sat.test-asd
  (:use :cl :asdf))
(in-package :cl-sat.test-asd)


(defsystem cl-sat.test
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :description "Test system of cl-sat"
  :license "LLGPL"
  :depends-on (:cl-sat
               :fiveam)
  :components ((:module "t"
                :components
                ((:file "package"))))
  :perform (test-op :after (op c) (eval (read-from-string "(every #'fiveam::TEST-PASSED-P (5am:run! :cl-sat))"))
))
