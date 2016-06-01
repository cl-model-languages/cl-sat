#|
  This file is a part of cl-sat project.
  Copyright (c) 2016 Masataro Asai (guicho2.71828@gmail.com)
|#

#|
  Common Lisp API to Boolean SAT Solvers

  Author: Masataro Asai (guicho2.71828@gmail.com)
|#



(in-package :cl-user)
(defpackage cl-sat-asd
  (:use :cl :asdf))
(in-package :cl-sat-asd)


(defsystem cl-sat
  :version "0.1"
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :license "LLGPL"
  :depends-on (:trivia :alexandria :iterate)
  :components ((:module "src"
                :components
                ((:file "package"))))
  :description "Common Lisp API to Boolean SAT Solvers"
  :in-order-to ((test-op (test-op :cl-sat.test))))
