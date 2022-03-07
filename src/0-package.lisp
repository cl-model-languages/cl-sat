#|
  This file is a part of cl-sat project.
  Copyright (c) 2016 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-sat
  (:nicknames :sat)
  (:use :cl :trivia :alexandria :iterate)
  (:export
   #:solve
   #:sat-instance
   #:print-cnf
   #:with-temp
   #:*instance*
   #:variables
   #:to-nnf
   #:to-cnf
   #:to-cnf-naive
   #:to-cnf-tseytin
   #:symbolicate-form
   #:*verbosity*
   #:parse-dimacs-output
   #:imply
   #:=>
   #:iff
   #:<=>
   #:xor
   #:var
   #:aux
   #:simplify-nnf
   #:to-anf
   #:expand-extensions
   #:parse-assignments))
(in-package :cl-sat)




