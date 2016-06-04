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
   #:form-cnf
   #:print-cnf
   #:with-temp
   #:*instance*
   #:sat-instance-variables))
(in-package :cl-sat)




