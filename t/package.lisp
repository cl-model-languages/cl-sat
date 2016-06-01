#|
  This file is a part of cl-sat project.
  Copyright (c) 2016 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage :cl-sat.test
  (:use :cl
        :cl-sat
        :fiveam
        :trivia :alexandria :iterate))
(in-package :cl-sat.test)



(def-suite :cl-sat)
(in-suite :cl-sat)

;; run test with (run! test-name) 

(test form-cnf
      (is (equal (form-cnf '(and a b c))
                 '(and (or a) (or b) (or c)))
          "flatten")

      (is (equal (form-cnf '(or a b c))
                 '(and (or a b c)))
          "flatten")

      (is (equal (form-cnf
                  '(and (or a !b c) d))
                 '(and
                   (or a (not b) c)
                   (or d)))
          "negate")

      (is (equal (form-cnf '(and (and (and a))))
                 '(and (or a)))
          "flatten")


      (is (equal (form-cnf '(not (and a b)))
                 '(and
                   (or (not a) (not b))))
          "dnf")

      (is (equal (form-cnf '(not (or a b)))
                 '(and
                   (or (not a))
                   (or (not b))))
          "dnf"))


(test instantiate
  (finishes (make-instance 'sat-instance :form '(and a b c)))
  (finishes (make-instance 'sat-instance :form '(or a b c)))
  (finishes (make-instance 'sat-instance :form '(and (or a !b c) d)))
  (finishes (make-instance 'sat-instance :form '(and (and (and a)))))
  (finishes (make-instance 'sat-instance :form '(not (and a b))))
  (finishes (make-instance 'sat-instance :form '(not (or a b)))))

(test print-cnf
  (fresh-line)
  (finishes (print-cnf (make-instance 'sat-instance :form '(and a b c))))
  (finishes (print-cnf (make-instance 'sat-instance :form '(or a b c))))
  (finishes (print-cnf (make-instance 'sat-instance :form '(and (or a !b c) d))))
  (finishes (print-cnf (make-instance 'sat-instance :form '(and (and (and a))))))
  (finishes (print-cnf (make-instance 'sat-instance :form '(not (and a b)))))
  (finishes (print-cnf (make-instance 'sat-instance :form '(not (or a b))))))



