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

(test symbolicate-form

  (is (equal (symbolicate-form
              '(and (or a !b c) d))
             '(and (or a (not b) c) d)))

  (is (equal (symbolicate-form
              '(and (or a 1 c) d))
             '(and (or a cl-sat.namespace::VAR1 c) d)))
  
  (is (equal (symbolicate-form
              '(and (or a -1 c) d))
             '(and (or a (not cl-sat.namespace::VAR1) c) d)))

  (signals error (symbolicate-form '!!!!!))
  
  ;; https://www.satcompetition.org/2009/format-benchmarks2009.html
  ;; 0 is not allowed as a literal
  (signals error (symbolicate-form 0)))

(test to-nnf

  (is (equal (to-nnf
              '(not (or a b c)))
             '(and (not a) (not b) (not c))))

  (is (equal (to-nnf
              '(not (and a b c)))
             '(or (not a) (not b) (not c)))))

(test to-cnf
  
  (is (equal (to-cnf '(and a b c))
             '(and (or a) (or b) (or c)))
      "flatten")

  (is (equal (to-cnf '(or a b c))
             '(and (or a b c)))
      "flatten")

  (is (equal (to-cnf
              '(and (or a !b c) d))
             '(and
               (or a (not b) c)
               (or d)))
      "negate")

  (is (equal (to-cnf 'a) '(and (or a)))
      "negate")
  (is (equal (to-cnf '!a) '(and (or (not a))))
      "negate")
  (is (equal (to-cnf '!!a) '(and (or a)))
      "negate")
  (is (equal (to-cnf '!!!a) '(and (or (not a))))
      "negate")


  (is (equal (to-cnf '(and (and (and a))))
             '(and (or a)))
      "flatten")


  (is (equal (to-cnf '(not (and a b)))
             '(and
               (or (not a) (not b))))
      "dnf")

  (is (equal (to-cnf '(not (or a b)))
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



