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

(defun s= (a b)
  "equivalence between two logical formula"
  (match* (a b)
    (((list* op1 rest1)
      (list* op2 rest2))
     (and (eq op1 op2)
          (set-equal rest1 rest2 :test 's=)))
    (((symbol)
      (symbol))
     (eq a b))))

(def-suite :cl-sat)
(in-suite :cl-sat)

;; run test with (run! test-name) 

(test symbolicate-form

  (is (s= '(and (or a (not b) c) d)
          (symbolicate-form
           '(and (or a !b c) d))))

  (is (s= '(and (or a cl-sat.variables::VAR1 c) d)
          (symbolicate-form
           '(and (or a 1 c) d))))
  
  (is (s= '(and (or a (not cl-sat.variables::VAR1) c) d)
          (symbolicate-form
           '(and (or a -1 c) d))))

  (signals error (symbolicate-form '!!!!!))
  
  (is (s= 'a       (symbolicate-form 'a)))
  (is (s= '(not a) (symbolicate-form '!a)))
  (is (s= 'a       (symbolicate-form '!!a)))
  (is (s= '(not a) (symbolicate-form '!!!a)))
  
  ;; https://www.satcompetition.org/2009/format-benchmarks2009.html
  ;; 0 is not allowed as a literal
  (signals error (symbolicate-form 0)))



(test to-nnf

  (is (s= (to-nnf
           '(not (or a b c)))
          '(and (not a) (not b) (not c))))

  (is (s= (to-nnf
           '(not (and a b c)))
          '(or (not a) (not b) (not c)))))



(test to-cnf-naive
  
  (is (s= '(and a b c)
          (to-cnf-naive '(and a b c))))

  (is (s= '(or a b c)
          (to-cnf-naive '(or a b c))))

  (is (s= '(and (or a (not b) c) d)
          (to-cnf-naive
           (to-nnf
            (symbolicate-form
             '(and (or a !b c) d))))))

  (is (s= 'a (to-cnf-naive '(and (and (and a))))))


  (is (s= '(or (not a) (not b))
          (to-cnf-naive
           (to-nnf '(not (and a b))))))

  (is (s= '(and (not a) (not b))
          (to-cnf-naive
           (to-nnf '(not (or a b))))))

  (is (s= '(and (not a) (not b))
          (to-cnf-naive
           (to-nnf '(not (or a b))))))

  (is (s= '(and (not a) (not b))
          (to-cnf-naive
           (to-nnf '(not (or a b))))))

  (is (s= '(and p (or q a r) (or q b r) s)
          (to-cnf-naive
           '(and p (or q (and a b) r) s))))

  (is (s= '(and (or a c) (or b c))
          (to-cnf-naive
           '(or (and a b) c))))

  (is (s= '(and (or a c) (or b c) (or a d) (or b d))
          (to-cnf-naive
           '(or (and a b) (and c d)))))

  (is (s= '(and
            (or a c e) (or b c e) (or a d e) (or b d e)
            (or a c f) (or b c f) (or a d f) (or b d f))
          (to-cnf-naive
           '(or (and a b) (and c d) (and e f)))))

  (is (s= '(and
             (or a c) (or a d) (or b c) (or b d)
             (or e g) (or e h) (or f g) (or f h))
          (to-cnf-naive
           '(and
             (or (and a b) (and c d))
             (or (and e f) (and g h))))))
  )

(test to-cnf-tseytin
  (is (s= '(AND
            (OR CL-SAT.AUX-VARIABLES::AUX0 (NOT CL-SAT.AUX-VARIABLES::AUX1))
            (OR CL-SAT.AUX-VARIABLES::AUX0 (NOT CL-SAT.AUX-VARIABLES::AUX2))
            (OR CL-SAT.AUX-VARIABLES::AUX0 (NOT CL-SAT.AUX-VARIABLES::AUX3))
            (OR CL-SAT.AUX-VARIABLES::AUX0 (NOT CL-SAT.AUX-VARIABLES::AUX4))
            (OR CL-SAT.AUX-VARIABLES::AUX0 (NOT CL-SAT.AUX-VARIABLES::AUX5))
            (OR
             (NOT CL-SAT.AUX-VARIABLES::AUX0)
             CL-SAT.AUX-VARIABLES::AUX1
             CL-SAT.AUX-VARIABLES::AUX2
             CL-SAT.AUX-VARIABLES::AUX3
             CL-SAT.AUX-VARIABLES::AUX4
             CL-SAT.AUX-VARIABLES::AUX5)
            (OR CL-SAT.AUX-VARIABLES::AUX5 (NOT I) (NOT J))
            (OR (NOT CL-SAT.AUX-VARIABLES::AUX5) I)
            (OR (NOT CL-SAT.AUX-VARIABLES::AUX5) J)
            (OR CL-SAT.AUX-VARIABLES::AUX4 (NOT G) (NOT H))
            (OR (NOT CL-SAT.AUX-VARIABLES::AUX4) G)
            (OR (NOT CL-SAT.AUX-VARIABLES::AUX4) H)
            (OR CL-SAT.AUX-VARIABLES::AUX3 (NOT E) (NOT F))
            (OR (NOT CL-SAT.AUX-VARIABLES::AUX3) E)
            (OR (NOT CL-SAT.AUX-VARIABLES::AUX3) F)
            (OR CL-SAT.AUX-VARIABLES::AUX2 (NOT C) (NOT D))
            (OR (NOT CL-SAT.AUX-VARIABLES::AUX2) C)
            (OR (NOT CL-SAT.AUX-VARIABLES::AUX2) D)
            (OR CL-SAT.AUX-VARIABLES::AUX1 (NOT A) (NOT B))
            (OR (NOT CL-SAT.AUX-VARIABLES::AUX1) A)
            (OR (NOT CL-SAT.AUX-VARIABLES::AUX1) B))
          (to-cnf-tseytin
           ;; if you do this with the naive method, it becomes a pageful of conjunctions
           '(or (and a b) (and c d) (and e f) (and g h) (and i j))))))

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



