(in-package :cl-sat)
;; http://people.sc.fsu.edu/~jburkardt/data/cnf/cnf.html
;; ( x(1) OR ( NOT x(3) ) )
;; AND
;; ( x(2) OR x(3) OR ( NOT x(1) ) ).

;; c  simple_v3_c2.cnf      <<< comments
;; c
;; p cnf 3 2                <<< problem spec, cnf type, #vars, #clauses
;; 1 -3 0
;; 2 3 -1 0


(defvar *verbosity* 0)
(declaim (type (integer 0 3) *verbosity*))

(defun print-cnf (instance &optional (stream *standard-output*) (*verbosity* *verbosity*))
  (ematch instance
    ((sat-instance cnf variables)
     (when (<= 1 *verbosity*)
       (pprint-logical-block (stream nil :per-line-prefix "c ")
         (when (<= 2 *verbosity*)
           (format stream "~&~a" cnf))
         (iter (for i from 1)
               (for v in-vector variables)
               (format stream "~&Variable ~a : ~a" i v))))

     (match cnf
       ((or (list* 'and clauses)
            (<> clauses (list cnf)))
        (format stream "~&p cnf ~A ~A" (length variables) (length clauses))

        (iter (for c in clauses)
              (ematch c
                ((or (list* 'or terms)
                     (<> terms (list c)))
                 (when (<= 3 *verbosity*)
                   (format stream "~&c ~a" c))
                 (format stream "~&~{~a ~}0"
                         (iter (for term in terms)
                               (collect
                                   (ematch term
                                     ((list 'not atom)
                                      (- (1+ (position atom variables))))
                                     (atom
                                      (1+ (position atom variables))))))))))
        (fresh-line stream))))))



;; https://www.satcompetition.org/2004/format-solvers2004.html

(defun parse-dimacs-output (file instance)
  (handler-case
      (parse-true-dimacs-output file instance)
    (match-error ()
      (format *error-output* "~&Non-conforming output format! Assuming it is a list of variables...~%")
      (handler-case
          (parse-assignments file instance)
        (type-error ()
          (format *error-output* "~&Not a list of assignments! Trying to ignore SAT or non-numbers...~%")
          (parse-assignments-ignoring-symbols file instance))))))

(defun parse-true-dimacs-output (file instance)
  (iter (for line in-file file using #'read-line)

        (with sure = nil)
        (with satisfiable = nil)
        (with assignments = (make-array (length (sat-instance-variables instance))
                                        :element-type '(integer 0 2)
                                        :initial-element 2))

        ;; hack for common bugs in glucose-based solvers
        (when (equal line "WARNING: for repeatability, setting FPU to use double precision")
          (next-iteration))

        (ematch line
          ((string* #\c _)
           ;; do nothing
           )
          ((string* #\v _)
           (with-input-from-string (s (subseq line 2))
             (iter (for v in-stream s)
                   (until (zerop v))
                   (setf (aref assignments (1- (abs v)))
                         (if (plusp v) 1 0)))))
          ("s SATISFIABLE"
           (setf sure t satisfiable t))
          ("s UNSATISFIABLE"
           (setf sure t satisfiable nil))
          ("s UNKNOWN"
           (setf sure nil satisfiable nil)))

        (finally
         (iter (for a in-vector assignments with-index i)
               (for v = (aref (sat-instance-variables instance) i))
               (case a
                 (1 (when (not (eq (find-package :cl-sat.aux-variables)
                                   (symbol-package v)))
                      (collect v into trues)))
                 (2 (when (not (eq (find-package :cl-sat.aux-variables)
                                   (symbol-package v)))
                      (collect v into dont-care))))
               (finally
                (return-from parse-true-dimacs-output
                  (values trues satisfiable sure dont-care)))))))

(defun parse-assignments (file instance)
  (iter (for assignment in-file file)
        (check-type assignment integer)
        (with variables = (sat-instance-variables instance))
        (when (plusp assignment)
          (let ((variable (aref variables (1- assignment))))
            (when (not (eq (find-package :cl-sat.aux-variables)
                           (symbol-package variable)))
              (collect variable into trues))))
        (finally
         (return
           (values trues t t)))))

(defun parse-assignments-ignoring-symbols (file instance)
  (iter (for assignment in-file file)
        (when (not (integerp assignment))
          (next-iteration))
        (with variables = (sat-instance-variables instance))
        (when (plusp assignment)
          (let ((variable (aref variables (1- assignment))))
            (when (not (eq (find-package :cl-sat.aux-variables)
                           (symbol-package variable)))
              (collect variable into trues))))
        (finally
         (return
           (values trues t t)))))
