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

;; Output Format
;; We ask for the solvers to DISPLAY messages on the standard output that will be used to check the results and to RETURN EXIT CODE to be handled by the SAT-Ex system. The output format is partly inspired by the previously defined DIMACS output specification and may be used to manually check some results.
;; Messages
;; There is no specific order in the solvers output lines. However, all line, according to its first char, must belong to one of the three following categories:
;; 
;;     comments (any information that authors want to emphasize, such like #backtracks, #flips,... or internal cpu-time), beginning with the two chars "c "
;;     solution (satisfiable or not). Only one line of this type is allowed. This line must begin with the two chars " s " and must be one of the following ones:
;;         s SATISFIABLE
;;         s UNSATISFIABLE
;;         s UNKNOWN
;;     values of a solution (if any), beginning with the two chars "v " (to be precised in the following).
;; 
;; When a solver answers UNKNOWN, it is charged with the maximum allowed time SATTIMEOUT.
;; 
;; Technically, the two first chars are important and must be strictly respected: scripts will use traditional grep commands to parse results (and at least to partition standard output).
;; 
;; If the solver does not display a solution line (or if the solution line is not valid), then UNKNOWN will be assumed.
;; Providing a model
;; If the solver outputs SATISFIABLE, it should provide a model (or an implicant) of the instance that will be used to check the correctness of the answer. I.e., it must provide a 0-terminated sequence of distinct non-contradictory literals that makes every clause of the input formula true. It is NOT necessary to list the literals corresponding to all variables if a smaller amount of literals suffices. The order of the literals does not matter. Arbitrary white space characters, including ordinary white spaces, newline and tabulation characters, are allowed between the literals, as long as each line containing the literals is a values line, i.e. it begins with the two chars "v ".
;; 
;; If the solver cannot provide such a certificate for satisfiable instances, then the author(s) are asked to contact Laurent Simon directly; then the decision concerning the solver will be made (e.g., running the solver hors concours).
;; 
;; Note that we do not require a proof for unsatisfiability. The values lines should only appear with SATISFIABLE instance.
;; 
;; For instance, the following outputs are valid for the instances given in example:
;; 
;; mycomputer:~$ ./mysolver myinstance-sat
;; c mysolver 6.55957 starting with SATTIMEOUT fixed to 1000s
;; c Trying to guess a solution...
;; s SATISFIABLE
;; v -3 4
;; v -6 18 21
;; v 1 -7 0
;; c Done (mycputime is 234s).

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

;; https://dwheeler.com/essays/minisat-user-guide.html
;; SAT
;; 1 2 -3 4 5 0

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


;; todo: RUP proof in SAT competition 2009 https://www.satcompetition.org/2009/
