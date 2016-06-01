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

(defun print-cnf (instance &optional (stream *standard-output*))
  (ematch instance
    ((sat-instance :cnf (list* 'and clauses)
                   variables)
     (pprint-logical-block (stream nil :per-line-prefix "c ")
       (iter (for i from 0)
             (for v in variables)
             (format stream "~&Variable ~a : ~a" i v)))
     (format stream "~&p cnf ~A ~A" (length variables) (length clauses))
     (iter (for c in clauses)
           (ematch c
             ((list* 'or terms)
              (format stream "~&c ~a" c)
              (format stream "~&~{~a ~}0"
                      (iter (for term in terms)
                            (collect
                                (ematch term
                                  ((list 'not atom)
                                   (- (1+ (position atom variables))))
                                  (atom
                                   (1+ (position atom variables))))))))))
     (fresh-line stream))))



