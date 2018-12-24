#|

Parses a s-exp and turn it into a logical form that is compatible to SAT solvers

Notes:

NNF: Negation Normal Form. a tree of ANDs and ORs ending with either positive / negative literals. There are no NOTs applied to ANDs and ORs

CNF: Conjunctive Normal Form. ANDs of ORs of positive / negative literals. Also known as "product of sums" forms.

DNF: Disjunctive Normal Form. (Not Duke Nukem Forever) ORs of ANDs of positive / negative literals.

ANF: Algebraic Normal Form. XORs of ANDs of positive literals. No NOTs. This form is canonical -- there is only one form up to permutations.

|#

(in-package :cl-sat)
;; allow both (not symbol) and !symbol
;; allow numbers (as in cnf)

(defun var (number)
  "intern a number to a symbol"
  (intern (format nil "VAR~a" number)))

(defun symbolicate-form (tree)
  "
This function is the first step of converting the input into a normal form.
It normalizes the input tree containing numbers and !-negated vars into AND-OR-NOT tree of symbols.
Note that it does not guarantee to return any type of normal forms (e.g. NNF,CNF,DNF,ANF).
"
  (ematch tree
    ((symbol name)
     (let ((pos (position-if (lambda (c) (char/= c #\!)) name)))
       (cond
         ((null pos)
          ;; all characters are !, 
          (error "Found an invalid symbol ~a whose name consists of ! only." tree))
         ((evenp pos)
          ;; positive literal. !s in the middle of the names are not considered
          ;; here, we consider only those at the beginning of literal name.
          tree)
         (t
          `(not ,(intern (subseq name pos) (symbol-package tree)))))))
    ((< 0)
     `(not ,(var (- tree))))
    ((> 0)
     (var tree))
    ((list* head rest)
     (list* head (mapcar #'symbolicate-form rest)))))

(defun composite-negate-p (form)
  "Returns true if a form is a negation of a tree."
  (ematch form
    ((list 'not (symbol)) nil)
    ((list 'not (list* _)) t)))

(defun negate (form)
  "Simple negation not involving De-Morgan's law."
  (ematch form
    ((list 'not form)
     form)
    (_
     `(not ,form))))

(defun %form->cnf (form)
  ;; (and (or ...) (or ...))
  (match form
    ((list* 'and rest)
     `(and ,@(mapcar #'%form->cnf rest)))
    ((list* 'or rest)
     (assert (notany (lambda-match ((list* 'and _) t) ((list* 'or _) t)) rest))
     form)
    ((list 'not (list* 'or rest))
     `(and ,@(mapcar (compose #'%form->cnf #'negate) rest)))
    ((list 'not (list* 'and rest))
     (%form->cnf
      `(or ,@(mapcar #'negate rest))))
    (_
     form)))

(defun flatten-cnf (form)
  (let (acc)
    (labels ((rec (form)
               (match form
                 ((list* 'and rest)
                  (mapcar #'rec rest))
                 ((list* 'or _)
                  (push form acc))
                 (_
                  (push `(or ,form) acc)))))
      (rec form)
      `(and ,@(nreverse acc)))))

(defun form-cnf (form)
  (flatten-cnf
   (%form->cnf
    (symbolicate-form form))))
