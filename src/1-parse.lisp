#|

Parses a s-exp and turn it into a logical form that is compatible to SAT solvers

Notes:

NNF: Negation Normal Form. a tree of ANDs and ORs ending with either positive / negative literals. There are no NOTs applied to ANDs and ORs

CNF: Conjunctive Normal Form. ANDs of ORs of positive / negative literals. Also known as "product of sums" forms.

DNF: Disjunctive Normal Form. (Not Duke Nukem Forever) ORs of ANDs of positive / negative literals.

ANF: Algebraic Normal Form. XORs of ANDs of positive literals. No NOTs. This form is canonical -- there is only one form up to permutations.



We first convert a form into a SYMBOLIC FORM:

+ convert every numbers into a symbol in CL-SAT.VARIABLES PACKAGE,
+ convert every negative numbers into (NOT VAR) form, 
+ convert every !-negated symbols into (NOT VAR) form.

We next convert it into an NNF, and finally convert it into a CNF.

|#

(in-package :cl-sat)
;; allow both (not symbol) and !symbol
;; allow numbers (as in cnf)

(defpackage cl-sat.variables)
(defpackage cl-sat.aux-variables)

(defun var (number)
  "intern a number to a symbol"
  (intern (format nil "VAR~a" number) :cl-sat.variables))

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
    ((= 0)
     ;; https://www.satcompetition.org/2009/format-benchmarks2009.html
     ;; 0 is not allowed as a literal
     (signal 'type-error :expected-type '(or symbol cons (and integer (not (eql 0)))) tree))
    ((list* head rest)
     (list* head (mapcar #'symbolicate-form rest)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NNF

(defun negate (form)
  "Simple negation not involving De-Morgan's law."
  (ematch form
    ((list 'not form)
     form)
    (_
     `(not ,form))))

(defun to-nnf (form)
  "Applying De-Morgan's law, the resulting tree contains negations
only at the leaf nodes."
  (ematch form
    ((list* 'and rest) `(and ,@(mapcar #'to-nnf rest)))
    ((list* 'or rest)  `(or  ,@(mapcar #'to-nnf rest)))
    ((list 'not (list* 'or rest))
     ;; De-Morgan's law
     `(and ,@(mapcar (compose #'to-nnf #'negate) rest)))
    ((list 'not (list* 'and rest))
     ;; De-Morgan's law
     `(or  ,@(mapcar (compose #'to-nnf #'negate) rest)))
    ((list 'not (symbol))
     form)
    ((symbol)
     form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CNF

(defun composite-negate-p (form)
  "Returns true if a form is a negation of a tree."
  (ematch form
    ((list 'not (symbol)) nil)
    ((list 'not (list* _)) t)))

(defun %to-cnf (form)
  ;; (and (or ...) (or ...))
  (match form
    ((list* 'and rest)
     `(and ,@(mapcar #'%to-cnf rest)))
    ((list* 'or rest)
     (assert (notany (lambda-match ((list* 'and _) t) ((list* 'or _) t)) rest))
     form)
    ((list 'not (list* 'or rest))
     `(and ,@(mapcar (compose #'%to-cnf #'negate) rest)))
    ((list 'not (list* 'and rest))
     (%to-cnf
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

(defun to-cnf (form)
  (flatten-cnf
   (%to-cnf
    (symbolicate-form form))))
