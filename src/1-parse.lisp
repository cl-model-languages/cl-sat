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

(defun aux (number)
  "intern a number to an auxiliary symbol"
  (intern (format nil "AUX~a" number) :cl-sat.aux-variables))


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
          (intern (subseq name pos) (symbol-package tree)))
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
;; Naive CNF

(defun dispatch (form top k)
  (ematch* (form top)
    (((list 'and first) _)
     (dispatch first top k))

    (((list* 'and body) t)
     ;; if top=t, we are in a toplevel AND, which is allowed in CNF.
     ;; Continuations are cut and evaluated immediately.
     ;;
     ;; note: each dispatch for AND returns a list which lacks AND as the first element.
     (funcall k `(and ,@(mappend (lambda (e)
                                   (match (dispatch e t #'identity)
                                     ((list* 'and rest) rest)
                                     (it                (list it))))
                                 body))))
    
    (((list* 'and first body) nil)
     ;; Otherwise, we are inside ORs -- now we are at X of (and P (or Q X=(and a b c) R ) S ).
     ;; we must turn this inside-out, i.e.:
     ;; (and P (or Q X=a R ) (or Q X=b R ) (or Q X=c R ) S ).
     ;; We call the continuation k to obtain the substitutions.
     ;; continuation k is a function that returns a form (or Q X R ) given X, thus we call it with each element.
     (dispatch first t
               (lambda (result1)
                 (match* ((funcall k result1) (dispatch `(and ,@body) nil k))
                   ;; merge ORs while removing redundancy
                   (((list* 'and result1) (list* 'and result2))
                    `(and ,@result1 ,@result2))
                   ((it (list* 'and result2))
                    `(and ,it ,@result2))
                   (((list* 'and result1) it)
                    `(and ,@result1 ,it))
                   ((it1 it2)
                    `(and ,it1 ,it2))))))
    
    (((list  'or first)  _)                  ; == first
     (dispatch first top k))

    (((list* 'or first body) _)
     (dispatch first nil
               (lambda (result1)
                 ;; (format *trace-output* "~& entered the 1st OR callback: ~a ~%" result1)
                 (dispatch `(or ,@body) nil
                           (lambda (result2)
                             ;; (format *trace-output* "~& entered the 2nd OR callback: ~a ~%" result2)
                             (funcall k
                                      (match* (result1 result2)
                                        ;; merge ORs while removing redundancy
                                        (((list* 'or result1) (list* 'or result2))
                                         `(or ,@result1 ,@result2))
                                        ((it (list* 'or result2))
                                         `(or ,it ,@result2))
                                        (((list* 'or result1) it)
                                         `(or ,@result1 ,it))
                                        ((it1 it2)
                                         `(or ,it1 ,it2)))))))))
    (((list 'not _) _)
     (funcall k form))
    (((symbol) _)
     (funcall k form))))

(defun to-cnf-naive (form)
  "Convert a NNF into a flattened CNF via a naive method."
  (dispatch form t #'identity))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tseytin transformation

(defun to-cnf-tseytin (form)
  "Convert a NNF into a flattened CNF.
OR branches containing ANDs that could result in an exponential CNF
are converted into a linear-sized equisatisfiable formula
via Tseytin transformation by Tseytin 1968.

aux-variables are generated in CL-SAT.AUX-VARIABLES package.

G.S. Tseytin: On the complexity of derivation in propositional calculus. Presented at the Leningrad Seminar on Mathematical Logic held in September 1966.

"
  (let ((aux-var-index -1)
        (subformulas (make-hash-table :test 'equal))
        conjunctions)
    ;; collect subformulas
    (labels ((aux+ () (aux (incf aux-var-index)))
             (rec (form)
               (match form
                 ((list (or 'and 'or) single)
                  (rec single))
                 ((list* (and op (or 'and 'or)) rest)
                  (let ((aux (ensure-gethash form subformulas (aux+))) ; no duplicates
                        (substituted `(,op ,@(mapcar #'rec rest))))
                    ;; add a new formula: (aux <=> substituted)  =  ((aux => substituted) && (aux <= substituted))
                    (push `(or ,(negate aux) ,substituted) conjunctions)
                    (push `(or ,aux ,(negate substituted)) conjunctions)
                    aux))
                 (_
                  ;; return literals as it is
                  form))))
      (push (rec form) conjunctions))
    (to-cnf-naive (to-nnf `(and ,@conjunctions)))))


(defun to-cnf (form)
  (to-cnf-tseytin
   (to-nnf
    (symbolicate-form form))))
