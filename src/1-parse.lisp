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

(defun var (suffix &optional (prefix "V"))
  "Helper function: Interns SUFFIX (usually a number, but can be any printable object) to a symbol with the optional PREFIX.
The new symbol is interned in package CL-SAT.VARIABLES."
  (intern (format nil "~a~a" prefix suffix) :cl-sat.variables))

(defun aux (suffix &optional (prefix "A"))
  "intern a suffix to an auxiliary symbol"
  (intern (format nil "~a~a" prefix suffix) :cl-sat.aux-variables))


(defun symbolicate-form (tree)
  "
This function is the first step of converting the input into a normal form.
It normalizes the input tree containing numbers and !-negated vars into a tree of symbols.
Note that it does not guarantee to return any type of normal forms (e.g. NNF,CNF,DNF,ANF).
It accepts any types of compound forms, not limited to AND/OR/NOT.
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
only at the leaf nodes. Supports OR,AND,NOT,IMPLY,IFF."
  (simplify-nnf (%to-nnf (expand-extensions form))))

(defun expand-extensions (form)
  "Translate extended logical operations IMPLY, IFF into AND, OR, NOT"
  (ematch form
    ((list 'imply lhs rhs)
     (let ((lhs (expand-extensions lhs))
           (rhs (expand-extensions rhs)))
       `(or (not ,lhs) ,rhs)))
    ((list 'iff lhs rhs)
     (let ((lhs (expand-extensions lhs))
           (rhs (expand-extensions rhs)))
       `(and (or (not ,lhs) ,rhs)
             (or (not ,rhs) ,lhs))))
    ((list* 'and rest) `(and ,@(mapcar #'expand-extensions rest)))
    ((list* 'or  rest) `(or  ,@(mapcar #'expand-extensions rest)))
    ((list* 'not rest) `(not ,@(mapcar #'expand-extensions rest)))
    ((symbol)
     form)))

(defun %to-nnf (form)
  (ematch form
    ((list* 'and rest) `(and ,@(mapcar #'%to-nnf rest)))
    ((list* 'or rest)  `(or  ,@(mapcar #'%to-nnf rest)))
    ;; negated
    ((list 'not (list* 'or rest))
     ;; De-Morgan's law
     `(and ,@(mapcar (compose #'%to-nnf #'negate) rest)))
    ((list 'not (list* 'and rest))
     ;; De-Morgan's law
     `(or  ,@(mapcar (compose #'%to-nnf #'negate) rest)))
    ((list 'not (list 'not further))
     (%to-nnf further))
    ((list 'not (symbol))
     form)
    ((symbol)
     form)))


(defun %merge-same-clauses (type forms)
  (iter (for elem in forms)
        (match elem
          ((list* (eq type) subrest)
           (appending subrest))
          (_
           (collecting elem)))))

(defun symbol< (a b)
  (match* (a b)
    (((symbol :package p1) (symbol :package p2))
     (cond
       ((and (not p1) (not p2))
        (string< a b))
       ((and (not p1) p2)
        t)
       ((and p1 (not p2))
        nil)
       ((and p1 p2)
        (if (eq p1 p2)
            (string< a b)
            (string< (package-name p1) (package-name p2))))))))

(defun symbol<= (a b)
  (or (eq a b)
      (symbol< a b)))

(defun clause< (a b)
  (match* (a b)
    (((symbol) (symbol))
     (symbol< a b))
    (((symbol) (type list))
     t)
    (((type list) (symbol))
     nil)
    (((list* f1 r1) (list* f2 r2))
     (or (clause< f1 f2)
         (and (equal f1 f2)
              (some #'clause< r1 r2))))))

(defun %sort-clauses (forms)
  (sort (copy-list forms)
        #'clause<))

(defun simplify-nnf (form)
  "Remove some obvious constants / conflicts. The result does not contain:
Single compound forms:
 (and X), (or X)
Compound forms that contains true/false consants:
 (and ... (or) ... ) -> (or)
 (or ... (and) ... ) -> (and)
 (or ... X ... (not X) ... ) -> (and)
 (and ... X ... (not X) ... ) -> (or)
Duplicated forms:
 (and ... X ... X ... ) -> (and ... X ... ...)
 (or  ... X ... X ... ) -> (or  ... X ... ...)
"
  (ematch form
    ((list 'and x) x)
    ((list 'or x) x)
    ((list* 'and rest)
     (let* ((rest (mapcar #'simplify-nnf rest))
            (rest (%merge-same-clauses 'and rest))
            (rest (%sort-clauses rest)))
       (cond
         ((member '(or) rest :test 'equal)
          '(or))
         ((iter outer
                (for (c1 . rest) on rest)
                (iter (for c2 in rest)
                      (in outer
                          (thereis
                           (match c2
                             ((list 'not (equal c1)) t))))))
          '(or))
         (t
          (list* 'and (remove-duplicates rest :test 'equal))))))
    ((list* 'or rest)
     (let* ((rest (mapcar #'simplify-nnf rest))
            (rest (%merge-same-clauses 'or rest))
            (rest (%sort-clauses rest)))
       (cond
         ((member '(and) rest :test 'equal)
          '(and))
         ((iter outer
                (for (c1 . rest) on rest)
                (iter (for c2 in rest)
                      (in outer
                          (thereis
                           (match c2
                             ((list 'not (equal c1)) t))))))
          ;; (or ... A ... (not A) ...)
          '(and))
         (t
          (list* 'or (remove-duplicates rest :test 'equal))))))
    ;; non-nnf is rejected here
    ((list 'not (symbol))
     form)
    ((symbol)
     form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Naive CNF

(defun dispatch (form top k)
  "Naive implementation that expands the inner ANDs of ORs into ORs of ANDs (potentially exponentially explode).
Implemented using Continuation Passing Style. Calling K with a value is equivalent to returning the result of processing FORM.

TOP is a boolean which, when NIL, means the current position is inside an OR."

  (ematch* (form top)
    ;; length 0

    (((list _) _)
     ;; always satisfiable (and) or unsatisfiable (or).
     ;; return the form as it is.
     (funcall k form))

    ;; length 1
    (((list 'not _) _)
     (funcall k form))
    
    (((list _ first) _)
     ;; (and x) is equivalent to x, (or x) is equivalent to x.
     (dispatch first top k))

    ;; length >= 2
    
    (((list* 'and rest) _)
     ;; (and (or)) --- does not come in here.
     (cond
       ((find '(or) rest :test 'equal)
        ;; the entire AND is unsatisfiable.
        (funcall k '(or)))
       ((find '(and) rest :test 'equal)
        ;; the clause can be ignored.
        (dispatch (remove '(and) form :test 'equal) top k))
       (t
        (trivia.skip:skip))))
    
    (((list* 'and rest) t)
     ;; if top=t, we are in a toplevel AND, which is allowed in CNF.
     ;; Continuations are cut and evaluated immediately.
     ;;
     ;; note: each dispatch for AND returns a list which lacks AND as the first element.
     (funcall k `(and ,@(mappend (lambda (e)
                                   ;; (format *trace-output* "~&Top AND callback~%")
                                   (match (dispatch e t #'identity)
                                     ((list* 'and rest) rest)
                                     (it                (list it))))
                                 rest))))
    
    (((list* 'and first rest) nil)
     ;; Otherwise, we are inside ORs -- now we are at X of (and P (or Q X=(and A B C) R ) S ).
     ;; we must turn this inside-out, i.e.:
     ;; (and P (or Q X=A R ) (or Q X=B R ) (or Q X=C R ) S ).
     ;; We call the continuation k to obtain the substitutions.
     ;; continuation k is a function that returns a form (or Q X R ) given X, thus we call it with each element.
     (dispatch first t
               (lambda (result1)
                 ;; (format *trace-output* "~&AND callback in OR~%")
                 (match* ((funcall k result1)             ; == process A --> (or Q A R)
                          (dispatch `(and ,@rest) nil k)) ; == process (and B C) --> (and (or Q B R) (or Q C R))
                   ;; merge ANDs while removing redundancy
                   (('(and) it) it)
                   ((it '(and)) it)
                   (('(or) _) '(or))
                   ((_ '(or)) '(or))
                   (((list* 'and result1) (list* 'and result2))
                    `(and ,@result1 ,@result2))
                   ((it (list* 'and result2)) ; <-- above case ends up here
                    `(and ,it ,@result2))
                   (((list* 'and result1) it)
                    `(and ,@result1 ,it))
                   ((it1 it2)
                    `(and ,it1 ,it2))))))

    (((list* 'or rest) _)
     ;; (or (and)) --- does not come in here.
     (cond
       ((find '(and) rest :test 'equal)
        ;; the entire OR is always satisfiable.
        (funcall k '(and)))
       ((find '(or) rest :test 'equal)
        ;; the clause can be ignored.
        (dispatch (remove '(or) form :test 'equal) top k))
       (t
        (trivia.skip:skip))))

    (((list* 'or first rest) _)
     (dispatch first nil
               (lambda (result1)
                 ;; (format *trace-output* "~& entered the 1st OR callback: ~a ~%" result1)
                 (dispatch `(or ,@rest) nil
                           (lambda (result2)
                             ;; (format *trace-output* "~& entered the 2nd OR callback: ~a ~%" result2)
                             (funcall k
                                      (progn
                                        ;; format1 *trace-output* "~& continue: ~a ~%"
                                         (match* (result1 result2)
                                           ;; merge ORs while removing redundancy
                                           (('(or) it) it)
                                           ((it '(or)) it)
                                           (('(and) _) '(and))
                                           ((_ '(and)) '(and))
                                           (((list* 'or result1) (list* 'or result2))
                                            `(or ,@result1 ,@result2))
                                           ((it (list* 'or result2))
                                            `(or ,it ,@result2))
                                           (((list* 'or result1) it)
                                            `(or ,@result1 ,it))
                                           ((it1 it2)
                                            `(or ,it1 ,it2))))))))))
    (((symbol) _)
     (funcall k form))))

(defun to-cnf-naive (nnf)
  "Convert a NNF into a flattened CNF via a naive method."
  (dispatch nnf t #'identity))

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
                  (let ((aux (ensure-gethash form subformulas (aux+)))
                        (substituted (mapcar #'rec rest)))
                    ;; add a new formula: (aux <=> substituted)
                    ;; =  ((aux => substituted) && (aux <= substituted))
                    ;; =  (and (or (not aux) substituted) (or aux (not substituted)))
                    (ecase op
                      (and
                       ;; (or (not aux) (and a b c)) = (and (or (not aux) a) (or (not aux) b) (or (not aux) c))
                       (dolist (var substituted)
                         (push `(or ,(negate aux) ,var) conjunctions))
                       
                       ;; (or aux (not (and a b c))) = (or aux (not a) (not b) (not c))
                       (push `(or ,aux ,@(mapcar #'negate substituted)) conjunctions))
                      
                      (or
                       ;; (or (not aux) (or a b c)) = (or aux a b c)
                       (push `(or ,(negate aux) ,@substituted) conjunctions)
                       
                       ;; (or aux (not (or a b c))) = (or aux (and (not a) (not b) (not c))) = (and (or aux (not a)) (or aux (not b)) (or aux (not c)))
                       (dolist (var substituted)
                         (push `(or ,aux ,(negate var)) conjunctions))))
                    aux))
                 (_
                  ;; return literals as it is
                  form))))
      (push (rec form) conjunctions))
    `(and ,@conjunctions)))


(defun to-cnf (form &optional (converter #'to-cnf-tseytin))
  (funcall converter
           (to-nnf
            (symbolicate-form form))))
