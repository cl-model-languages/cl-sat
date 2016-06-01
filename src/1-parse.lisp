(in-package :cl-sat)
;; allow both (not symbol) and !symbol
;; allow numbers (as in cnf)

(defun var (number)
  "intern a number to a symbol"
  (intern (format nil "VAR~a" number)))

(defun normalize-form (tree)
  (ematch tree
    ((symbol :name (and name (string* #\!)))
     `(not ,(intern (subseq name 1) (symbol-package tree))))
    ((symbol)
     tree)
    ((< 0)
     `(not ,(var (- tree))))
    ((> 0)
     (var tree))
    ((cons a b)
     (cons (normalize-form a)
           (normalize-form b)))))

(defun composite-negate-p (form)
  (match form
    ((list 'not (list* _)) t)))

(defun negate (form)
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
    (normalize-form form))))
