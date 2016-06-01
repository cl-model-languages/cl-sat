#|
  This file is a part of cl-sat project.
  Copyright (c) 2016 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-sat
  (:nicknames :sat)
  (:use :cl :trivia :alexandria :iterate)
  (:export
   #:solve
   #:sat-instance
   #:form-cnf))
(in-package :cl-sat)


(defclass sat-instance ()
     ((cnf :reader cnf :initarg :cnf)))

(defmethod initialize-instance ((i sat-instance) &rest args &key form cnf &allow-other-keys)
  (assert (not (and form cnf)) nil )
  (cond
    ((and form cnf)
     (error "incompatible keywords: :form and :cnf specified at the same time"))
    (form
     (remf args :form)
     (apply #'call-next-method i :cnf (form-cnf form) args))
    (t
     (call-next-method))))

(defgeneric solve (input solver-designator &rest args &key &allow-other-keys))

(defmethod solve ((i sat-instance) solver &rest args &key &allow-other-keys)
  (aplpy #'solve
         (make-string-input-stream (with-output-to-string (s) (print-cnf i s)))
         solver
         args))

(defmethod solve ((i list) solver &rest args &key &allow-other-keys)
  (aplpy #'solve
         (make-instance sat-instance :form i)
         solver
         args))

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
     (assert (notany (lambda-match ((list* 'and _) t)) rest))
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

;; http://people.sc.fsu.edu/~jburkardt/data/cnf/cnf.html
;; ( x(1) OR ( NOT x(3) ) )
;; AND
;; ( x(2) OR x(3) OR ( NOT x(1) ) ).

;; c  simple_v3_c2.cnf      <<< comments
;; c
;; p cnf 3 2                <<< problem spec, cnf type, #vars, #clauses
;; 1 -3 0
;; 2 3 -1 0
