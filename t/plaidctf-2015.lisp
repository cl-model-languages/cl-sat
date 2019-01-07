(ql:quickload :cl-ppcre)
(ql:quickload :iterate)
(ql:quickload :alexandria)

(ql:quickload :cl-sat)
(ql:quickload :cl-sat.glucose)


(defpackage :plaid
  (:use :cl :iterate :alexandria))

(in-package :plaid)

;; Get this file from
;; https://raw.githubusercontent.com/ctfs/write-ups-2015/master/plaidctf-2015/reversing/re-gex/regex_57f2cf49f6a354b4e8896c57a4e3c973.txt

(defvar *path* (asdf:system-relative-pathname :cl-sat "t/regex_57f2cf49f6a354b4e8896c57a4e3c973.txt"))

(unless (probe-file *path*)
  (uiop:run-program
   `("curl"
     "https://raw.githubusercontent.com/ctfs/write-ups-2015/master/plaidctf-2015/reversing/re-gex/regex_57f2cf49f6a354b4e8896c57a4e3c973.txt")
   :output (asdf:system-relative-pathname :cl-sat "t/regex_57f2cf49f6a354b4e8896c57a4e3c973.txt")))

(defparameter *regex* (alexandria:read-file-into-string *path*))

(defparameter *syntax-tree* (cl-ppcre:parse-string *regex*))


(defconstant  +len+ 171)
(defparameter +chars+ "plaidctf")


;; Prepare data structure
(defun sym-for-char (ch i)
  (intern (format nil "~a~d" (char-upcase ch) i)
          :plaid))

(defun syms-for-char (i)
  (iter:iter (for ch in-vector +chars+)
             (collect (sym-for-char ch i))))

(defun syms-for-chars (i chars)
  (iter:iter (for ch in chars)
             (collect (sym-for-char ch i))))

#+(or)
(syms-for-char 10)

(defun exactly-one-of (syms)
  (cons
    'or
    (iter:iter (for not-negated in syms)
               (for with-1-not-negated = 
                    (mapcar (lambda (i)
                              (if (eql i not-negated)
                                i
                              `(not ,i)))
                            syms))
               (collect (cons 'and with-1-not-negated)))))

#+(or)
(exactly-one-of (syms-for-char 10))



(defun re-clauses-to-sat-vars (pos clauses)
  (let ((cls (first clauses)))
    (trivia:match cls
                  ((list* :char-class _)
                   (cons (syms-for-chars pos (cdr cls))
                         (re-clauses-to-sat-vars (1+ pos)
                                                 (cdr clauses))))
                  (:everything
                    (re-clauses-to-sat-vars (1+ pos)
                                            (cdr clauses)))
                  ((list :greedy-repetition from to :everything)
                   (if (eql from to)
                     (re-clauses-to-sat-vars (+ pos from)
                                             (cdr clauses)))))))


(defun syntax-tree-to-sat-expr-original ()
  (concatenate
    'list
    ;'(and)
    (iter:iter (for i below +len+)
               (for syms = (syms-for-char i))
               (collect (exactly-one-of syms)))
    (iter:iter outer
               #+(or)
               (repeat 10)
               (for desc in (second (third *syntax-tree*)))
               (if (and (consp desc)
                        (eq :sequence (first desc)))
                 (let ((char-sets (re-clauses-to-sat-vars 0 (rest desc))))
                   (collect
                     (iter:iterate 
                         (with now-positive = ())
                         (for negative in char-sets)
                         (for clause = `(and (not (or ,@ negative))
                                             ,@ (mapcar
                                                  (lambda (chars)
                                                    (cons 'or chars))
                                                  now-positive)))
                         (collect clause into clauses)
                         (push negative now-positive)
                         (finally 
                           (return 
                             (cons 'or clauses))))))))))


(defun syntax-tree-to-sat-expr ()
  (concatenate
    'list
    '(and)
    (iter:iter (for i below +len+)
               (for syms = (syms-for-char i))
               (collect (exactly-one-of syms)))
    (iter:iter outer
               #+(or)
               (repeat 10)
               (for desc in (second (third *syntax-tree*)))
               (if (and (consp desc)
                        (eq :sequence (first desc)))
                 (let* ((char-sets (re-clauses-to-sat-vars 0 (rest desc)))
                        (this-re-clause
                          (iter:iterate 
                              (with now-positive = ())
                              (for negative in char-sets)
                              (for clause = `(and ,@ (mapcar 
                                                       (lambda (clause)
                                                         (list 'not clause))
                                                       negative)
                                                  ,@ (mapcar
                                                       (lambda (chars)
                                                         (cons 'or chars))
                                                       now-positive)))
                              (collect clause into clauses)
                              (push negative now-positive)
                              (finally 
                                (return 
                                  (if clauses
                                    (cons 'or clauses)))))))
                   (if this-re-clause
                     (collect this-re-clause)))))))


(defparameter *sat-expression* 
  (syntax-tree-to-sat-expr))


#+(or)
(defparameter *cnf* (sat:to-cnf *sat-expression*))

#+(or)
(defparameter *result* (sat:solve *sat-expression*
                                  :minisat))

#+(or)
(defparameter *result* (sat:solve *sat-expression*
                                  :glucose))


(time (sat:solve *sat-expression* :minisat))
