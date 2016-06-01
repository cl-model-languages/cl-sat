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

(defun sat-instance-variables (instance)
  (ematch instance
    ((sat-instance cnf)
     (remove-duplicates
      (set-difference
       (flatten cnf) '(and or not))))))
