(in-package :cl-sat)

(defmethod initialize-instance ((i sat-instance) &rest args &key form cnf &allow-other-keys)
  (assert (not (and form cnf)) nil )
  (cond
    ((and form cnf)
     (error "incompatible keywords: :form and :cnf specified at the same time"))
    (form
     (remf args :form)
     (apply #'call-next-method i :cnf (to-cnf form) args))
    (t
     (call-next-method))))

(defvar *instance*)


(defmethod solve ((*instance* sat-instance) solver &rest args)
  (with-temp (tmp :template "cnf.XXXXXXX")
    (with-output-to-file (s tmp :if-exists :supersede)
      (print-cnf *instance* s))
    (apply #'solve (pathname tmp) solver args)))

(defmethod solve ((i list) solver &rest args)
  (apply #'solve
         (make-instance 'sat-instance :form i)
         solver
         args))


(defun sat-instance-variables (instance)
  (remove-duplicates
   (set-difference
    (flatten (cnf instance))
    '(and or not))))
