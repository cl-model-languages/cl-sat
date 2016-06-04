(in-package :cl-sat)

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

;; swank/gray::slime-output-stream
;; (defvar *interactive-output*
;;     (if (featurep :swank)
;; 
;;         *standard-output*))
;; 
;; (defvar *interactive-error*
;;     (if (featurep :swank)
;;         swank/gray::slime-output-stream
;;         *error-output*))

(defmethod solve ((i sat-instance) solver &rest args &key &allow-other-keys)
  (with-temp (tmp :template "cnf.XXXXXXX")
    (with-output-to-file (s tmp :if-exists :supersede)
      (print-cnf i s))
    (apply #'solve (pathname tmp) solver args)))

(defmethod solve ((i list) solver &rest args &key &allow-other-keys)
  (apply #'solve
         (make-instance 'sat-instance :form i)
         solver
         args))


(defun sat-instance-variables (instance)
  (remove-duplicates
   (set-difference
    (flatten (cnf instance))
    '(and or not))))
