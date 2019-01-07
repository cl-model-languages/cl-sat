(in-package :cl-sat)

(defclass sat-instance ()
  ((cnf :reader cnf :initarg :cnf)
   (%variables)))

(defgeneric solve (input solver-designator &rest args))

(defmethod initialize-instance ((i sat-instance) &rest args &key form cnf (converter #'to-cnf-tseytin) &allow-other-keys)
  (assert (not (and form cnf)) nil )
  (cond
    ((and form cnf)
     (error "incompatible keywords: :form and :cnf specified at the same time"))
    (form
     (remf args :form)
     (apply #'call-next-method i :cnf (to-cnf form converter) args))
    (t
     (call-next-method))))

(defvar *instance*)

(defmethod solve ((*instance* sat-instance) solver &rest args &key debug &allow-other-keys)
  (with-temp (tmp :template "cnf.XXXXXXX" :debug debug)
    (with-output-to-file (s tmp :if-exists :supersede)
      (print-cnf *instance* s))
    (apply #'solve (pathname tmp) solver args)))

(defmethod solve ((i list) solver &rest args &key (converter #'to-cnf-tseytin) &allow-other-keys)
  (apply #'solve
         (make-instance 'sat-instance :form i :converter converter)
         solver
         args))


(defun sat-instance-variables (instance)
  (with-slots (%variables) instance
    (if (slot-boundp instance '%variables)
        %variables
        (setf %variables
              (remove-duplicates
               (set-difference
                (flatten (cnf instance))
                '(and or not)))))))

