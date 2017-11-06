(in-package :cl-sat)

(defclass sat-instance ()
  ((cnf :reader cnf :initarg :cnf)))

(defgeneric solve (input solver-designator &rest args))
