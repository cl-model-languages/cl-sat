(in-package :cl-sat)

(defmacro with-temp ((var &key directory template (tmpdir "/tmp/") debug) &body body)
  "Create a temporary file, then remove the file by unwind-protect.
Most arguments are analogous to mktemp.
When DIRECTORY is non-nil, creates a directory instead.
When DEBUG is non-nil, it does not remove the directory so that you can investigate what happened inside the directory."
  (declare (ignorable template tmpdir))
  `(let ((,var (uiop:run-program
		 #-BSD (format nil "mktemp --tmpdir='~a' ~@[-d~*~] ~a" ,tmpdir
				    ,directory ,template)
		 ;; BSD also includes Darwin (Mac OS X) (c.f. trivial-features's SPEC.md)
		 #+BSD (if ,directory "mktemp -d" "mktemp")
		 :output '(:string :stripped t))))
     (unwind-protect
         (progn ,@body)
       (if ,debug
           (format t "~&not removing ~a for debugging" ,var)
           (uiop:run-program (format nil "rm -rf ~a" (namestring ,var)) :ignore-error-status t)))))


(defun format1 (stream format-control first-arg &rest more-args)
  (apply #'format stream format-control first-arg more-args)
  first-arg)
