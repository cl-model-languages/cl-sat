(in-package :cl-sat)

(defmacro with-temp ((var &key directory template (tmpdir "/tmp/") debug) &body body)
  `(let ((,var (uiop:run-program (format nil "mktemp --tmpdir='~a' ~@[-d~*~] ~a" ,tmpdir ,directory ,template)
                                 :output '(:string :stripped t))))
     (unwind-protect
         (progn ,@body)
       ,(if debug
            `(format t "~&not removing ~a for debugging" ,var)
            `(uiop:run-program (format nil "rm -rf ~a" (namestring ,var)) :ignore-error-status t)))))
