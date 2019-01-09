
(in-package :cl-sat)

(defvar *base-url*
  `((2016 . "https://baldur.iti.kit.edu/sat-competition-2016/solvers/")
    (2017 . "https://baldur.iti.kit.edu/sat-competition-2017/solvers/")
    (2018 . "http://sat2018.forsyte.tuwien.ac.at/solvers/")))

(defun download-solver (year track name)
  (check-type year fixnum)
  (let* ((dir (namestring (asdf:system-relative-pathname :cl-sat (format nil "solvers/~a/~a/" year track))))
         (zip (namestring (merge-pathnames (format nil "~a.zip" name) dir)))
         (runner (namestring (merge-pathnames (format nil "~a/bin/starexec_run_default" name) dir)))
         (bin (namestring (merge-pathnames (format nil "~a/bin/" name) dir)))
         (home (namestring (merge-pathnames (format nil "~a/" name) dir))))
    (ensure-directories-exist zip)
    (unless (probe-file zip)
      (alexandria:unwind-protect-case ()
          (progn
            (uiop:run-program `("wget" ,(format nil "~a/~a/~a.zip" (cdr (assoc year *base-url*)) track name) "-O" ,zip)
                              :output t :error t)
            (uiop:run-program `("sh" "-c" ,(format nil "cd ~a; unzip ~a.zip" dir name))
                              :output t :error t)
            (uiop:run-program `("sh" "-c" ,(format nil "cd ~a; bash starexec_build" home))
                              :output t :error t))
        (:abort
         (format *error-output* "~&Aborting, cleaning up~%")
         (uiop:run-program `("rm" "-rv" ,zip ,home)
                           :output t :error t :ignore-error-status t))))
    (unless (probe-file runner)
      (error "Runner script ~a is missing in ~a !" runner bin))
    (values runner bin)))

(defmethod solve ((input pathname) (competition (eql :competition)) &rest options &key debug year track name &allow-other-keys)
  (remf options :debug)
  (remf options :solver)
  
  (with-temp (dir :directory t :template "glucose.XXXXXXXX" :debug debug)
    (multiple-value-bind (runner bin) (download-solver year track name)
      (let* ((command (format nil "cd ~a ; bash ~a ~a ~a"
                              bin
                              runner
                              (namestring input)
                              (namestring dir)))
             (result (format nil "~a/result" dir)))
        (format t "~&; ~a" command)
        (multiple-value-match (uiop:run-program command
                                                :output result
                                                :error-output t
                                                :ignore-error-status t)
          ((_ _ 0)
           ;; indeterminite
           (values nil nil nil))
          ((_ _ 10)
           ;; sat
           (parse-dimacs-output result *instance*))
          ((_ _ 20)
           ;; unsat
           (values nil nil t)))))))