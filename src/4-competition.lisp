
(in-package :cl-sat)

(defvar *base-url*
  `((2016 . "https://baldur.iti.kit.edu/sat-competition-2016/solvers/")
    (2017 . "https://baldur.iti.kit.edu/sat-competition-2017/solvers/")
    (2018 . "http://sat2018.forsyte.tuwien.ac.at/solvers/")
    (2019 . "http://sat-race-2019.ciirc.cvut.cz/solvers/")))

(define-condition competition-setup-error (error)
  ((year :initarg :year)
   (track :initarg :track)
   (name :initarg :name))
  (:report
   (lambda (c s)
     (print c s))))

(defmethod print-object ((c competition-setup-error) s)
  (print-unreadable-object (c s :type t)
    (with-slots (year track name) c
       (format s "~a ~a ~a" year track name))))

(define-condition download-error (competition-setup-error) ())
(define-condition unzip-error (competition-setup-error) ())
(define-condition build-error (competition-setup-error) ())
(define-condition chmod-error (competition-setup-error) ())

(defun download-solver (year track name)
  (check-type year fixnum)
  (let* ((dir (namestring (asdf:system-relative-pathname :cl-sat (format nil "solvers/~a/~a/" year track))))
         (zip (namestring (merge-pathnames (format nil "~a.zip" name) dir)))
         (runner (namestring (merge-pathnames (format nil "~a/bin/starexec_run_default" name) dir)))
         (bin (namestring (merge-pathnames (format nil "~a/bin/" name) dir)))
         (home (namestring (merge-pathnames (format nil "~a/" name) dir))))
    (ensure-directories-exist zip)
    (unless (probe-file runner)
      (alexandria:unwind-protect-case ()
          (progn
            (handler-case
                (uiop:run-program `("wget" ,(format nil "~a/~a/~a.zip" (cdr (assoc year *base-url*)) track name) "-O" ,zip)
                                  :output t :error t)
              (uiop:subprocess-error ()
                (error 'download-error :year year :track track :name name)))
            (handler-case
                (uiop:run-program `("sh" "-c" ,(format nil "cd ~a; unzip ~a.zip" dir name))
                                  :output t :error t)
              (uiop:subprocess-error ()
                (error 'unzip-error :year year :track track :name name))) 
            (handler-case
                (uiop:run-program `("sh" "-c" ,(format nil "cd ~a; chmod +x starexec_build build/*; MAKEFLAGS=\"-j 4\" ./starexec_build" home))
                                  :output t :error t)
              (uiop:subprocess-error ()
                (error 'build-error :year year :track track :name name))))
        (:abort
         (format *error-output* "~&Aborting, cleaning up~%")
         (uiop:run-program `("rm" "-rv" ,zip ,home)
                           :output t :error t :ignore-error-status t))))
    (unless (probe-file runner)
      (error "Runner script ~a is missing in ~a !" runner bin))
    (handler-case
        (uiop:run-program `("sh" "-c" ,(format nil "chmod +x ~a/*" bin))
                          :output t :error-output t)
      (uiop:subprocess-error ()
        (error 'chmod-error :year year :track track :name name)))
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
