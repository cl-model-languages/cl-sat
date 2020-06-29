(in-package :cl-sat)

;; These constants are borrowed from the tempname utility
;; of coreutils.
(defparameter +random-string-size-min+ 3)
(defparameter +temp-file-attempts+ (* 62 62 62))

(defmacro with-temp ((var &key directory (template "tmp.XXXXXXX") (tmpdir (uiop:temporary-directory)) debug) &body body)
  "Create a temporary file, then remove the file by unwind-protect.
Most arguments are analogous to mktemp.
TEMPLATE should be a string that ends with one or more X's, these X's will be replaced by random characters.
When DIRECTORY is non-nil, creates a directory instead.
When DEBUG is non-nil, it does not remove the directory so that you can investigate what happened inside the directory.
An error of type file-error is signalled if a unique file name can't be generated after a number of attempts."
  (declare (ignorable template tmpdir))
  `(let ((,var
          (let* ((template-without-xs (string-right-trim "X" template)))
            (attempt-create-temp directory
                                 tmpdir
                                 template-without-xs
                                 (- (length template) (length template-without-xs))
                                 +temp-file-attempts+))))
     (unwind-protect
          (progn ,@body)
       (if ,debug
           (format t "~&not removing ~a for debugging" ,var)
           (if ,directory
               (uiop:delete-directory-tree (make-pathname :directory (list :absolute ,var))
                                           :if-does-not-exist :ignore
                                           :validate t)
               (delete-file ,var))))))

(defun attempt-create-temp (directory base-dir name-prefix random-string-size attempts)
  "Creates a file/directory in BASE-DIR with NAME-PREFIX as a prefix of the name and RANDOM-STRING-SIZE
   random base62 characters at the end.
   Returns the name of the created file.
   Signals an error if it can't generate a unique name after ATTEMPTS attempts."
  (when (> +random-string-size-min+ random-string-size)
    (error "Random string part of temporary file name isn't long enough."))
  (if (<= attempts 0)
      (error "Couldn't create a unique temp file/folder.")
      (let ((path (merge-pathnames (let ((name (generate-temp-name name-prefix random-string-size)))
                                     (if directory
                                         (make-pathname :directory `(:relative ,name))
                                         (uiop:parse-unix-namestring name)))
                                   (uiop:parse-unix-namestring base-dir))))
        (if (create-nonexisting directory path)
            (namestring path)
            (attempt-create-temp directory base-dir name-prefix random-string-size (1- attempts))))))

(defun generate-temp-name (name-prefix random-string-size)
  "Generates a random name for a temp file/directory.
   NAME-PREFIX is the prefix of the name, after which RANDOM-STRING-SIZE random characters are added. "
  (concatenate 'string name-prefix (random-base62 random-string-size)))

(defun random-base62 (n)
  "Returns a random base62 string with n characters."
  (let ((table "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"))
    (coerce (loop repeat n collect (aref table (random (length table))))
            'string)))

(defun create-nonexisting (directory path)
  "Attempts to create a file/directory, returns NIL if it exists already and T otherwise."
  (handler-case
      (if directory
          (multiple-value-bind (_ was-nonexisting) (ensure-directories-exist path)
            (declare (ignore _))
            was-nonexisting)
          (progn
            (open path
                  :direction :io
                  :if-exists :error)
            t))
    (file-error (_)
      (declare (ignore _))
      nil)))

(defun format1 (stream format-control first-arg &rest more-args)
  (apply #'format stream format-control first-arg more-args)
  first-arg)
