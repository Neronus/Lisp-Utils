(defpackage randBG
  (:use cl)
  (:export main))

(in-package randBG)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (sl:set-script-macros))

(import '(cl-fad::pathname-as-directory cl-fad::list-directory))

(defparameter *home-dir* (user-homedir-pathname))

(defparameter *bg-dir* (merge-pathnames #P"backgrounds/" *home-dir*))

(defun ls (path)
  (let ((dir (sb-posix:opendir path))
        (dirpath (path name-as-directory path)))
    (unwind-protect
         (loop
            :for  entry = (sb-posix:readdir dir)
            :until (sb-alien:null-alien entry)
            :for name = (sb-posix:dirent-name entry)
            :unless (or (string= name ".") (string= name ".."))
            :collect name)
      (sb-posix:closedir dir))))

(defparameter *resolutions*
  (mapcar (lambda (d) (pathname-as-directory
                       (merge-pathnames d *bg-dir*))) '("1440x900" "1280x1024")))

(defun random-elt (list)
  (unless (and (listp list) (not (null list))) (error "list is not a list"))
  (let* ((length (length list))
         (index (random length)))
    (elt list index)))

(defmacro or-raise (script)
  "Execute the form script, which we expect to be of form (script ...) or friends. Signal an error if the return value is not 0."
  (let ((output (gensym "output"))
        (exit-code (gensym "exit-code")))
  `(multiple-value-bind (,output ,exit-code) ,script
     (if (not (zerop ,exit-code))
         (error "~A" ',script)
         (values ,output ,exit-code)))))

(defun set-bg (path)
  (or-raise [ habak -mp 0,0 -hi ?path ]))

(defun append-images (&rest paths)
  (let ((pathname (sl:mktemp)))
    (or-raise [convert ?(format nil "~{'~A'~^ ~}" paths) +append ?pathname ])
    pathname))

(defun main (args)
  (declare (ignore args))
  (let* ((*random-state* (make-random-state t))
         (imgs
          (mapcar (lambda (dir) (merge-pathnames (random-elt (ls dir)) dir))
                  *resolutions*))
         (merged (apply #'append-images imgs)))
    (set-bg merged)
    (delete-file merged)))
