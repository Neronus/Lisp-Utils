(defpackage randBG
  (:use cl)
  (:export main))

(in-package randBG)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (sl:set-script-macros))

(import '(cl-fad::pathname-as-directory cl-fad::list-directory))

(defparameter *home-dir* (user-homedir-pathname))

(defparameter *bg-dir* (merge-pathnames #P"backgrounds/" *home-dir*))

(defun path-/ (directory &rest names)
  (declare (inline path-/)
           (type (or (cons string) (cons pathname)) names)
           (type (or string pathname) directory))
  (loop for name in names
        for acc = (cl-fad:pathname-as-directory directory) then (cl-fad:pathname-as-directory acc)
        do (setf acc (merge-pathnames acc name))
        finally (return acc)))


(defun ls (path)
  (let ((dir (sb-posix:opendir path)))
    (unwind-protect
         (loop
            :for  entry = (sb-posix:readdir dir)
            :until (sb-alien:null-alien entry)
            :for name = (sb-posix:dirent-name entry)
            :unless (or (string= name ".") (string= name ".."))
            :collect name)
      (sb-posix:closedir dir))))

(defparameter *resolutions*
  (mapcar (lambda (res) (path-/ *bg-dir* res))
          '("1680x1050")))

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
  [ habak -mp 0,0 -hi ?path ])

(defun append-images (&rest paths)
  (let ((pathname (sl:mktemp)))
    (or-raise [convert ?(format nil "~{'~A'~^ ~}" paths) +append ?pathname ])
    pathname))

(defun main (args)
  (pop args)
  (let* ((*random-state* (make-random-state t))
         (imgs
          (mapcar #'ls *resolutions*)))
    (labels ((random-imgs ()
               (mapcar (lambda (imgs res) (path-/  res (random-elt imgs))) imgs *resolutions*))
             (set-bgs ()
               (let ((merged (apply #'append-images (random-imgs))))
                 (set-bg merged)
                 (delete-file merged))))
      (if (null args)
          (set-bgs)
          (let ((interval (parse-integer (car args) :junk-allowed t)))
            (when (null interval)
              (format *error-output* "supplied argument ~s is not a number~%" (car args))
              (sb-ext:quit :unix-status 1))
            (loop do (set-bgs) do (sleep interval)))))))
                
