(defpackage randBG
  (:use cl utils)
  (:import-from alexandria random-elt)
  (:export main))

(in-package randBG)

(eval-when (:execute :compile-toplevel :load-toplevel)
  (sl:enable)
  (short-lambda:enable))

(import '(cl-fad::pathname-as-directory cl-fad::list-directory))

(defparameter *home-dir* (user-homedir-pathname))

(defparameter *bg-dir* (merge-pathnames #P"backgrounds/" *home-dir*))

(defparameter *resolutions*
  (mapcar #%(path-/ *bg-dir* %1)
          '("1440x900" "1280x1024")))

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
                
