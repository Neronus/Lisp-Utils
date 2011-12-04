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
  (let ((pathname (first (sl:lines-to-list [mktemp]))))
    (or-raise [convert ?(format nil "~{'~A'~^ ~}" paths) +append ?pathname ])
    pathname))

(defun random-imgs ()
  "Randomly select an image from each directory specified by *resolutions*"
  (let ((imgs (mapcar #'ls *resolutions*)))
    (mapcar #%(path-/ %1 (random-elt %2)) *resolutions* imgs)))

(defun set-bgs (imgs)
  "Merge the images into one image and call set-bg.

The images will be merged from left to right as they appear in the list."
  (let ((merged (apply #'append-images imgs)))
    (set-bg merged)
    (delete-file merged)))

(defun usage (name)
  (concatenate 'string "Usage: " name " [interval]

If interval is not given, then set one background image randomly.
If interval is given, then update the image every <interval> seconds."))

(defun main (args)
  (let* ((name (pop args))
         (*random-state* (make-random-state t)))
    (if (null args)
        (set-bgs (random-imgs))
        (let ((interval (parse-integer (car args) :junk-allowed t)))
          (cond
            ((null interval)
             (format *error-output* "Supplied argument ~s is not a number~%~%" (car args))
             (princ (usage name) *error-output*) (terpri)
             (sb-ext:quit :unix-status 1))
            (t
             ;;; In case of an error, record that error, sleep, try again
             (loop
                (handler-case
                    (progn (set-bgs (random-imgs)) (sleep interval))
                  (error (err)
                    (princ err *error-output*) (terpri))))))))))
