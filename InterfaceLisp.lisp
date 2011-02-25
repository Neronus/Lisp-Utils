(defpackage interface-lift
  (:use cl drakma cl-ppcre drakma utils)
  (:export main))

(in-package interface-lift)

(sl:set-script-macros)

(defparameter *dimensions* "1680x1050")

(defparameter *index-url* "http://interfacelift.com/wallpaper_beta/downloads/date/widescreen/")

(defparameter *download-url* "http://interfacelift.com/wallpaper_beta/grab")

(defparameter *directory* "/home/christian/lispbackgrounds/")

(defparameter *image-regexp* #[http://interfacelift.com/wallpaper_beta/previews/(\d+)_([^"]+).jpg]#) ;; " 

(defvar *directory*)

(defvar *from*)

(defvar *to*)

(defun index-url (index)
  (declare (type integer index))
   (format nil "~a/~a/index~d.html" *index-url* *dimensions* index))

(defun get-page (index)
  (format t "Downloading index page ~s~%" index)
  (http-request (index-url index)
                :user-agent :firefox
                :redirect t
                :close t))

(defun get-images (string)
  (let ((images nil))
    (do-register-groups (number name) (*image-regexp* string (nreverse images) :sharedp t)
      (push (list number name) images))))

(defun image-url (image)
  (destructuring-bind (number name) image
    (format nil "~a/~a_~a_~a.jpg" *download-url* number name *dimensions*)))

(defun save-pathname (image)
  (destructuring-bind (number name) image
    (path-/ *directory* *dimensions* (format nil "~a_~a.jpg" number name))))

(defun save-image (image &optional overwrite)
  (restart-case
      (with-open-file (out (save-pathname image)
                           :direction :output
                           :element-type '(unsigned-byte 8)
                           :if-does-not-exist :create
                           :if-exists (if overwrite :supersede :error))
        (format t "~%Saving ~s " image) (finish-output)
        (with-open-stream (img-stream (http-request (image-url image) :external-format-in '(unsigned-byte 8) :want-stream t))
          (loop for c = (read-byte img-stream nil nil)
             for i = 0 then (+ i 1)
             until (null c)
             do (write-byte c out)
             do (finish-output)
             when (zerop (mod i 65536)) do (write-char #\.))))
    (overwrite ()
      :report "Overwrite file"
      :test (lambda (c) (typep c 'file-error))
      (save-image image t))
    (retry ()
      :report "Retry saving the file"
      :test (lambda (c) (typep c 'file-error))
      (save-image image overwrite))))

(defun image-loop ()
  (with-simple-restart (stop "Stop downloading")
    (loop for index from *from* to *to*
       for images = (get-images (get-page index))
       do (loop for image in images
             do (with-simple-restart (skip "Skip this file") (save-image image))))))

(defun main (argv)
  (let ((name (pop argv)))
    (when (not (= 4 (length argv)))
      (format *error-output* "Usage: ~a <resolution> <directory> <from> <to>~%" name)
      (sb-ext:quit :unix-status 1)))
  (destructuring-bind (resolution directory from to) argv
    (setf *from* (parse-integer from))
    (setf *to* (parse-integer to))
    (setf *dimensions* resolution)
    (setf *directory* directory)
    (image-loop)))
      