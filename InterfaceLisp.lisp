(defpackage interface-lift
  (:use cl drakma utils split-sequence iterate)
  (:export main))

(in-package interface-lift)

(eval-when (:execute :load-toplevel :compile-toplevel)
  ;; Enable #[ ... ] reader macros
  (sl:enable))

(defvar *dimensions*)

(defconstant +index-url+ "http://interfacelift.com/wallpaper/downloads/date/"
  "Base url of index pages")

(defconstant +download-url+ "http://interfacelift.com/wallpaper/"
  "Base url of all downloads")

(defconstant +image-xpath+
   "//div[@class='item']//div[@class='preview']/div[@class='download']/div/a"
  "The xpath query used to find images in index pages")

;; Peculiar syntax ahead; see package sl
;; #[ ... ] mearly is another way to quote
;; If slime doesn't want to compile the file, remove the call of function
;; #'check-parens from #'slime-compile-file
(defparameter *image-regexp* #[href="/wallpaper/([\d\w]+)/(\d+)_([^"]+)_\d+x\d+.jpg]#) ;; "
;; (defparameter *image-regexp* "href=\"/wallpaper/([\\d\\w]+)/(\\d+)_([^\"]+)_\\d+x\\d+.jpg")
                                                                                    
(defvar *directory* nil "Directory into which images should be saved")

(defvar *from* nil "Number of index page to start searching from")

(defvar *to* nil "Number of last index page which should be searched")

(defstruct image number name hash)

;; (defun make-image (number name hash)
;;   "Create a new image datastructure"
;;   (list number name hash))

;; (defun image-number (image) (first image))
;; (defun image-name (image) (second image))
;; (defun image-hash (image) (third image))

(defun group ()
  "Return either widescreen or fullscreen depending on the resolution."
  (destructuring-bind (width height)
      (mapcar #'parse-integer (split-sequence #\x *dimensions*))
    (if (= (/ width height) 8/5)
        "widescreen"
        "fullscreen")))

(defun index-url (index)
  "Url of index with number index"
  (declare (type integer index))
   (format nil "~a~a/~a/index~d.html" +index-url+ (group) *dimensions* index))

(defun get-page (index)
  "Download contents of index page index"
  (format t "Downloading index page ~s~%" index)
  (http-request (index-url index)
                :user-agent :firefox
                :redirect t
                :close t))

(defun get-images (string)
  "Extract list of image urls from string.

An image is described by a list of two elements: First its number (as a string), then its name."
  (libxml2.html:with-parse-html (xml string)
    (iter (for node in-xpath-result +image-xpath+ on xml)
          (collect
              (let* ((href (xtree:attribute-value node "href"))
                     (parts (split-sequence #\/ href :remove-empty-subseqs t))
                     (hash (second parts)))
                (destructuring-bind
                      (number name &rest rest) (split-sequence #\_ (third parts))
                  (declare (ignore rest))
                  (make-image :number number :name name :hash hash)))))))
           
                     
  ;; (let ((images nil))
  ;;   (do-register-groups (hash number name) (*image-regexp* string (nreverse images) :sharedp t)
  ;;     (push (make-image number name hash) images))))

(defun image-url (image)
  "Construct url to download image described by an image."
  (format nil "~a~a/~a_~a_~a.jpg" +download-url+
          (image-hash image) (image-number image) (image-name image) *dimensions*))

(defun save-pathname (image)
  "Pathname of file to which the image should be saved"
  (path-/ *directory* *dimensions* (format nil "~a_~a.jpg" (image-number image) (image-name image))))

(defun save-image (image &optional overwrite)
  "Save the image.

If overwrite is NIL, then a signal of type file-error is raised.
Otherwise the file is simply overwritten.

Two restarts are bound:
  * Overwrite, which allows to overwrite the image
  * Retry which allows to retry the download and the writing"
  (restart-case
      (with-open-file (out (save-pathname image)
                           :direction :output
                           :element-type '(unsigned-byte 8)
                           :if-does-not-exist :create
                           :if-exists (if overwrite :supersede :error))
        (format t "~%Saving ~s " (image-name image)) (finish-output)
        (with-open-stream (img-stream (http-request (image-url image) :external-format-in '(unsigned-byte 8) :want-stream t))
          (loop :for c = (read-byte img-stream nil nil)
             :for i = 0 :then (+ i 1)
             :until (null c)
             :do (write-byte c out)
             :do (finish-output)
             :when (zerop (mod i 65536)) :do (write-char #\.))))
    (overwrite ()
      :report "Overwrite file"
      :test (lambda (c) (typep c 'file-error))
      (save-image image t))
    (retry ()
      :report "Retry saving the file"
      :test (lambda (c) (typep c 'file-error))
      (save-image image overwrite))))

(defun image-loop ()
  "Download all images from index pages *from*-*to*"
  ;; TODO make sure the direcory exists
  (loop :for index :from *from* :to *to*
        :for images = (get-images (get-page index))
        :do (loop :for image in images
                  :do (with-simple-restart
                          (skip "Skip this file")
                        (save-image image)))))

(defun print-usage (name)
  (format *error-output* "Usage: ~a <resolution> <directory> <from> <to>

<resolution> -- comma-separated list of resolutions for which images should be downloaded
<directory>  -- Directory into which images should be downloaded
<from>       -- Number of first index page from which images should be downloaded
<to>         -- Number of last index page from which images should be downloaded~%" name))

(defun main (argv)
  (let ((name (pop argv)))
    (when (not (= 4 (length argv)))
      (print-usage name)
      (sb-ext:quit :unix-status 1)))
  (destructuring-bind (resolution directory from to) argv
    (setf *from* (parse-integer from))
    (setf *to* (parse-integer to))
    (setf *directory* directory)
    (with-simple-restart (stop "Stop downloading")
      (dolist (dim (split-sequence #\, resolution :remove-empty-subseqs t))
        (with-simple-restart (next "Next dimension")
          (let ((*dimensions* dim))
            (image-loop)))))))
