;;; -*- mode: Lisp; mode: Allout -*-

;;; Author: Christian von Essen <christian@mvonessen.de>
;;; Script to collect comics and generate webpages from them
;;; Version: 0.1
;;; Features:
;;; * Quick default methods via xpath queries to get comics
;;; * Automatically store a comics on the disk (one per day)
;;; * Generate webpage from comics, only showing new ones (since day before)
;;; * web-navigation on comic archive

;;; Go to the end of the file for some example comic definitions

;;;_ Quickloads
(ql:quickload "drakma")
(ql:quickload "cl-libxml2")
(ql:quickload "cl-fad")
(ql:quickload "cl-who")
(ql:quickload "lisp-unit")
(ql:quickload "cl-ppcre")
(ql:quickload "split-sequence")

;;;_ Package declaration
(defpackage comics
  (:use cl drakma xpath cl-ppcre libxml2.tree cl-fad cl-who lisp-unit)
  (:export main))

(in-package comics)

;;;_ Global variables (also used for configuration)
(defparameter *comic-base* #P"/home/christian/comics/")
(defparameter *comic-config* #P "/home/christian/comics/comics.lisp")
(defparameter *url-base* "file:///home/christian/comics/")
(defparameter *comic-archive* nil)
(defparameter *comic-archive-file* "/home/christian/comics/archive.lisp")

;;;_ Comic specification
(defstruct comic url name retriever)

(defparameter *comics* nil "List of comics")

(defmacro comic (name url retriever)
  "Specify a new comic.

NAME - Name of the cmoics. Used to find it via FIND-COMIC.
URL - Url to link to and to scrape
RETRIEVER - function to call to find a strip.

The RETRIEVER should return the url of a strip (if any) as first
value.  Any other value is considered additional information and will
be displayed on the webpage.

For RETRIEVER examples look at XPATH and XPATH-FILTER."
  `(setq *comics*
         (append (remove-if (lambda (c) (string= (comic-name c) ,name)) *comics*)
                 (list (make-comic :name ,name :url ,url :retriever ',retriever)))))

(defun find-comic (name)
  "Find an already existing comic"
  (find name *comics* :key #'comic-name :test #'string=))

(defun fetch (comic)
  "Download a comic's site and apply its retriever on it."
  ;; This is a little wierd because we save source code (i.e., cons cells)
  ;; instead of compiled functions in our *comics* list.
  ;; We expect this source code to be a form that returns a function.
  ;; We expect this function to accept one argument.
  ;; Hence we 1. get the form from the comic
  ;;          2. Build a lambda expression calling this form
  ;;          3. Compile this lambda expression
  ;;          4. Call the compiled function
  (let* ((f (compile nil
                     `(lambda (comic)
                        (let ((retriever ,(comic-retriever comic)))
                          (funcall retriever comic))))))
    (funcall f comic)))

;;;_. Comic default retrievers
(defun get-html (url)
  "Download an url and return it as an libxml document"
  (html:parse-html (drakma:http-request url :user-agent :firefox)))

(defun xpath (xpath &rest attributes)
  "Build a retriever that searches for one single node specified by the xpath.
Returns the attributes as multiple values."
  (lambda (comic)
    (let* ((node (xpath:find-single-node (get-html (comic-url comic)) xpath)))
      (unless node (error "Could not find strip with xpath \"~A\"." xpath))
      (mapcar (lambda (attr) (libxml2.tree:attribute-value node attr)) attributes))))

(defun xpath-filter (xpath filter &rest attributes)
  "Build a retriever that searches for a list of nodes specified by the xpath.
This list is then searched for the first node that is accepted by FILTER.
Returns the attributes of the first match as multiple values."
  (lambda (comic)
    (let* ((imgs (xpath:find-list (get-html (comic-url comic)) xpath))
           (img
            (find-if filter imgs)))
      (unless img
        (error "Could not find strip with xpath \"~A\" and filter." xpath))
      (mapcar (lambda (attr) (libxml2.tree:attribute-value img attr)) attributes))))

;;;_ The comic archive
;;; The archive is an index of comics. It stores the information we know about
;;; our comics.
(defun universal-time->date (time)
  "Turn a univiersal time integer (as for example returned by GET-UNIVERSAL-TIME) into a date. See DATE->UNIVERSAL-TIME"
  (multiple-value-bind (second minute hour date month year dow daylight zone) (decode-universal-time time)
    (declare (ignore second minute hour dow daylight zone))
    (list year month date)))

(defun date->universal-time (date)
  "Turn a date into an universal time integer. See UNIVERSAL-TIME->DATE"
  (encode-universal-time 0 0 0 (third date) (second date) (first date)))

(defun current-date ()
  "Return the current date."
  (universal-time->date (get-universal-time)))

(defun previous-day (date)
  "Return the date before the given one."
  (let* ((universal (date->universal-time date))
         (yesterday (- universal (* 60 60 24))))
    (universal-time->date yesterday)))

(defun next-day (date)
  "Return the date after the given one."
  (let* ((universal (date->universal-time date))
         (yesterday (+ universal (* 60 60 24))))
    (universal-time->date yesterday)))

(defstruct archived-strip
  url date additional error)

(defun archive-strip (comic strip)
  "Put the given strip into the comic archive.

The given strip replaces an existing one."
  (declare (type comic comic))
  (declare (type archived-strip strip))
  (let ((archive (assoc (comic-name comic) *comic-archive* :test #'string=))
        (date (archived-strip-date strip)))
    (cond ((null archive)
           (push (cons (comic-name comic) (list strip)) *comic-archive*))
          ((equalp (archived-strip-date (cadr archive)) date)
           (setf (cadr archive) strip))
          (t
           (push strip (cdr archive))))))
  

(defun archive (comic url data)
  "Add the given comic (with url and additional data) to the archive."
  (declare (type string url))
  (declare (type comic comic))
  (archive-strip comic
                 (make-archived-strip :url url
                                      :date (current-date)
                                      :additional data
                                      :error nil)))

(defun archive-error (comic c)
  "Register the given condition as error for the given comic."
  (declare (type condition c))
  (let* ((message (format nil "~A" c))
         (strip (make-archived-strip :url nil
                                     :error message
                                     :date (current-date)
                                     :additional nil)))
    (archive-strip comic strip)))


(defun search-archive (comic)
  "Return the data saved for the comic"
  (cdr (assoc (comic-name comic) *comic-archive* :test #'string=)))

(defun search-archive-time (comic date)
  "Search for a comic strip for the specified date."
  (declare (optimize (debug 3)))
  (let ((strips (search-archive comic)))
    (when strips
      (find-if (lambda (strip) (equal (archived-strip-date strip) date)) strips))))

(defun save-archive ()
  "Save the archive in *COMIC-ARCHIVE-FILE*"
  (with-open-file (stream *comic-archive-file*
                          :direction :output :if-exists :overwrite :if-does-not-exist :create)
    (let ((*print-readably* t))
      (print *comic-archive* stream))))

(defun load-archive ()
  "Load the archive from *COMIC-ARCHIVE-FILE*."
  (with-open-file (stream *comic-archive-file*)
    (setf *comic-archive*
          (let ((*package* (find-package 'comics)))
            ;; We have to set the package here. Otherwise the reader doesn't know
            ;; about the structure archived-strip
            (read stream)))))

;;;_ Saving comics
(defun comic-dir (comic)
  "Return the directory a comic should be saved in.

Depends on global variable *COMIC-BASE*."
  (make-pathname :defaults *comic-base*
                 :directory (append (pathname-directory *comic-base*) (list (comic-name comic)))))

(defun comic-file-name (extension &optional (time (current-date)))
  "Constructs a filename from the current date and using the given extension."
  (destructuring-bind (year month date) time
    (format nil "~4,'0D-~2,'0D-~2,'0D.~a" year month date extension)))

;;; This condition is signaled by SAVE when the
;;; comic has already been saved according to our archives
(define-condition comic-saved (condition)
  ((comic :reader comic-saved-comic :initarg :comic)
   (url :reader comic-saved-url     :initarg :url))
  (:report (lambda (condition stream)
             (format stream "Comic ~a is already saved for url ~a."
                     (comic-name (comic-saved-comic condition))
                     (comic-saved-url condition)))))

(defun savedp (comic url)
  "Is the specified url already saved for the given comic."
  (some (lambda (data) (string= url (archived-strip-url data))) (search-archive comic)))

(defun url-extension (url)
  "Given an url that designates a filename, return its extension.
If the url has no filename, then an error is signaled.
If the url has no extension, NIL is returned. Otherwise it's extension is returned."
  (if (string= (subseq url (1- (length url))) "/")
      (error "Url has no filename.")
      (multiple-value-bind (filename stop)
          (split-sequence:split-sequence #\/ url :from-end t :count 1)
        (if (zerop stop)
            (error "Malformed url")
            (multiple-value-bind (extension stop)
                (split-sequence:split-sequence #\. (car filename) :from-end t :count 1)
              (if (zerop stop)
                  nil
                  (car extension)))))))

(define-test url-extension
  (assert-equalp "png" (url-extension "http://www.test.de/img.png"))
  (assert-false (url-extension "http://www.test.de/img"))
  (assert-error 'simple-error (url-extension "http://www.test.de/")))

(defun save (comic)
  "Save the comic for today.

If the comic already has been saved according to SAVEDP,
then a condition of type COMIC-SAVED is signalled.
In that case, two restarts are bound:
- OVERWRITE: save comic anywab
- ABORT: Don't save the comic
If no restart is invoked, SAVE behaves as if ABORT had been invoked.

Returns T if the comic has been saved, NIL otherwise."
  (handler-bind
      ((error (lambda (c)
                (archive-error comic c)
                (return-from save t))))
      (destructuring-bind (url &rest rest) (fetch comic)
        (when (and (not (null url)) (savedp comic url))
          (case
              (restart-case (signal 'comic-saved :comic comic :url url)
                (overwrite () :report "Overwrite the current comic" 'overwrite)
                (abort     () :report "Don't save the comic" 'abort))
            ((nil abort) (return-from save nil))
            (overwrite nil)))
        (let* ((dir (comic-dir comic))
               (extension (url-extension url))
               (file-name (comic-file-name extension))
               (path (make-pathname :defaults dir :name file-name))
               (stream (flexi-streams:flexi-stream-stream (http-request url :want-stream t))))
          (ensure-directories-exist dir)
          (with-open-file (out path :direction :output :if-does-not-exist :create
                               :if-exists :overwrite :element-type 'unsigned-byte)
            (copy-stream stream out)
            (archive comic url rest))
          #+sbcl (sb-posix:chmod path #b110100100)))
    t))

;;;_ Generate webpage
(defun strip-link (comic strip date)
  (concatenate 'string *url-base* "/"
               (comic-name comic) "/"
               (comic-file-name (url-extension (archived-strip-url strip)) date)))

(defun comic-paragraph (stream comic &optional (date (current-date)))
  "Write a paragraph to the stream about the comic.

If there is a strip for the given date in the archive, then a link
to that strip is included. Otherwise only its title is shown."
  (with-html-output (stream)
  (htm
   (:h1 (:a :href (comic-url comic) (str (comic-name comic))))
   (:p
    (let ((strip (search-archive-time comic date)))
      (cond
        ((not strip) (htm "No new comic found"))
        ((archived-strip-url strip)
         (htm
          (:img (:img :src (strip-link comic strip date)))
          (dolist (add (archived-strip-additional strip))
            (htm (:p (str add))))))
        (t
         (htm (:b "Error: " (str (archived-strip-error strip)))))))))))

(defun index-name (date)
  "Turn a date into a filename for a webpage."
  (if (equalp date (current-date))
      "index.html"
      (apply #'format nil "index-~4,'0d-~2,'0d-~2,'0d.html" date)))

(defun index-url (date)
  "Turn a date into a URL for comic webpage.

Influenced by *url-base*"
  (concatenate 'string *url-base* (index-name date)))

(defun index-file (date)
  "Turn a date into a path for the comics webpage.

Influence by *comic-base*"
  (format nil "~a/~a" *comic-base* (index-name date)))

(defun webpage (&optional filename (date (current-date)))
  "Write the webpage for the given date to the given filename.

If FILENAME is NIL, then it is constructed from DATE.
If DATE is NIL, it is assumed to be the CURRENT-DATE."
  (when (null filename)
    (setq filename (index-file date)))
  (with-open-file (stream filename :direction :output :if-does-not-exist :create :if-exists :overwrite)
    (with-html-output (stream)
      (htm
       (:html
        (:head (:title "Strips for " (str date)))
        (:body
         (:h1 "Strips for " (str date))
         (:a :href (index-url (previous-day date)) (str "Previous"))
         (unless (equalp date (current-date))
           (htm (:a :href (index-url (next-day date)) (str "Next")))))
         (loop :for comic :in *comics*
            :do (comic-paragraph stream comic date))))))
  #+sbcl (sb-posix:chmod filename #b110100100))

;;;_ Comic specifications

(comic "XKCD" "http://xkcd.org"
       (xpath "//img[@title]" "src" "title"))

(comic "QC" "http://www.questionablecontent.net"
       (xpath "id('strip')" "src"))

(comic "SMBC" "http://www.smbc-comics.com"
       (xpath-filter "//td[@class='comicboxcenter']//img"
                     (lambda (node) (search "/comics/" (attribute-value node "src"))) "src"))

(comic "Abstruse Goose" "http://abstrusegoose.com/"
       (xpath "//div[@class='storycontent']/p/img"  "src" "title"))

(comic "Sinfest" "http://www.sinfest.net"
       (xpath-filter "//img"
                     (lambda (node) (search "/comics" (attribute-value node "src"))) "src"))

(comic "PHD Comics" "http://www.phdcomics.com/comics.php"
       (xpath-filter "//table/tr/td/img"
                     (lambda (node) (search "/comics/archive" (attribute-value node "src"))) "src"))

(comic "CTRL-ALT-DEL" "http://www.cad-comic.com/cad/"
       (xpath-filter "//img"
                     (lambda (node) (search "/comics" (attribute-value node "src"))) "src"))

(comic "CTRL-ALT-DEL Sillies" "http://www.cad-comic.com/sillies/"
       (xpath-filter "//img"
                     (lambda (node) (search "/comics" (attribute-value node "src"))) "src"))

(comic "Nicht lustig" "http://www.nichtlustig.de/main.html"
       (xpath "//link[@rel='image_src']" "href"))

(comic "PVP" "http://www.pvponline.com"
       (xpath "//div[@id='comic']/img" "src" "title"))

(comic "Penny Arcade" "http://penny-arcade.com/comic"
       (xpath "//div[@class='post comic']/img" "src"))

;; No dilbert. What the fuck is UTF-8lias???
;;(comic "Dilbert" "http://www.dilbert.com"
;;       (xpath "//div[@class='STR_Image/a/img" "src"))

;;;_ Main
(defun write-comic-definition (comic stream)
  (let ((*print-readably* t))
    (print
     `(comic ,(comic-name comic)
             ,(comic-url comic)
             ,(comic-retriever comic))
     stream)))

(defun dump ()
  #+sbcl (sb-ext:save-lisp-and-die "comics" :executable t :toplevel #'(lambda () (main sb-ext:*posix-argv*)))
  #-sbcl (error "Can't dump. Running instance is not SBCL"))

(defun main (args)
  (when (probe-file *comic-archive-file*)
	  (load-archive))
  (let ((command (second args)))
    (cond
      ((string= command "save")
       (print "Parsing...") (terpri)
       (loop :for comic :in *comics*
          :do (when (save comic)
                (format t "Saved ~a.~%" (comic-name comic))))
       (save-archive))
      ((string= command "generate")
       (webpage nil (previous-day (current-date)))
       (webpage))
      ((string= command "show")
       (let ((*print-readably* nil))
         (mapc (lambda (comic) (write-comic-definition comic *standard-output*)) *comics*))
       (terpri))
      (t (format *error-output* "Unknown command: ~a." command))))
  nil)
