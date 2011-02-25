(defpackage utils
  (:use cl cl-fad)
  (:export ls path-/))

(in-package utils)

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