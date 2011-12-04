;;;; shelisp: unix shell interface for CommonLisp
;;;; Copyright (c) 2003-2006 Alexandru Dan Corlan MD PhD (http://dan.corlan.net)
;;;; 

;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License version 2, as published by
;;;; the Free Software Foundation.

;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.

;;;; You should have received a copy of the GNU General Public License
;;;; along with this program; if not, write to the Free Software
;;;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;;;; HISTORY
;;;; created: july 4, 2006
;;;; v2:      august 20, 2006
;;;;          #[ ]# syntax for template strings; eval expressions cand
;;;;          be added with ?expr
;;;;          read-preserving-whitespace now used in '?', cleaner results
;;;;          wrappers for: tex gs ls pwd mktemp dirname basename md5sum
;;;;          recursive version of md5sum that also works for directories
;;;; v2.1:    august 14, 2007
;;;;          sbcl compatibility
;;;;
;;;;          Made lines-to-list significantly faster (use loop instead of recursion
;;;;          Let mixed-script use mixed-template
;;;;          Converted several calls of the form (script (format ...))
;;;;            to the reader macro form
;;;;          Removed several occurences of the dangling paranthesis
;;;;          Created a special variable *shell* that gets executed in the
;;;;            script command

(defpackage shelisp
  (:use cl trivial-shell)
  (:nicknames sl)
  (:export lines-to-list script *shell* enable script))

(in-package shelisp)

(defvar *shelisp-temp-directory* "/tmp/"
  "Directory where all temporary files are created.")

(defparameter *shell* "/bin/sh"
  "Program to use to execute shell commands.")

(defun script (str &key (program *shell*))
  "Execute the STR string as a script of the program, with the eventual options,
   and return the standard-output of the command as a string."
  (shell-command program :input str))

(defun mixed-template (&rest strlist)
  "Execute the STR string as a script of the program, with the eventual options,
   and return the standard-output of the command as a string."
  (let ((evs (apply #'concatenate 'string
                    (mapcar #'(lambda (x)
                                (format nil "~A" x))
                            strlist))))
    evs))

(defun mixed-script (&rest strlist)
  "Execute the string as a script of the program, with the eventual options,
   and return the standard-output of the command as a string."
  (script (apply #'mixed-template strlist)))

(defun lines-to-list (text)
  "Transform the string TEXT into a list of strings, each representing
   on line of TEXT. This is suitable to postprocessing the standard output
   of many Unix commands (such as find or df) that return one result
   per line."
  (loop with from-ptr = 0
        for to-ptr from 0 below (length text)
        when (char= (elt text to-ptr) #\newline)
        collect
          (prog1
              (subseq text from-ptr to-ptr)
            (setf from-ptr (1+ to-ptr)))))

(defun read-script-list (str end-char)
  "This is the 'delayed' read macro, that provides an
   expression that will be evaluated at eval time."
  (do ((bu (make-string 100000 :initial-element #\Space))
                                        ; bu should be smaller and growing
       (bul 0)
       (mixl nil))
      (nil)
    (setf (elt bu bul) (read-char str))
    (cond
      ((eql (elt bu bul) end-char)
       (if (or (eql bul 0) (not (eql (elt bu (1- bul)) #\\)))
           (progn
             (setf mixl (cons (subseq bu 0 bul) mixl))
             (return-from read-script-list (reverse mixl)))
           (setf (elt bu (1- bul)) end-char)))
      ((eql (elt bu bul) #\?)
       (if (or (eql bul 0) (not (eql (elt bu (1- bul)) #\\)))
           (let ((form  (read-preserving-whitespace str)))
             (setf mixl (cons (subseq bu 0 bul) mixl))
             (setf mixl (cons form mixl))
             (setf bul 0))
           (setf (elt bu (1- bul)) #\?)))
      (t (incf bul)))))

(defun read-template-list (str end-char-1 end-char-2)
  " This is the 'delayed' read macro, that provides an
    expression that will be evaluated at eval time.
    problem: must ungetc the space after the ? expression! "
  (do ((bu (make-string 100000 :initial-element #\Space))
       (bul 0)
       (mixl nil))
      (nil)
    (setf (elt bu bul) (read-char str))
    (cond
      ((and (> bul 2) (eql (elt bu bul) end-char-2) (eql (elt bu (1- bul)) end-char-1))
       (setf mixl (cons (subseq bu 0 (1- bul)) mixl))
       (return-from read-template-list (reverse mixl)))
      ((eql (elt bu bul) #\?)
       (if (or (eql bul 0) (not (eql (elt bu (1- bul)) #\\)))
           (let ((form  (read-preserving-whitespace str)))
             (setf mixl (cons (subseq bu 0 bul) mixl))
             (setf mixl (cons form mixl))
             (setf bul 0))
           (setf (elt bu (1- bul)) #\?)))
      (t (incf bul)))))

(defun read-script-line (str end-char)
  "This is the 'direct' read macro, that executes the embedded
   expressions at read time."
  (do ((bu (make-string 1000 :initial-element #\Space))
       (bul 0))
      (nil)
    (setf (elt bu bul) (read-char str))
    (cond
      ((eql (elt bu bul) end-char)
       (if (or (eql bul 0) (not (eql (elt bu (1- bul)) #\\)))
           (return-from read-script-line (subseq bu 0 bul))
           (setf (elt bu (1- bul)) end-char)))
      ((eql (elt bu bul) #\?)
       (if (or (eql bul 0) (not (eql (elt bu (1- bul)) #\\)))
           (let ((form (format nil "~A" (eval (read-preserving-whitespace str)))))
             (replace bu form :start1 bul)
             (incf bul (length form)))
           (setf (elt bu (1- bul)) #\?)))
      (t (incf bul)))))

(defun enter-shell-mode (stream)
  "Read and execute successive shell commands, with eventual
   lisp expressions embedded. Expressions are evaluated at
   read time, as soon as a line is delivered. Implements the !! macro."

  (do () (nil)
    (princ "$ " *standard-output*)
    (let ((ll (read-script-line stream #\Newline)))
      (when (and (> (length ll) 1) (string= (subseq ll 0 2) "!!"))
        (return-from enter-shell-mode))
      (princ (script ll)))))

(defun simple-shell-escape-reader (stream char)
  (declare (ignore char))
  (let ((ll (read-script-line stream #\Newline)))
    (when (and (> (length ll) 0) (string= (subseq ll 0 1) "!"))
      (enter-shell-mode stream)
      (return-from simple-shell-escape-reader))
    (princ (script ll)))
  nil)

(defun embedded-shell-escape-reader (stream char)
  (declare (ignore char))
  (cons 'mixed-script (read-script-list stream #\] )))

(defun template-escape-reader (stream char1 char2)
  (declare (ignore char1 char2))
  (cons 'mixed-template (read-template-list stream #\] #\#)))

(defun storable-template-escape-reader (stream char1 char2)
  (declare (ignore char1 char2))
  (list 'quote (cons 'mixed-template (read-template-list stream #\} #\#))))


(defun enable (&optional (copy-readtable t))
  (when copy-readtable
    (setf *readtable* (copy-readtable)))
  (set-macro-character #\! #'simple-shell-escape-reader nil)
  (set-macro-character #\[ #'embedded-shell-escape-reader nil)
  (set-dispatch-macro-character #\# #\[ #'template-escape-reader)
  (set-dispatch-macro-character #\# #\{ #'storable-template-escape-reader))