;;; cl-flsave.lisp
;;; Time-stamp: <2023-03-26 10:40:24 wlh>

;;; Commentary:
;;; The code here deals with saving certain files in a directory into
;;; a tar file as sequenced files.  There is one exported procedures:
;;; - flsave &key :dir :type :v => executes dedup, rename, tar, delete
;;; functions in order.  The :v option will show output; otherwise
;;; there is a single line output showing how many bytes were written.

;;; Code:

(defpackage :svurl
  (:use :cl
   :lolh.utils)
  (:export
   :flsave))
(in-package :svurl)

(defparameter *home* (uiop:getenv-absolute-directory "HOME"))
(defparameter *downloads* (uiop:merge-pathnames*
                           (uiop:parse-unix-namestring "Downloads/")
                           *home*))
(defparameter *documents* (uiop:merge-pathnames*
                           (uiop:parse-unix-namestring "Documents/")
                           *home*))
(defparameter *tarfile* (uiop:merge-pathnames*
                         (uiop:parse-unix-namestring "files.tar")
                         *documents*))
(defparameter *tarfile-name* (uiop:unix-namestring *tarfile*))
(defparameter *jpg* "*.jpg")

(defun files-with-type (&key (dir *downloads*) (type *jpg*) (name nil) (name-only nil))
  "Return a list of pathnames or namestrings with the given type
from a directory.
If :name is nil (the default) return pathnames; if t, return namestrings.
If :name-only is nil, return absolute namestrings; if t, return file-name only."
  (let ((files (uiop:directory-files dir type))
	(func (cond
		((when (and name name-only) #'file-namestring))
		((when name #'namestring))
		((when name-only #'pathname-name))
		(t #'identity))))
    (mapcar func files)))

(defun tar-list ()
  "Start an asynchronous tar process to list all of the files onto a stream."
  (eval
   `(uiop:launch-program '("tar" "-tf" ,*tarfile-name*)
			 :output :stream)))

(defun next-num ()
  "Determine the next number to use by finding the last file number
in the tar file and adding 1.  If the tar file does not exist, create
it and return 0."
  (unless (uiop:file-exists-p *tarfile*)
    (eval `(uiop:run-program '("touch" ,*tarfile-name*)))
    (return-from next-num 0))
  (1+ (values
       (parse-integer
        (uiop:run-program '("tail" "-n" "1")
			  :input (uiop:process-info-output (tar-list))
			  :output :string)
        :end 6))))

(defun dedup-files (&key (dir *downloads*) (type *jpg*) (v nil))
  "Remove duplicate files from a directory using lolh-utils:filesz."
  (if v (format t "~2&Deduping ~A.~%" dir))
  (let* ((files (files-with-type :dir dir :type type))
	 (dedup (lolh.utils:filesz-load-and-dedup files)))
    (when (> (length files)
	     (length dedup))
      (loop for f in (nset-difference files dedup :test #'eql)
	    do (delete-file f)
	       (if v (format t "~A deleted...~%" f))))
    (if v (format t "~&Dir ~A has been deduped." dir))))

(defun rename-files (&key (dir *downloads*) (type *jpg*) (v nil))
  "Rename the files in the given directory as numbered files."
  (if v (format t "~2&Renaming files in ~A." dir))
  (let ((files (files-with-type :dir dir :type type)))
    (loop for file in files
          for c upfrom (next-num)
          for newfile = (format nil "~6,'0D.~A" c (string-left-trim "*." type))
          do (uiop:rename-file-overwriting-target file newfile)
             (if v (format t "~&Renamed ~A to ~A~%" file newfile)))
    (if v (format t "~&Dir ~A has been renamed." dir))))

(defun tar-files (&key (dir *downloads*) (type *jpg*) (v nil))
  "Save files of type into a tar file."
  (if v (format t "~2&Tarring files in ~A.~%" dir))
  (let ((files (files-with-type :dir dir :type type :name t :name-only t))
        (c (format nil "-C~A" *downloads*))
	(o (if v "-vrf" "-rf")))
    (eval `(uiop:run-program '("tar" ,o ,*tarfile-name*
                               "--totals"
                               ,c
                               ,@files)
                             :error-output t))
    (if v (format t "~&Dir ~A has been tarred." dir))))

(defun delete-files (&key (dir *downloads*) (type *jpg*) (v nil))
  "Delete all of the files in the directory of type.~%"
  (if v (format t "~2&Deleting files in ~A." dir))
  (let ((files (files-with-type :dir dir :type type :name t))
	(o (if v "-vf" "-f")))
    (eval `(uiop:run-program '("rm" ,o ,@files)
			     :output t
			     :error-output t)))
  (if v (format t "~&The files ~A~A have been deleted.~2%" dir type)))

(defun flsave (&key (dir *downloads*) (type *jpg*) (v nil))
  "Execute the three procedures to process the files in the directory."
  (if v (format t "~&Saving files in ~A." dir))
  (dedup-files :dir dir :type type :v v)
  (rename-files :dir dir :type type :v v)
  (tar-files :dir dir :type type :v v)
  (delete-files :dir dir :type type :v v))

;;; cl-flsave.lisp ends here
