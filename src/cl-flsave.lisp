;;; cl-flsave.lisp
;;; Time-stamp: <2023-03-25 00:19:00 minilolh>

;;; Commentary:
;;; The code here deals with saving certain files in a directory into
;;; a tar file as sequenced files.  There are three exported procedures:
;;; - rename-files &key :dir :type => renames the targeted files into a sequence
;;; - tar-files &key :dir :type => saves the sequenced files into a tar file
;;; - flsave &key :dir :type => executes both of the foregoing procedures

;;; Code:

(defpackage :svurl
  (:use :cl)
  (:export
   :rename-files
   :tar-files
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
(defconstant *jpg* "*.jpg")

(defun files-with-type (&key (dir *downloads*) (type *jpg*) (name nil) (name-only nil))
  "Return a list of pathnames or namestrings with the given type from a directory.
If :name is nil (the default) return pathnames; if t, return namestrings.
If :name-only is nil, return absolute namestrings; if t, return file-name only."
  (let ((jpgfiles (uiop:directory-files dir type)))
    (when (and name name-only)
      (return-from files-with-type (mapcar #'file-namestring jpgfiles)))
    (when name
      (return-from files-with-type (mapcar #'namestring jpgfiles)))
    (when name-only
      (return-from files-with-type (mapcar #'pathname-name jpgfiles)))
    (when (not (or name name-only))
      jpgfiles)))

(defun tar-list ()
  "Start an asynchronous tar process to list all of the files onto a stream."
  (eval
   `(uiop:launch-program
     '("tar" "-tf" ,*tarfile-name*)
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
        (uiop:run-program
         '("tail" "-n" "1")
         :input (uiop:process-info-output (tar-list))
         :output :string)
        :end 6))))

(defun depup-files (&key (dir *downloads*) (type *jpg*))
  "Not yet implemented.")

(defun rename-files (&key (dir *downloads*) (type *jpg*))
  "Rename the files in the given directory as numbered files."
  (let ((files (files-with-type :dir dir :type type)))
    (loop for file in files
          for c upfrom (next-num)
          for newfile = (format nil "~6,'0D.~A" c (string-left-trim "*." type))
          do (uiop:rename-file-overwriting-target file newfile)
             (format t "Renamed ~A to ~A~%" file newfile))))

(defun tar-files (&key (dir *downloads*) (type *jpg*))
  "Save files of type into a tar file."
  (let ((files (files-with-type :dir dir :type type :name t :name-only t))
        (c (format nil "-C~A" *downloads*)))
    (eval `(uiop:run-program '("tar" "-vrf" ,*tarfile-name*
                               "--totals"
                               ,c
                               ,@files)
                             :error-output t))))

(defun flsave (&key (dir *downloads*) (type *jpg*))
  "Execute the three procedures to process the files in the directory."
  (depup-files :dir dir :type type)
  (rename-files :dir dir :type type)
  (tar-files :dir dir :type type))

;;; cl-flsave.lisp ends here
