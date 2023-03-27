;;; cl-svurl-defs.lisp --- Definitions
;;; Time-stamp: <2023-03-26 23:04:06 minilolh>

;;; Commentary:

;;; Code:

(in-package :cl-svurl)

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
(defparameter *txt-files* '("saved" "used" "hosts"))
(defparameter *txt-files-pathnames* (mapcar
                                     (lambda (f)
                                       (uiop:unix-namestring
                                        (uiop:merge-pathnames*
                                         (make-pathname :name f :type "txt")
                                         *documents*))) *txt-files*))
(defparameter *jpg* "*.jpg")


;;; cl-svurl-defs.lisp ends here
