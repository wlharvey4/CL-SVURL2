;;; cl-svurl-defs.lisp --- Definitions
;;; Time-stamp: <2023-03-27 23:42:36 minilolh>

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

(defparameter *jpg* "*.jpg")

(defparameter *u* (uiop:ensure-pathname
                   (uiop:merge-pathnames*
                     (make-pathname :directory '(:relative "U"))
                     (uiop:temporary-directory))
                   :want-directory t ; has no file component
                   :ensure-directory t ; ends in a directory component
                   :ensure-directories-exist t))

(defparameter *tarfile* (uiop:merge-pathnames*
                         (make-pathname :name "files" :type "tar")
                         *documents*))
(defparameter *tarfile-name* (uiop:unix-namestring *tarfile*))

(defparameter *txt-files* '("saved" "used" "hosts"))
(defparameter *txt-files-names* (mapcar
                                     (lambda (f)
                                       (uiop:unix-namestring
                                        (make-pathname :name f :type "txt")))
                                     *txt-files*))

;;; cl-svurl-defs.lisp ends here
