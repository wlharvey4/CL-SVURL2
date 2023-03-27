;;; cl-svurl.lisp --- SVURL in Common Lisp
;;; Time-stamp: <2023-03-27 01:52:50 minilolh>

;;; Author: WLHarvey4
;;; Created: 2023-03-26
;;; Version: 0.0.2

;;; Commentary:

;;; Code:

(in-package :cl-svurl)

(defun extract-txts ()
  "Extract `-x' the three txt files:
- `saved'
- `used'
- `hosts'"
  (eval `(uiop:run-program '("tar" "-xf" ,*tarfile-name*
                             "-C" ,(uiop:unix-namestring *documents*)))))

(defun search-url (&key file url)
  "Search FILE for LINE.  Return the index into the file upon a successful match,
or NIL upon failure.  FILE should be one of:
- `saved'
- `used'
- `hosts'"
  (let* ((path (uiop:merge-pathnames* (make-pathname :name file :type "txt") *documents*))
         (text-ls (uiop:read-file-lines path)))
    (position url text-ls :test #'search)))

(defun add-url (&key file url)
  "Add LINE to the end of FILE.  FILE should be one of:
- `saved'
- `used'
` `hosts'"
  (let* ((path (uiop:merge-pathnames* (make-pathname :name file :type "txt") *documents*))
         (uri (quri:uri url))
         (base-uri (quri:make-uri :scheme (quri:uri-scheme uri)
                                  :host (quri:uri-host uri)
                                  :path (quri:uri-path uri))))
    (with-open-file (s path
                       :direction :output
                       :if-exists :append
                       :if-does-not-exist :error)
      (write-line (quri:render-uri base-uri) s))))

;;; cl-svurl.lisp ends here
