;;; cl-svurl2.asd --- ASD File for CL-SVURL System
;;; Time-stamp: <2023-03-27 01:13:06 minilolh>

;;; Author: LOLH
;;; Created: 2023-03-24
;;; Version 0.0.2

;;; Commentary:
;;; This is the system cl-svurl2, because there is already an CL-SVURL.
;;; This system uses ASDF and UIOP, whereas when I made CL-SVURL I had
;;; no experience or knowledge of these systems, and utilized CCL rather
;;; than trying to make it portable.
;;; The component `cl-flsave' removes duplicate files utilizing the
;;; `lolh.utils:filesz'
;;; This system renames the files, saves them into a tar file, and then
;;; deletes them from the directory.

;;; Code:

(defsystem "cl-svurl2"
  :description "SVURL implemented in Common Lisp."
  :version 0.0.2
  :author "wlharvey4"
  :depends-on ("lolh-utils" "quri")
  :components
  ((:module "src"
    :components
            ((:file "cl-svurl-package")
             (:file "cl-svurl-defs" :depends-on ("cl-svurl-package"))
             (:file "cl-flsave" :depends-on ("cl-svurl-defs" "cl-svurl-package"))
             (:file "cl-svurl" :depends-on ("cl-svurl-defs" "cl-svurl-package"))))))

;;; cl-svurl.asd ends here
