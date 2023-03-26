;;; cl-svurl2.asd --- ASD File for CL-SVURL System
;;; Time-stamp: <2023-03-26 10:45:52 wlh>

;;; Author: LOLH
;;; Created: 2023-03-24
;;; Version 0.0.2

;;; Commentary:
;;; This is the system cl-svurl2, because there is already an CL-SVURL.
;;; This system uses ASDF and UIOP, whereas when I made CL-SVURL I had
;;; no experience or knowledge of these systems, and utilized CCL rather
;;; than trying to make it portable.
;;; The component cl-flsave removes duplicate fils (not yet implemented),
;;; renames files, and then saves files into a tar file.

;;; Code:

(defsystem "cl-svurl2"
  :description "SVURL implemented in Common Lisp."
  :version 0.0.2
  :author "wlharvey4"
  :depends-on ("lolh-utils")
  :components
  ((:module "src"
    :components
            ((:file "cl-flsave")))))

;;; cl-svurl.asd ends here
