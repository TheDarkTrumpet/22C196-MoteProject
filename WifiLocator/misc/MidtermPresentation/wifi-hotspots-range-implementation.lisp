(eval-when (:compile-toplevel :load-toplevel :execute)
  (progn 
    (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
					   (user-homedir-pathname))))
      (when (probe-file quicklisp-init)
	(load quicklisp-init)))
    (require :cl-ppcre)))

(defpackage :wifi-classifier-prototype
  (:use :cl :cl-ppcre))

(in-package :wifi-classifier-prototype)

; Basic loading of files
(defun load-assoc-files (filelist)
  "Given a list of files, in the format of:
ROOM_NAME . FILE_NAME
we'll return an ALIST such that it's:
 (ROOM_NAME . ((MAC_ADDR . '(LIST_OF_ADDRS))
               (...)))
"
  (loop for f in filelist
	for room_label = (car f)
	for file = (cdr f)
	for loaded-file = (load-wifi-formatted-file ffile)
	while f
	collect (cons room_label loaded-file)))


