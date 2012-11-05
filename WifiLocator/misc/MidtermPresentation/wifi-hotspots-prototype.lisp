; wifi-hotspots-prototype.lisp
; Simple set of functions to process the converted wifi-output, and give some bits of information about it
; Most prototyping, algorithmic wise, will likely happen here.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (progn 
    (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
					   (user-homedir-pathname))))
      (when (probe-file quicklisp-init)
	(load quicklisp-init)))
    (require :cl-ppcre)))

(defpackage :wifi-hotspots-prototype
  (:use :cl :cl-ppcre))

(in-package :wifi-hotspots-prototype)
