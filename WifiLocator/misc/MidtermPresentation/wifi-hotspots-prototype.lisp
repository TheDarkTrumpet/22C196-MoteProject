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


;;;;; Loading the formatted file ;;;;;;;

(defun load-wifi-formatted-file (file)
  "Loads the file passed in, returns an ALIST of WIFI mac addresses, and the abs signal strength"
  (let ((mac-addr-db '()))
    (with-open-file (f file :direction :input)
      (loop for l = (read-line f nil nil)
	    for l-split = (split "\\\t" l)
	    for mac-addr = (first l-split)
	    for signal-str = (second l-split)
	    while l
	    do
	       (if (null (cdr (assoc mac-addr mac-addr-db :test #'string-equal)))
		   (push (cons mac-addr (list (abs (read-from-string signal-str))))
			 mac-addr-db)
		   (push (abs (read-from-string signal-str))
			 (cdr (assoc mac-addr mac-addr-db :test #'string-equal))))))
    mac-addr-db))


;;;;;; Outputs an alternate formatted file ;;;;;;;;

(defun output-transposed-wifi-file (infile outfile)
  "Given an infile, and outfile, we'll reformat the file from being:
MAC_ADDR <strength>
...
TO.....
MAC_ADDR <strength_1> <strength_2>"
  (let ((loaded-wifi-file (load-wifi-formatted-file infile)))
    (with-open-file (o outfile :direction :output :if-does-not-exist :create :if-exists :supersede)
      (loop for element in loaded-wifi-file
	    for mac-addr = (car element)
	    for strengths = (cdr element)
	    do
	       (progn
		 (format o "~a	" mac-addr)
		 (format o "~{~a~^	~}~%" strengths))))))
