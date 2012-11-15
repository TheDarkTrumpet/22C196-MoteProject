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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Basic loading of files ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
	for loaded-file = (load-wifi-formatted-file file)
	while f
	collect (cons room_label loaded-file)))


(defun rangeify-range (strengths)
  (let ((smin (apply #'min strengths))
	(smax (apply #'max strengths)))
    `((min . ,smin)
      (max . ,smax))))

(defun assoc-files-to-room-range (loaded-afiles)
  "Given the output from load-assoc-files, we simply transform the file to be something like:
 (ROOM_NAME . ((MAC_ADDR_1 MIN_RANGE MAX_RANGE)
               (MAC_ADDR_2 MIN_RANGE ...)
               ...)
  ..)"
  (let* ((new-room-alist '()))
    (loop for room-alist in loaded-afiles
	  for room = (car room-alist)
	  for mac-addr-list = (cdr room-alist)
	  do
	     (push (cons room
			 (loop for m in mac-addr-list
			       for mac-addr = (car m)
			       for strengths = (cdr m)
			       collect (cons mac-addr (rangeify-range strengths))))
		   new-room-alist))
    new-room-alist))

;;;; MBA
;(defvar *room-alist-data-store*
;  '(("4505" . "/Users/dthole/programming/22C196-MoteProject/WifiLocator/misc/MidtermPresentation/data/wifiData_4505.reformatted.csv")
;    ("4511" . "/Users/dthole/programming/22C196-MoteProject/WifiLocator/misc/MidtermPresentation/data/wifiData_4511.reformatted.csv")
;    ("4th-floor-hallway" . "/Users/dthole/programming/22C196-MoteProject/WifiLocator/misc/MidtermPresentation/data/wifiData_hallway.reformatted.csv")))

;;;; Laptop
(defvar *room-alist-data-store*
    '(("4505" . "/home/dthole/programming/22C196-MoteProject/WifiLocator/misc/MidtermPresentation/data/wifiData_4505.reformatted.csv")
      ("4511" . "/home/dthole/programming/22C196-MoteProject/WifiLocator/misc/MidtermPresentation/data/wifiData_4511.reformatted.csv")
      ("4th-floor-hallway" . "/home/dthole/programming/22C196-MoteProject/WifiLocator/misc/MidtermPresentation/data/wifiData_hallway.reformatted.csv")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Classification Options ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *room-ranges* '())

(defun in-bucketp (sig mac-range)
  (let ((mr-min (cdr (assoc 'min mac-range)))
	(mr-max (cdr (assoc 'max mac-range))))
    (and (<= sig mr-min)
	 (>= sig mr-max))))

(defun classify-location (signal-list)
  "Given a signal-list in the format of:
 (MAC_ADDR_1 <signal_strength>
  MAC_ADDR_2 <signal_strength_2)"
  )
