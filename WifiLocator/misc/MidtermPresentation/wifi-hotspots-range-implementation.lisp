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


;; (defun assoc-files-to-room-range (loaded-afiles)
;;   "Given the output from load-assoc-files, we simply transform the file to be something like:
;;  (ROOM_NAME . ((MAC_ADDR_1 MIN_RANGE MAX_RANGE)
;;                (MAC_ADDR_2 MIN_RANGE ...)
;;                ...)
;;   ..)"
;;   (let* ((new-room-alist '()))
;;     (loop for room-alist in loaded-afiles
;; 	  for room = (car room-alist)
;; 	  for mac-addr-
;; 
;;   )

(defvar *room-alist-data-store*
  '(("4505" . "/Users/dthole/programming/22C196-MoteProject/WifiLocator/misc/MidtermPresentation/data/wifiData_4505.reformatted.csv")
    ("4511" . "/Users/dthole/programming/22C196-MoteProject/WifiLocator/misc/MidtermPresentation/data/wifiData_4511.reformatted.csv")
    ("4th-floor-hallway" . "/Users/dthole/programming/22C196-MoteProject/WifiLocator/misc/MidtermPresentation/data/wifiData_hallway.reformatted.csv")))

