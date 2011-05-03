;; Copyright (c) 2011 nklein software
;; MIT License. See included LICENSE.txt file for licensing details.

(in-package :unet-example)

(defvar *servers* nil)

(defun open-server (port)
  (let ((server (make-instance 'unet:server :port port)))
    (setf *servers* (cons server *servers*))
    server))

(defmacro defserver (name port)
  `(defparameter ,name (unet-example::open-server ,port)))

(defun close-server (server)
  (setf *servers* (remove server *servers*))
  (unet:server-close server))

(defun close-all-servers ()
  (mapc #'unet:server-close *servers*)
  (setf *servers* nil))
