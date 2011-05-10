;; Copyright (c) 2011 nklein software
;; MIT License. See included LICENSE.txt file for licensing details.

(in-package :unet-example)

(defvar *servers* nil)
(defvar *addrs* (make-hash-table))

(defun open-server (port)
  (let ((server (make-instance 'unet:server :port port)))
    (setf *servers* (cons server *servers*)
          (gethash server *addrs*) (make-instance 'unet:recipient
                                                  :hostname "localhost"
                                                  :port port))
    server))

(defmacro defserver (name port)
  `(defparameter ,name (unet-example::open-server ,port)))

(defun close-server (server)
  (setf *servers* (remove server *servers*))
  (remhash server *addrs*)
  (unet:server-close server))

(defun close-all-servers ()
  (mapc #'unet:server-close *servers*)
  (setf *servers* nil))

(defun server-addr (server)
  (gethash server *addrs*))
