;; Copyright (c) 2011 nklein software
;; MIT License. See included LICENSE.txt file for licensing details.

(in-package :unet)

(define-condition invalid-hostname (error)
  ((got :initarg :got)))

(defmethod print-object ((exception invalid-hostname) stream)
  (print-unreadable-object (exception stream :type t)
    (format stream ":GOT ~A" (slot-value exception 'got))))

(defclass recipient ()
  ((host :initform (error 'invalid-hostname :got nil))
   (port :initarg :port))
  (:default-initargs :port 26354))

(defmethod initialize-instance :before ((recipient recipient)
					&key (hostname nil hostname-p))
  (when hostname-p
    (handler-case
	(setf (slot-value recipient 'host) (iolib:ensure-hostname hostname))
      (t () (error 'invalid-hostname :got hostname)))))
