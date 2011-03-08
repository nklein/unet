;; Copyright (c) 2011 nklein software
;; MIT License. See included LICENSE.txt file for licensing details.

(in-package :unet)

(defclass channel ()
  ((socket     :initarg :socket     :reader channel-socket)
   (ordered    :initarg :ordered    :reader channel-ordered-p)
   (recipients :initarg :recipients :reader channel-recipients))
  (:default-initargs :socket (error "Must specify socket")
                     :ordered nil
                     :recipients nil))

(defun wrap-message (channel message recipient) ; XXX: need implementation
  (declare (ignore channel recipient))
  message)

(declaim (ftype (function (channel userial:buffer &key (recipients list))
			  (values))
		send-message))
(defun send-message (channel message &key (recipients nil recipients-p))
  (dolist (recipient (if recipients-p
			 recipients
			 (channel-recipients channel)))
    (iolib:send-to (channel-socket channel)
		   (wrap-message channel message recipient)
		   :remote-host (recipient-host recipient)
		   :remote-port (recipient-port recipient)))
  (values))
