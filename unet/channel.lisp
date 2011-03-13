;; Copyright (c) 2011 nklein software
;; MIT License. See included LICENSE.txt file for licensing details.

(in-package :unet)

(defclass channel ()
  ((socket     :initarg :socket     :reader channel-socket)
   (channel-id :initarg :channel-id :reader channel-id)
   (recipients :initform (make-hash-table)))
  (:default-initargs :socket     (error "Must specify socket")
                     :channel-id (error "Must specify channel-id")
		     :recipients nil))

(defstruct channel-recipient
  (sequence-number 0 :type fixnum)
  (un-acked nil))

(declaim (ftype (function (channel recipient)
			  (values channel-recipient boolean))
		get-channel-recipient))
(defun get-channel-recipient (channel recipient)
  (declare (type channel channel)
	   (type recipient recipient))
  (with-slots (recipients) channel
    (multiple-value-bind (ch-rr present-p)
	(gethash recipient recipients (make-channel-recipient))
      (declare (type channel-recipient ch-rr))
      (unless present-p
	(setf (gethash recipient recipients) ch-rr))
      (values ch-rr present-p))))

(defmethod initialize-instance :after ((channel channel)
				       &key recipients &allow-other-keys)
  (mapc #'(lambda (rr)
	    (declare (type recipient rr))
	    (nth-value 0 (get-channel-recipient channel rr)))
	recipients))

(defgeneric wrap-chunk (channel message channel-recipient out))

(defmethod wrap-chunk :before ((channel channel) chunk channel-recipient out)
  (declare (type channel channel)
	   (type userial:buffer chunk out)
	   (type channel-recipient channel-recipient))
  (userial:serialize* (:channel-id      (channel-id channel)
		       :sequence-number (channel-recipient-sequence-number
					   channel-recipient))
		      :buffer out))

(defmethod wrap-chunk ((channel channel) chunk channel-recipient out)
  (declare (type channel channel)
	   (type userial:buffer chunk out)
	   (type channel-recipient channel-recipient))
  (userial:serialize* (:uint16 (userial:buffer-length :buffer chunk)
		       :bytes  chunk)
		      :buffer out))

(declaim (ftype (function (channel) list) channel-recipients))
(defun channel-recipients (channel)
  (maphash #'(lambda (key val)
	       (declare (ignore val))
	       key)
	   (slot-value channel 'recipients)))

(declaim (ftype (function (channel recipient) boolean)
		channel-add-recipient))
(defun channel-add-recipient (channel recipient)
  (declare (type channel channel)
	   (type recipient recipient))
  (multiple-value-bind (rr new-p)
      (get-channel-recipient channel recipient)
    (declare (ignore rr))
    new-p))

(declaim (ftype (function (channel recipient) boolean)
		channel-remove-recipient))
(defun channel-remove-recipient (channel recipient)
  (declare (type channel channel)
	   (type recipient recipient))
  (remhash recipient (slot-value channel 'recipients)))

(declaim (ftype (function (channel userial:buffer &key (:to list))
			  (values))
		send-message))
(defun send-message (channel message &key (to nil to-p))
  (declare (type channel channel)
	   (type userial:buffer message))
  (if to-p
      (mapc #'(lambda (rr)
		(declare (type recipient rr))
		(let ((ch-rr (get-channel-recipient channel rr)))
		  (iolib:send-to (channel-socket channel)
				 (wrap-chunk channel message ch-rr
					     (userial:make-buffer))
				 :remote-host (recipient-host rr)
				 :remote-port (recipient-port rr))))
	    to)
      (maphash #'(lambda (rr ch-rr)
		   (declare (type recipient rr)
			    (type channel-recipient ch-rr))
		   (iolib:send-to (channel-socket channel)
				  (wrap-chunk channel message ch-rr
					      (userial:make-buffer))
				  :remote-host (recipient-host rr)
				  :remote-port (recipient-port rr)))
	       (slot-value channel 'recipients)))
  (values))
