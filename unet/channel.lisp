;; Copyright (c) 2011 nklein software
;; MIT License. See included LICENSE.txt file for licensing details.

(in-package :unet)

;; ----------------------------------------------------------------------
;; recipient-class-list generic -- [PRIVATE]
;; ----------------------------------------------------------------------
(defgeneric recipient-class-list (class))

;; ----------------------------------------------------------------------
;; expand-recipient-class-list -- [PRIVATE]
;; ----------------------------------------------------------------------
(defun expand-recipient-class-list (classes)
  (when classes
    (append (recipient-class-list (first classes))
	    (expand-recipient-class-list (rest classes)))))

;; ----------------------------------------------------------------------
;; channel-base class -- [PRIVATE]
;; ----------------------------------------------------------------------
(defclass channel-base ()
  ((%incoming-queue :initform nil :accessor channel-incoming-queue)
   (%recipients :initform (make-hash-table :test #'equalp)
		:accessor channel-recipients)))

;; ----------------------------------------------------------------------
;; channel-add-recipient -- [PUBLIC]
;; ----------------------------------------------------------------------
(declaim (ftype (function (channel-base recipient) boolean)
		channel-add-recipient))
(defun channel-add-recipient (channel recipient)
  (unless (gethash recipient (channel-recipients channel))
    (let* ((channel-class (type-of channel))
	   (recipient-class (first (recipient-class-list channel-class)))
	   (channel-recipient (make-instance recipient-class
					     :recipient recipient)))
      (setf (gethash recipient (channel-recipients channel))
	    channel-recipient))
    t))

;; ----------------------------------------------------------------------
;; channel-remove-recipient -- [PUBLIC]
;; ----------------------------------------------------------------------
(declaim (ftype (function (channel-base recipient) t)
		channel-remove-recipient))
(defun channel-remove-recipient (channel recipient) 
  (remhash recipient (channel-recipients channel)))

;; ----------------------------------------------------------------------
;; channel-recipient-base class -- [PRIVATE]
;; ----------------------------------------------------------------------
(defclass channel-recipient-base ()
  ((%pending-packets :initform nil :accessor pending-packets)
   (%recipient :initarg :recipient :accessor channel-recipient-recipient)))

;; ----------------------------------------------------------------------
;; define-channel macro -- [PUBLIC]
;; ----------------------------------------------------------------------
(defmacro define-channel ((channel-class
			     (&rest channel-super-classes)
			     (&rest channel-slots)
			     &rest channel-options)
			  channel-recipient-class
			    (&rest channel-recipient-slots)
			    &rest channel-recipient-options)
  (let ((recipient-super-classes (apply #'append
					(mapcar #'recipient-class-list
						channel-super-classes))))
  `(progn
     (defclass ,channel-class (,@channel-super-classes unet::channel-base)
       ,channel-slots
       ,@channel-options)
     (defmethod recipient-class-list ((class (eql ',channel-class)))
       '(,channel-recipient-class ,@recipient-super-classes))
     (defclass ,channel-recipient-class (,@recipient-super-classes
					 unet::channel-recipient-base)
       ,channel-recipient-slots
       ,@channel-recipient-options))))

;; ----------------------------------------------------------------------
;; raw-channel channel class -- [PUBLIC]
;; ----------------------------------------------------------------------
(define-channel (raw-channel () ()) raw-recipient ())

;; ----------------------------------------------------------------------
;; prepare-packets generic -- [PUBLIC]
;; ----------------------------------------------------------------------
(defgeneric prepare-packets (channel channel-recipient payload
				     &key &allow-other-keys)
  (:documentation "Various channel mixins will implement :around methods for this method to add information to this packet or to do any per-packet processing that they need to do.  Any packets that need to be sent to the CHANNEL-RECIPIENT should be stored in the (PENDING-PACKETS CHANNEL-RECIPIENT) list.  Each packet is a USERIAL::BUFFER.  The PAYLOAD is also a USERIAL::BUFFER.

Note: There may be more than one packet to send because a mixin decided it also needs to resend other packets, ack previously received packets, chunk this payload up into multiple packets, or what-have-you, those packets should be stored in a list in the CHANNEL-RECIPIENT."))

;; ----------------------------------------------------------------------
;; handle-packet generic -- [PUBLIC]
;; ----------------------------------------------------------------------
(defgeneric message-for-channel-p (message channel)
  (:documentation "There can be multiple channels on a given server.  When the server receives a packet, it must decide which channel should get the packet.  This server invokes this method on each channel in turn until it finds one that will accept this message.  This method should return non-NIL if the given CHANNEL should get this MESSAGE."))

;; ----------------------------------------------------------------------
;; handle-packet generic -- [PUBLIC]
;; ----------------------------------------------------------------------
(defgeneric handle-packet (channel channel-recipient &key &allow-other-keys)
  (:documentation "Various channel mixins will implement :around methods for this method to extract information that their PREPARE-PACKET method added and do any per-packet processing needed.  Any packets that need to be sent back the the CHANNEL-RECIPIENT should be stored in then (PENDING-PACKETS CHANNEL-RECIPIENT) list.  The return value of this method should be a single payload as a USERIAL::BUFFER."))
