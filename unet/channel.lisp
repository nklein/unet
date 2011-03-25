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
  ())

;; ----------------------------------------------------------------------
;; channel-recipient-base class -- [PRIVATE]
;; ----------------------------------------------------------------------
(defclass channel-recipient-base ()
  ((%pending-packets :initform nil :accessor pending-packets)))

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
;; prepare-packets generic -- [PUBLIC]
;; ----------------------------------------------------------------------
(defgeneric prepare-packets (channel channel-recipient payload)
  (:documentation "Various channel mixins will implement :around methods for this method to add information to this packet or to do any per-packet processing that they need to do.  Any packets that need to be sent to the CHANNEL-RECIPIENT should be stored in the (PENDING-PACKETS CHANNEL-RECIPIENT) list.  Each packet is a USERIAL::BUFFER.  The PAYLOAD is also a USERIAL::BUFFER.

Note: There may be more than one packet to send because a mixin decided it also needs to resend other packets, ack previously received packets, chunk this payload up into multiple packets, or what-have-you, those packets should be stored in a list in the CHANNEL-RECIPIENT."))

;; ----------------------------------------------------------------------
;; handle-packet generic -- [PUBLIC]
;; ----------------------------------------------------------------------
(defgeneric handle-packet (channel channel-recipient)
  (:documentation "Various channel mixins will implement :around methods for this method to extract information that their PREPARE-PACKET method added and do any per-packet processing needed.  Any packets that need to be sent back the the CHANNEL-RECIPIENT should be stored in then (PENDING-PACKETS CHANNEL-RECIPIENT) list.  The return value of this method should be a single payload as a USERIAL::BUFFER."))
