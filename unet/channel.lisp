;; Copyright (c) 2011 nklein software
;; MIT License. See included LICENSE.txt file for licensing details.

(in-package :unet)

;; ----------------------------------------------------------------------
;; define-channel
;; ----------------------------------------------------------------------
(defmacro define-channel (channel-class channel-recipient-class
			  (&rest super-classes)
			  (&rest slots)
			  &rest options)
  `(progn
     (defclass ,channel-class ,super-classes ,slots ,@options)
     (make-channel-recipient-class ,super-classes ,channel-recipient-class)
     (defmethod recipient-type ((channel-type (eql ',channel-class)))
       ',channel-recipient-class)))

;; ----------------------------------------------------------------------
;; recipient-type method used to prepare base-types for recipients
;; ----------------------------------------------------------------------
(defgeneric recipient-type (channel-type))
(defmethod recipient-type ((channel-type (eql 'channel)))
  'channel-recipient)

;; ----------------------------------------------------------------------
;; make-channel-recipient-class
;; ----------------------------------------------------------------------
(defmacro make-channel-recipient-class ((&rest super-classes)
					channel-recipient-class)
  `(defclass ,channel-recipient-class ,(mapcar #'recipient-type super-classes)
     ()))

;; ======================================================================
;; channel class
;; ======================================================================
(define-channel channel channel-recipient ()
  ((payload-key :initarg :payload-key)
   (recipients :initform (make-hash-table) :accessor channel-recipients))
  (:default-initargs :payload-key :bytes))

(defmethod print-object ((obj channel) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (with-slots (payload-key recipients) obj
      (format stream ":PAYLOAD-KEY ~S :RECIPIENTS ~S"
	      payload-key recipients))))

(defmethod print-object ((obj channel-recipient) stream)
  (print-unreadable-object (obj stream :type t :identity t)))

;; ----------------------------------------------------------------------
;; prepare-packet and handle-packet generics
;; ----------------------------------------------------------------------
(defgeneric prepare-packet (channel recipient payload buffer))
(defgeneric parse-packet (channel sender buffer))

;; ----------------------------------------------------------------------
;; base method for prepare-packet
;; ----------------------------------------------------------------------
(defmethod prepare-packet (channel recipient payload buffer)
  (declare (ignore recipient))
  (with-slots (payload-key) channel
    (userial:serialize payload-key payload :buffer buffer)))

;; ----------------------------------------------------------------------
;; base method for handle
;; ----------------------------------------------------------------------
(defmethod parse-packet (channel sender buffer)
  (declare (ignore sender))
  (with-slots (payload-key) channel
    (userial:unserialize payload-key :buffer buffer)))
