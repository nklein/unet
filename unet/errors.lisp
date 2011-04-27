;; Copyright (c) 2011 nklein software
;; MIT License. See included LICENSE.txt file for licensing details.

(in-package :unet)

;; ======================================================================
;; invalid-hostname exception
;; ======================================================================
(define-condition invalid-hostname-error (error)
  ((given :initarg :given :reader invalid-hostname-given))
  (:documentation "Error condition raised when a hostname is invalid"))

(defmethod print-object ((exception invalid-hostname-error) stream)
  (print-unreadable-object (exception stream :type t)
    (format stream ":GIVEN ~S"
	           (invalid-hostname-given exception))))

;; ======================================================================
;; no-hostname-given-error
;; ======================================================================
(define-condition no-hostname-given-error (invalid-hostname-error)
  ()
  (:documentation "Error condition raised when no hostname was specified"))

(defmethod initialize-instance :after ((exception no-hostname-given-error)
                                       &key &allow-other-keys)
  (log-it :no-hostname-given-error))

;; ======================================================================
;; no-such-host-error
;; ======================================================================
(define-condition no-such-host-error (invalid-hostname-error)
  ()
  (:documentation "Error condition raised when no address is found for a hostname"))

(defmethod initialize-instance :after ((exception no-such-host-error)
                                       &key &allow-other-keys)
  (log-it :no-such-host-error :string (invalid-hostname-given exception)))

;; ======================================================================
;; transient-name-service-error
;; ======================================================================
(define-condition transient-name-service-error (invalid-hostname-error)
  ()
  (:documentation "Error condition raised when the naming service has a transient failure"))

(defmethod initialize-instance :after ((exception transient-name-service-error)
                                       &key &allow-other-keys)
  (log-it :transient-name-service-error
          :string (invalid-hostname-given exception)))

;; ======================================================================
;; permanent-name-service-error
;; ======================================================================
(define-condition permanent-name-service-error (error)
  ()
  (:documentation "Error condition raised when the naming service is unable to resolve any hostnames at all"))

(defmethod initialize-instance :after ((exception permanent-name-service-error)
                                       &key &allow-other-keys)
  (log-it :permanent-name-service-error))

(defmethod print-object ((exception permanent-name-service-error) stream)
  (print-unreadable-object (exception stream :type t)))

;; ======================================================================
;; invalid-port exception
;; ======================================================================
(define-condition invalid-port-error (type-error)
  ()
  (:documentation "Error condition raised when a port number is invalid")
  (:default-initargs :expected-type '(unsigned-byte 16)))

(defun invalid-port-datum (exception)
  (type-error-datum exception))

(defun invalid-port-expected-type (exception)
  (type-error-expected-type exception))

(defmethod initialize-instance :after ((exception invalid-port-error)
                                       &key datum &allow-other-keys)
  (log-it :invalid-port-error
          :string (format nil "~A" (type-of datum))
          :string (format nil "~A" datum)))

(defmethod print-object ((exception invalid-port-error) stream)
  (print-unreadable-object (exception stream :type t)
    (format stream ":DATUM ~S :EXPECTED-TYPE ~S"
	           (invalid-port-datum exception)
		   (invalid-port-expected-type exception))))

;; ======================================================================
;; recipient-not-on-channel exception
;; ======================================================================
(define-condition recipient-not-on-channel-error (error)
  ((channel :initarg :channel :reader recipient-not-on-channel-channel)
   (recipient :initarg :recipient :reader recipient-not-on-channel-recipient))
  (:documentation "Error condition raised when trying to send a message to or receive a message from a recipient on a channel the recipient has not been added to."))

(defmethod initialize-instance :after ((err recipient-not-on-channel-error)
                                       &key channel recipient
                                       &allow-other-keys)
  (log-it :recipient-not-on-channel-error
          :unet-recipient recipient
          :unet-channel   channel))

(defmethod print-object ((exception recipient-not-on-channel-error) stream)
  (print-unreadable-object (exception stream :type t)
    (format stream ":CHANNEL ~S :RECIPIENT ~S"
	           (recipient-not-on-channel-channel exception)
		   (recipient-not-on-channel-recipient exception))))
