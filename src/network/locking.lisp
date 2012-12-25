;;;; This file provides a wrapping class that makes a network interface
;;;; thread-safe.

(defpackage :unet-network-locking
  (:use :cl)
  (:export :network-provider)
  (:import-from :unet-network
                :make-remote-address
                :create-datagram-socket
                :send-datagram
                :recv-datagram
                :close-socket))

;;; package used to intern addresses
(defpackage :unet-network-locking)

;;; I love Hannah.
(in-package :unet-network-locking)

;;; Mock network provider class
(defclass network-provider ()
  ((provider :initarg :provider :reader get-provider)
   (mutex :initform (bordeaux-threads:make-lock) :reader get-mutex))
  (:documentation "Network provider wrapper classes which uses a global lock to ensure thread-safety."))

(defclass network-address ()
  ((provider :initarg :provider :reader get-provider)
   (address  :initarg :address  :reader get-address))
  (:documentation "Wrapper for address which uses the provider's mutex"))

(defclass network-socket ()
  ((provider :initarg :provider :reader get-provider)
   (socket   :initarg :socket   :reader get-socket))
  (:documentation "Wrapper for socket which uses the provider's mutex"))

;;; Macro to mutex-lock a provider for some body
(defmacro with-provider-guard ((provider) &body body)
  (let ((mutex-var (gensym "MUTEX-")))
    `(let ((,mutex-var (get-mutex ,provider)))
       (bordeaux-threads:with-lock-held (,mutex-var)
         ,@body))))

;;; function to wrap a socket
(defun wrap-socket (provider socket)
  (when socket
    (make-instance 'network-socket :provider provider
                                   :socket socket)))

;;; function to wrap an address
(defun wrap-address (provider address)
  (when address
    (make-instance 'network-address :provider provider
                                    :address address)))

;;; Method to create a remote address
(defmethod make-remote-address ((network-provider network-provider)
                                hostname
                                port)
  (let ((address (with-provider-guard (network-provider)
                   (make-remote-address (get-provider network-provider)
                                        hostname
                                        port))))
    (wrap-address network-provider address)))

;;; Method to create a datagram socket
(defmethod create-datagram-socket ((network-provider network-provider)
                                   port)
  (let ((socket (with-provider-guard (network-provider)
                  (create-datagram-socket (get-provider network-provider)
                                          port))))
    (wrap-socket network-provider socket)))

;;; Method to send a datagram with a socket
(defmethod send-datagram ((datagram-socket network-socket)
                          datagram
                          (socket-address network-address))
  (with-provider-guard ((get-provider datagram-socket))
    (send-datagram (get-socket datagram-socket)
                   datagram
                   (get-address socket-address))))


;;; Method to receive a datagram from a socket
(defmethod recv-datagram ((datagram-socket network-socket))
  (let ((provider (get-provider datagram-socket)))
    (multiple-value-bind (msg from)
        (with-provider-guard (provider)
          (recv-datagram (get-socket datagram-socket)))
      (values msg (wrap-address provider from)))))

;;; Method to close a datagram socket
(defmethod close-socket ((datagram-socket network-socket))
  (with-provider-guard ((get-provider datagram-socket))
    (close-socket (get-socket datagram-socket))))