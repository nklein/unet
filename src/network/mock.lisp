;;;; This file provides a base-class and methods needed for specific
;;;; network back-ends.

(defpackage :unet-network-mock
  (:use :cl)
  (:export :network-provider)
  (:import-from :unet-network
                :make-remote-address))

;;; package used to intern addresses
(defpackage :unet-network-mock-addresses)

;;; I love Hannah.
(in-package :unet-network-mock)

;;; Mock network provider class
(defclass network-provider ()
  ()
  (:documentation "Mock network provider class which simulates a network using queues"))

;;; Method to create a remote address
(defmethod make-remote-address ((network-provider network-provider)
                                (hostname string)
                                (port integer))
  (declare (ignore network-provider))
  (assert (<= 0 port 65535))
  (intern (format nil "~S~D" hostname port)))

;;; Method to create a datagram socket
(defgeneric create-datagram-socket (network-provider port)
  (:documentation "Create a SOCKET instance good for sending datagram messages, bound to the given PORT, using the given NETWORK-PROVIDER."))

;;; Method to send a datagram with a socket
(defgeneric send-datagram (datagram-socket datagram socket-address)
  (:documentation "Method to send a DATAGRAM to a given SOCKET-ADDRESS using a given DATAGRAM-SOCKET"))

;;; Method to close a datagram socket
(defgeneric close-socket (datagram-socket)
  (:documentation "Close a given DATAGRAM-SOCKET"))