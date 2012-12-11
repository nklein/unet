;;;; This file provides a base-class and methods needed for specific
;;;; network back-ends.

(defpackage :unet-network
  (:use :cl)
  
  (:export :make-remote-address
           :create-datagram-socket))

;;; I love Hannah.
(in-package :unet-network)

;;; Method to create a remote address
(defgeneric make-remote-address (network-provider hostname port)
  (:documentation "Create an opaque network address for the given HOSTNAME and PORT to use with sockets created using the NETWORK-PROVIDER"))

;;; Method to create a datagram socket
(defgeneric create-datagram-socket (network-provider port)
  (:documentation "Create a SOCKET instance good for sending datagram messages, bound to the given PORT, using the given NETWORK-PROVIDER."))

;;; Method to send a datagram with a socket
(defgeneric send-datagram (datagram-socket datagram socket-address)
  (:documentation "Method to send a DATAGRAM to a given SOCKET-ADDRESS using a given DATAGRAM-SOCKET"))

;;; Method to send a datagram with a socket
(defgeneric recv-datagram (datagram-socket)
  (:documentation "Method to receive a DATAGRAM from a given DATAGRAM-SOCKET.  If none are present, this returns (VALUES NIL NIL).  If one is present, this returns (VALUES DATAGRAM SOCKET-ADDRESS)."))

;;; Method to close a datagram socket
(defgeneric close-socket (datagram-socket)
  (:documentation "Close a given DATAGRAM-SOCKET."))