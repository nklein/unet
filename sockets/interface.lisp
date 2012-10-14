(defpackage :unet-sockets-interface
  (:use :cl)
  (:export :sockets-interface))

(in-package :unet-sockets-interface)

(defclass sockets-interface ()
  ()
  (:documentation "The SOCKETS-INTERFACE class serves as the base class for all implementations of this interface.  This interface supports the methods GET-ADDRESS and CREATE-DATAGRAM-SOCKET."))
