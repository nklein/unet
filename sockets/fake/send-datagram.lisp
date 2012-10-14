;;;
;;; File: unet/sockets/fake/send-datagram.lisp
;;; Author: Patrick Stein <pat@nklein.com>
;;;

;;;---------------------------------------------------------------------------
;;; Visibility: This package should not be imported or used from outside
;;;             of this directory.
(defpackage :unet-sockets-fake-send-datagram
  (:use :cl)
  (:import-from :jpl-queues
                #:enqueue)
  (:import-from :unet-sockets
                #:send-datagram)
  (:import-from :unet-sockets-fake-get-socket-from-interface
                #:get-socket-from-interface)
  (:import-from :unet-sockets-fake-socket
                #:fake-socket
                #:socket-get-interface
                #:socket-incoming-queue))

(in-package :unet-sockets-fake-send-datagram)

;;;---------------------------------------------------------------------------
;;; Visibility: Public via the UNET-SOCKETS package
(defmethod send-datagram ((socket fake-socket)
                          data
                          ip-address
                          port)
  "This method implements the SOCKETS-SEND-DATAGRAM method of the UNET-SOCKETS:SOCKETS-INTERFACE for the FAKE-SOCKETS-INTERFACE.  For the FAKE-SOCKETS-INTERFACE, this finds the destination queue using the IP-ADDRESS and PORT managed by the FAKE-SOCKETS-INTERFACE that created the sending SOCKET and adds the DATA to the incoming queue for that destination."
  (let ((destination (get-socket-from-interface (socket-get-interface socket)
                                                ip-address
                                                port)))
    (enqueue data (socket-incoming-queue destination))))
