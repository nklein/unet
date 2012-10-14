;;;
;;; File: unet/sockets/fake/poll-datagram.lisp
;;; Author: Patrick Stein <pat@nklein.com>
;;;

;;;---------------------------------------------------------------------------
;;; Visibility: This package should not be imported or used from outside
;;;             of this directory.
(defpackage :unet-sockets-fake-poll-datagram
  (:use :cl)
  (:import-from :jpl-queues
                #:dequeue)
  (:import-from :unet-sockets
                #:poll-datagram)
  (:import-from :unet-sockets-fake-get-socket-from-interface
                #:get-socket-from-interface)
  (:import-from :unet-sockets-fake-socket
                #:fake-socket
                #:socket-get-interface
                #:socket-incoming-queue))

(in-package :unet-sockets-fake-poll-datagram)

;;;---------------------------------------------------------------------------
;;; Visibility: Public via the UNET-SOCKETS package
(defmethod poll-datagram ((socket fake-socket))
  "This method implements the POLL-DATAGRAM method of the UNET-SOCKETS:SOCKETS-INTERFACE for the FAKE-SOCKETS-INTERFACE.  For the FAKE-SOCKETS-INTERFACE, this checks the incoming queue of the given SOCKET and returns the next value on it if there is one."
  (let ((queue (socket-incoming-queue socket)))
    (dequeue queue)))