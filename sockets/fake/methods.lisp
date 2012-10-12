;;;
;;; File: unet/sockets/fake/methods.lisp
;;; Author: Patrick Stein <pat@nklein.com>
;;;

;;;---------------------------------------------------------------------------
;;; Visibility: This package should not be imported or used from outside
;;;             of this directory.
(defpackage :unet-sockets-fake-methods
  (:use :cl)
  (:documentation "This package implements the methods required of the FAKE-SOCKETS-INTERFACE to support the UNET-SOCKETS:SOCKETS-INTERFACE.  It implements the SOCKETS-GET-ADDRESS, SOCKETS-CREATE-DATAGRAM-SOCKET, SOCKETS-SEND-DATAGRAM, and SOCKETS-POLL-DATAGRAM methods required by UNET-SOCKETS:SOCKETS-INTERFACE.")
  (:import-from :jpl-queues
                #:enqueue
                #:empty?
                #:dequeue)
  (:import-from :unet-sockets
                #:sockets-get-address
                #:sockets-create-datagram-socket
                #:sockets-send-datagram
                #:sockets-poll-datagram)
  (:import-from :unet-sockets-fake-interface
                #:fake-sockets-interface
                #:add-socket-to-interface
                #:get-socket-from-interface)
  (:import-from :unet-sockets-fake-socket
                #:fake-socket
                #:socket-get-interface
                #:socket-incoming-queue))

(in-package :unet-sockets-fake-methods)

;;;---------------------------------------------------------------------------
;;; Visibility: Public via the UNET-SOCKETS package
(defmethod sockets-get-address
    ((sockets-interface fake-sockets-interface) (hostname string))
  "This method implements the UNET-SOCKETS:SOCKETS-GET-ADDRESS method for the FAKE-SOCKETS-INTERFACE.  For the FAKE-SOCKETS-INTERFACE, we simply make a keyword from the given HOSTNAME as we do not need the IP-ADDRESS to actually mean anything in the outside world."
  (values (intern (string-upcase hostname) "KEYWORD")))

;;;---------------------------------------------------------------------------
;;; Visibility: Public via the UNET-SOCKETS package
(defmethod sockets-create-datagram-socket
    ((sockets-interface fake-sockets-interface) ip-address port)
  "This method implements the SOCKETS-CREATE-DATAGRAM-SOCKET method of the UNET-SOCKETS:SOCKETS-INTERFACE for the FAKE-SOCKETS-INTERFACE.  For the FAKE-SOCKETS-INTERFACE, this adds a new queue to be managed by the FAKE-SOCKETS-INTERFACE and indexed by the IP-ADDRESS and PORT."
  (let ((socket (make-instance 'fake-socket
                               :interface sockets-interface)))
    (add-socket-to-interface sockets-interface ip-address port socket)
    socket))

;;;---------------------------------------------------------------------------
;;; Visibility: Public via the UNET-SOCKETS package
(defmethod sockets-send-datagram ((socket fake-socket) data ip-address port)
  "This method implements the SOCKETS-SEND-DATAGRAM method of the UNET-SOCKETS:SOCKETS-INTERFACE for the FAKE-SOCKETS-INTERFACE.  For the FAKE-SOCKETS-INTERFACE, this finds the destination queue using the IP-ADDRESS and PORT managed by the FAKE-SOCKETS-INTERFACE that created the sending SOCKET and adds the DATA to the incoming queue for that destination."
  (let ((destination (get-socket-from-interface (socket-get-interface socket)
                                                ip-address
                                                port)))
    (enqueue data (socket-incoming-queue destination))))

;;;---------------------------------------------------------------------------
;;; Visibility: Public via the UNET-SOCKETS package
(defmethod sockets-poll-datagram ((socket fake-socket))
  "This method implements the SOCKETS-POLL-DATAGRAM method of the UNET-SOCKETS:SOCKETS-INTERFACE for the FAKE-SOCKETS-INTERFACE.  For the FAKE-SOCKETS-INTERFACE, this checks the incoming queue of the given SOCKET and returns the next value on it if there is one."
  (let ((queue (socket-incoming-queue socket)))
    (cond
      ((empty? queue) (values nil nil))
      (t (values (dequeue queue) t)))))