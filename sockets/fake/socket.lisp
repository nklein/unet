;;;
;;; File: unet/sockets/fake/socket.lisp
;;; Author: Patrick Stein <pat@nklein.com>
;;;

;;;---------------------------------------------------------------------------
;;; Visibility: This package should not be imported or used from outside
;;;             of this directory.
(defpackage :unet-sockets-fake-socket
  (:use :cl)
  (:import-from :jpl-queues
                #:unbounded-fifo-queue)
  (:import-from :unet-sockets
                #:base-socket)
  (:import-from :unet-sockets-fake-interface
                #:fake-sockets-interface)
  (:export :fake-socket
           :socket-get-interface
           :socket-incoming-queue))

(in-package :unet-sockets-fake-socket)

;;;---------------------------------------------------------------------------
;;; Visibility: Should not be referenced outside of this directory.
(defclass fake-socket (sockets-base-socket)
  ((interface :initarg :interface :type 'fake-sockets-interface
              :reader socket-get-interface)
   (incoming-queue :initform (make-instance 'unbounded-fifo-queue)
                   :reader socket-incoming-queue))
  (:documentation "This class is used by the FAKE-SOCKETS-INTERFACE for each socket that it has to manage.  Each socket tracks the FAKE-SOCKETS-INTERFACE on which it was created and the incoming queue for this socket."))
