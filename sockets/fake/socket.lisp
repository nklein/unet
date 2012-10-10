(defpackage :unet-sockets-fake-socket
  (:use :cl)
  (:import-from :jpl-queues
                #:unbounded-fifo-queue)
  (:import-from :unet-sockets
                #:sockets-base-socket)
  (:import-from :unet-sockets-fake-interface
                #:fake-sockets-interface)
  (:export :fake-socket
           :socket-get-interface
           :socket-incoming-queue))

(in-package :unet-sockets-fake-socket)

(defclass fake-socket (sockets-base-socket)
  ((interface :initarg :interface :type 'fake-sockets-interface
              :reader socket-get-interface)
   (incoming-queue :initform (make-instance 'unbounded-fifo-queue)
                   :reader socket-incoming-queue)))
