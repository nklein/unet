(defpackage :unet-sockets-fake-methods
  (:use :cl)
  (:import-from :jpl-queues
                #:enqueue
                #:empty?
                #:dequeue)
  (:import-from :unet-sockets
                #:sockets-hostname-not-found-error
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

(defmethod sockets-get-address
    ((sockets-interface fake-sockets-interface) hostname)
  (if (string/= hostname "localhost")
      (error 'sockets-hostname-not-found-error :hostname hostname)
    t))

(defmethod sockets-create-datagram-socket
    ((sockets-interface fake-sockets-interface) ip-address port)
  (declare (ignore ip-address))
  (let ((socket (make-instance 'fake-socket
                               :interface sockets-interface)))
    (add-socket-to-interface sockets-interface port socket)
    socket))

(defmethod sockets-send-datagram ((socket fake-socket) data ip-address port)
  (declare (ignore ip-address))
  (let ((destination (get-socket-from-interface (socket-get-interface socket)
                                                port)))
    (enqueue data (socket-incoming-queue destination))))

(defmethod sockets-poll-datagram ((socket fake-socket))
  (let ((queue (socket-incoming-queue socket)))
    (cond
      ((empty? queue) (values nil nil))
      (t (values (dequeue queue) t)))))