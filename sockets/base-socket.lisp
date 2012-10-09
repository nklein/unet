(defpackage :unet-sockets-base-socket
  (:use :cl)
  (:export :sockets-base-socket
           :sockets-send-datagram
           :sockets-poll-datagram))

(in-package :unet-sockets-base-socket)

(defclass sockets-base-socket ()
  ())

(defgeneric sockets-send-datagram (sockets-base-socket data ip-address port))
(defgeneric sockets-poll-datagram (sockets-base-socket))