(defpackage :unet-sockets-send-datagram
  (:use :cl)
  (:export :send-datagram))

(in-package :unet-sockets-send-datagram)

(defgeneric send-datagram (socket data ip-address port)
  (:documentation "Given the SOCKET that was returned by CREATE-DATAGRAM-SOCKET, send the DATA to the given PORT at the specified IP-ADDRESS (as returned by GET-ADDRESS)."))
