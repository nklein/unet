(defpackage :unet-sockets-poll-datagram
  (:use :cl)
  (:export :poll-datagram))

(in-package :unet-sockets-poll-datagram)

(defgeneric poll-datagram (socket)
  (:documentation "Poll the SOCKET that was returned from CREATE-DATAGRAM-SOCKET for messages.  This method return the next datagram available from the socket or NIL if there were none."))