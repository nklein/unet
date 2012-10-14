(defpackage :unet-sockets-create-datagram-socket
  (:use :cl)
  (:export :create-datagram-socket))

(in-package :unet-sockets-create-datagram-socket)

(defgeneric create-datagram-socket (sockets-interface ip-address port)
  (:documentation "Create a socket instance bound to IP-ADDRESS and PORT.  The IP-ADDRESS can be either the keyword :INADDR-ANY or an address as returned by GET-ADDRESS.  This method returns an instance of a subclass of BASE-SOCKET."))