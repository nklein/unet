(defpackage :unet-sockets-interface
  (:use :cl)
  (:export :sockets-interface
           :sockets-hostname-not-found-error
           :sockets-hostname-not-found-error-hostname
           :sockets-get-address
           :sockets-create-datagram-socket))

(in-package :unet-sockets-interface)

(defclass sockets-interface ()
  ())

(define-condition sockets-hostname-not-found-error (error)
  ((hostname :initarg :hostname
             :reader sockets-hostname-not-found-error-hostname)))

(defmethod print-object ((hnf sockets-hostname-not-found-error) stream)
  (with-accessors ((hostname sockets-hostname-not-found-error-hostname)) hnf
    (print-unreadable-object (hnf stream :type t)
      (format stream ":HOSTNAME ~S" hostname))))

(defgeneric sockets-get-address (sockets-interface hostname)
  (:documentation "Using the SOCKETS-INTERFACE, resolve the HOSTNAME string into an IP address.  This will throw a SOCKETS-HOSTNAME-NOT-FOUND-ERROR if the IP address of the given HOSTNAME cannot be determined."))

(defgeneric sockets-create-datagram-socket (sockets-interface ip-address port)
  (:documentation "Create a SOCKETS-BASE-SOCKET instance bound to IP-ADDRESS and PORT.  The IP-ADDRESS can be either the keyword IPADDR-ANY or an address as returned by SOCKET-GET-ADDRESS."))