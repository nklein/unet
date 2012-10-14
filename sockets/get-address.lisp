(defpackage :unet-sockets-get-address
  (:use :cl)
  (:export :get-address))

(in-package :unet-sockets-get-address)

(defgeneric get-address (sockets-interface hostname)
  (:documentation "Using the SOCKETS-INTERFACE instance, resolve the HOSTNAME string into an IP address.  This method will throw a HOSTNAME-NOT-FOUND-ERROR if the IP address of the given HOSTNAME cannot be determined.  The returned address should be considered an opaque type.  It is implementation-specific."))
