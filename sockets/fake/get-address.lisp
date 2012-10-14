;;;
;;; File: unet/sockets/fake/get-address.lisp
;;; Author: Patrick Stein <pat@nklein.com>
;;;

;;;---------------------------------------------------------------------------
;;; Visibility: This package should not be imported or used from outside
;;;             of this directory.
(defpackage :unet-sockets-fake-get-address
  (:use :cl)
  (:import-from :unet-sockets
                #:get-address)
  (:import-from :unet-sockets-fake-interface
                #:fake-sockets-interface))

(in-package :unet-sockets-fake-get-address)

;;;---------------------------------------------------------------------------
;;; Visibility: Public via the UNET-SOCKETS package
(defmethod get-address
    ((sockets-interface fake-sockets-interface) (hostname string))
  "This method implements the UNET-SOCKETS:SOCKETS-GET-ADDRESS method for the FAKE-SOCKETS-INTERFACE.  For the FAKE-SOCKETS-INTERFACE, we simply make a keyword from the given HOSTNAME as we do not need the IP-ADDRESS to actually mean anything in the outside world."
  (values (intern (string-upcase hostname) "KEYWORD")))
