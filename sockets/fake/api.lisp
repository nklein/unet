;;;
;;; File: unet/sockets/fake/api.lisp
;;; Author: Patrick Stein <pat@nklein.com>
;;;

;;;---------------------------------------------------------------------------
;;; Visibility: This is the only package in this directory which should be
;;;             referenced or used from outside of this directory.
(org.tfeb.conduit-packages:defpackage :unet-sockets-fake
  (:use :cl)
  (:documentation "The UNET-SOCKETS-FAKE package provides the FAKE-SOCKETS-INTERFACE class which implements the UNET-SOCKETS:SOCKETS-INTERFACE.  It does this by managing local queues for each declared socket rather than actually using network interfaces.")
  (:extends/including :unet-sockets-fake-interface
                      #:fake-sockets-interface))
