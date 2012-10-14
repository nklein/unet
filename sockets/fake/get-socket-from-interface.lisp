;;;
;;; File: unet/sockets/fake/get-socket-from-interface.lisp
;;; Author: Patrick Stein <pat@nklein.com>
;;;

;;;---------------------------------------------------------------------------
;;; Visibility: This package should not be imported or used from outside
;;;             of this directory.
(defpackage :unet-sockets-fake-get-socket-from-interface
  (:use :cl)
  (:import-from :unet-sockets-fake-make-hash-key
                #:make-hash-key)
  (:import-from :unet-sockets-fake-interface
                #:interface-hash-table)
  (:export :get-socket-from-interface))

(in-package :unet-sockets-fake-get-socket-from-interface)

;;;---------------------------------------------------------------------------
;;; Visibility: Exported from this package
(defun get-socket-from-interface (interface ip-address port)
  "This function is used to retrieve a SOCKET from the INTERFACE's set of managed sockets.  The IP-ADDRESS and PORT are used to find the appropriate SOCKET."
  (let ((key (make-hash-key ip-address port)))
    (values (gethash key (interface-hash-table interface)))))
