;;;
;;; File: unet/sockets/fake/add-socket-to-interface.lisp
;;; Author: Patrick Stein <pat@nklein.com>
;;;

;;;---------------------------------------------------------------------------
;;; Visibility: This package should not be imported or used from outside
;;;             of this directory.
(defpackage :unet-sockets-fake-add-socket-to-interface
  (:use :cl)
  (:import-from :unet-sockets-fake-interface
                #:interface-hash-table)
  (:import-from :unet-sockets-fake-make-hash-key
                #:make-hash-key)
  (:import-from :unet-sockets-fake-get-socket-from-interface
                #:get-socket-from-interface)
  (:export :add-socket-to-interface))

(in-package :unet-sockets-fake-add-socket-to-interface)

;;;---------------------------------------------------------------------------
;;; Visibility: Exported from this package
(defun add-socket-to-interface (interface ip-address port socket)
  "This function is used to add a new SOCKET to the INTERFACE's set of managed sockets.  The IP-ADDRESS and PORT will be used at a later time to retrieve this SOCKET."
  (unless (get-socket-from-interface interface ip-address port)
    (let ((key (make-hash-key ip-address port)))
      (setf (gethash key (interface-hash-table interface)) socket))))
