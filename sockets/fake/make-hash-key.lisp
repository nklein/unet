;;;
;;; File: unet/sockets/fake/make-hash-key.lisp
;;; Author: Patrick Stein <pat@nklein.com>
;;;

;;;---------------------------------------------------------------------------
;;; Visibility: This package should not be imported or used from outside
;;;             of this directory.
(defpackage :unet-sockets-fake-make-hash-key
  (:use :cl)
  (:export :make-hash-key))

(in-package :unet-sockets-fake-make-hash-key)

;;;---------------------------------------------------------------------------
;;; Visibility: Should only be used by other packages in this directory.
(defun make-hash-key (ip-address port)
  "This function is used to create a hash key from a given IP-ADDRESS and PORT"
  (cons ip-address port))
