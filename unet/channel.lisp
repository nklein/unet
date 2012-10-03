
(defpackage :unet-channel
  (:use :cl)
  (:export :channel
           :channel-name
           :channel-no-name-error))

(in-package :unet-channel)

(define-condition channel-no-name-error (error)
  ())

(defun channel-internal ())

(defclass channel ()
  ((name :initarg :name :reader channel-name :type string))
  (:default-initargs :name (error 'channel-no-name-error)))