(defpackage :unet-sockets-hostname-not-found-error
  (:use :cl)
  (:export :hostname-not-found-error
           :hostname-not-found-error-hostname))

(in-package :unet-sockets-hostname-not-found-error)

(define-condition hostname-not-found-error (error)
  ((hostname :initarg :hostname
             :reader hostname-not-found-error-hostname)))

(defmethod print-object ((hnf hostname-not-found-error) stream)
  (with-accessors ((hostname hostname-not-found-error-hostname)) hnf
    (print-unreadable-object (hnf stream :type t)
      (format stream ":HOSTNAME ~S" hostname))))

