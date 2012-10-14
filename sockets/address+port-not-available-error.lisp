(defpackage :unet-sockets-address+port-not-available-error
  (:use :cl)
  (:export :address+port-not-available-error
           :address+port-not-available-error-address
           :address+port-not-available-error-port))

(in-package :unet-sockets-address+port-not-available-error)

(define-condition address+port-not-available-error (error)
  ((address :initarg :address
            :reader address+port-not-available-error-address)
   (port    :initarg :port
            :reader address+port-not-available-error-port)))

(defmethod print-object ((ana address+port-not-available-error) stream)
  (with-accessors ((address address+port-not-available-error-address)
                   (port address+port-not-available-error-port))
      ana
    (print-unreadable-object (ana stream :type t)
      (format stream ":ADDRESS ~S :PORT ~S" address port))))


