;;;; This file provides a base-class and methods needed for specific
;;;; network back-ends.

(defpackage :unet-network-mock
  (:use :cl)
  (:export :network-provider)
  (:import-from :jpl-queues)
  (:import-from :unet-network
                :make-remote-address
                :create-datagram-socket
                :send-datagram
                :recv-datagram
                :close-socket))

;;; package used to intern addresses
(defpackage :unet-network-mock-addresses)

;;; I love Hannah.
(in-package :unet-network-mock)

;;; Mock network provider class
(defclass network-provider ()
  ((queues :initform (make-hash-table)
           :reader get-queues)
   (mutex  :initform nil
           :initarg :mutex
           :reader get-mutex))
  (:documentation "Mock network provider class which simulates a network using queues"))

;;; Macro to mutex-lock a provider for some body
(defmacro with-provider-guard ((provider) &body body)
  (let ((mutex-var (gensym "MUTEX-"))
        (body-func (gensym "BODY-")))
    `(flet ((,body-func ()
              ,@body))
        (let ((,mutex-var (get-mutex ,provider)))
          (if ,mutex-var
              (bordeaux-threads:with-lock-held (,mutex-var)
                (,body-func))
              (,body-func))))))

;;; Function to determine if something is a valid port
(defun portp (port)
  (and (integerp port)
       (<= 0 port 65535)))

;;; Binding when macro
(defmacro bwhen ((var form) &body body)
  `(let ((,var ,form))
     (when ,var
       ,@body)))

;;; Function to find a queue if it exists for the given address
(defun find-queue (queues address)
  (gethash address queues))

;;; Function to ensure a queue exists for the given address
(defun ensure-queue (queues address)
  (or (find-queue queues address)
      (setf (gethash address queues)
            (make-instance 'jpl-queues:bounded-fifo-queue :capacity 20))))

(defun remove-queue (queues address)
  (remhash address queues))

;;; Method to create a remote address
(defmethod make-remote-address ((network-provider network-provider)
                                (hostname string)
                                (port integer))
  (declare (ignore network-provider))
  (assert (portp port))
  (let ((name (format nil "~A:~D" (string-upcase hostname) port)))
    (values (intern name 'unet-network-mock-addresses))))

;;; Getter and setter for the network-provider of a socket
(defun socket-network-provider (socket)
  (symbol-value socket))

(defun (setf socket-network-provider) (provider socket)
  (setf (symbol-value socket) provider))

;;; Method to create a datagram socket
(defmethod create-datagram-socket ((network-provider network-provider)
                                   (port integer))
  (assert (portp port))
  (let ((address (make-remote-address network-provider "localhost" port)))
    (with-provider-guard (network-provider)
      (ensure-queue (get-queues network-provider) address)
      (setf (socket-network-provider address) network-provider))
    address))

;;; Method to send a datagram with a socket
(defmethod send-datagram ((datagram-socket symbol)
                          (datagram sequence)
                          (socket-address symbol))
  (let ((network-provider (socket-network-provider datagram-socket)))
    (with-provider-guard (network-provider)
      (bwhen (queue (find-queue (get-queues network-provider) socket-address))
        (jpl-queues:enqueue (list datagram datagram-socket) queue)
        t))))

;;; Method to receive a datagram from a socket
(defmethod recv-datagram ((datagram-socket symbol))
  (let ((network-provider (socket-network-provider datagram-socket)))
    (with-provider-guard (network-provider)
      (let ((queue (find-queue (get-queues network-provider) datagram-socket)))
        (if (and queue (not (jpl-queues:empty? queue)))
            (destructuring-bind (datagram from) (jpl-queues:dequeue queue)
              (values datagram from))
            (values nil nil))))))

;;; Method to close a datagram socket
(defmethod close-socket (datagram-socket)
  (let ((network-provider (socket-network-provider datagram-socket)))
    (with-provider-guard (network-provider)
      (remove-queue (get-queues network-provider) datagram-socket))))