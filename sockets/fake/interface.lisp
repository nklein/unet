(defpackage :unet-sockets-fake-interface
  (:use :cl)
  (:import-from :unet-sockets
                #:sockets-interface)
  (:export :fake-sockets-interface
           :add-socket-to-interface
           :get-socket-from-interface))

(in-package :unet-sockets-fake-interface)

(defclass fake-sockets-interface (unet-sockets-interface:sockets-interface)
  ((sockets :initform (make-hash-table :test #'equal)
            :reader interface-hash-table)))

(defun add-socket-to-interface (interface port socket)
  (setf (gethash port (interface-hash-table interface)) socket))

(defun get-socket-from-interface (interface port)
  (gethash port (interface-hash-table interface)))
