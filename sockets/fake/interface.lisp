;;;
;;; File: unet/sockets/fake/interface.lisp
;;; Author: Patrick Stein <pat@nklein.com>
;;;

;;;---------------------------------------------------------------------------
;;; Visibility: This package should not be imported or used from outside
;;;             of this directory.
(defpackage :unet-sockets-fake-interface
  (:use :cl)
  (:documentation "This package defines the FAKE-SOCKETS-INTERFACE which implements the UNET-SOCKETS:SOCKETS-INTERFACE.  This package also defines some utility functions used by other packages in this directory to implement the methods needed for the UNET-SOCKETS:SOCKETS-INTERFACE.")
  (:import-from :unet-sockets
                #:sockets-interface)
  (:export :fake-sockets-interface
           :add-socket-to-interface
           :get-socket-from-interface))

(in-package :unet-sockets-fake-interface)

;;;---------------------------------------------------------------------------
;;; Visibility: Public via the UNET-SOCKETS-FAKE package.
(defclass fake-sockets-interface (sockets-interface)
  ((sockets :initform (make-hash-table :test #'equal)
            :reader interface-hash-table))
  (:documentation "This class implements the UNET-SOCKETS:SOCKETS-INTERFACE.  It does so without a network connection by managing a list of incoming-queues for each declared socket.  These queues are kept in a hash table indexed by the IP-ADDRESS and PORT of the socket."))

;;;---------------------------------------------------------------------------
;;; Visibility: Private to this package
(defun make-hash-key (ip-address port)
  "This function is used to create a hash key from a given IP-ADDRESS and PORT"
  (cons ip-address port))

;;;---------------------------------------------------------------------------
;;; Visibility: Exported from this package
(defun add-socket-to-interface (interface ip-address port socket)
  "This function is used to add a new SOCKET to the INTERFACE's set of managed sockets.  The IP-ADDRESS and PORT will be used at a later time to retrieve this SOCKET."
  (let ((key (make-hash-key ip-address port)))
    (setf (gethash key (interface-hash-table interface)) socket)))

;;;---------------------------------------------------------------------------
;;; Visibility: Exported from this package
(defun get-socket-from-interface (interface ip-address port)
  "This function is used to retrieve a SOCKET from the INTERFACE's set of managed sockets.  The IP-ADDRESS and PORT are used to find the appropriate SOCKET."
  (let ((key (make-hash-key ip-address port)))
    (values (gethash key (interface-hash-table interface)))))
