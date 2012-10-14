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
  (:export :fake-sockets-interface))

(in-package :unet-sockets-fake-interface)

;;;---------------------------------------------------------------------------
;;; Visibility: Public via the UNET-SOCKETS-FAKE package.
(defclass fake-sockets-interface (sockets-interface)
  ((sockets :initform (make-hash-table :test #'equal)
            :reader interface-hash-table))
  (:documentation "This class implements the UNET-SOCKETS:SOCKETS-INTERFACE.  It does so without a network connection by managing a list of incoming-queues for each declared socket.  These queues are kept in a hash table indexed by the IP-ADDRESS and PORT of the socket."))
