;;;
;;; File: unet/sockets/fake/create-datagram-socket.lisp
;;; Author: Patrick Stein <pat@nklein.com>
;;;

;;;---------------------------------------------------------------------------
;;; Visibility: This package should not be imported or used from outside
;;;             of this directory.
(defpackage :unet-sockets-fake-create-datagram-socket
  (:use :cl)
  (:import-from :unet-sockets
                #:address+port-not-available-error
                #:create-datagram-socket)
  (:import-from :unet-sockets-fake-interface
                #:fake-sockets-interface)
  (:import-from :unet-sockets-fake-add-socket-to-interface
                #:add-socket-to-interface)
  (:import-from :unet-sockets-fake-socket
                #:fake-socket))

(in-package :unet-sockets-fake-create-datagram-socket)

;;;---------------------------------------------------------------------------
;;; Visibility: Public via the UNET-SOCKETS package
(defmethod create-datagram-socket ((sockets-interface fake-sockets-interface)
                                   ip-address
                                   port)
  "This method implements the CREATE-DATAGRAM-SOCKET method of the UNET-SOCKETS:SOCKETS-INTERFACE for the FAKE-SOCKETS-INTERFACE.  For the FAKE-SOCKETS-INTERFACE, this adds a new queue to be managed by the FAKE-SOCKETS-INTERFACE and indexed by the IP-ADDRESS and PORT."
  (let ((socket (make-instance 'fake-socket
                               :interface sockets-interface)))
    (unless (add-socket-to-interface sockets-interface ip-address port socket)
      (error 'address+port-not-available-error
             :address ip-address
             :port port))
    socket))
