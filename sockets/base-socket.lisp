(defpackage :unet-sockets-base-socket
  (:use :cl)
  (:export :base-socket))

(in-package :unet-sockets-base-socket)

(defclass base-socket ()
  ()
  (:documentation "The BASE-SOCKET class is the base class for all sockets returned from specializations of the CREATE-DATAGRAM-SOCKET method"))
