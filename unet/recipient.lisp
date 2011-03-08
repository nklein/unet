;; Copyright (c) 2011 nklein software
;; MIT License. See included LICENSE.txt file for licensing details.

(in-package :unet)

(defclass recipient ()
  ((host :initarg :host :reader recipient-host)
   (port :initarg :port :reader recipient-port))
  (:default-initargs :host (iolib:ensure-hostname "localhost")
                     :port 26354))
