;; Copyright (c) 2011 nklein software
;; MIT License. See included LICENSE.txt file for licensing details.

(in-package :unet-example)

(defmacro defchannel (channel server channel-class &rest channel-args)
  `(defparameter ,channel
     (make-instance ',channel-class :server ,server ,@channel-args)))

(defun add-recipients (channel &rest servers)
  (mapc #'(lambda (ss)
            (unet:channel-add-recipient channel (server-addr ss)))
        servers)
  t)

(defun remove-recipients (channel &rest servers)
  (mapc #'(lambda (ss)
            (unet:channel-remove-recipient channel (server-addr ss)))
        servers)
  t)
