;; Copyright (c) 2012 nklein software
;; MIT License. See included LICENSE.txt file for licensing details.

(in-package :unet)

(defclass reliable-channel (sequence-number-mixin base-channel)
  ((unacked-packets :initform nil)
   (acks-sent :initform nil)))
