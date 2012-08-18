;; Copyright (c) 2012 nklein software
;; MIT License. See included LICENSE.txt file for licensing details.

(in-package :unet)

(defclass ordered-channel (sequence-number-mixin base-channel)
  ((last-received-sequence-number :initform 0)))
