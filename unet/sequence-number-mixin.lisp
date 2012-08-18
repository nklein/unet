;; Copyright (c) 2012 nklein software
;; MIT License. See included LICENSE.txt file for licensing details.

(in-package :unet)

(defclass sequence-number-mixin ()
  ((sequence-number :initform 0)))

(defun next-sequence-number (sequence-number-mixin)
  (declare (type sequence-number-mixin sequence-number-mixin))
  (with-slots (sequence-number) sequence-number-mixin
    (incf sequence-number)))

(defmethod send-payload progn ((channel sequence-number-mixin) packet payload)
  (add-to-packet packet :sequence-number (next-sequence-number channel)))