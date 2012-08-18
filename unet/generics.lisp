;; Copyright (c) 2012 nklein software
;; MIT License. See included LICENSE.txt file for licensing details.

(in-package :unet)

(defgeneric send-payload (channel packet payload)
  (:method-combination progn))
(defgeneric receive-packet (channel packet))

(defun make-packet ()
  (cons nil nil))

(defun add-to-packet (packet name item)
  (setf (car packet)
        (append (car packet) (list (cons name item))))
  packet)

(defun get-from-packet (packet name)
  (let ((form (assoc name (car packet))))
    (values (rest form) (when form t))))