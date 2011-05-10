;; Copyright (c) 2011 nklein software
;; MIT License. See included LICENSE.txt file for licensing details.

(in-package :unet-example)

(defun packet (&rest pairs)
  (let ((buffer (userial:make-buffer)))
    (loop :for kk :in pairs :by #'cddr
          :for vv :in (rest pairs) :by #'cddr
          :do (userial:serialize kk vv :buffer buffer))
    buffer))
  