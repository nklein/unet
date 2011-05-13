;; Copyright (c) 2011 nklein software
;; MIT License. See included LICENSE.txt file for licensing details.

(in-package :unet)

(defvar *logger* nil)

(declaim (ftype (function ((or nil function)) (values)) set-logger))
(defun set-logger (logger)
  (declare (type (or nil function) logger))
  (setf *logger* logger)
  (values))

(defun to-string (something)
  (format nil "~A" something))

(defmacro log-it (category &rest serialize-args)
  `(when *logger*
     (funcall *logger* ,category #'(lambda ()
                                     (userial:serialize*
                                        (:unet-logger-category ,category
                                         ,@serialize-args)
                                        :buffer (userial:make-buffer))))))
