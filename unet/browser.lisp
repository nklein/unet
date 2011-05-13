;; Copyright (c) 2011 nklein software
;; MIT License. See included LICENSE.txt file for licensing details.

(in-package :unet)

(declaim (special *current-log-browser*))
(defvar *current-log-browser* nil)

(defclass log-browser ()
  ((stream :initarg :stream :accessor log-browser-stream)
   (spots :initform nil :accessor log-browser-spots)
   (reader :initform (error "must specify reader")
           :initarg :header-reader
           :accessor log-header-reader)))

(defmethod initialize-instance :around ((obj log-browser)
                                        &rest args
                                        &key (filename "" filename-p)
                                        &allow-other-keys)
  (if filename-p
      (apply #'call-next-method
             obj
             :stream (open filename :element-type '(unsigned-byte 8))
             args)
      (call-next-method)))

(defun next-log-message (&optional (count 1)
                                   (browser unet:*current-log-browser*))
  (with-accessors ((stream log-browser-stream)
                   (spots log-browser-spots)
                   (reader log-header-reader)) browser
    (when (< (file-position stream) (file-length stream))
      (push (file-position stream) spots)
      (let* ((ret (nth-value 0 (funcall reader stream)))
             (payload-length (third ret)))
        (file-position stream (+ (file-position stream) payload-length))
        (case count
          (1 ret)
          (t (next-log-message (1- count) browser)))))))

(defun previous-log-message (&optional (count 1)
                                       (browser unet:*current-log-browser*))
  (with-accessors ((stream log-browser-stream)
                   (spots log-browser-spots)
                   (reader log-header-reader)) browser
    (setf spots (nthcdr count spots))
    (file-position stream (if spots (first spots) 0))
    (let* ((ret (nth-value 0 (funcall reader stream)))
           (payload-length (third ret)))
      (file-position stream (+ (file-position stream) payload-length))
      ret)))
