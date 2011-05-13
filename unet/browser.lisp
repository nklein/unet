;; Copyright (c) 2011 nklein software
;; MIT License. See included LICENSE.txt file for licensing details.

(in-package :unet)

(defun buffer-from-stream (stream length)
  (let ((buf (userial:make-buffer length)))
    (userial:buffer-advance :amount length :buffer buf)
    (read-sequence buf stream)
    (userial:buffer-rewind :buffer buf)))

(defun read-logged-header (stream)
  (userial:unserialize-let* (:uint64 universal-timestamp
                             :uint32 timestamp-fraction
                             :unet-logger-category category
                             :uint64 length) (buffer-from-stream stream 21)
    (multiple-value-bind (secs mins hrs day mon year)
        (decode-universal-time universal-timestamp 0)
      (list (list year mon day hrs mins secs
                  (/ timestamp-fraction internal-time-units-per-second))
            category
            length))))

(declaim (special *current-log-browser*))
(defvar *current-log-browser* nil)

(defclass log-browser ()
  ((stream :initarg :stream :accessor log-browser-stream)
   (spots :initform nil :accessor log-browser-spots)
   (payload :initform nil :accessor log-browser-payload)
   (reader :initform #'read-logged-header
           :initarg :header-reader
           :accessor log-header-reader)))

(defmethod initialize-instance :around ((obj log-browser)
                                        &rest args
                                        &key (filename "" filename-p)
                                        &allow-other-keys)
  (prog1
      (if filename-p
          (apply #'call-next-method
                 obj
                 :stream (open filename :element-type '(unsigned-byte 8))
                 args)
        (call-next-method))
    (next-log-message 1 obj)))

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
                   (reader log-header-reader)
                   (payload log-browser-payload)) browser
    (setf spots (nthcdr count spots))
    (file-position stream (if spots (first spots) 0))
    (let* ((ret (nth-value 0 (funcall reader stream)))
           (payload-length (third ret)))
      (setf payload (buffer-from-stream stream payload-length))
      ret)))

(defun print-log-message (&optional (stream t)
                                    (browser unet:*current-log-browser*))
  (with-accessors ((payload log-browser-payload)) browser
     (let ((type (userial:unserialize :unet-logger-category :buffer payload)))
       (print-log-packet type payload stream))))