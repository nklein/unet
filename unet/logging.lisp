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


(defun log-unet-messages (category form-generator)
  (cl-log:log-message category (funcall form-generator)))

(defclass serialized-messenger (cl-log:base-messenger)
  ((filename :initarg :filename :reader serialized-messenger-filename)))

(defmethod cl-log:messenger-send-message ((messenger serialized-messenger)
                                          message)
  (let ((hdr (userial:make-buffer 16))
        (timestamp (cl-log:message-timestamp message))
        (payload (cl-log:message-description message)))
    (userial:serialize* (:uint64 (cl-log:timestamp-universal-time timestamp)
                         :uint32 (cl-log:timestamp-fraction timestamp)
                         :unet-logger-category
                           (cl-log:message-category message) 
                         :uint64 (userial:buffer-length :buffer payload))
                        :buffer hdr)
    (with-open-file (stream (serialized-messenger-filename messenger)
                     :direction :output
                     :if-does-not-exist :create
                     :if-exists :append
                     :element-type '(unsigned-byte 8))
      (write-sequence hdr stream)
      (write-sequence payload stream))))

(defun start-logging (&key (category :unet-all) (logfile #P"unet.log")
                           (name "unet-logging"))
  (unless (cl-log:log-manager)
    (setf (cl-log:log-manager)
          (make-instance 'cl-log:log-manager
                         :message-class 'cl-log:base-message)))
  (unet:set-logger #'log-unet-messages)
  (cl-log:start-messenger 'serialized-messenger
                          :category category
                          :name name
                          :filename logfile))

(defun stop-logging (&optional (name "unet-logging"))
  (cl-log:stop-messenger name))
