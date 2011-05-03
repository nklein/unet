;; Copyright (c) 2011 nklein software
;; MIT License. See included LICENSE.txt file for licensing details.

(in-package :unet-example)

(cl-log:defcategory :unet-validate-hostname)
(cl-log:defcategory :unet-specify-new-hostname)
(cl-log:defcategory :unet-validate-port-number)
(cl-log:defcategory :unet-specify-new-port-number)
(cl-log:defcategory :unet-initialize-recipient)
(cl-log:defcategory :unet-initialize-channel)
(cl-log:defcategory :unet-channel-add-recipient)
(cl-log:defcategory :unet-channel-remove-recipient)
(cl-log:defcategory :unet-add-to-channel)
(cl-log:defcategory :unet-send-pending-packets)
(cl-log:defcategory :unet-send-packet-all)
(cl-log:defcategory :unet-send-packet-some)
(cl-log:defcategory :unet-receive-packet)
(cl-log:defcategory :unet-next-packet)

(cl-log:defcategory :unet-validation (or :unet-validate-hostname
                                         :unet-validate-port-number))

(cl-log:defcategory :unet-restart (or :unet-specify-new-hostname
                                      :unet-specify-new-port-number
                                      :unet-add-to-channel))

(cl-log:defcategory :unet-recipient-info (or :unet-initialize-recipient
                                             :unet-channel-add-recipient
                                             :unet-channel-remove-recipient))

(cl-log:defcategory :unet-channel-info (or :unet-initialize-channel
                                           :unet-channel-add-recipient
                                           :unet-channel-remove-recipient))

(cl-log:defcategory :unet-sending-info (or :unet-send-pending-packets
                                           :unet-send-packet-all
                                           :unet-send-packet-some))

(cl-log:defcategory :unet-receiving-info (or :unet-receive-packet
                                             :unet-next-packet))

(cl-log:defcategory :unet-all (or :unet-validation
                                  :unet-restart
                                  :unet-recipient-info
                                  :unet-channel-info
                                  :unet-sending-info
                                  :unet-receiving-info))

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

(defun start-logging (&key (category :unet-all) (logfile #P"unet.log"))
  (unless (cl-log:log-manager)
    (setf (cl-log:log-manager)
          (make-instance 'cl-log:log-manager
                         :message-class 'cl-log:base-message)))
  (unet:set-logger #'log-unet-messages)
  (cl-log:start-messenger 'serialized-messenger
                          :category category
                          :name "unet-example"
                          :filename logfile))

(defun stop-logging ()
  (cl-log:stop-messenger "unet-example"))
