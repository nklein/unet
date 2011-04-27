
(require :unet)
(require :cl-log)

(use-package :userial)
(use-package :unet)
(use-package :cl-log)

(defun log-unet-messages (category form-generator)
  (cl-log:log-message category (funcall form-generator)))

(defun make-string-message (str)
  (serialize :string str :buffer (make-buffer)))

(defclass serialized-messenger (base-messenger)
  ((filename :initarg :filename :reader serialized-messenger-filename)))

(defmethod messenger-send-message ((messenger serialized-messenger)
                                   message)
  (let ((hdr (make-buffer 16))
        (timestamp (message-timestamp message)))
    (serialize* (:uint64 (timestamp-universal-time timestamp)
                 :uint32 (timestamp-fraction timestamp)
                 :unet-logger-category (message-category message) 
                 :uint64 (buffer-length :buffer (message-description message)))
                :buffer hdr)
    (with-open-file (stream (serialized-messenger-filename messenger)
                     :direction :output
                     :if-does-not-exist :create
                     :if-exists :append
                     :element-type '(unsigned-byte 8))
      (write-sequence hdr stream)
      (write-sequence (message-description message) stream))))

(defun run-with-logging (&optional (logfile #P"unet.log"))
  (setf (log-manager) (make-instance 'log-manager
                                     :message-class 'base-message))
  (set-logger #'log-unet-messages)
  (let* ((logger (start-messenger 'serialized-messenger
                                  :name "binary-logger"
                                  :filename logfile))
         (p1 26354)
         (p2 55419)
         (s1 (make-instance 'server :port p1))
         (s2 (make-instance 'server :port p2))
         (r1 (make-instance 'recipient :hostname "localhost" :port p1))
         (r2 (make-instance 'recipient :hostname "localhost" :port p2))
         (c1 (make-instance 'raw-channel :server s1))
         (c2 (make-instance 'raw-channel :server s2)))
    (channel-add-recipient c1 r2)
    (send-packet c1 nil (make-string-message "Foo<1>"))
    (send-packet c1 nil (make-string-message "Bar<2>"))
    (handler-case
        (progn
          (next-packet c2 t)
          (channel-add-recipient c2 r1)
          (send-packet c2 nil (make-string-message "Got Foo<1>"))
          (next-packet c1)
          (next-packet c1 t)
          (next-packet c2))
      (recipient-not-on-channel-error ()
        (let ((restart (find-restart 'add-to-channel)))
          (when restart
            (invoke-restart restart)))))
    (stop-messenger logger)))

         