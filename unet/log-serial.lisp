;; Copyright (c) 2011 nklein software
;; MIT License. See included LICENSE.txt file for licensing details.

(in-package :unet)

(userial:make-enum-serializer :unet-logger-category
                              (:unet-validate-hostname
                               :unet-specify-new-hostname
                               :unet-validate-port-number
                               :unet-specify-new-port-number
                               :unet-initialize-recipient
                               :unet-initialize-channel
                               :unet-channel-add-recipient
                               :unet-channel-remove-recipient
                               :unet-add-to-channel
                               :unet-send-pending-packets
                               :unet-send-packet-all
                               :unet-send-packet-some
                               :unet-receive-packet
                               :unet-next-packet
                               :unet-initialize-server
                               :unet-server-get-single-message
                               :unet-server-process-single-message
                               :unet-server-unprocessed-single-message
                               :unet-server-check-for-messages
                               :unet-server-channels-with-messages
                               :unet-server-close))

(userial:make-simple-serializer :unet-host
                                (object (vector 0 0 0 0))
                                (:uint8 (elt object 0)
                                 :uint8 (elt object 1)
                                 :uint8 (elt object 2)
                                 :uint8 (elt object 3)))

(userial:make-accessor-serializer :unet-recipient
                                  (make-instance 'recipient)
                                  (:unet-host recipient-host
                                   :uint16    recipient-port))

(defmethod userial:serialize ((type (eql :unet-channel)) value
                              &key (buffer userial:*buffer*) &allow-other-keys)
  (userial:serialize :string (symbol-name (channel-id value))
                     :buffer buffer))

(defmethod userial:unserialize ((type (eql :unet-channel))
                                &key (buffer userial:*buffer*)
                                &allow-other-keys)
  (values (intern (userial:unserialize :string :buffer buffer) "KEYWORD")
          buffer))

(userial:make-list-serializer :unet-recipient-list :unet-recipient)
(userial:make-list-serializer :unet-channel-list :unet-channel)