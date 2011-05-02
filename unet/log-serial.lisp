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
                               :unet-next-packet))

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
  (userial:unserialize :string :buffer buffer))

(defmethod userial:serialize ((type (eql :unet-recipient-list)) value
                              &key (buffer userial:*buffer*) &allow-other-keys)
  (userial:serialize :uint16 (length value) :buffer buffer)
  (loop :for rr :in value
     :do (userial:serialize :unet-recipient rr :buffer buffer))
  buffer)

(defmethod userial:unserialize ((type (eql :unet-recipient-list))
                                &key (buffer userial:*buffer*)
                                &allow-other-keys)
  (let ((len (userial:unserialize :uint16 :buffer buffer)))
    (values (loop :for ii :from 1 :to len
               :collecting (userial:unserialize :unet-recipient
                                                :buffer buffer))
            buffer)))