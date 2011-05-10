;; Copyright (c) 2011 nklein software
;; MIT License. See included LICENSE.txt file for licensing details.

(in-package :unet-logged)

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
