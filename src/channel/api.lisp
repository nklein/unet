;;;; This file provides the API for the channel system

(defpackage :unet-channel
  (:use :cl)
  (:import-from :unet-channel-component
                  :defchannel-slots
                  :defchannel-peer-slots
                  :defchannel-packet-slots
                  :defchannel-fragmenter
                  :defchannel-encoder
                  :defchannel-decoder
                  :return-packet
                  :stop-encoding
                  :stop-decoding
                  :encode-next
                  :decode-next)
  (:import-from :unet-channel-base
                  :channel
                  :channel-peer
                  :channel-packet
                    :buffer-of
                    :peer-of)
  (:import-from :unet-channel-channel
                  :defchannel)

  (:export :defchannel
             :defchannel-slots
             :defchannel-peer-slots
             :defchannel-packet-slots
             :defchannel-fragmenter
             :defchannel-encoder
             :defchannel-decoder
           :channel
           :channel-peer
           :channel-packet
             :buffer-of
             :peer-of
           :return-packet
           :stop-encoding
           :stop-decoding
           :encode-next
           :decode-next))

(in-package :unet-channel-base)
