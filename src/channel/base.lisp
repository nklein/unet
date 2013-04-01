;;;; This file provides baseline slots and methods for channels.

(defpackage :unet-channel-base
  (:use :cl)
  (:import-from :userial
                  :buffer
                  :serialize
                  :unserialize
                  :with-buffer)
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
                  ))

(in-package :unet-channel-base)

;;; Base channel slots
(defchannel-slots channel
  ())

;;; Base peer slots
(defchannel-peer-slots channel
  ())

;;; Base packet slots
(defchannel-packet-slots channel
  ((buffer :accessor buffer-of
           :type buffer)
   (peer :accessor peer-of
         :type channel-peer)))

;;; Base fragmenter
(defchannel-fragmenter channel (channel peer packet))

;;; Base encoder
(defchannel-encoder channel (channel peer packet)
  (return-packet packet))

;;; Base decoder
(defchannel-decoder channel (channel peer packet)
  (setf (peer-of packet) peer)
  (return-packet packet))
