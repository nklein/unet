
(defpackage :unet-base-channel
  (:use cl)
  (:import-from :unet-defchannel :defchannel)
  (:export :base-channel
           :channel-name
           :channel-no-name-specified))

(in-package :unet-base-channel)

(define-condition channel-no-name-specified (error)
  ())

(defchannel base-channel ()
  (:channel (channel-name :initarg :name
                          :type keyword
                          :reader channel-name))
  (:channel-initargs :name (error 'channel-no-name-specified)))