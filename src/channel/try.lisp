;;;; This file lets me try to use the channel stuff and see how it goes.

(defpackage :unet-channel-try
  (:use :cl :unet-channel :userial))

(in-package :unet-channel-try)

(defchannel-slots d1
  ((d1-tag :reader d1-tag-of
           :initarg :d1-tag))
  (:default-initargs :d1-tag 'd1-tag))

(defchannel-peer-slots d2
  ((d2-tag :reader d2-tag-of
           :initarg :d2-tag))
  (:default-initargs :d2-tag 'd2-tag))

(defchannel-packet-slots d1
  ((d1-tag :accessor d1-tag-of)
   (d2-tag :accessor d2-tag-of)))

(defchannel-encoder d1 (ch1 pr1 pk1 &rest rest1)
  (format t "D1: WRITING ~S~%" rest1)
  (serialize :keyword (d1-tag-of ch1)))

(defchannel-decoder d1 (ch1 pr1 pk1 &rest rest1)
  (format t "D1: READING ~S~%" rest1)
  (setf (d1-tag-of pk1) (unserialize :keyword)))

(defchannel-encoder d2 (ch2 pr2 pk2 &rest rest2)
  (format t "D2: WRITING ~S~%" rest2)
  (serialize :keyword (d2-tag-of pr2)))

(defchannel-decoder d2 (ch2 pr2 pk2 &rest rest2)
  (format t "D2: READING ~S~%" rest2)
  (setf (d2-tag-of pk2) (unserialize :keyword)))

(defchannel d1d2-channel (d1 d2))