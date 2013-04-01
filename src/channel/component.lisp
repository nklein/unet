;;;; This file provides structures and methods needed to track
;;;; channel components.

(defpackage :unet-channel-component
  (:use :cl)
  (:import-from :userial
                  :with-buffer)
  (:export :extend-symbol
           :defchannel-slots
           :defchannel-peer-slots
           :defchannel-packet-slots
           :defchannel-fragmenter
           :defchannel-encoder
           :defchannel-decoder
           :defchannel
           :return-packet
           :stop-encoding
           :stop-decoding
           :encode-next
           :decode-next))

(in-package :unet-channel-component)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; class used to track channel component definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass channel-component ()
  (;; arguments and body for the fragmenter methods
   (fragmenter-args :accessor fragmenter-args-of
                    :initform '(channel peer packet))
   (fragmenter-body :accessor fragmenter-body-of
                    :initform nil)
   ;; arguments and body for the encoder methods
   (encoder-args :accessor encoder-args-of
                 :initform '(channel peer packet))
   (encoder-body :accessor encoder-body-of
                 :initform nil)
   ;; arguments and body for the decoder methods
   (decoder-args :accessor decoder-args-of
                 :initform '(channel peer packet))
   (decoder-body :accessor decoder-body-of
                 :initform nil)))

;;; function used retrieve the channel-component instance for a given
;;; component name
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun ensure-component (name)
    (anaphora:acond
      ((get name 'channel-component-info) anaphora:it)
      (t (let ((n (make-instance 'channel-component)))
           (setf (get name 'channel-component-info) n))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dealing with component-related slots
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Macro used below to initialize slots and initarg information for
;;; different portions of the channel-component class based on their
;;; accessors.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %add-slot-class (name slots rest)
    `(defclass ,name ()
       ,slots
       ,@rest)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun extend-symbol (symbol extension)
    (intern (concatenate 'string (symbol-name symbol) extension)
            (symbol-package symbol))))

;;; macro to create component-slots for the channel
(defmacro defchannel-slots (name slots &rest rest)
  (%add-slot-class name slots rest))

;;; macro to create component slots for the peer
(defmacro defchannel-peer-slots (name slots &rest rest)
  (%add-slot-class (extend-symbol name "-PEER") slots rest))

;;; macro to create component slots for the packet
(defmacro defchannel-packet-slots (name slots &rest rest)
  (%add-slot-class (extend-symbol name "-PACKET") slots rest))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dealing with component-related methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; macro used below internally to assign the argument list and body
;;; for the component based on the accessors for the method.
(defmacro %add-function-form (name args body &key args-of body-of)
  (let ((component (gensym "COMPONENT-VAR-")))
    `(let ((,component (gensym "COMPONENT-")))
       `(eval-when (:compile-toplevel :load-toplevel :execute)
          (let ((,,component (ensure-component ',,name)))
            (setf (,,args-of ,,component) ',,args
                  (,,body-of ,,component) ',,body))))))

;;; macro to set component args and body for the fragmenter method
(defmacro defchannel-fragmenter (name (channel peer payload &rest rest)
                                   &body body)
  (%add-function-form name `(,channel ,peer ,payload ,@rest) body
                      :args-of 'fragmenter-args-of
                      :body-of 'fragmenter-body-of))

;;; macro to set component args and body for the encoder method
(defmacro defchannel-encoder (name (channel peer payload &rest rest)
                                &body body)
  (%add-function-form name `(,channel ,peer ,payload ,@rest) body
                      :args-of 'encoder-args-of
                      :body-of 'encoder-body-of))

;;; macro to set component args and body for the decoder method
(defmacro defchannel-decoder (name (channel peer packet &rest rest)
                                &body body)
  (%add-function-form name `(,channel ,peer ,packet ,@rest) body
                      :args-of 'decoder-args-of
                      :body-of 'decoder-body-of))

