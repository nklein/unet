
(defpackage :unet-defchannel
  (:use :cl)
  (:export :defchannel))

(in-package :unet-defchannel)

(defun %get-slots (tag options)
  (rest (assoc tag options)))

(defun %get-initargs (tag options)
  (rest (assoc tag options)))

(defmacro %define-channel (name superclasses options)
  `(defclass ,name ,superclasses
     ,(%get-slots :channel options)
     (:default-initargs ,@(%get-initargs :channel-initargs options))))

(defun %recipient-name (name)
  (intern (concatenate 'string (symbol-name name) (symbol-name '-recipient))))

(defmacro %define-channel-recipient (name superclasses options)
  `(defclass ,(%recipient-name name) ,(mapcar #'%recipient-name superclasses)
     ,(%get-slots :recipient options)
     (:default-initargs ,@(%get-initargs :recipient-initargs options))))

(defun %packet-name (name)
  (intern (concatenate 'string (symbol-name name) (symbol-name '-packet))))

(defmacro %define-channel-packet (name superclasses options)
  `(defclass ,(%packet-name name) ,(mapcar #'%packet-name superclasses)
     ,(%get-slots :packet options)
     (:default-initargs ,@(%get-initargs :packet-initargs options))))

(defmacro defchannel (name superclasses &rest options)
  `(progn
     (%define-channel ,name ,superclasses ,options)
     (%define-channel-recipient ,name ,superclasses ,options)
     (%define-channel-packet ,name ,superclasses ,options)))
