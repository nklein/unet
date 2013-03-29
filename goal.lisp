(ql:quickload :userial)
(ql:quickload :anaphora)

(defpackage :channel-sample
  (:use cl userial anaphora))

(in-package :channel-sample)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *channel-components* (make-hash-table)))

(defclass channel-component ()
  ((channel-slots :accessor slots-of
                  :initform nil)
   (channel-initargs :accessor initargs-of
                     :initform nil)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun ensure-component (name)
    (acond
      ((gethash name *channel-components*) it)
      (t (let ((n (make-instance 'channel-component)))
           (setf (gethash name *channel-components*) n))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun ensure-slot-generics (slot)
    (loop :for pair :on (cdr slot) :by #'cddr
       :when (member (car pair) '(:accessor :reader :writer))
       :unless (fboundp (cadr pair))
       :collecting `(defgeneric ,(cadr pair) (instance)))))

(defmacro defchannel-slots (name slots &rest rest)
  (let ((component (gensym "COMPONENT-")))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (let ((,component (ensure-component ',name)))
         ,@(reduce #'append (mapcar #'ensure-slot-generics slots))
         (setf (slots-of ,component) ',slots
               ,@(awhen (rest (assoc :default-initargs rest))
                   `((initargs-of ,component) ',it)))))))

(defmacro defchannel-recipient-slots (name slots &rest rest)
  (declare (ignore name rest))
  `(progn ,@(reduce #'append (mapcar #'ensure-slot-generics slots))))

(defmacro defchannel-packet-slots (name slots &rest rest)
  (declare (ignore name rest))
  `(progn ,@(reduce #'append (mapcar #'ensure-slot-generics slots))))

(defmacro defchannel-encoder (name (channel recipient payload) &body body)
  (declare (ignore name channel recipient payload body)))

(defmacro defchannel-decoder (name (channel recipient packet) &body body)
  (declare (ignore name channel recipient packet body)))

(defchannel-slots ordered
    ((max-sequence-number :initarg :max-sequence-number
                          :reader max-sequence-number-of))
  (:default-initargs :max-sequence-number 65535))

(defchannel-recipient-slots ordered
    ((ordered-sequence-number :accessor ordered-sequence-number-of
                              :initform 0)))

(defun next-ordered-sequence-number (channel recipient)
  (let ((n (1+ (ordered-sequence-number-of recipient))))
    (when (< (max-sequence-number-of channel) n)
      (setf n 0))
    n))

(defchannel-encoder ordered (channel recipient payload)
  (serialize :uint16 (next-ordered-sequence-number channel recipient)))

(defchannel-decoder ordered (channel recipient packet)
  (setf (ordered-sequence-number-of packet) (unserialize :uint16)))

#+notyet
(defchannel ordered-channel (ordered))
