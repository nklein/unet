(ql:quickload :userial)
(ql:quickload :anaphora)

(defpackage :channel-sample
  (:use cl userial anaphora))

(in-package :channel-sample)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *channel-components* (make-hash-table)))

(defclass channel-component ()
  ((channel-slots :accessor channel-slots-of
                  :initform nil)
   (channel-initargs :accessor channel-initargs-of
                     :initform nil)
   (recipient-slots :accessor recipient-slots-of
                    :initform nil)
   (recipient-initargs :accessor recipient-initargs-of
                       :initform nil)
   (packet-slots :accessor packet-slots-of
                 :initform nil)
   (packet-initargs :accessor packet-initargs-of
                    :initform nil)
   (encoder-args :accessor encoder-args-of
                 :initform nil)
   (encoder-body :accessor encoder-body-of
                 :initform nil)
   (decoder-args :accessor decoder-args-of
                 :initform nil)
   (decoder-body :accessor decoder-body-of
                 :initform nil)
   (fragmenter-args :accessor fragmenter-args-of
                    :initform nil)
   (fragmenter-body :accessor fragmenter-body-of
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

(defmacro %add-slot-forms (name slots rest
                           &key slot-accessor initargs-accessor)
  (let ((component (gensym "COMPONENT-VAR-")))
    `(let ((,component (gensym "COMPONENT-")))
       `(eval-when (:compile-toplevel :load-toplevel :execute)
          (let ((,,component (ensure-component ',,name)))
            ,@(reduce #'append (mapcar #'ensure-slot-generics ,slots))
            (setf (,,slot-accessor ,,component) ',slots
                  ,@(awhen (rest (assoc :default-initargs ,rest))
                           `((,,initargs-accessor ,,component) ',it))))))))

(defmacro defchannel-slots (name slots &rest rest)
  (%add-slot-forms name slots rest
                   :slot-accessor 'channel-slots-of
                   :initargs-accessor 'channel-initargs-of))

(defmacro defchannel-recipient-slots (name slots &rest rest)
  (%add-slot-forms name slots rest
                   :slot-accessor 'recipient-slots-of
                   :initargs-accessor 'recipient-initargs-of))

(defmacro defchannel-packet-slots (name slots &rest rest)
  (%add-slot-forms name slots rest
                   :slot-accessor 'packet-slots-of
                   :initargs-accessor 'packet-initargs-of))

(defmacro %add-function-form (name args body &key args-of body-of)
  (let ((component (gensym "COMPONENT-VAR-")))
    `(let ((,component (gensym "COMPONENT-")))
       `(eval-when (:compile-toplevel :load-toplevel :execute)
          (let ((,,component (ensure-component ',,name)))
            (setf (,,args-of ,,component) ',,args
                  (,,body-of ,,component) ',,body))))))

(defmacro defchannel-fragmenter (name (channel recipient payload) &body body)
  (%add-function-form name `(,channel ,recipient ,payload) body
                      :args-of 'fragmenter-args-of
                      :body-of 'fragmenter-body-of))

(defmacro defchannel-encoder (name (channel recipient payload) &body body)
  (%add-function-form name `(,channel ,recipient ,payload) body
                      :args-of 'encoder-args-of
                      :body-of 'encoder-body-of))

(defmacro defchannel-encoder (name (channel recipient payload) &body body)
  (let ((component (gensym "COMPONENT-")))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (let ((,component (ensure-component ',name)))
         (setf (encoder-args-of ,component) '(,channel ,recipient ,payload)
               (encoder-body-of ,component) ',body)))))

(defmacro defchannel-decoder (name (channel recipient packet) &body body)
  (%add-function-form name `(,channel ,recipient ,packet) body
                      :args-of 'decoder-args-of
                      :body-of 'decoder-body-of))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; example of use
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defchannel-slots ordered
    ((max-sequence-number :initarg :max-sequence-number
                          :reader max-sequence-number-of))
  (:default-initargs :max-sequence-number 65535))

(defchannel-recipient-slots ordered
    ((ordered-sequence-number :accessor ordered-sequence-number-of
                              :initform 0)))

(defchannel-packet-slots ordered
    ((ordered-sequence-number :accessor ordered-sequence-number-of)))

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
