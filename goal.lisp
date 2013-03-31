(ql:quickload :userial)
(ql:quickload :anaphora)

(defpackage :channel-sample
  (:use cl userial anaphora))

(in-package :channel-sample)

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
      ((get name 'channel-component-info) it)
      (t (let ((n (make-instance 'channel-component)))
           (setf (get name 'channel-component-info) n))))))

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

(defmacro defchannel (name (&rest component-names))
  ;; XXX - finish this
  `(progn
     (%create-decoder ,name ,component-names)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Target code generation...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Given the following:
(defchannel-packet-slots d1 ((d1-info :accessor d1-info-of)))
(defchannel-encoder d1 (chl1 rcpt1 pkt1)
  (serialize :d1 :blah-blah-blah-1))
(defchannel-decoder d1 (chl1 rcpt1 pkt1)
  (setf (d1-info-of pkt1) (unserialize :d1)))

(defchannel-packet-slots d2 ((d2-info :accessor d2-info-of)))
(defchannel-encoder d2 (chl2 rcpt2 pkt2)
  (serialize :d2 :blah-blah-blah-2))
(defchannel-decoder d2 (chl2 rcpt2 pkt2)
  (setf (d2-info-of pkt2) (unserialize :d2)))

;;; Then we want this form:
(defchannel d1-d2-channel (d1 d2))
;;; To produce code like this:
(defclass d1-d2-channel (channel)
  ())
(defclass d1-d2-channel-recipient (channel-recipient)
  ())
(defclass d1-d2-channel-packet (channel-packet)
  ((d1-info :accessor d1-info-of)
   (d2-info :accessor d2-info-of)))

(defchannel-packet-slots channel
  ((recipient :accessor recipient-of)
   (buffer :accessor buffer-of
           :initform (make-array 4096
                                 :element-type '(unsigned-byte 8)
                                 :adjustable t
                                 :fill-pointer 0))
   (payload :accessor payload-of
            :initform nil)))
(defchannel-decoder channel (ch rc pk)
  (setf (payload-of pk) (unserialize :payload)
        (recipient-of rc) pk)
  (return-packet pk))

(defclass channel-packet ()
  ((recipient :accessor recipient-of)
   (buffer :accessor buffer-of
           :initform (make-array 4096
                                 :element-type '(unsigned-byte 8)
                                 :adjustable t
                                 :fill-pointer 0))
   (payload :accessor payload-of
            :initform nil)))

(defmethod channel-fragmenter ((channel d1-d2-channel)
                               (recipient d1-d2-channel-recipient)
                               (packet d1-d2-channel-packet))
  (declare (ignorable channel recipient))
  (return-from channel-fragmenter (list packet)))

(defun extend-symbol (symbol extension)
  (intern (concatenate 'string (symbol-name symbol) extension)
          (symbol-package symbol)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %make-decoder-flet (name)
    (let* ((component (ensure-component name))
           (args (decoder-args-of component))
           (body (decoder-body-of component)))
      `(,name (,@args)
         (declare (ignorable ,@args))
         ,@body))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %make-decoder-call (name loop-var list-of-packets channel recipient packet)
    `(dolist (,loop-var ,list-of-packets)
       (destructuring-bind (,recipient ,packet) ,loop-var
         (with-buffer (buffer-of ,packet)
           (,name ,channel ,recipient ,packet))))))

(defgeneric channel-decoder (channel recipient packet))

(defmacro %create-decoder (name component-names)
  (let ((channel (gensym "CHANNEL-"))
        (recipient (gensym "RECIPIENT-"))
        (packet (gensym "PACKET-"))
        (loop-var (gensym "LOOP-VAR-"))
        (list-of-packets (gensym "LIST-OF-PACKETS-"))
        (return-list (gensym "RETURN-LIST-"))
        (channel-type name)
        (recipient-type (extend-symbol name "-RECIPIENT"))
        (packet-type (extend-symbol name "-PACKET")))
    `(defmethod channel-decoder ((,channel ,channel-type)
                                 (,recipient ,recipient-type)
                                 (,packet ,packet-type))
       (let ((,list-of-packets (list (list ,recipient ,packet)))
             (,return-list nil))
         (flet ((insert-packet (recipient packet)
                  (check-type recipient ,recipient-type)
                  (check-type packet ,packet-type)
                  (push (list recipient packet) ,list-of-packets))
                (return-packet (packet)
                  (check-type packet ,packet-type)
                  (push packet ,return-list))
                (stop-decoding ()
                  (return-from channel-decoder (nreverse ,return-list))))
           (flet ,(mapcar #'(lambda (name)
                              (%make-decoder-flet name))
                          component-names)
             ,@(mapcar #'(lambda (name)
                           (%make-decoder-call name
                                               loop-var
                                               list-of-packets
                                               channel
                                               recipient
                                               packet))
                       component-names))
           (stop-decoding))))))

(%create-decoder d1-d2-channel (d1 d2 channel))

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
