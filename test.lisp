(defpackage #:test
  (:use #:cl))

(in-package #:test)

(defclass channel-component ()
  ((super-classes           :accessor get-super-classes
                            :initarg :super-classes)
   (slots                   :accessor get-slots
                            :initarg :slots)
   (packetize-macrolet-form :accessor get-packetize-macrolet-form
                            :initarg :packetize-macrolet-form)
   (serialize-macrolet-form :accessor get-serialize-macrolet-form
                            :initarg :serialize-macrolet-form))
  (:default-initargs
     :super-classes nil
     :slots nil
     :packetize-macrolet-form nil
     :serialize-macrolet-form nil))

(defvar *channel-components* (make-hash-table))

(defmacro defchannel (name super-classes slots)
  (let ((name-sym (gensym "NAME-")))
    `(let ((,name-sym ',name))
       (setf (gethash ,name-sym *channel-components*)
             (make-instance 'channel-component
                            :super-classes '(,@super-classes)
                            :slots '(,@slots))))))

;;; ------------------------------------------------------------

(defmacro with-channel ((channel-var) channel-component &body body)
  `(let ((,channel-var (gethash ,channel-component *channel-components*)))
     (when ,channel-var
       ,@body)))

;;; ------------------------------------------------------------

(defmacro with-super-classes ((super-classes-var) channel-var &body body)
  `(when ,channel-var
     (with-accessors ((,super-classes-var get-super-classes))
         ,channel-var
       ,@body)))

;;; ------------------------------------------------------------

(defmacro with-channel-slots ((slots-var) channel-var &body body)
  `(when ,channel-var
     (with-accessors ((,slots-var get-slots))
         ,channel-var
       ,@body)))

;;; ------------------------------------------------------------

(defmacro with-packetizer ((packetize-var) channel-var &body body)
  `(when ,channel-var
     (with-accessors ((,packetize-var get-packetize-macrolet-form))
         ,channel-var
       ,@body)))

(defmacro defpacketizer (channel-component (payload-var) &body body)
  (let ((channel (gensym "CHANNEL-"))
        (packetize (gensym "PACKETIZE-")))
    `(with-channel (,channel) ',channel-component
       (with-packetizer (,packetize) ,channel
          (setf ,packetize '((,payload-var) ,@body))))))

;;; ------------------------------------------------------------

(defmacro with-serializer ((serialize-var) channel-var &body body)
  `(when ,channel-var
     (with-accessors ((,serialize-var get-serialize-macrolet-form))
         ,channel-var
       ,@body)))

(defmacro defserializer (channel-component (channel-var payload-var)
                                           &body body)
  (let ((channel (gensym "CHANNEL-"))
        (serialize (gensym "SERIALIZE-")))
    `(with-channel (,channel) ',channel-component
       (with-serializer (,serialize) ,channel
          (setf ,serialize '((,channel-var ,payload-var) ,@body))))))

;;; ------------------------------------------------------------

(defun get-serializer (channel-component-sym)
  (with-channel (channel) channel-component-sym
     (with-serializer (serializer) channel
        serializer)))

(defun get-serializers (channel-component-sym)
  (with-channel (channel) channel-component-sym
    (nreverse
     (list* (get-serializer channel-component-sym)
            (with-super-classes (supers) channel
               (mapcar #'get-serializer supers))))))

(defun make-serializer-from-list (list channel-var payload-var)
  (cond
   ((null list) `(serialize ,channel-var ,payload-var))
   (t (destructuring-bind (first &rest rest) list
        (cond
         ((null first)
             (make-serializer-from-list rest channel-var payload-var))
         (t `(flet ((serialize ,@first))
               ,(make-serializer-from-list rest channel-var payload-var))))))))

(defmacro make-serializer (channel-component-sym)
  (let ((lambda-channel (gensym "LCHANNEL-"))
        (lambda-payload (gensym "LPAYLOAD-"))
        (channel-var (gensym "CHANNEL-"))
        (payload-var (gensym "PAYLOAD-"))
        (serializers (get-serializers channel-component-sym)))
    `(lambda (,lambda-channel ,lambda-payload)
       (flet ((serialize (,channel-var ,payload-var)
                 (declare (ignore ,channel-var))
                 (userial:serialize :string ,payload-var)))
         ,(make-serializer-from-list serializers
                                     lambda-channel
                                     lambda-payload)))))

(make-serializer no)

(defclass channel-info ()
  ((name :initarg :name)))

(progn
  (userial:buffer-rewind)
  (funcall (make-serializer nof)
           (make-instance 'channel-info :name :foo)
           "FOOBAR"))
#(3 70 79 79 0 0 0 42 0 1 0 3 6 70 79 79 66 65 82)



(userial:buffer-rewind)








;;; ------------------------------------------------------------

;;; ------------------------------------------------------------

(defchannel named-channel () ((name :initarg :channel-name)))

(defchannel fragment-channel () ())

(defchannel ordered-channel () ())

(defchannel nfo (named-channel fragment-channel ordered-channel) ())

(defchannel nof (named-channel ordered-channel fragment-channel) ())

(defchannel no (named-channel ordered-channel) ())

(defpacketizer fragment-channel (payload)
  (labels ((fragment-sequence (payload accum)
             (cond
               ((string= payload "") (nreverse accum))
               (t (let ((len (min (length payload) 5)))
                    (fragment-sequence (subseq payload 5)
                                       (cons (subseq payload 0 5) accum))))))))
  (fragment-sequence (serialize payload) nil))

(defserializer named-channel (channel payload)
  (with-slots (name) channel
    (userial:serialize :keyword name))
  (serialize channel payload))

(defserializer fragment-channel (channel payload)
  (declare (ignore channel))
  (userial:serialize* :uint16 1 :uint16 3 :string payload))

(defserializer ordered-channel (channel payload)
  (userial:serialize* :uint32 42)
  (serialize channel payload))
