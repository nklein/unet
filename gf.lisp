
(ql:quickload :userial)
(ql:quickload :closer-mop)

(use-package :closer-mop)

(defclass encode-standard-generic-function (standard-generic-function)
  ()
  (:metaclass funcallable-standard-class))

(define-method-combination encode-combination ()
        ((methods (encode-combination)
                  :order :most-specific-first
                  :required t))
  `(progn
     ,@(mapcar (lambda (method)
                 `(call-method ,method))
               methods)
     (userial:serialize)
     )
  `(call-method ,(first methods) '(,@(rest methods))))

(defgeneric encode (channel payload)
  (:method-combination encode-combination)
  (:generic-function-class encode-standard-generic-function))

(defmethod encode (channel (payload string))
  (declare (ignore channel))
  (userial:serialize :string payload))

(defclass name-mixin ()
  ((name :initarg :name :initform :undefined)))

(defmethod encode ((channel name-mixin) payload)
  (with-slots (name) channel
    (userial:serialize :keyword name))
  (call-next-method))

(defclass ordered-mixin ()
  ((sequence-number :initform 42)))

(defmethod encode ((channel ordered-mixin) payload)
  (with-slots (sequence-number) channel
    (userial:serialize :uint32 sequence-number))
  (call-next-method))

(defclass fragment-mixin ()
  ((fragment-count :initform 3)
   (fragment :initform 0)))

(defmethod encode ((channel fragment-mixin) payload)
  (with-slots (fragment-count fragment) channel
    (incf fragment)
    (userial:serialize :uint32 fragment-count))
  (call-next-method))

(defclass no-channel (ordered-mixin name-mixin)
  ())

(let ((channel (make-instance 'no-channel :name :no)))
  (with-slots (sequence-number) channel
    (setf sequence-number 10))
  (encode channel "FOOBIE"))

