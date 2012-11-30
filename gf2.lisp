
(ql:quickload :userial)
(ql:quickload :closer-mop)

(handler-bind ((name-conflict
                #'(lambda (conflict)
                    (with-accessors ((syms name-conflict-symbols)) conflict
                       (invoke-restart 'resolve-conflict (first syms))))))
 (use-package :closer-mop))

(defclass name-mixin ()
  ((name :initarg :name :initform (gensym "CH-"))))

(defclass ordered-mixin ()
  ((order-sequence :initform 0)))

(defclass no-channel (name-mixin ordered-mixin)
  ())

(defclass frag-method-class (standard-method)
  ())

(defclass frag-generic-function-class (standard-generic-function)
  ()
  (:metaclass funcallable-standard-class))

(defmethod compute-applicable-methods-using-classes ((gf frag-generic-function-class) args)
  (format t "compute-applicable-methods-using-classes~%")
  (call-next-method))

(defgeneric frag (channel payload)
  (:method (channel payload)
     (declare (ignorable channel))
     (format t "(frag t payload)~%")
     payload)
  (:method ((channel name-mixin) payload)
     (declare (ignorable channel payload))
     (format t "(frag name-mixin payload)~%")
     (call-next-method))
  (:method ((channel ordered-mixin) payload)
     (declare (ignorable channel payload))
     (format t "(frag ordered-mixin payload)~%")
     (call-next-method))
  (:method ((channel no-channel) payload)
     (declare (ignorable channel payload))
     (format t "(frag no-channel payload)~%")
     (call-next-method))
  (:generic-function-class frag-generic-function-class)
  (:method-class frag-method-class))

(defgeneric encode (channel payload)
  (:method ((channel name-mixin) payload)
     (userial:serialize :keyword (slot-value channel 'name)))
  (:method ((channel ordered-mixin) payload)
     (userial:serialize :uint32 (incf (slot-value channel 'order-sequence))))
  (:method ((channel no-channel) payload)
     (userial:serialize :keyword :no)))

(let ((ch (make-instance 'no-channel)))
  (userial:buffer-rewind)
  (encode ch "PAYLOAD"))
#(2 78 79 7 67 72 45 49 48 50 54 0 0 0 1)qqqqqqqqqqqqqqqqq    gbs.r,
