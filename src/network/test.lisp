(defpackage :unet-network-test
  (:use :cl)
  
  (:import-from :unet-network
                :make-remote-address
                :create-datagram-socket
                :send-datagram
                :recv-datagram
                :close-socket))

(in-package :unet-network-test)

;;; Make sure all of the expected API points exist
(nst:def-test-group existence-tests ()
  (nst:def-test network-package-exists (:true)
    (find-package :unet-network))
  
  (nst:def-test network-methods-exist (:each (:function-ish))
    '(unet-network:make-remote-address
      unet-network:create-datagram-socket
      unet-network:send-datagram
      unet-network:recv-datagram
      unet-network:close-socket)))


(nst:def-test-group mock-existence-tests ()
  (nst:def-test network-mock-package-exists (:true)
    (find-package :unet-network-mock))
  
  (nst:def-test network-mock-network-provider (:true)
    (find-class 'unet-network-mock:network-provider)))

;;; Make sure we can prepare a mock-network-provider
(nst:def-fixtures mock-network-provider
    (:documentation "Defines a mock network provider called PROVIDER")
  (provider (make-instance 'unet-network-mock:network-provider)))

(nst:def-test-group mock-make-provider-tests (mock-network-provider)
  (nst:def-test mock-make-provider (:true)
    provider))

(nst:def-test-group mock-make-remote-address-tests (mock-network-provider)
  (nst:def-test mock-make-remote-address (:true)
    (make-remote-address provider "localhost" 1187))
  
  (nst:def-test mock-make-remote-address-equalp (:forms-equal)
    (make-remote-address provider "localhost" 1187)
    (make-remote-address provider "localhost" 1187))
  
  (nst:def-test mock-make-remote-address-caseless (:forms-equal)
    (make-remote-address provider "localHost" 1187)
    (make-remote-address provider "LOCALHOST" 1187)))

;;; Prepare sockets for alice and bob
(nst:def-fixtures alice-and-bob
    (:documentation "Defines a socket called ALICE on port 31337 and another called BOB on port 54321"
     :special (provider))
  (alice (create-datagram-socket provider 31337))
  (bob (create-datagram-socket provider 54321)))

(nst:def-test-group mock-create-datagram-socket-tests (mock-network-provider
                                                       alice-and-bob)
  (nst:def-test mock-create-datagram-socket (:each :true)
    (list alice bob))
  
  (nst:def-test mock-no-message (:values (:eq nil) (:eq nil))
    (recv-datagram alice))
  
  (nst:def-test mock-single-message (:values (:equal "Hello")
                                             (:equal alice))
    (progn
      (send-datagram alice "Hello" bob)
      (recv-datagram bob)))
  
  (nst:def-test mock-only-one-message (:values (:eq nil) (:eq nil))
    (progn
      (send-datagram alice "Hello" bob)
      (recv-datagram bob)
      (recv-datagram bob)))
  
  (nst:def-test mock-no-messages-after-close (:values (:eq nil) (:eq nil))
    (progn
      (send-datagram alice "Hello" bob)
      (close-socket bob)
      (recv-datagram bob))))

;;; Make sure we can prepare a thread-safe mock-network-provider
#+thread-support
(nst:def-fixtures mock-mt-network-provider
    (:documentation "Defines a thread-safe mock network provider called PROVIDER")
  (provider (make-instance 'unet-network-mock:network-provider
                           :mutex (bordeaux-threads:make-lock))))

#+thread-support
(nst:def-test-group mock-mt-make-provider-tests (mock-mt-network-provider)
  (nst:def-test mock-make-provider (:true)
    provider))

(defun wait-for-message (local)
  #+thread-support
  (bordeaux-threads:thread-yield)
  (multiple-value-bind (msg from) (recv-datagram local)
    (if from
        (values msg from)
        (wait-for-message local))))

;;; Define a class used to create a thread that does some number of
;;; sends and receives.
(defclass send-recv-state ()
  ((local :initarg :local)
   (remote :initarg :remote)
   (to-send :initarg :to-send)
   (to-recv :initarg :to-recv)
   (counter :initform 0)
   (thread :accessor thread)
   (received :accessor received :initform nil)))

(defgeneric send (state))
(defgeneric recv (state))
(defgeneric cont (state))

(defmethod send ((state send-recv-state))
  (with-slots (to-send counter local remote) state
    (when (plusp to-send)
      (let ((msg (format nil "Hi ~A" counter)))
        (send-datagram local msg remote))
      (decf to-send)))
  (cont state))

(defmethod recv ((state send-recv-state))
  (with-slots (to-recv local received) state
    (multiple-value-bind (msg from) (wait-for-message local)
      (when from
        (setf received (append received (list (list msg from))))
        (decf to-recv))))
  (cont state))

(defmethod cont ((state send-recv-state))
  (with-slots (counter to-send to-recv) state
    (unless (and (zerop to-send)
                 (zerop to-recv))
      #+thread-support
      (bordeaux-threads:thread-yield)
      (case (mod (incf counter) 3)
        ((0 1) (send state))
        (2 (recv state))))))

#+thread-support
(defun run-send-recv-thread (&key local remote (to-send 0) (to-recv 0))
  (let* ((state (make-instance 'send-recv-state :local local
                                                :remote remote
                                                :to-send to-send
                                                :to-recv to-recv))
         (thread (bordeaux-threads:make-thread (lambda ()
                                                 (cont state)))))
    (setf (thread state) thread)
    state))

(defmacro with-thread ((&key local remote (to-send 0) (to-recv 0)) &body body)
  (let ((state (gensym "STATE-")))
    `(let ((,state (run-send-recv-thread :local ,local
                                         :remote ,remote
                                         :to-send ,to-send
                                         :to-recv ,to-recv)))
       (unwind-protect
            (progn
              ,@body)
         (bordeaux-threads:join-thread (thread ,state)))
       (received ,state))))

#+thread-support
(nst:def-test-group mock-mt-socket-tests (mock-mt-network-provider
                                          alice-and-bob)
  (nst:def-test mock-mt-recv-one-message (:values (:equal "Hi 1")
                                                  (:equal alice))
    (let (msg from)
      (with-thread (:local alice :remote bob :to-send 1)
        (multiple-value-bind (-msg -from) (wait-for-message bob)
          (setf msg -msg
                from -from)))
      (values msg from)))
  
  (nst:def-test mock-mt-send-one-message (:seq (:seq (:equal "Hi")
                                                     (:true)))
    (with-thread (:local alice :remote bob :to-recv 1)
      (send-datagram bob "Hi" alice)))
  
  (nst:def-test mock-mt-send-many-messages (:each (:seq (:regex "^Hi \\d+$")
                                                        (:true)))
    (let ((msg-count 1000))
      (with-thread (:local alice :remote bob :to-recv msg-count)
        (dotimes (cntr msg-count)
          (send-datagram bob (format nil "Hi ~A" cntr) alice)))))
  
  (nst:def-test mock-mt-send-recv-many (:each (:seq (:regex "^Hi \\d+$")
                                                    (:true)))
    (let ((high-count 1000)
          (low-count  50))
      (with-thread (:local alice :remote bob
                    :to-send low-count :to-recv high-count)
        (with-thread (:local bob :remote alice
                      :to-send high-count :to-recv low-count)
          t)))))
