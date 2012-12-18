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

(defun run-send-recv (&key local remote (to-send 0) (to-recv 0))
  (let ((counter 0))
    (labels ((run (to-send to-recv)
               #+thread-support
               (bordeaux-threads:thread-yield)
               (flet ((send ()
                        (let ((msg (format nil "Hi ~A" (incf counter))))
                          (send-datagram local msg remote))
                        (run (1- to-send) to-recv))
                      
                      (recv ()
                        (multiple-value-bind (msg from) (recv-datagram local)
                          (cond
                            (from
                            
                             (format t "~A GOT: ~A from ~A~%"
                                     (incf counter) msg from)
                             (run to-send (1- to-recv)))
                            (t (run to-send to-recv)))))
                      
                      (cont ()
                        (incf counter)
                        (run to-send to-recv)))
                 
                 (case (mod counter 3)
                   (0 (cond
                        ((plusp to-send) (send))
                        ((plusp to-recv) (recv))))
                   (1 (cond
                        ((plusp to-send) (send))
                        (t (cont))))
                   (2 (cond
                        ((plusp to-recv) (recv))
                        (t (cont))))))))

      (lambda () (run to-send to-recv)))))

(defun run-send-recv-thread (&key local remote (to-send 0) (to-recv 0))
  (bordeaux-threads:make-thread (run-send-recv :local local
                                               :remote remote
                                               :to-send to-send
                                               :to-recv to-recv)))

(defun wait-for-message (local)
  #+thread-support
  (bordeaux-threads:thread-yield)
  (multiple-value-bind (msg from) (recv-datagram local)
    (if from
        (values msg from)
        (wait-for-message local))))

#+thread-support
(nst:def-test-group mock-mt-socket-tests (mock-mt-network-provider
                                          alice-and-bob)
  (nst:def-test mock-mt-send-one-message (:values (:equal "Hi 1")
                                               (:equal alice))
    (let ((thread (run-send-recv-thread :local alice :remote bob :to-send 1)))
      (unwind-protect
          (wait-for-message bob)
        (bordeaux-threads:join-thread thread)))))
