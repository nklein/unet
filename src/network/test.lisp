(defpackage :unet-network-test
  (:use :cl
        :unet-network))

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
