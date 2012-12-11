(defpackage :unet-network-test
  (:use :cl))

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