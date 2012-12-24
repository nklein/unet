(defpackage :unet-test
  (:use :cl)
  (:export :run-tests))

(in-package :unet-test)

;;; run all of the tests
(defun run-tests ()
  (nst:nst-cmd :run-package :unet-logging-test
                            :unet-network-test))