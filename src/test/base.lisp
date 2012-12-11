(defpackage :unet-test-base
  (:use :cl))

(in-package :unet-test-base)

;;; Define a test that checks if something is a function or macro
(nst:def-criterion (:function-ish () (sym))
  (cond
    ((fboundp sym) (nst:make-success-report))
    ((not (null (macro-function sym))) (nst:make-success-report))
    (t (nst:make-failure-report :format "No such function or macro ~A"
                                :args (list sym)))))

;;; Define a test that checks if something matches a regex
(nst:def-criterion (:regex (needle) (haystack))
  (let ((scanner (cl-ppcre:create-scanner needle :single-line-mode t)))
    (if (cl-ppcre:scan scanner haystack)
        (nst:make-success-report)
        (nst:make-failure-report :format "Could not find ~S in ~S"
                                 :args (list needle haystack)))))
