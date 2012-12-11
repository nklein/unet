(defpackage :unet-logging-test
  (:use :cl)
  
  (:import-from :unet-logging
                :make-logger
                :add-logger-category
                :start-logging
                :stop-logging
                :log-string))

(in-package :unet-logging-test)

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

;;; Make sure all of the expected API points exist
(nst:def-test-group existence-tests ()
  (nst:def-test logging-package-exists (:true)
    (find-package :unet-logging))
  
  (nst:def-test make-logger-exists (:function-ish)
    'unet-logging:make-logger)
  
  (nst:def-test add-logger-category-exists (:function-ish)
    'unet-logging:add-logger-category)

  (nst:def-test start-logging-exists (:function-ish)
    'unet-logging:start-logging)

  (nst:def-test stop-logging-exists (:function-ish)
    'unet-logging:stop-logging)

  (nst:def-test log-string (:function-ish)
    'unet-logging:stop-logging))

;;; Make sure we can prepare a logger
(nst:def-fixtures simple-logger
    (:documentation "Defines a logger LOGGER.")
  (logger (make-logger)))

(nst:def-test-group instantiate-logger (simple-logger)
  (nst:def-test make-logger-succeeds (:true)
    logger)
  
  (nst:def-test add-base-category-succeeds (:eq :animal)
    (add-logger-category logger :animal))

  (nst:def-test add-sub-category-succeeds (:eq :animal)
    (add-logger-category logger :animal :mammal))
  
  (nst:def-test add-multiple-sub-categories-succeeds (:eq :mammal)
    (add-logger-category logger :mammal :dog :cat :mouse :human)))

;;; Make sure we can log a simple message
(nst:def-fixtures categoried-logger
    (:documentation "Defines a logger LOGGER with some basic categories."
     :setup (progn
              (add-logger-category logger :fish)
              (add-logger-category logger :primate :ape :chimp :human)
              (add-logger-category logger :mammal :dog :cat :mouse :primate)
              (add-logger-category logger :animal :mammal :fish)))
  (logger (make-logger)))

(defmacro with-logging-to-string ((logger &rest categories) &body body)
  (let ((handle (gensym "LOGGER-HANDLE-"))
        (stream (gensym "STREAM-")))
    `(with-output-to-string (,stream)
       (start-logging ,logger ,stream ',handle ,@categories)
       (unwind-protect
            (progn ,@body)
         (stop-logging ,logger ',handle)))))

(nst:def-test-group log-simple-messages (categoried-logger)
  (nst:def-test log-a-message-to-nowhere (:not :err)
    (log-string logger :dog "Woof"))
  
  (nst:def-test log-message-works (:regex ".*DOG Woof$")
    (with-logging-to-string (logger)
      (log-string logger :dog "Woof")))
  
  (nst:def-test log-message-for-subcategory (:regex ".*DOG Woof$")
    (with-logging-to-string (logger :mammal)
      (log-string logger :dog "Woof")))
  
  (nst:def-test log-message-unclaimed (:equal "")
    (with-logging-to-string (logger :mammal)
      (log-string logger :fish "Bloop")))
  
  (nst:def-test log-messages-to-different-logs
      (:values (:regex ".*CAT Meow.*DOG Woof$") (:regex ".*FISH Bloop$"))
    (let (m f)
      (setf m (with-logging-to-string (logger :mammal)
                (setf f (with-logging-to-string (logger :fish)
                          (log-string logger :cat "Meow")
                          (log-string logger :dog "Woof")
                          (log-string logger :fish "Bloop")))))
      (values m f))))
