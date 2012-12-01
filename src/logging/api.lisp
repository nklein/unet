
(defpackage :unet-logging
  (:use :cl)
  
  (:export :make-logger
           :add-logger-category
           :start-logging
           :stop-logging
           :log-string)
  
  (:import-from :unet-logging-base
                :add-logger-category
                :start-logging
                :stop-logging)
  
  (:import-from :unet-logging-cl-log
                :cl-log-logger
                :get-cl-log-manager))

(in-package :unet-logging)

;;; Create a logger instance
(defun make-logger ()
  (make-instance 'cl-log-logger))

(defmacro log-string (logger category string)
  `(unet-logging-cl-log:log-string ,logger ,category ,string))