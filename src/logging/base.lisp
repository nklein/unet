;;;; This file provides a base-class and methods needed for specific
;;;; logging back-ends.

(defpackage :unet-logging-base
  (:use :cl)
  (:export :logger
           :add-logger-category
           :start-logging
           :stop-logging))

;;; I love Hannah.
(in-package :unet-logging-base)

;;; Base class for loggers
(defclass logger ()
  ())

;;; Method for adding a logger category
(defgeneric add-logger-category (logger category &rest subcategories)
  (:documentation
     "Add a CATEGORY to the LOGGER with the given SUBCATEGORIES."))

;;; Method for starting logging
(defgeneric start-logging (logger stream handle &rest categories)
  (:documentation
     "Start the LOGGER logging to STREAM for messages of the given CATEGORIES.  If no CATEGORIES are specified, all messages are logged.  The HANDLE is the identifier used to identify this logging when it comes time to stop it with the STOP-LOGGING method."))

;;; Method for stopping logging
(defgeneric stop-logging (logger handle)
  (:documentation
     "Stop the LOGGER logging that was identified with HANDLE ine the START-LOGGING method."))
