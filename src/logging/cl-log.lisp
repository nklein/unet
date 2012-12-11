;;;; This file provides a wrapper around the logging mechanism.
;;;; It currently uses Nick Levine's cl-log, but hopefully abstracts
;;;; the interface to the point where this could be swapped out for
;;;; some other library if desired without having to changing anything
;;;; outside of this file.

(defpackage :unet-logging-cl-log
  (:use :cl)
  
  (:export :cl-log-logger
           :log-string)
  
  (:import-from :unet-logging-base :logger
                                   :add-logger-category
                                   :start-logging
                                   :stop-logging))

;;; Hannah has dibs.
(in-package :unet-logging-cl-log)

;;; Logger class based on cl-log.
(defclass cl-log-logger (logger)
  ((manager :initform (make-instance 'cl-log:log-manager
                         :categories (make-instance 'cl-log:category-set)
                         :message-class 'cl-log:formatted-message)
            :reader get-cl-log-manager)))

;;; Private function for turning a category list into a filter
(defun make-category-decl (category subcategories)
  `(or ,category ,@subcategories))

(defun logger-categories (logger)
  (cl-log:log-manager-category-set (get-cl-log-manager logger)))

;;; Method for adding a logger category
(defmethod add-logger-category ((logger cl-log-logger)
                                category &rest subcategories)
  (cl-log:defcategory-fn category
      (make-category-decl category subcategories)
      (logger-categories logger)))

;;; Private function for turning a category list into a filter
(defun make-category-filter (categories)
  (case (length categories)
    (0 nil)
    (1 (first categories))
    (t `(or ,@categories))))

;;; Method for starting logging
(defmethod start-logging ((logger cl-log-logger) stream handle &rest categories)
  (cl-log:start-messenger 'cl-log:text-stream-messenger
                          :stream stream
                          :name handle
                          :manager (get-cl-log-manager logger)
                          :filter (make-category-filter categories)))

;;; Method for stopping logging
(defmethod stop-logging ((logger cl-log-logger) handle)
  (cl-log:stop-messenger handle
                         :manager (get-cl-log-manager logger)))

;;; Exported macro for logging a string
(defmacro log-string (logger category string)
  (let ((manager-var (gensym "MANAGER-")))
    `(cl-log:log-manager-message (get-cl-log-manager ,logger)
                                 ,category
                                 ,string)))
