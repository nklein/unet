;;;; This file provides a wrapper around the logging mechanism.
;;;; It currently uses Nick Levine's cl-log, but hopefully abstracts
;;;; the interface to the point where this could be swapped out for
;;;; some other library if desired without having to changing anything
;;;; outside of this file.

(defpackage :unet-logging-cl-log
  (:use :cl)
  (:export :defcategory
           :log-string
           :log-binary)
  (:import-from :unet-utils-let-gensyms :let-gensyms))

(in-package :unet-logging-cl-log)

;;; Prepare a set of categories that will not interfere with other
;;; users of cl-log.
(defvar *categories* (make-instance 'cl-log:category-set))

;;; Prepare some log managers
(defvar *string-log-manager* nil)
(defvar *binary-log-manager* nil)

(defmacro ensure-logger (logger message-class)
  `(progn
     (unless ,logger
       (setf ,logger (make-instance 'cl-log:log-manager
                                    :message-class ,message-class)))
     ,logger))

(defun get-string-logger ()
  (ensure-logger *string-log-manager* 'cl-log:formatted-message))

(defun get-binary-logger ()
  (ensure-logger *binary-log-manager* 'cl-log:base-message))

(defmacro set-log-file (logfile &key messenger-name
                                     messenger-class
                                     manager)
  (let-gensyms ((logfile-var logfile)
                (name-var messenger-name)
                (class-var messenger-class)
                (manager-var manager))
    `(cond
       (,logfile-var (cl-log:start-messenger ,class-var
                                             :name ,name-var
                                             :manager ,manager-var
                                             :filename ,logfile-var))
       (t (cl-log:stop-messenger ,name-var
                                 :manager ,manager-var)))))

;;; Exported macro defcategory
(defmacro defcategory (category &rest subcategories)
  `(cl-log:defcategory ,category
                       ,(unless (null subcategories)
                           `(or ,category ,@subcategories))
     *categories*))

;;; Exported function set-string-log-file
(defun set-string-log-file (logfile)
  (set-log-file logfile
                :messenger-name "string-messenger"
                :messenger-class 'cl-log:text-file-messenger
                :manager (get-string-logger)))

;;; Exported function set-binary-log-file
(defun set-binary-log-file (logfile)
  (set-log-file logfile
                :messenger-name "binary-messenger"
                :messenger-class 'cl-log:text-file-messenger
                :manager (get-binary-logger)))

;;; Exported function log-string
(defmacro log-string (category string)
  `(cl-log:log-manager-message *string-log-manager*
                               ,category
                               ,string))

;;; Exported function log-string
(defmacro log-binary (category binary)
  `(cl-log:log-manager-message *binary-log-manager*
                               ,category
                               ,binary))
