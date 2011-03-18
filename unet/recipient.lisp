;; Copyright (c) 2011 nklein software
;; MIT License. See included LICENSE.txt file for licensing details.

(in-package :unet)

(defclass recipient ()
  ((host :accessor recipient-host)
   (port :initarg :port
	 :accessor recipient-port))
  (:default-initargs :port 26354))

(defmethod print-object ((recipient recipient) stream)
  (print-unreadable-object (recipient stream :type t :identity t)
    (format stream ":HOST ~S :PORT ~S"
	           (recipient-host recipient)
		   (recipient-port recipient))))

(defun validate-hostname (recipient hostname hostname-p)
  (restart-case
    (handler-case
	(if hostname-p
	    (setf (recipient-host recipient) (iolib:ensure-hostname hostname))
	    (error 'invalid-hostname-error :datum nil
		                           :reason :must-specify-hostname))
      (invalid-hostname-error (exception) (error exception))
      (t (exception)
	(error 'invalid-hostname-error
	       :datum hostname
	       :reason (typecase exception
			 (iolib.sockets:resolver-no-name-error :no-such-host)
			 (iolib.sockets:resolver-fail-error :no-name-service)
			 (iolib.sockets:resolver-again-error :transient-error)
			 (t :unknown)))))
    (specify-new-hostname (new-hostname)
      :report "Specify a new hostname"
      :interactive (lambda ()
		     (format *query-io* "Enter a new hostname: ")
                     (force-output *query-io*)
		     (list (read-line *query-io*)))
      (validate-hostname recipient new-hostname t))))

(defun validate-port-number (port)
  ;; make sure port number is valid
  (restart-case (progn
		  (unless (typep port '(unsigned-byte 16))
		    (error 'invalid-port-error :datum port))
		  port)
    (specify-new-port (new-port)
      :report "Specify a new port number"
      :interactive (lambda ()
		     (format *query-io*
			     "Enter a port number between 0 and 65536: ")
                     (force-output *query-io*)
		     (list (read *query-io*)))
      (validate-port-number new-port))))

(defmethod initialize-instance :around ((recipient recipient)
					&key (hostname nil hostname-p) port)
  ;; make sure the hostname, if given, is valid
  (validate-hostname recipient hostname hostname-p)
  (call-next-method recipient :port (validate-port-number port)))
