;; Copyright (c) 2011 nklein software
;; MIT License. See included LICENSE.txt file for licensing details.

(in-package :unet)

;; ======================================================================
;; recipient class
;; ======================================================================
(defstruct (recipient (:constructor nil))
  host port)

(defmethod print-object ((recipient recipient) stream)
  (print-unreadable-object (recipient stream :type t :identity t)
    (format stream ":HOST ~S :PORT ~S"
	           (recipient-host recipient)
		   (recipient-port recipient))))

;; ----------------------------------------------------------------------
;; validate-hostname
;; ----------------------------------------------------------------------
(defun validate-hostname (hostname hostname-p)
  (log-it :unet-validate-hostname :string hostname
                                  :boolean (if hostname-p t nil))
  (handler-case
      (restart-case
	  (handler-case
	      (if hostname-p
		  (iolib:ensure-hostname hostname)
                  (error 'no-hostname-given-error :given nil))
	    (no-hostname-given-error (exception)
	      (error exception))
	    (iolib.sockets:resolver-no-name-error ()
	      (error 'no-such-host-error :given hostname))
	    (iolib.sockets:resolver-again-error ()
	      (error 'transient-name-service-error :given hostname)))
	(specify-new-hostname (new-hostname)
	  :report "Specify a new hostname"
	  :interactive (lambda ()
			 (format *query-io* "Enter a new hostname: ")
			 (force-output *query-io*)
                         (list (read-line *query-io*)))
          (log-it :unet-specify-new-hostname :string new-hostname)
	  (validate-hostname new-hostname t)))
    (iolib.sockets:resolver-fail-error ()
      (error 'permanent-name-service-error))))

;; ----------------------------------------------------------------------
;; validate-port-number
;; ----------------------------------------------------------------------
(defun validate-port-number (port)
  (log-it :unet-validate-port-number :uint16 port)
  ;; make sure port number is valid
  (restart-case (progn
		  (unless (typep port '(unsigned-byte 16))
		    (error 'invalid-port-error :datum port))
		  port)
    (specify-new-port (new-port)
      :report "Specify a new port number"
      :interactive (lambda ()
		     (format *query-io*
			     "Enter a port number between 0 and 65535: ")
                     (force-output *query-io*)
		     (list (read *query-io*)))
      (log-it :unet-specify-new-port-number :uint16 new-port)
      (validate-port-number new-port))))

;; ----------------------------------------------------------------------
;; initialize-instance :around recipient
;; ----------------------------------------------------------------------
(defmethod initialize-instance :around ((recipient recipient)
					&key (hostname nil hostname-p) port)
  (call-next-method)
  ;; make sure the hostname and port are valid
  (with-slots ((rh host) (rp port)) recipient
    (setf rh (iolib.sockets:address-to-vector
                (if (iolib.sockets:addressp hostname)
                    hostname
                    (validate-hostname hostname hostname-p)))
	  rp (validate-port-number port))
    (log-it :unet-initialize-recipient :unet-host rh :uint16 rp)))
