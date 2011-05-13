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
(defun validate-hostname (hostname)
  (log-it :unet-validate-hostname :string (to-string hostname))
  (handler-case
      (restart-case
	  (handler-case
	      (if hostname
		  (usocket::host-to-vector-quad hostname)
                  (error 'no-hostname-given-error :given nil))
	    (usocket:ns-host-not-found-error ()
	      (error 'no-such-host-error :given hostname))
	    (usocket:ns-try-again-condition ()
	      (error 'transient-name-service-error :given hostname)))
	(specify-new-hostname (new-hostname)
	  :report "Specify a new hostname"
	  :interactive (lambda ()
			 (format *query-io* "Enter a new hostname: ")
			 (force-output *query-io*)
                         (list (read-line *query-io*)))
          (log-it :unet-specify-new-hostname :string (to-string new-hostname))
	  (validate-hostname new-hostname)))
    (usocket:ns-no-recovery-error ()
      (error 'permanent-name-service-error))))

;; ----------------------------------------------------------------------
;; validate-port-number
;; ----------------------------------------------------------------------
(defun validate-port-number (port)
  (log-it :unet-validate-port-number :string (to-string port))
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
      (log-it :unet-specify-new-port-number :string (to-string new-port))
      (validate-port-number new-port))))

;; ----------------------------------------------------------------------
;; initialize-instance :around recipient
;; ----------------------------------------------------------------------
(defmethod initialize-instance :around ((recipient recipient)
					&key hostname port)
  (call-next-method)
  ;; make sure the hostname and port are valid
  (with-slots ((rh host) (rp port)) recipient
    (setf rh (if (and (not (stringp hostname)) (vectorp hostname))
                 hostname
                 (validate-hostname hostname))
	  rp (validate-port-number port))
    (log-it :unet-initialize-recipient :unet-host rh :uint16 rp)))
