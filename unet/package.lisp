;; Copyright (c) 2011 nklein software
;; MIT License. See included LICENSE.txt file for licensing details.

(defpackage :unet
  (:use :cl)
  (:export :invalid-hostname-error
	     :invalid-hostname-given
	     :no-such-host-error
	     :transient-name-service-error
	     :permanent-name-service-error
	   :invalid-port-error
	     :invalid-port-datum
	     :invalid-port-expected-type
	   :recipient-not-on-channel-error
	     :recipient-not-on-channel-channel
	     :recipient-not-on-channel-recipient
	   :recipient
	   :server
	   :define-channel
	     :channel-add-recipient
	     :channel-remove-recipient
	     :send-packet
	   :raw-channel))
