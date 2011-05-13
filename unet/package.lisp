;; Copyright (c) 2011 nklein software
;; MIT License. See included LICENSE.txt file for licensing details.

(defpackage :unet
  (:use :cl)
  (:export :set-logger
             :start-logging
             :stop-logging
           :invalid-hostname-error
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
             :server-channels-with-messages
             :server-close
	   :define-channel
	     :channel-add-recipient
	     :channel-remove-recipient
	     :send-packet
             :next-packet
	   :raw-channel
           :set-log-browser
             :*current-log-browser*
             :log-browser
             :next-log-message
             :previous-log-message
             :print-log-message))
