;; Copyright (c) 2011 nklein software
;; MIT License. See included LICENSE.txt file for licensing details.

(defpackage :unet
  (:use :cl)
  (:export :invalid-hostname-error
	     :invalid-hostname-datum
	     :invalid-hostname-reason
	   :invalid-port-error
	     :invalid-port-datum
	     :invalid-port-expected-type
	   :recipient
	   :channel
	     :channel-socket
	     :channel-recipients
	     :channel-add-recipient
	     :channel-remove-recipient
	   :send-message))
