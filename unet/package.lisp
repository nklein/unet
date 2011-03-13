;; Copyright (c) 2011 nklein software
;; MIT License. See included LICENSE.txt file for licensing details.

(defpackage :unet
  (:use :cl)
  (:export :recipient
	     :recipient-host
	     :recipient-port
	   :channel
	     :channel-socket
	     :channel-recipients
	     :channel-add-recipient
	     :channel-remove-recipient
	   :send-message))
