;; Copyright (c) 2011 nklein software
;; MIT License. See included LICENSE.txt file for licensing details.

(in-package :unet)

#|
The raw-channel does nothing to the payload in either the
prepare-packet or handle-packet calls.  In fact, the raw-channel
class short-circuits those methods so they do not even reserialize
the packet.

It will claim to accept any packet.  As such, if there is to
be any raw-channel for a server, it should be the only one and
it should be added to the server before any other channels so
that it will be checked last.
|#

;; ----------------------------------------------------------------------
;; raw-channel channel class -- [PUBLIC]
;; ----------------------------------------------------------------------
(define-channel (raw-channel () ()) raw-recipient ())

;; ----------------------------------------------------------------------
;; raw-channel prepare-packets method
;; ----------------------------------------------------------------------
(defmethod prepare-packets ((channel raw-channel)
			    (channel-recipient raw-recipient)
			    payload &key &allow-other-keys)
  (declare (ignore channel))
  (push payload (pending-packets channel-recipient)))

;; ----------------------------------------------------------------------
;; raw-channel message-for-channel-p method
;; ----------------------------------------------------------------------
(defmethod message-for-channel-p (message (channel raw-channel))
  (declare (ignore message))
  t)

;; ----------------------------------------------------------------------
;; raw-channel handle-packet method
;; ----------------------------------------------------------------------
(defmethod handle-packet ((channel raw-channel)
			  (channel-recipient raw-recipient)
			  packet
			  &key &allow-other-keys)
  (declare (ignore channel channel-recipient))
  packet)
