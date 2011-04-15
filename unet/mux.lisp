;; Copyright (c) 2011 nklein software
;; MIT License. See included LICENSE.txt file for licensing details.

(in-package :unet)

;; ----------------------------------------------------------------------
;; multiplex-channel -- [PUBLIC]
;; ----------------------------------------------------------------------
(define-channel (multiplex-channel ()
		   ((channel-id  :initarg :channel-id)))
                multiplex-recipient ())

(defmethod initialize-instance :after ((channel multiplex-channel)
				       &key &allow-other-keys)
  (unless (slot-boundp channel 'channel-id)
    (error "XXX: need to specify channel-id")))

;; ----------------------------------------------------------------------
;; prepare-packets multiplex-method -- [PUBLIC]
;; ----------------------------------------------------------------------
(defmethod prepare-packets :around ((channel multiplex-channel)
				    (channel-recipient multiplex-recipient)
				    payload
				    &key channel-key)
  (with-slots (channel-id) channel
    (userial:serialize channel-key channel-id))
  (call-next-method))

;; ----------------------------------------------------------------------
;; handle-packet multiplex-method (query-mode) -- [PUBLIC]
;; ----------------------------------------------------------------------
(defmethod handle-packet ((channel multiplex-channel)
			  (channel-recipient (eql nil))
			  &key channel-key)
  (prog1
      (userial:unserialize channel-key)
    (userial:buffer-rewind)))

;; ----------------------------------------------------------------------
;; handle-packet multiplex-method -- [PUBLIC]
;; ----------------------------------------------------------------------
(defmethod handle-packet :around ((channel multiplex-channel)
				  (channel-recipient multiplex-recipient)
				  &key channel-key)
  (with-slots (channel-id) channel
    (unless (equal channel-id (userial:unserialize channel-key))
      (error "XXX wrong channel")))
  (call-next-method))
