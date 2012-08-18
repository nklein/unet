;; Copyright (c) 2012 nklein software
;; MIT License. See included LICENSE.txt file for licensing details.

(in-package :unet)

(defclass base-channel ()
  ((channel-id :initform (error "Must specify channel-id")
               :initarg :channel-id)
   (socket :initform nil :initarg socket)
   (packet-callback :initform #'identity
                    :initarg :packet-callback)))

(defmethod send-payload :around ((channel base-channel) packet payload)
  (declare (ignore payload))
  (with-slots (channel-id) channel
    (add-to-packet packet :channel channel-id)
    (call-next-method)))

(defmethod send-payload progn ((channel base-channel) packet payload)
  (declare (ignore channel))
  (add-to-packet packet :payload payload))