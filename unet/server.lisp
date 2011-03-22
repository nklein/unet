;; Copyright (c) 2011 nklein software
;; MIT License. See included LICENSE.txt file for licensing details.

(in-package :unet)

;; ======================================================================
;; server class
;; ======================================================================
(defclass server ()
  ((socket   :accessor server-socket)
   (channels :initform nil :accessor server-channels))
  (:default-initargs :port 26354))

(defmethod print-object ((obj server) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (with-accessors ((socket server-socket) (channels server-channels)) obj
      (format stream ":PORT ~S :CHANNELS ~S :SOCKET ~S"
	      (iolib.sockets:local-port socket) channels socket))))

;; ----------------------------------------------------------------------
;; initialize-instance :around server
;; ----------------------------------------------------------------------
(defmethod initialize-instance :around ((obj server) &key port)
  (with-accessors ((socket server-socket) (channels server-channels)) obj
    (setf socket (iolib.sockets:make-socket
		     :address-family :internet
		     :type :datagram
		     :local-host iolib.sockets:+ipv4-unspecified+
		     :local-port (validate-port-number port)
		     :reuse-address t)))
  (call-next-method))

;; ----------------------------------------------------------------------
;; server-add-channel
;; ----------------------------------------------------------------------
(declaim (ftype (function (server channel) (values)) server-add-channel))
(defun server-add-channel (server channel)
  (declare (type server server)
	   (type channel channel))
  (with-accessors ((channels server-channels)) server
    (cons channel (remove channel channels))))

;; ----------------------------------------------------------------------
;; server-remove-channel
;; ----------------------------------------------------------------------
(declaim (ftype (function (server channel) (values)) server-remove-channel))
(defun server-remove-channel (server channel)
  (declare (type server server)
	   (type channel channel))
  (with-accessors ((channels server-channels)) server
    (setf channels (remove channel channels))))
