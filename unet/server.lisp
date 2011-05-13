;; Copyright (c) 2011 nklein software
;; MIT License. See included LICENSE.txt file for licensing details.

(in-package :unet)

(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(define-constant +max-packet-size+ (* 1024 1024))

;; ======================================================================
;; server class
;; ======================================================================
(defclass server ()
  ((socket :accessor server-socket)
   (channels :initform nil :accessor server-channels)
   (unchecked-messages :initform 0 :accessor server-unchecked-messages)
   (buffer :initform (make-array (list +max-packet-size+)
				 :element-type '(unsigned-byte 8)
				 :initial-element 0)
	   :accessor server-buffer)
   (listener-thread :initform nil :accessor server-listener-thread))
  (:default-initargs :port 26354))

(defun server-port (server)
  (usocket:get-local-port (server-socket server)))

(defmethod print-object ((obj server) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (with-accessors ((socket server-socket)
		     (channels server-channels)
		     (listener server-listener-thread)
                     (unchecked server-unchecked-messages)) obj
      (format stream ":PORT ~S :CHANNELS ~S :SOCKET ~S :UNCHECKED-MSGS ~S"
	      (server-port obj) channels socket unchecked)
      (format stream " :LISTENER ~S" listener))))

;; ----------------------------------------------------------------------
;; initialize-instance :around server
;; ----------------------------------------------------------------------
(defmethod initialize-instance :around ((obj server) &key port)
  (let ((port (validate-port-number port)))
    (with-accessors ((socket server-socket) (channels server-channels)) obj
      (setf socket (usocket:socket-connect nil nil
                                           :protocol :datagram
                                           :element-type '(unsigned-byte 8)
                                           :local-port port)))
    (call-next-method)
    (log-it :unet-initialize-server :uint16 port)))

;; ----------------------------------------------------------------------
;; server-add-channel -- [PRIVATE]
;; ----------------------------------------------------------------------
(declaim (ftype (function (server channel-base) (values)) server-add-channel))
(defun server-add-channel (server channel)
  (declare (type server server)
	   (type channel-base channel))
  (with-accessors ((channels server-channels)) server
    (setf channels (cons channel (remove channel channels))))
  (values))

;; ----------------------------------------------------------------------
;; server-get-single-message -- [PRIVATE]
;; ----------------------------------------------------------------------
(defun server-get-single-message (socket buffer block)
  (log-it :unet-server-get-single-message
          :uint16 (usocket:get-local-port socket) :boolean block)
  (flet ((socket-has-pending (socket)
           (usocket:wait-for-input (list socket) :timeout 0)
           (eq (usocket::state socket) :read)))
    (cond
      ((or block (socket-has-pending socket))
         (multiple-value-bind (buffer length host port)
             (usocket:socket-receive socket buffer
                                     (userial:buffer-capacity :buffer buffer))
           (values (packet-from-buffer buffer length)
                   (make-instance 'recipient :hostname host :port port))))
      (t (values nil nil)))))

;; ----------------------------------------------------------------------
;; server-process-single-message -- [PRIVATE]
;; ----------------------------------------------------------------------
(defun server-process-single-message (server packet recipient)
  (labels ((for-channel-p (ch)
             (message-for-channel-p (userial:buffer-rewind :buffer packet)
                                    ch)))
    (let ((channel (find-if #'for-channel-p (server-channels server))))
      (cond
        (channel (receive-packet channel recipient
                                 (userial:buffer-rewind :buffer packet))
                 (log-it :unet-server-process-single-message
                         :unet-host (recipient-host recipient)
                         :uint16 (recipient-port recipient))
                 t)
        (t       (log-it :unet-server-unprocessed-message
                         :unet-host (recipient-host recipient)
                         :uint16 (recipient-port recipient))
                 nil)))))

;; ----------------------------------------------------------------------
;; server-check-for-messages -- [PRIVATE]
;; ----------------------------------------------------------------------
(declaim (ftype (function (server &optional boolean) (integer 0 *))
		server-check-for-messages))
(defun server-check-for-messages (server &optional block)
  (log-it :unet-server-check-for-messages
          :uint16 (server-port server) :boolean block)
  (flet ((got-and-processed (blocking)
           (multiple-value-bind (packet recipient)
               (server-get-single-message (server-socket server)
                                          (server-buffer server)
                                          blocking)
             (when recipient
               (server-process-single-message server packet recipient)
               t))))
    (cond
      ((got-and-processed nil)
         (incf (server-unchecked-messages server))
         (server-check-for-messages server block))
      ((plusp (server-unchecked-messages server))
         (server-unchecked-messages server))
      ((not block)
         (server-unchecked-messages server))
      ((got-and-processed t)
         (incf (server-unchecked-messages server)))
      (t (server-unchecked-messages server)))))

;; ----------------------------------------------------------------------
;; server-channels-with-messages -- [PUBLIC]
;; ----------------------------------------------------------------------
(declaim (ftype (function (server &optional boolean) list)
                server-channels-with-messages))
(defun server-channels-with-messages (server &optional block)
  (server-check-for-messages server block)
  (let ((result (remove-if #'null (server-channels server)
                           :key #'channel-incoming-queue)))
    (log-it :unet-server-channels-with-messages
            :unet16 (usocket:get-local-port (server-socket server))
            :boolean block
            :channel-list result)
    result))

;; ----------------------------------------------------------------------
;; server-close -- [PUBLIC]
;; ----------------------------------------------------------------------
(declaim (ftype (function (server) (values)) server-close))
(defun server-close (server)
  (log-it :unet-server-close :uint16 (server-port server))
  (usocket:socket-close (server-socket server))
  (setf (server-socket server) nil)
  (values))
