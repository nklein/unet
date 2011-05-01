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

(defmethod print-object ((obj server) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (with-accessors ((socket server-socket)
		     (channels server-channels)
		     (listener server-listener-thread)
                     (unchecked server-unchecked-messages)) obj
      (format stream ":PORT ~S :CHANNELS ~S :SOCKET ~S :UNCHECKED-MSGS ~S"
	      (usocket:get-local-port socket) channels socket unchecked)
      (format stream " :LISTENER ~S" listener))))

;; ----------------------------------------------------------------------
;; initialize-instance :around server
;; ----------------------------------------------------------------------
(defmethod initialize-instance :around ((obj server) &key port)
  (with-accessors ((socket server-socket) (channels server-channels)) obj
    (setf socket (usocket:socket-connect nil nil
                     :protocol :datagram
                     :element-type '(unsigned-byte 8)
                     :local-port (validate-port-number port))))
  (call-next-method))

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
      (when channel
        (receive-packet channel recipient
                        (userial:buffer-rewind :buffer packet))
        t))))

;; ----------------------------------------------------------------------
;; server-check-for-messages -- [PRIVATE]
;; ----------------------------------------------------------------------
(declaim (ftype (function (server &optional boolean) (integer 0 *))
		server-check-for-messages))
(defun server-check-for-messages (server &optional block)
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
  (remove-if #'null (server-channels server) :key #'channel-incoming-queue))
