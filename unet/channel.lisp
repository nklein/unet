;; Copyright (c) 2011 nklein software
;; MIT License. See included LICENSE.txt file for licensing details.

(in-package :unet)

;; ----------------------------------------------------------------------
;; recipient-class-list generic -- [PRIVATE]
;; ----------------------------------------------------------------------
(defgeneric recipient-class-list (class))

;; ----------------------------------------------------------------------
;; expand-recipient-class-list -- [PRIVATE]
;; ----------------------------------------------------------------------
(defun expand-recipient-class-list (classes)
  (when classes
    (append (recipient-class-list (first classes))
	    (expand-recipient-class-list (rest classes)))))

;; ----------------------------------------------------------------------
;; channel-base class -- [PRIVATE]
;; ----------------------------------------------------------------------
(defclass channel-base ()
  ((%server :initarg :server :accessor channel-server)
   (%incoming-queue :initform nil :accessor channel-incoming-queue)
   (%recipients :initform (make-hash-table :test #'equalp)
		:accessor channel-recipients))
  (:default-initargs :server (error "XXX must specify a server")))

(defmethod print-object ((channel channel-base) stream)
  (print-unreadable-object (channel stream :type t)
    (format stream ":INCOMING-QUEUE ~S :RECIPIENTS ~S"
		   (length (channel-incoming-queue channel))
		   (hash-table-count (channel-recipients channel)))))

;; ----------------------------------------------------------------------
;; initialize-instance for channel-base
;; ----------------------------------------------------------------------
(defmethod initialize-instance :after ((obj channel-base)
				       &key &allow-other-keys)
  (server-add-channel (channel-server obj) obj))

;; ----------------------------------------------------------------------
;; channel-add-recipient -- [PUBLIC]
;; ----------------------------------------------------------------------
(declaim (ftype (function (channel-base recipient) boolean)
		channel-add-recipient))
(defun channel-add-recipient (channel recipient)
  (unless (gethash recipient (channel-recipients channel))
    (let* ((channel-class (type-of channel))
	   (recipient-class (first (recipient-class-list channel-class)))
	   (channel-recipient (make-instance recipient-class
					     :recipient recipient)))
      (setf (gethash recipient (channel-recipients channel))
	    channel-recipient))
    t))

;; ----------------------------------------------------------------------
;; channel-remove-recipient -- [PUBLIC]
;; ----------------------------------------------------------------------
(declaim (ftype (function (channel-base recipient) t)
		channel-remove-recipient))
(defun channel-remove-recipient (channel recipient) 
  (remhash recipient (channel-recipients channel)))

;; ----------------------------------------------------------------------
;; channel-recipient-base class -- [PRIVATE]
;; ----------------------------------------------------------------------
(defclass channel-recipient-base ()
  ((%pending-packets :initform nil :accessor pending-packets)
   (%recipient :initarg :recipient :accessor channel-recipient-recipient)))

;; ----------------------------------------------------------------------
;; define-channel macro -- [PUBLIC]
;; ----------------------------------------------------------------------
(defmacro define-channel ((channel-class
			     (&rest channel-super-classes)
			     (&rest channel-slots)
			     &rest channel-options)
			  channel-recipient-class
			    (&rest channel-recipient-slots)
			    &rest channel-recipient-options)
  (let ((recipient-super-classes (apply #'append
					(mapcar #'recipient-class-list
						channel-super-classes))))
  `(progn
     (defclass ,channel-class (,@channel-super-classes unet::channel-base)
       ,channel-slots
       ,@channel-options)
     (defmethod recipient-class-list ((class (eql ',channel-class)))
       '(,channel-recipient-class ,@recipient-super-classes))
     (defclass ,channel-recipient-class (,@recipient-super-classes
					 unet::channel-recipient-base)
       ,channel-recipient-slots
       ,@channel-recipient-options))))

;; ----------------------------------------------------------------------
;; prepare-packets generic -- [PRIVATE]
;; ----------------------------------------------------------------------
(defgeneric prepare-packets (channel channel-recipient payload
				     &key &allow-other-keys)
  (:documentation "Various channel mixins will implement :around methods for this method to add information to this packet or to do any per-packet processing that they need to do.  Any packets that need to be sent to the CHANNEL-RECIPIENT should be stored in the (PENDING-PACKETS CHANNEL-RECIPIENT) list.  Each packet is a USERIAL::BUFFER.  The PAYLOAD is also a USERIAL::BUFFER.

Note: There may be more than one packet to send because a mixin decided it also needs to resend other packets, ack previously received packets, chunk this payload up into multiple packets, or what-have-you, those packets should be stored in a list in the CHANNEL-RECIPIENT."))

;; ----------------------------------------------------------------------
;; message-for-channel-p generic -- [PUBLIC]
;; ----------------------------------------------------------------------
(defgeneric message-for-channel-p (message channel)
  (:documentation "There can be multiple channels on a given server.  When the server receives a packet, it must decide which channel should get the packet.  This server invokes this method on each channel in turn until it finds one that will accept this message.  This method should return non-NIL if the given CHANNEL should get this MESSAGE."))

;; ----------------------------------------------------------------------
;; handle-packet generic -- [PUBLIC]
;; ----------------------------------------------------------------------
(defgeneric handle-packet (channel channel-recipient packet
			   &key &allow-other-keys)
  (:documentation "Various channel mixins will implement :around methods for this method to extract information that their PREPARE-PACKET method added to the PACKET and do any per-packet processing needed.  Any packets that need to be sent back the the CHANNEL-RECIPIENT should be stored in then (PENDING-PACKETS CHANNEL-RECIPIENT) list.  The return value of this method should be a single payload as a USERIAL::BUFFER."))

;; ----------------------------------------------------------------------
;; get-channel-recipient -- [PRIVATE]
;; ----------------------------------------------------------------------
(declaim (ftype (function (channel-base recipient) channel-recipient-base)
		get-channel-recipient))
(defun get-channel-recipient (channel recipient)
  (let ((rr (gethash recipient (channel-recipients channel))))
    (restart-case
	(unless rr
	  (error 'recipient-not-on-channel-error :channel channel
		                                 :recipient recipient))
      (add-to-channel ()
	:report "Add recipient to channel."
	(channel-add-recipient channel recipient)
	(setf rr (get-channel-recipient channel recipient))))
    rr))

;; ----------------------------------------------------------------------
;; send-pending-packets -- [PUBLIC]
;; ----------------------------------------------------------------------
(declaim (ftype (function (iolib:socket channel-recipient-base) (values))
		send-pending-packets))
(defun send-pending-packets (socket channel-recipient)
  (with-accessors ((recipient channel-recipient-recipient)
		   (pending-packets pending-packets)) channel-recipient
    (let ((host (recipient-host recipient))
	  (port (recipient-port recipient)))
      (mapc #'(lambda (pp)
		(iolib:send-to socket pp :remote-host host :remote-port port))
	    (nreverse pending-packets))
      (setf pending-packets nil)))
  (values))

;; ----------------------------------------------------------------------
;; send-packet -- [PUBLIC]
;; ----------------------------------------------------------------------
(declaim (ftype (function (channel-base list userial:buffer
			   &rest list &key &allow-other-keys) (values))
		send-packet))
(defun send-packet (channel recipients payload
		    &rest keys &key &allow-other-keys)
  "Send PAYLOAD to all RECIPIENTS via the given CHANNEL.  If RECIPIENTS is NIL, send to all of the recipients on the channel."
  (let ((socket (server-socket (channel-server channel))))
    (labels ((send-to-recipient (recipient)
	       (let ((rr (get-channel-recipient channel recipient)))
		 (apply #'prepare-packets channel rr payload keys)
		 (send-pending-packets socket rr))))
      (mapc #'send-to-recipient (or recipients (channel-recipients channel)))))
  (values))

;; ----------------------------------------------------------------------
;; receive-packet -- [PRIVATE]
;; ----------------------------------------------------------------------
(declaim (ftype (function (channel-base recipient userial:buffer
			   &rest list &key &allow-other-keys) (values))
		receive-packet))
(defun receive-packet (channel recipient packet
		       &rest keys &key &allow-other-keys)
  (let ((rr (get-channel-recipient channel recipient))
	(packet (userial:buffer-rewind :buffer packet))
	(socket (server-socket (channel-server channel))))
    (let ((payload (apply #'handle-packet channel rr packet keys)))
      (when payload
	(with-accessors ((incoming-queue channel-incoming-queue)) channel
	  (setf incoming-queue (nconc incoming-queue
				      (list (list payload recipient)))))))
    (send-pending-packets socket rr)))

;; ----------------------------------------------------------------------
;; packet-from-buffer -- [PRIVATE]
;; ----------------------------------------------------------------------
(defun packet-from-buffer (buffer length)
  (let ((buf (userial:make-buffer length)))
    (setf (userial:buffer-length :buffer buf) length
	  (subseq buf 0 length) buffer)
    buf))

;; ----------------------------------------------------------------------
;; server-check-for-messages -- [PRIVATE]
;; ----------------------------------------------------------------------
(declaim (ftype (function (server &optional t) boolean)
		server-check-for-messages))
(defun server-check-for-messages (server &optional block)
  (let ((socket (server-socket server))
	(buffer (server-buffer server)))
    (multiple-value-bind (buffer length host port)
	(iolib:receive-from socket :buffer buffer
			           :dont-wait (not block))
      (when host
	(let ((packet (packet-from-buffer buffer length)))
	  (labels ((for-channel-p (ch)
		     (message-for-channel-p
		         (userial:buffer-rewind :buffer packet) ch)))
	    (let ((channel (find-if #'for-channel-p (server-channels server))))
	      (receive-packet channel
			      (make-instance 'recipient :hostname host
					                :port port)
			      (userial:buffer-rewind :buffer packet)))))
	t))))

;; ----------------------------------------------------------------------
;; next-packet function -- [PUBLIC]
;; ----------------------------------------------------------------------
(declaim (ftype (function (channel-base &optional boolean)
			  (values (or userial:buffer nil)
				  (or recipient nil)))
		next-packet))
(defun next-packet (channel &optional block)
  (let ((got (pop (channel-incoming-queue channel))))
    (cond
      (got (values (first got) (second got)))
      (t   (server-check-for-messages (channel-server channel) block)
	   (let ((got (pop (channel-incoming-queue channel))))
	     (if got
		 (values (first got) (second got))
		 (values nil nil)))))))
