;;;; This file provides channel generation

(defpackage :unet-channel-channel
  (:use :cl)
  (:import-from :userial
                  :buffer
                  :serialize
                  :unserialize
                  :with-buffer)
  (:import-from :unet-channel-component
                  :ensure-component
                  :extend-symbol
                  :channel-component
                    :fragmenter-args-of
                    :fragmenter-body-of
                    :encoder-args-of
                    :encoder-body-of
                    :decoder-args-of
                    :decoder-body-of
                    :return-packet
                    :stop-encoding
                    :stop-decoding
                    :encode-next
                    :decode-next)
  (:import-from :unet-channel-base
                  :channel
                  :channel-peer
                  :channel-packet
                    :buffer-of
                    :peer-of))

(in-package :unet-channel-channel)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dealing with defining a channel's classes based on
;;; the components of the channel.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %create-class (name extension component-names)
    `(defclass ,(extend-symbol name extension)
         (,@(mapcar #'(lambda (component-name)
                        (extend-symbol component-name extension))
                    component-names)
          ,@(unless (eql name 'channel)
              `(,(extend-symbol 'channel extension))))
       ())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dealing with defining a channel's methods based on
;;; the components of the channel.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; function to make local encoder for a given component
;;; with a given next component.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %make-encoder-local-func (channel name next)
    (let* ((component (ensure-component name))
           (args (encoder-args-of component))
           (body (encoder-body-of component)))
      `(,name (,@args)
         ,(if next
              `(declare (ignorable ,(first args)))
              `(declare (ignorable ,@args)))
         (flet ((stop-encoding ()
                  (return-from ,name))
                ,@(when next
                    `((encode-next (peer packet)
                        (with-buffer (buffer-of packet)
                          (,next ,channel peer packet))))))
           ,@body
           ,@(when next
               `((encode-next ,@(subseq args 1 3)))))))))

;;; function to make a channel encoder given the components.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %create-encoder (name component-names)
    (let ((channel (gensym "CHANNEL-"))
          (peer (gensym "PEER-"))
          (packet (gensym "PACKET-"))
          (return-list (gensym "RETURN-LIST-"))
          (channel-type name)
          (peer-type (extend-symbol name "-PEER"))
          (packet-type (extend-symbol name "-PACKET"))
          (component-names (append component-names (list 'channel))))
      `(defmethod channel-encoder ((,channel ,channel-type)
                                   (,peer ,peer-type)
                                   (,packet ,packet-type))
         (let ((,return-list nil))
           (flet ((return-packet (packet)
                    (check-type packet ,packet-type)
                    (push packet ,return-list)))
             (labels ,(loop :for prev = nil :then name
                         :for name :in (reverse component-names)
                         :collecting (%make-encoder-local-func channel
                                                               name
                                                               prev))
               (with-buffer (buffer-of ,packet)
                 (,(first component-names) ,channel ,peer ,packet))
               (nreverse ,return-list))))))))

;;; function to make local decoder for a given component
;;; with a given next component.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %make-decoder-local-func (channel name next)
    (let* ((component (ensure-component name))
           (args (decoder-args-of component))
           (body (decoder-body-of component)))
      `(,name (,@args)
         ,(if next
              `(declare (ignorable ,(first args)))
              `(declare (ignorable ,@args)))
         (flet ((stop-decoding ()
                  (return-from ,name))
                ,@(when next
                    `((decode-next (peer packet)
                        (with-buffer (buffer-of packet)
                          (,next ,channel peer packet))))))
           ,@body
           ,@(when next
               `((decode-next ,@(subseq args 1 3)))))))))

;;; function to make a channel decoder given the components.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %create-decoder (name component-names)
    (let ((channel (gensym "CHANNEL-"))
          (peer (gensym "PEER-"))
          (packet (gensym "PACKET-"))
          (return-list (gensym "RETURN-LIST-"))
          (channel-type name)
          (peer-type (extend-symbol name "-PEER"))
          (packet-type (extend-symbol name "-PACKET"))
          (component-names (append component-names (list 'channel))))
      `(defmethod channel-decoder ((,channel ,channel-type)
                                   (,peer ,peer-type)
                                   (,packet ,packet-type))
         (let ((,return-list nil))
           (flet ((return-packet (packet)
                    (check-type packet ,packet-type)
                    (push packet ,return-list)))
             (labels ,(loop :for prev = nil :then name
                         :for name :in (reverse component-names)
                         :collecting (%make-decoder-local-func channel
                                                               name
                                                               prev))
               (with-buffer (buffer-of ,packet)
                 (,(first component-names) ,channel ,peer ,packet))
               (nreverse ,return-list))))))))

(defmacro defchannel (name (&rest component-names))
  ;; XXX - add fragmenter
  `(progn
     ,(%create-class name "" component-names)
     ,(%create-class name "-PEER" component-names)
     ,(%create-class name "-PACKET" component-names)
     ,(%create-encoder name component-names)
     ,(%create-decoder name component-names)))