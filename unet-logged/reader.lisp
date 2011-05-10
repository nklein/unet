;; Copyright (c) 2011 nklein software
;; MIT License. See included LICENSE.txt file for licensing details.

(in-package :unet-logged)

(defun buffer-from-stream (stream length)
  (let ((buf (userial:make-buffer length)))
    (userial:buffer-advance :amount length :buffer buf)
    (read-sequence buf stream)
    (userial:buffer-rewind :buffer buf)))

(defun read-logged-header (stream)
  (userial:unserialize-let* (:uint64 universal-timestamp
                             :uint32 timestamp-fraction
                             :unet-logger-category category
                             :uint64 length) (buffer-from-stream stream 21)
    (multiple-value-bind (secs mins hrs day mon year)
        (decode-universal-time universal-timestamp 0)
      (list (list year mon day hrs mins secs
                  (/ timestamp-fraction internal-time-units-per-second))
            category
            length))))
