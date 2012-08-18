;; Copyright (c) 2012 nklein software
;; MIT License. See included LICENSE.txt file for licensing details.

(in-package :unet)

(defclass fragmented-channel (base-channel)
  ((pending-fragments :initform nil)))
