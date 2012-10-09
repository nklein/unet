
(org.tfeb.conduit-packages:defpackage :unet
  (:use :cl)
  (:extends/including :unet-base-channel #:channel-name
                                         #:base-channel
                                         #:channel-no-name-specified-error))
