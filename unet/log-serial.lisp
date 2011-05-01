;; Copyright (c) 2011 nklein software
;; MIT License. See included LICENSE.txt file for licensing details.

(in-package :unet)

(userial:make-enum-serializer :unet-logger-category
                              (:unet-validate-hostname
                               :unet-specify-new-hostname
                               :unet-validate-port-number
                               :unet-specify-new-port-number
                               :unet-initialize-recipient
                               
                               ))

(userial:make-simple-serializer :unet-host
                                (object (vector 0 0 0 0))
                                (:uint8 (elt object 0)
                                 :uint8 (elt object 1)
                                 :uint8 (elt object 2)
                                 :uint8 (elt object 3)))
