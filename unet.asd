;; Copyright (c) 2012 nklein software
;; MIT License. See included LICENSE.txt file for licensing details.

(asdf:defsystem :unet
  :description
    "unet: a variable quality-of-service datagram network layer"
  :version "0.1.2012.10.02"
  :author "Patrick Stein <pat@nklein.com>"
  :licence "MIT"
  :depends-on ("userial" "conduit-packages")
  :components ((:module "unet"
                :serial t
		:components ((:file "defchannel")
                             (:file "base-channel")
                             (:file "api")))
	       (:static-file "LICENSE.txt")))
