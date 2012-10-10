;; Copyright (c) 2012 nklein software
;; MIT License. See included LICENSE.txt file for licensing details.

(asdf:defsystem :unet
  :description
    "unet: a variable quality-of-service datagram network layer"
  :version "0.1.2012.10.02"
  :author "Patrick Stein <pat@nklein.com>"
  :licence "MIT"
  :depends-on ("userial" "unet-sockets" "conduit-packages")
  :components ((:module "unet"
		:components ((:file "defchannel")
                             (:file "base-channel" :depends-on ("defchannel"))
                             (:file "api" :depends-on ("defchannel"
                                                       "base-channel"))))
	       (:static-file "LICENSE.txt")))
