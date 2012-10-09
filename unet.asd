;; Copyright (c) 2012 nklein software
;; MIT License. See included LICENSE.txt file for licensing details.

(asdf:defsystem :unet
  :description
    "unet: a variable quality-of-service datagram network layer"
  :version "0.1.2012.10.02"
  :author "Patrick Stein <pat@nklein.com>"
  :licence "MIT"
  :depends-on ("userial" "conduit-packages")
  :components ((:module "sockets"
                :components ((:file "interface")
                             (:file "base-socket")
                             (:file "api" :depends-on ("interface"
                                                       "base-socket"))))
               (:module "unet"
                :depends-on "sockets"
		:components ((:file "defchannel")
                             (:file "base-channel" :depends-on ("defchannel"))
                             (:file "api" :depends-on ("defchannel"
                                                       "base-channel"))))
	       (:static-file "LICENSE.txt")))
