;; Copyright (c) 2011 nklein software
;; MIT License. See included LICENSE.txt file for licensing details.

(asdf:defsystem :unet
  :description
    "unet: a variable quality-of-service datagram network layer"
  :version "0.1.2011.03.07"
  :author "Patrick Stein <pat@nklein.com>"
  :licence "MIT"
  :depends-on ("userial" "iolib")
  :components ((:module "unet"
                :serial t
		:components ((:file "package")
                             (:file "errors")
			     (:file "recipient")
			     (:file "channel")
			     (:file "server")
			     (:file "mux")))
	       (:static-file "README.mkdn")
	       (:static-file "LICENSE.txt")))
