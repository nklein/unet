;; Copyright (c) 2011 nklein software
;; MIT License. See included LICENSE.txt file for licensing details.

(asdf:defsystem :unet-example
  :description
    "unet-example: functions one can use to kick the tires on the unet library"
  :version "0.1.2011.03.07"
  :author "Patrick Stein <pat@nklein.com>"
  :licence "MIT"
  :depends-on ("unet" "cl-log")
  :components ((:module "example"
                :serial t
		:components ((:file "package")
                             (:file "servers")
                             (:file "channels")
                             (:file "packets")))
	       (:static-file "README.mkdn")
	       (:static-file "LICENSE.txt")))
