;; Copyright (c) 2012 nklein software
;; MIT License. See included LICENSE.txt file for licensing details.

(asdf:defsystem :unet
  :description
    "unet: a variable quality-of-service datagram network layer"
  :version "0.1.2012.08.17"
  :author "Patrick Stein <pat@nklein.com>"
  :licence "MIT"
  :depends-on ("userial")
  :components ((:module "unet"
                :serial t
		:components ((:file "package")
                             (:file "generics")
                             (:file "base-channel")
                             (:file "sequence-number-mixin")
                             (:file "ordered-channel")
                             (:file "reliable-channel")
                             (:file "reliable-ordered-channel")
                             (:file "fragmented-channel")))
	       (:static-file "README.mkdn")
	       (:static-file "LICENSE.txt")))
