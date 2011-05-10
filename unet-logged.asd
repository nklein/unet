;; Copyright (c) 2011 nklein software
;; MIT License. See included LICENSE.txt file for licensing details.

(asdf:defsystem :unet-logged
  :description
    "unet-logged: unet with cl-log"
  :version "0.1.2011.03.07"
  :author "Patrick Stein <pat@nklein.com>"
  :licence "MIT"
  :depends-on ("unet" "userial" "usocket" "cl-log")
  :components ((:module "unet-logged"
                :serial t
		:components ((:file "package")
                             (:file "categories")
                             (:file "logger")
                             (:file "reader")))
	       (:static-file "README.mkdn")
	       (:static-file "LICENSE.txt")))
