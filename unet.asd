;;;; Copyright (c) 2012 nklein software
;;;; MIT License. See included LICENSE.txt file for licensing details.

(asdf:defsystem :unet
  :description
    "unet: a variable quality-of-service datagram network layer"
  :version "0.1.2012.11.29"
  :author "Patrick Stein <pat@nklein.com>"
  :encoding :utf-8
  :licence "MIT"
  :depends-on ("userial" "cl-log")
  :components
    ((:module "src"
        :components (;;; logging functions
                     (:file "logging/base")
                     (:file "logging/cl-log"
                        :depends-on ("logging/base"))
                     (:file "logging/api"
                        :depends-on ("logging/base"
                                     "logging/cl-log"))

                     ;;; other system files
                     (:static-file "README.mkdn")
                     (:static-file "LICENSE.txt")))))
