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
        :components (;;; utility functions
                     (:file "utils/let-gensyms")
                     
                     ;;; logging functions
                     (:file "logging/cl-log"
                        :depends-on ("utils/let-gensyms"))

                     ;;; other system files
                     (:static-file "README.mkdn")
                     (:static-file "LICENSE.txt")))))
