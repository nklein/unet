;;;; Copyright (c) 2012 nklein software
;;;; MIT License. See included LICENSE.txt file for licensing details.

(asdf:defsystem :unet-test
  :description
    "unet-test: test suite for unet"
  :version "0.1.2012.11.29"
  :author "Patrick Stein <pat@nklein.com>"
  :encoding :utf-8
  :licence "MIT"
  :depends-on ("unet" "nst" "cl-ppcre")
  :components
    ((:module "src"
        :components (;;; logging function test
                     (:file "logging/test")
                     (:static-file "LICENSE.txt")))))
