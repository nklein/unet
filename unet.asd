;;;; Copyright (c) 2012 nklein software
;;;; MIT License. See included LICENSE.txt file for licensing details.

(asdf:defsystem :unet
  :description
    "unet: a variable quality-of-service datagram network layer"
  :version "0.1.2012.11.29"
  :author "Patrick Stein <pat@nklein.com>"
  :encoding :utf-8
  :licence "MIT"
  :depends-on ("userial" "anaphora" "cl-log" "jpl-queues")
  :components
    ((:module "src"
        :components (;;; logging functions
                     (:file "logging/base")
                     (:file "logging/cl-log"
                        :depends-on ("logging/base"))
                     (:file "logging/api"
                        :depends-on ("logging/base"
                                     "logging/cl-log"))

                     ;;; network functions
                     (:file "network/api")
                     (:file "network/mock"
                        :depends-on ("network/api"))
                     (:file "network/locking"
                        :depends-on ("network/api"))

                     ;;; channel definitions
                     (:file "channel/component")
                     (:file "channel/base"
                        :depends-on ("channel/component"))
                     (:file "channel/channel"
                        :depends-on ("channel/component"
                                     "channel/base"))
                     (:file "channel/api"
                        :depends-on ("channel/component"
                                     "channel/base"
                                     "channel/channel"))

                     ;;; other system files
                     (:static-file "README.mkdn")
                     (:static-file "LICENSE.txt")))))

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
                     (:file "test/base")
                     (:file "logging/test"
                        :depends-on ("test/base"))
                     (:file "network/test"
                        :depends-on ("test/base"))
                     (:file "test/run")
                     (:static-file "LICENSE.txt")))))

(defmethod asdf:perform ((op asdf:test-op)
                         (system (eql (asdf:find-system :unet))))
  (asdf:load-system :unet-test)
  (funcall (find-symbol (symbol-name :run-tests) :unet-test)))
