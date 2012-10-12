;; Copyright (c) 2012 nklein software
;; MIT License. See included LICENSE.txt file for licensing details.

(asdf:defsystem :unet-sockets-fake
  :description
    "unet-sockets-fake: fake sockets layer implemented with queues"
  :version "0.1.2012.10.02"
  :author "Patrick Stein <pat@nklein.com>"
  :licence "MIT"
  :depends-on ("unet-sockets" "jpl-queues" "conduit-packages")
  :components
    ((:module "sockets"
      :components
        ((:module "fake"
          :components ((:file "interface")
                       (:file "socket" :depends-on ("interface"))
                       (:file "methods" :depends-on ("interface"
                                                     "socket"))
                       (:file "api" :depends-on ("interface"))))))))
