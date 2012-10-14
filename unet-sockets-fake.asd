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
                       (:file "make-hash-key")
                       (:file "get-socket-from-interface"
                              :depends-on ("interface"
                                           "make-hash-key"))
                       (:file "add-socket-to-interface"
                              :depends-on ("interface"
                                           "make-hash-key"
                                           "get-socket-from-interface"))
                       (:file "get-address" :depends-on ("interface"))
                       (:file "create-datagram-socket"
                              :depends-on ("interface"
                                           "socket"
                                           "add-socket-to-interface"))
                       (:file "send-datagram"
                              :depends-on ("interface"
                                           "socket"
                                           "get-socket-from-interface"))
                       (:file "poll-datagram"
                              :depends-on ("interface"
                                           "socket"
                                           "get-socket-from-interface"))
                       (:file "api" :depends-on ("interface"))))))))
