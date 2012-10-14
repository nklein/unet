;; Copyright (c) 2012 nklein software
;; MIT License. See included LICENSE.txt file for licensing details.

(asdf:defsystem :unet-sockets
  :description
    "unet-sockets: socket interface layer used by unet"
  :version "0.1.2012.10.02"
  :author "Patrick Stein <pat@nklein.com>"
  :licence "MIT"
  :depends-on ("conduit-packages")
  :components (
    (:module "sockets"
     :components ((:file "interface")
                  (:file "base-socket")
                  (:file "hostname-not-found-error")
                  (:file "get-address")
                  (:file "address+port-not-available-error")
                  (:file "create-datagram-socket")
                  (:file "send-datagram")
                  (:file "poll-datagram")
                  (:file "api" :depends-on ("interface"
                                            "base-socket"
                                            "hostname-not-found-error"
                                            "get-address"
                                            "address+port-not-available-error"
                                            "create-datagram-socket"
                                            "send-datagram"
                                            "poll-datagram"))))))
