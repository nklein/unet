;; Copyright (c) 2012 nklein software
;; MIT License. See included LICENSE.txt file for licensing details.

(asdf:defsystem :unet-sockets
  :description
    "unet-sockets: socket interfacelayer used by unet"
  :version "0.1.2012.10.02"
  :author "Patrick Stein <pat@nklein.com>"
  :licence "MIT"
  :depends-on ("conduit-packages")
  :components ((:module "sockets"
                :components ((:file "interface")
                             (:file "base-socket")
                             (:file "api" :depends-on ("interface"
                                                       "base-socket"))))))
