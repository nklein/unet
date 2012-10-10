(org.tfeb.conduit-packages:defpackage :unet-sockets-fake
  (:use :cl)
  (:extends/including :unet-sockets-fake-interface
                      #:fake-sockets-interface))
