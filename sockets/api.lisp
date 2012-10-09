(org.tfeb.conduit-packages:defpackage :unet-sockets
  (:use :cl)
  (:extends/including :unet-sockets-interface
                      #:sockets-interface
                      #:sockets-hostname-not-found-error
                      #:sockets-hostname-not-found-error-hostname
                      #:sockets-get-address
                      #:sockets-create-datagram-socket)
  
  (:extends/including :unet-sockets-base-socket
                      #:sockets-base-socket
                      #:sockets-send-datagram
                      #:sockets-poll-datagram))
