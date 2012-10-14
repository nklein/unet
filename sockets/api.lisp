(org.tfeb.conduit-packages:defpackage :unet-sockets
  (:use :cl)
  (:extends/including :unet-sockets-interface
                      #:sockets-interface)
  
  (:extends/including :unet-sockets-base-socket
                      #:base-socket)
  
  (:extends/including :unet-sockets-hostname-not-found-error
                      #:hostname-not-found-error
                      #:hostname-not-found-error-hostname)

  (:extends/including :unet-sockets-get-address
                      #:get-address)

  (:extends/including :unet-sockets-address+port-not-available-error
                      #:address+port-not-available-error
                      #:address+port-not-available-error-address
                      #:address+port-not-available-error-port)

  (:extends/including :unet-sockets-create-datagram-socket
                      #:create-datagram-socket)
  
  (:extends/including :unet-sockets-send-datagram
                      #:send-datagram)
  
  (:extends/including :unet-sockets-poll-datagram
                      #:poll-datagram))
