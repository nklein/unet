
(require :unet-example)
(in-package :unet-example-user)

(start-logging :logfile  #P"all.log"
               :name     "all-logger")
(start-logging :category :unet-sending-info
               :logfile  #P"send.log"
               :name     "send-logger")

(defserver server 26354)
(defserver client1 11870)
(defserver client2 11871)

(defchannel srv->all server raw-channel)
(add-recipients srv->all client1 client2)

(defchannel c1->srv client1 raw-channel)
(add-recipients c1->srv server)

(defchannel c2->srv client2 raw-channel)
(add-recipients c2->srv server)

(send-packet srv->all nil (packet :string "Foo to all"))
(send-packet srv->all (list (server-addr client1)) (packet :string "Foo to 1"))

(stop-logging "send-logger")

(format t "CLIENT1: ~S~%" (unserialize :string :buffer (next-packet c1->srv)))
(format t "CLIENT1: ~S~%" (unserialize :string :buffer (next-packet c1->srv)))

(format t "SERVER:  Nothing yet: ~S~%"
          (multiple-value-list (next-packet srv->all)))
(send-packet c1->srv nil (packet :string "Ack two packets"))
(format t "SERVER:  ~S~%" (unserialize :string :buffer (next-packet srv->all)))

(format t "CLIENT2: ~S~%" (unserialize :string :buffer (next-packet c2->srv)))
(format t "CLIENT2: ~S~%" (multiple-value-list (next-packet c2->srv)))

(close-server server)
(close-all-servers)

(stop-logging "all-logger")
