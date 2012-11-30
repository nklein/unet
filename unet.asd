;; Copyright (c) 2012 nklein software
;; MIT License. See included LICENSE.txt file for licensing details.

(asdf:defsystem :unet
  :description
    "unet: a variable quality-of-service datagram network layer"
  :version "0.1.2012.11.29"
  :author "Patrick Stein <pat@nklein.com>"
  :licence "MIT"
  :depends-on ("userial" "cl-log")
  :components
    ((:module "src"
        :components
          ((:module "utils"
              :components ((:file "let-gensyms")))
           (:module "logging"
              :depends-on ("utils")
              :components ((:file "cl-log")))))
     (:static-file "README.mkdn")
     (:static-file "LICENSE.txt")))
