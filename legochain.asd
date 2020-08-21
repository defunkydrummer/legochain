(asdf:defsystem :legochain
  :author "Flavio Egoavil <f_egoavil@hotmail.com>"
  :maintainer "Flavio Egoavil <f_egoavil@hotmail.com>"
  :license "MIT"
  :version "0.1"
  :depends-on (#:cl-ppcre               ;REGEX
               #:cl-conspack            ;serializacion
               #:ironclad               ;criptography 
               #:bt-semaphore           ;semamphores
               #:usocket                ;sockets
               #:usocket-server 
               #:portable-threads)      ;threads 
  :serial T
  :components ((:file "package")
               (:file "legochain")
               (:file "peertopeer"))
  :description  "Toy blockchain by defunkydrummer (flavio).")

