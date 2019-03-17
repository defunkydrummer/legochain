(asdf:defsystem :legochain
  :author "Flavio Egoavil <f_egoavil@hotmail.com>"
  :maintainer "Flavio Egoavil <f_egoavil@hotmail.com>"
  :license "MIT"
  :version "0.1"
  :depends-on (#:cl-ppcre
               #:cl-conspack
               #:ironclad
               #:bt-semaphore
               #:usocket
               #:usocket-server) 
  :serial T
  :components ((:file "legochain"))
  :description  "Toy blockchain by defunkydrummer (flavio).")


