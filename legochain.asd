(asdf:defsystem :legochain
  :author "Flavio Egoavil <f_egoavil@hotmail.com>"
  :maintainer "Flavio Egoavil <f_egoavil@hotmail.com>"
  :license "MIT"
  :version "0.1"
  :depends-on
  (#:alexandria #:cl-ppcre
                #:cl-conspack
                #:ironclad)
  :serial T
  :components ((:file "legochain"))
  :description  "Toy blockchain.")




