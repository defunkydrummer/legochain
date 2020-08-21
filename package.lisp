(defpackage :legochain
  (:use :cl)
  (:export
   :blockchain
   :compute-hash
   :decode
   :add-data
   :push-block-to-blockchain
   :last-block-on-blockchain
   :verify-blockchain
   ))
