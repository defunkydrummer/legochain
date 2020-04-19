(defpackage :legochain
  (:use :cl)
  (:export
   :blockchain
   :compute-hash
   :decode-payload 
   :add-data-to-blockchain
   :push-block-to-blockchain
   :last-block-on-blockchain
   :verify-blockchain
   ))

(in-package :legochain)

(declaim (optimize (debug 3) (speed 0)))

;; Our blockchain.
(defclass blockchain ()
  ((blocks :initarg :blocks
           :initform nil
           :reader blockchain-blocks
           :documentation "The blockchain itself, a list of blocks."))
  (:documentation "A blockchain."))

;; The data that the first (initial or "genesis") block will carry.
(defparameter *initial-block-data*
  ;; homage to bitcoin's 1st block
  (conspack:encode "The Times 03/Jan/2009 Chancellor on brink of second bailout for banks"))

;; data type of our payloads...
(deftype payload-type () '(simple-array (unsigned-byte 8)))

(defclass bblock ()
  ;; The blockchain block.
  ((id :initarg :id
       :reader block-id
       :documentation "Id of the current block."
       :type integer)
   (previous-hash :initarg :previous-hash
                  :reader block-previous-hash
                  :type (or string null))
   ;; (this-hash :initarg this-hash
   ;;            :documentation "Hash value of the current block.")
   (timestamp :initarg :timestamp
              :reader block-timestamp)
   (nonce-value :initarg :nonce-value
                :documentation "Nonce value for this block.")
   (payload :initarg :payload
            :reader block-payload
            :documentation "The block's usable (payload) data."
            :type payload-type))
  (:documentation "A block in the blockchain"))

;; tell conspack how to encode such a block into a vector of bytes
(conspack:defencoding bblock
  id previous-hash timestamp nonce-value payload)

;; how to encode the blockchain
(conspack:defencoding blockchain
  blocks)

;; generic functions for encoding and decoding stuff
(defgeneric encode (x))
(defgeneric decode (x))

;; how to encode and decode the block 
(defmethod encode ((b bblock))
  (conspack:encode b))

;; encode a blockchain
(defmethod encode ((b blockchain))
  (conspack:encode b))

;; encodes payload data so it can be stored on a block.
(defmethod encode ((payload-data sequence))
  (conspack:encode payload-data))

;; this decodes any bytestream into a block or blockchain depending on what
;; we give to conspack
(defmethod decode ((bytestream sequence))
  (conspack:decode bytestream))

;; decode a block: decodes the payload for a block
(defmethod decode ((b bblock))
  (with-slots (payload) b
    (conspack:decode payload)))

;; push a block to the blockchain
(defmethod push-block ((the-block bblock) (chain blockchain))
  (with-slots (blocks) chain
    ;; just push the block!
    (push the-block blocks)))

;; the last (recent) block on the chain
(defmethod last-block ((chain blockchain))
  (with-slots (blocks) chain
    (first blocks))) ;latest

;; get a block from the blockchain, by index
(defmethod get-block ((chain blockchain) (index integer))
  "Get block by index on the blockchain, starting with 0 = initial block.
NOTE: Assumes the blockchain has been verified."
   (with-slots (blocks) chain
    (elt blocks
         ;; note tha the blocks are in reverse order, because they are pushed.
         (- (length blocks) index 1))))

;; compute hash for the block
(defmethod compute-hash ((b bblock))
  (ironclad:byte-array-to-hex-string 
   (ironclad:digest-sequence :sha256
                             (encode b))))



;; -------------------------- BLOCK MINING -----------------------

(defparameter *mining-difficulty* 4
  "Number of zeroes the hash must start with to satisfy the difficulty condition when mining.")

(defvar *regex-difficulty* nil
  "Compiled REGEX for checking difficulty.")

(defun regex-for-difficulty (difficulty-level)
  "Obtain REGEX for difficulty according to the level."
  (declare (type integer difficulty-level))
  (format nil "^0{~D}" difficulty-level))

(defun hash-complies-with-difficulty (h)
  "True if the hash complies with difficulty spec."
  (declare (type string h))
  ;; if it complies with the regex (regex returns non-nil)
  (if *regex-difficulty*
      (cl-ppcre:scan-to-strings *regex-difficulty* h)
      (error "REGEX for difficulty not compiled yet.")))

(defun mine-new-block (&key previous-hash payload id)
  "Mine (and return) a new block for the data.
That is, a block that satisfies the difficulty/challenge."
  ;; a terrible loop that will keep the computer busy...
  (let ((b nil)                         ;the final block
        (nonce-value 0))
    (format t "Mining block [~D]. Payload is ~D bytes...!~%"
            id
            (length payload))
    (loop
      :do
      (setf b
            (make-instance 'bblock
                           :payload payload
                           :id id
                           :previous-hash previous-hash
                           :timestamp (get-universal-time)
                           :nonce-value nonce-value
                           ))
      (incf nonce-value)
      ;; compute hash until it complies with target challenge.
      :until (hash-complies-with-difficulty (compute-hash b)))
    ;;and we return the block thus mined
    b))

;; ---------------------------------------------------------------------

(defmethod add-data ((chain blockchain) (payload sequence))
  "Add payload to the blockchain. 
this automatically mines a new block with the payload."
  ;; enforce specific payload type
  (declare (type payload-type payload))
  ;; get the id of the last object, the hash, etc
  (let* ((recent-block (last-block chain))
         (last-id (block-id recent-block))
         (last-hash (compute-hash recent-block)))
    ;; mine new block !
    (let ((new
            (mine-new-block
             :previous-hash last-hash
             :payload payload
             :id (1+ last-id)))) ;with an ID that is superior to the last one.
      ;; add this new block to the blockchain
      (push-block new chain))))


(defmethod add-data ((chain blockchain) (s string))
  "Add string to the blockchain. It will be encoded."
  (add-data chain (encode s)))

(defun start-my-blockchain (&optional (blank T))
  "Some startup tasks.
And creates a blank blockchain (unless blank = null)"
  ;; compile REGEX
  (unless *regex-difficulty*
    (setf *regex-difficulty*
          (cl-ppcre:create-scanner
           (regex-for-difficulty *mining-difficulty*))))

  ;; new blockchain
  (let ((b (make-instance 'blockchain)))
    ;; my blockchain needs a new block... the first block
    (unless blank
      (push-block (mine-new-block  
                                 :previous-hash nil
                                 :payload *initial-block-data*
                                 :id 0)
                                b))
    ;; return it
    b))



;; ----------------------- blockchain verification ----------------
(defmethod complies-with-rules ((head bblock) (previous bblock))
  "T if the block complies with the rules of the blockchain. 
Requires the previous block."
  (and 
   ;; hash of previous block equals previous-hash of current block
   (string= (block-previous-hash head)
            (compute-hash previous))
   ;; timestamps are consecutive
   ;; note: >= because (get-universal-time) only is precise to seconds.
   (>= (block-timestamp head)
       (block-timestamp previous))
   ;; ids are consecutive
   (> (block-id head)
      (block-id previous))
   ;; hash complies with challenge
   (hash-complies-with-difficulty (compute-hash head))
   ;; T here only so the AND block returns either T or NIL
   T
   ))

(defmethod complies-with-rules ((head bblock) (previous null))
  ;; If there's no previous block, the rules are OK.
  T)

(defmethod verify ((chain blockchain))
  "Check that all blocks comply the rules of the blockchain..

Returns: T if all is OK.

Also returns (as a secondary value): 
  list where all should be T. If an element at the array
  is NIL, then the block at that position failed the check."
  (if (blockchain-blocks chain)
      (let ((individual-verifications
              (maplist        ; maplist: applies successive CDR to the blocks list
               ;; apply this function...
               (lambda (blocklist)
                 ;; each block is verified against the previous.
                 (complies-with-rules (first blocklist)
                                      (second blocklist)))
               ;; ...over the blocks of the chain
               (blockchain-blocks chain))))
        ;; return values: primary and secondary
        (values
         ;; 1. true if all individual verifications are true (T)
         (not (some #'null individual-verifications))
         ;; 2. the individual results
         individual-verifications))
      ;; else: there are no blocks, return T (ok) and an empty list
      (values T ())))

(defmethod verify-against ((my-chain blockchain) (other-chain blockchain))
  "Verify my blockchain against a potential longer blockchain."
  (when (verify other-chain)
    (let ((last-mine (last-block my-chain))
          (last-other (last-block other-chain)))
      (when (< (block-id last-mine)
               (block-id last-other))
        ;; the other chain has a longer block.
        ;; check that the hash of my last block
        ;; is equal to the "previous-hash" of the
        ;; next block in the other chain
        
        (let ((their-next-block
                (get-block  other-chain
                            (1+ (block-id last-mine)))))
          (eql (block-previous-hash their-next-block)
               (compute-hash last-mine)))))))

;;-------------------------------------------------------------------


;; misc test / helper

;;show all blockchain
;; #+sbcl
;; (defun show-blockchain (chain)
;;   (mapcar (lambda (b)
;;             (mapcar
;;              (lambda (slot-name)
;;                (list slot-name
;;                      (slot-value b slot-name)))
;;              (mapcar #'sb-mop:slot-definition-name
;;                      (sb-mop:class-slots (class-of b)))))
;;           (blockchain-blocks chain)))

(defun add-stuff (chain)
  "Add test blocks to blockchain."
  (loop for str in '("Interesting stuff"
                     "Another block"
                     "Yet another block"
                     "WOW, blockchains are cool")
        do
        (add-data
         chain
         (encode str))))

(defun list-payloads (chain)
  "Show all payloads (decoded)"
  (loop for bl in (blockchain-blocks chain)
        collecting (decode bl)))



(defun test-blockchain ()
  "Simple blockchain test."
  (let ((test-data '("I. One"
                     "II. Two"
                     "III. Three"
                     "IV. Four"
                     ))
        (chain (start-my-blockchain nil))) ;non-blank
    
    ;; add strings to blockchain.
    (loop for str in test-data
          do
          (add-data chain
                    (encode str)))
    ;; verify blockchain
    (assert (verify chain))
    ;;compare payloads with original str
    ;;note: payloads are in reverse order than original test-data.
    ;;note: ignore initial block.
    (let
        ((a
           (butlast (loop for bl in (blockchain-blocks chain)
                          collecting (decode bl))))
         (b
           (reverse test-data)))
      ;; return the blockchain, the test as boolean values,
      ;; and the payload
      (values
       chain
       :test-result (equalp a b)
              :blockchain-data a
              :original-data b))))







