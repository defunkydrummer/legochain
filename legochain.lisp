(defpackage :legochain
  (:use :cl))

(in-package :legochain)

(defclass blockchain ()
  ((blocks :initarg :blocks
           :initform nil
           :reader blockchain-blocks
           :documentation "The blockchain itself, a list of blocks."))
  (:documentation "A blockchain."))

(defmethod push-block-to-blockchain ((the-block block) (chain blockchain))
  (with-slots (blocks) chain
    ;; just push the block!
    (push the-block blocks)))

;; the last (recent) block on the chain
(defmethod last-block-on-blockchain ((chain blockchain))
  (with-slots (blocks) chain
    (first blocks))) ;latest

(defparameter *dummy-value*
  (conspack:encode "This block has a dummy value. Congratulations!"))

;; data type of our payloads...
(deftype payload-type () `(or null
                              (simple-array (unsigned-byte 8))))

(defclass block ()
  ((id :initarg :id
       :reader block-id
       :documentation "Id of the current block.")
   (previous-hash :initarg :previous-hash
                  :reader block-previous-hash
                  :type string)
   ;; (this-hash :initarg this-hash
   ;;            :documentation "Hash value of the current block.")
   (timestamp :initarg :timestamp
              :reader block-timestamp)
   (nonce-value :initarg :nonce-value
                :documentation "Nonce value for this block.")
   (payload :initarg :payload
            :initform *dummy-value*
            :reader block-payload
            :documentation "The block's usable (payload) data."
            :type payload-type))
  (:documentation "A block in the blockchain"))

;; tell conspack how to encode such a block into a vector of bytes
(conspack:defencoding block
  id previous-hash timestamp nonce-value payload)

;; compute hash for the block
(defmethod compute-hash ((b block))
  (ironclad:byte-array-to-hex-string 
   (ironclad:digest-sequence :sha256
                             (conspack:encode b))))

;; decode the payload for a block
(defmethod decode-payload ((b block))
  (with-slots (payload) b
    (conspack:decode payload)))


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
    (loop
      :do
      (setf b
            (make-instance 'block
                           :payload payload
                           :id id
                           :previous-hash previous-hash
                           :timestamp (get-universal-time)
                           :nonce-value nonce-value
                           ))
      (incf nonce-value)
      ;; compute hash until it complies with target challenge.
      :until (hash-complies-with-difficulty
             (compute-hash b)))
    ;;and we return the block thus mined
    b
    ))



(defmethod add-data-to-blockchain ((chain blockchain)
                                  payload)
  "Add data to the blockchain. 
this automatically mines a new block with the payload."
  (declare (type payload-type payload))
  ;; get the id of the last object, the hash, etc
  (let* ((recent-block (last-block-on-blockchain chain))
         (last-id (block-id recent-block))
         (last-hash (compute-hash recent-block)))
    ;; mine new block !
    (let ((new
            (mine-new-block
             :previous-hash last-hash
             :payload payload
             :id (1+ last-id)))) ;with an ID that is superior to the last one.
      ;; add this new block to the blockchain
      (push-block-to-blockchain new chain))))


(defvar *my-blockchain* nil)


(defun startup ()
  "Some startup tasks. This erases the in-memory blockchain!"
  ;; compile REGEX
  (unless *regex-difficulty*
    (setf *regex-difficulty*
          (cl-ppcre:create-scanner
           (regex-for-difficulty *mining-difficulty*))))

  ;; new blockchain
  (setf *my-blockchain* (make-instance 'blockchain))
  ;; my blockchain needs a new block... the first block
  (push-block-to-blockchain *my-blockchain*
                            (mine-new-block  
                             :previous-hash nil
                             :payload *dummy-value*
                             :id 0)))


(defmethod verify-blockchain ((chain blockchain))
  "Check that all blocks comply the rules of the blockchain..
Returns array where all should be T"
  (let ((blocks (blockchain-blocks chain)))
     (maplist ;; applies successive CDR to the blocks list
      (lambda (blocklist)
        ;; The rules...
        ;; verification of the list of blocks
        (let  ((head (first blocklist))
               (previous (second blocklist)))
          ;; function that says head is good with previous
          (or (null previous)         ; no previous block
              ;; if there is a previous block:
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
               ))))
      blocks)))

;;show all blockchain
(defun show-blockchain ()
  (mapcar (lambda (b)
            (mapcar
             (lambda (slot-name)
               (list slot-name
                     (slot-value b slot-name)))
             (mapcar #'sb-mop:slot-definition-name
                     (sb-mop:class-slots (class-of b)))))
          (blockchain-blocks *my-blockchain*)))



(defun add-stuff ()
  "Add test blocks to blockchain."
  (loop for str in '("Interesting stuff"
                     "Another block"
                     "Yet another block"
                     "WOW, blockchains are cool")
        do
        (add-data-to-blockchain
         *my-blockchain*
         (conspack:encode str))))

(defun list-payloads ()
  "Show all payloads (decoded)"
  (loop for bl in (blockchain-blocks *my-blockchain*)
        collecting (conspack:decode
                    (block-payload bl))))










