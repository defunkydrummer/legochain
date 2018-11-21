(defpackage :legochain
  (:use :cl))

(in-package :legochain)

(defclass blockchain ()
  ((blocks :initarg :blocks
           :initform nil
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
       :documentation "Id of the current block.")
   (previous-hash :initarg :previous-hash
                  :type string)
   ;; (this-hash :initarg this-hash
   ;;            :documentation "Hash value of the current block.")
   (timestamp :initarg :timestamp)
   (nonce-value :initarg :nonce-value
                :documentation "Nonce value for this block.")
   (payload :initarg :payload
            :initform *dummy-value*
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



(defmethod add-data-to-blockhain ((chain blockchain)
                                  payload)
  "Add data to the blockchain. 
this automatically mines a new block with the payload."
  (declare (type payload-type payload))
  ;; get the id of the last object, the hash, etc
  (let* ((recent-block (last-block-on-blockchain chain))
         (last-id (slot-value recent-block 'id))
         (last-hash (compute-hash recent-block)))
    ;; mine new block !
    (let ((new
            (mine-new-block
             :previous-hash last-hash
             :payload payload
             :id (1+ last-id)))) ;with an ID that is superior to the last one.
      ;; add this new block to the blockchain
      (push-block-to-blockchain new chain))))


(defparameter *my-blockchain*
  (make-instance 'blockchain))

(defun startup ()
  "Some startup tasks."
  ;; compile REGEX
  (unless *regex-difficulty*
    (setf *regex-difficulty*
          (cl-ppcre:create-scanner
           (regex-for-difficulty *mining-difficulty*))))
  ;; my blockchain needs a new block... the first block
  (push-block-to-blockchain *my-blockchain*
                            (make-instance 'block
                                           :nonce-value 0
                                           :timestamp (get-universal-time)
                                           :previous-hash nil
                                           :id 0)))

(defmethod verify-blockchain ((chain blockchain))
  "Check that all blocks comply the rules of the blockchain..."
  (loop
    :with previous = nil
    :with good = T
    :for b in (slot-value chain 'blocks) ; for every block (starting with latest)
    :do
    ;; verification
    (setf good
          (and good
               (cond
                 ((typep previous 'boolean) T)
                 ((typep previous 'block)
                  (string= (slot-value b 'previous-hash)
                           (compute-hash previous)))
                 )))
    (setf previous b)
    :return
    good))


  









