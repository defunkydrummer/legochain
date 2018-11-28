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

(defclass blockchain ()
  ((blocks :initarg :blocks
           :initform nil
           :reader blockchain-blocks
           :documentation "The blockchain itself, a list of blocks."))
  (:documentation "A blockchain."))


(defparameter *dummy-value*
  (conspack:encode "This block has a dummy value. Congratulations!"))

;; data type of our payloads...
(deftype payload-type () `(or null
                              (simple-array (unsigned-byte 8))))

(defclass bblock ()
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
(conspack:defencoding bblock
  id previous-hash timestamp nonce-value payload)

;; how to encode the blockchain
(conspack:defencoding blockchain
  blocks)

(defmethod encode-block ((b bblock))
  (conspack:encode b))

(defun decode-block (bytestream)
  (conspack:decode bytestream))

(defmethod encode-blockchain ((b blockchain))
  (conspack:encode b))

(defun decode-blockchain (bytestream)
  (conspack:decode bytestream))

(defmethod push-block-to-blockchain ((the-block bblock) (chain blockchain))
  (with-slots (blocks) chain
    ;; just push the block!
    (push the-block blocks)))

;; the last (recent) block on the chain
(defmethod last-block-on-blockchain ((chain blockchain))
  (with-slots (blocks) chain
    (first blocks))) ;latest

;; compute hash for the block
(defmethod compute-hash ((b bblock))
  (ironclad:byte-array-to-hex-string 
   (ironclad:digest-sequence :sha256
                             (encode-block b))))

;; decode the payload for a block
(defmethod decode-payload ((b bblock))
  (with-slots (payload) b
    (conspack:decode payload)))

(defun encode-payload (payload-data)
  "Encode the payload so it can be stored on a block."
  (conspack:encode payload-data))


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
      :until (hash-complies-with-difficulty
             (compute-hash b)))
    ;;and we return the block thus mined
    b
    ))

;; ---------------------------------------------------------------------

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
  (push-block-to-blockchain (mine-new-block  
                             :previous-hash nil
                             :payload *dummy-value*
                             :id 0)
                            *my-blockchain*))



;; ----------------------- blockchain verification ----------------
(defun complies-with-rules-of-the-blockchain (head previous)
  "T if the block complies with the rules of the blockchain. 
Requires the previous block."
  (declare (type bblock head)
           (type (or null bblock) previous))
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
       )))


(defmethod verify-blockchain ((chain blockchain))
  "Check that all blocks comply the rules of the blockchain..
Returns array where all should be T. If an element at the array
is NIL, then the block at that position failed the check."
  (let ((blocks (blockchain-blocks chain)))
     (maplist ;; applies successive CDR to the blocks list
      (lambda (blocklist)
        ;; The rules...
        ;; verification of the list of blocks
        ;; each block is verified against the previous.
        (let  ((head (first blocklist))
               (previous (second blocklist)))
          (complies-with-rules-of-the-blockchain head previous)))
      blocks)))

(defmethod verify-blockchain-p ((chain blockchain))
  "Similar to verify-blockchain but limits the reply to either T or NIL."
  (every (lambda (x) (and x T))
         (verify-blockchain chain)))



;;-------------------------------------------------------------------


;; misc test / helper

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
         (encode-payload str))))

(defun list-payloads ()
  "Show all payloads (decoded)"
  (loop for bl in (blockchain-blocks *my-blockchain*)
        collecting (decode-payload bl)))



(defun test-blockchain ()
  "Simple blockchain test."
  (let ((test-data '("I. One"
                     "II. Two"
                     "III. Three"
                     "IV. Four"
                     )))
    (startup)
    ;; add strings to blockchain.
    (loop for str in test-data
          do
          (add-data-to-blockchain *my-blockchain*
                                  (encode-payload str)))
    ;; verify blockchain
    (assert (verify-blockchain-p *my-blockchain*))
    ;;compare payloads with original str
    ;;note: payloads are in reverse order than original test-data.
    ;;note: ignore initial block.
    (let
        ((a
           (butlast (loop for bl in (blockchain-blocks *my-blockchain*)
                          collecting (decode-payload bl))))
         (b
           (reverse test-data)))
      ;; return the test as boolean values,
      ;; and the payload
      (values (equalp a b)
              :blockchain-data a
              :original-data b))))






;;---------------------- SERVER ------------------------------


;; Type for the data interchanged between peers.
(deftype object-type () `(unsigned-byte 8))

(defun create-legochain-server (&key (host #(127 0 0 1))
                                     (port 667))
  "Create a legochain server using sockets and accept connections on port number X."
  (usocket:socket-server host port
                         ;; server handler function
                         (lambda (stream stdout)
                           (format stdout "Receiving connection from ~A:~D ~%"
                                   usocket:*remote-host*
                                   usocket:*remote-port*)
                           (loop
                             for o = (read stream nil 'eof)
                             do
                             (cond
                               ((eql o 'eof) (format stdout "EOF!"))
                               (t (format stdout "Received object: ~A~%" o)
                                  (return nil) ; exit our server handler function
                                  ))))
                         ;; arguments to our function
                         (list *standard-output*)
                         :in-new-thread T
                         :multi-threading T
                         :element-type 'object-type))

;; ----------------- CLIENT -----------------------------

(defun connect-to-legochain (&key (host #(127 0 0 1))
                                  (port 667))
  (let ((socket (usocket:socket-connect host
                                         port
                                         :element-type 'object-type
                                         :timeout 10
                                        )))
    ;; send something
    (write-sequence #(1 2 3 4) (usocket:socket-stream socket))
    
    (force-output (usocket:socket-stream socket))))






