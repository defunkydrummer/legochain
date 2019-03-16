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
  ;; The blockchain block.
  ;; We call it "bblock" because there is already a "block" builtin in Common Lisp.
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

(defun start-my-blockchain ()
  "Some startup tasks. This erases the in-memory blockchain!
And creates a blank blockchain (well, not blank, it has one block.)"
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
    (start-my-blockchain)
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






;;---------------------- PEER TO PEER ------------------------------


;; Type for the data interchanged between peers.
(defparameter *el-type* 'character)

;; default host
(defparameter *default-host* #(127 0 0 1))
;; default port
(defparameter *default-port* 6667)

;; The Peers, a list of plists containing keys:
;; :ip and :port
(defparameter *peers* (list '(:ip *default-host* :port 6667)))


;; PROTOCOL: --------------------------------------------------
;; message is a lisp object (readable string).
;; message has the operation symbol, and the data
(defparameter *operations*
  '(
    :request-all ; request complete blockchain from peer
    :request-all-response ; peer replies with the whole blockchain
    :get-latest ;obtain latest block
    :get-latest-response ; reply containing the latest block
    :new-block ; this message contains a newly created block
    :hi ; say 'hi (for test purposes)
    ))
;; the data type
(deftype encoded-data-type () 'string)
;; the data is a string that has passed through these steps:
;; 1. convert to byte array using CONSPACK
;; 2. convert to hex string using IRONCLAD..

(defun encode-data (obj)
  "Produces encoded string of the desired object."
  (ironclad:byte-array-to-hex-string (conspack:encode obj)))
;; we do the inverse for decoding
(defun decode-data (encoded)
  (declare (type encoded-data-type encoded)) 
  "Inverse of encode-data. "
  (conspack:decode (ironclad:hex-string-to-byte-array encoded)))
;; ------------------ end protocol -----------------------------

(defun send-message-to-stream (operation object to-stream)
  "Create a message to send through the socket, based on operation and the data. This also encodes object to data, using conspack."
  (assert (member operation *operations*))
  ;; message is: operation . data
  (print `(,operation . ,(encode-data object))
         to-stream))

(defun decode-message (obj)
  "Decode message. Object has already been READ, of course.
Returns operation and decoded object."
  (declare (type cons obj))
  (let ((operation (car obj))
        (data (cdr obj)))
    ;; assert that operation is valid, otherwise error.
    (unless (member operation *operations*)
      (error "Received invalid operation: ~A~%" operation))
    ;; assert type of encoded data, it should be
    ;; the one used by conspack...
    (unless (typep data 'encoded-data-type)
      (error "Invalid data type for data: ~A~%"
             (type-of data)))
    ;; decode data by using CONSPACK decode, and return
    (values operation
            (decode-data data))))


;; ----------------- CLIENT -----------------------------
;; Client sends messages to other peers' servers

(defun send (msg data
             &key (host *default-host*)
                  (port *default-port*))
  "Send a message to a specific peer"
  (let ((socket (usocket:socket-connect host
                                        port
                                        :element-type *el-type*
                                        :timeout 10
                                        )))
    ;; send message
    (send-message-to-stream msg data (usocket:socket-stream socket))
    ;; now!
    (force-output (usocket:socket-stream socket))))

(defun connect-to-legochain (&key (host *default-host*)
                                  (port *default-port*))
  "Send greetings to a specific peer."
  (send :hi (format nil "Hello from peer!")
        :host host
        :port port))

(defun please-send-me-blockchain (host port)
  "Send message: Please send me the complete blockchain you have."
  (send :request-all 
        ;; the data I send: my host, my port. 
        (list :host host :port port)
        :host host
        :port port))

(defun i-have-a-new-block (host port)
  "Send to the peer: I have a new block... here it is!"
  (send :new-block
        ;; the last block in my blockchain
        (last-block-on-blockchain *my-blockchain*)
        :host host
        :port port))

;;---------------------- SERVER ------------------------------
;; Server receives messages and performs actions accordingly.

(defun hi-message-handler (obj stream)
  (format stream "Client says Hi. Data: \"~A\"~%" obj))

(defun request-all-message-handler (obj stream)
  "Handle the message that requests the entire blockchain."
  (declare (ignore stream))
  ;; obj contains the host and the port.
  (let ((host (getf obj :host))
        (port (getf obj :port)))
    ;; reply with a request-all-response message
    (send :request-all-response ;here's my reply!
          ;; the whole blockchain
          *my-blockchain*
          ;; to the peer that sent that message.
          :host host
          :port port
          )))

(defun request-all-response-message-handler (obj stream)
  "Handle the message that brings the blockchain from another peer."
  ;; we validate the blockchain and see if the latest block is newer than ours.
  (if (and (eql (class-of obj) (find-class 'blockchain)) ; is it a blockchain object?
           (verify-blockchain obj)
           (> (block-id (last-block-on-blockchain obj))
              (block-id (last-block-on-blockchain *my-blockchain*))))
      ;; allright, we have a newer blockchain.
      (progn
        (format stream "Received a newer blochain!~%")
        ;; installing
        (setf *my-blockchain* obj))
      ;; no, blockhain not newer
      (format stream "Received an invalid or older blockchain: Ignoring.~%")))

(defun message-handler (msg stream)
  "Handle an incoming message."
  ;; message must be a lisp cons...
  (if (typep msg 'cons)   
      ;; decode it
      (multiple-value-bind (operation obj)
          (decode-message msg)
        ;; what do we do?
        ;; dispatch according to the type of operation
        (case operation
          (:hi (hi-message-handler obj stream))
          (:request-all
           (request-all-message-handler obj stream))
          (otherwise
           (format stream "Received an unimplemented operation: ~A~%" operation))))
      ;; not a good type of msg
      (format stream "Invalid data type of message: ~A~% "
              (type-of msg))))


(defun server-function (stream stdout host port)
  "Function that handles incoming packets."
  (format stdout "~A:~D Receiving connection from ~A:~D ~%"
          host
          port
          usocket:*remote-host*
          usocket:*remote-port*)
  (loop
    for o = (read stream nil 'eof)
    do
    (cond
      ((eql o 'eof) (format stdout "EOF!"))
      (t (format stdout "Received object: ~A~%" o)
         ;; handle the message
         (message-handler o stdout)
         
         ;; exit our server function.
         ;; that means we don't read further messages.
         (return nil)
         ))))

(defun create-legochain-server (&key (host *default-host*)
                                     (port *default-port*))
  "Create a legochain server using sockets and accept connections on port number X."
  (usocket:socket-server host port
                         ;; server handler function
                         #'server-function 
                         ;; arguments to our function, besides stream.
                         (list *standard-output* host port) 
                         :in-new-thread T
                         :multi-threading T
                         :element-type *el-type*))




