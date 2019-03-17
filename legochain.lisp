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

;; The data that the first (initial or "genesis") block will carry.
(defparameter *initial-block-data*
  ;; homage to bitcoin's 1st block
  (conspack:encode "The Times 03/Jan/2009 Chancellor on brink of second bailout for banks"))

;; data type of our payloads...
(deftype payload-type () '(simple-array (unsigned-byte 8)))

(defclass bblock ()
  ;; The blockchain block.
  ;; We call it "bblock" because there is already a "block" builtin in Common Lisp.
  ((id :initarg :id
       :reader block-id
       :documentation "Id of the current block."
       :type integer)
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

(defmethod get-block-by-index ((chain blockchain) (index integer))
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
                                   (payload simple-array))
  "Add payload to the blockchain. 
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


(defmethod add-data-to-blockchain ((chain blockchain)
                                   (s string))
  "Add string to the blockchain. It will be encoded."
  (add-data-to-blockchain chain (encode-payload s)))

(defvar *my-blockchain* nil)

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
      (push-block-to-blockchain (mine-new-block  
                                 :previous-hash nil
                                 :payload *initial-block-data*
                                 :id 0)
                                b))
    ;; return it
    b
    ))



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


(defmethod verify-blocks ((chain blockchain))
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
     blocks)
    ))

(defmethod blockchain-ok ((chain blockchain))
  "Applies verify-blocks and gives either T or NIL T = all blocks are OK."
  (if (blockchain-blocks chain)
      (every (lambda (x) (and x T))
             (verify-blocks chain))
      ;; else: there are no blocks --> chain is OK too.
      T
      ))


(defmethod verify-blockchain-against ((my-chain blockchain)
                                      (other-chain blockchain))
  "Verify my blockchain against a potential longer blockchain."
  (when (blockchain-ok other-chain)
    (let ((last-mine (last-block-on-blockchain my-chain))
          (last-other (last-block-on-blockchain other-chain)))
      (when (< (block-id last-mine)
               (block-id last-other))
        ;; the other chain has a longer block.
        ;; check that the hash of my last block
        ;; is equal to the "previous-hash" of the
        ;; next block in the other chain
        
        (let ((their-next-block
                (get-block-by-index other-chain
                                    (1+ (block-id last-mine)))))
          (eql (block-previous-hash their-next-block)
               (compute-hash last-mine)))))))

;;-------------------------------------------------------------------


;; misc test / helper

;;show all blockchain
(defun show-blockchain (chain)
  (mapcar (lambda (b)
            (mapcar
             (lambda (slot-name)
               (list slot-name
                     (slot-value b slot-name)))
             (mapcar #'sb-mop:slot-definition-name
                     (sb-mop:class-slots (class-of b)))))
          (blockchain-blocks chain)))

(defun add-stuff (chain)
  "Add test blocks to blockchain."
  (loop for str in '("Interesting stuff"
                     "Another block"
                     "Yet another block"
                     "WOW, blockchains are cool")
        do
        (add-data-to-blockchain
         chain
         (encode-payload str))))

(defun list-payloads (chain)
  "Show all payloads (decoded)"
  (loop for bl in (blockchain-blocks chain)
        collecting (decode-payload bl)))



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
          (add-data-to-blockchain chain
                                  (encode-payload str)))
    ;; verify blockchain
    (assert (blockchain-ok chain))
    ;;compare payloads with original str
    ;;note: payloads are in reverse order than original test-data.
    ;;note: ignore initial block.
    (let
        ((a
           (butlast (loop for bl in (blockchain-blocks chain)
                          collecting (decode-payload bl))))
         (b
           (reverse test-data)))
      ;; return the blockchain, the test as boolean values,
      ;; and the payload
      (values
       chain
       :test-result (equalp a b)
              :blockchain-data a
              :original-data b))))






;;---------------------- PEER TO PEER STUFF------------------------------


;; Type for the data interchanged between peers.
(defparameter *el-type* 'character)

;; default host
(defparameter *default-host* #(127 0 0 1))
;; default port
(defparameter *default-port* 6667)


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


;; ----------------- STATUS -----------------------------
;; Server status (a class)
;; This contains the status of our server

(defclass server ()
  ((host :initarg :host
         :initform *default-host*
         :reader server-host
         :documentation "Vector with IP of this server."
         :type simple-array)
   (port :initarg :port
         :initform *default-port*
         :reader server-port
         :documentation "Port of this server."
         ;; can be an integer from 0 to 65535, we tell Lisp.
         :type (integer 0 65535))
   (socket :accessor server-socket
           :documentation "The socket server of this server."
           :type usocket:stream-server-usocket)
   (thread :accessor server-thread
           :documentation "The thread where the server runs.")
   (blockchain :accessor server-blockchain
               :documentation "The blockchain of this server."
               :type blockchain)
   (peers :accessor server-peers
          :documentation "A list of peers"
          :type cons))
  (:documentation "Server status, config, and data."))



;; ----------------- CLIENT -----------------------------
;; Client sends messages to other peers' servers

(defun send (msg data host port)
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
        host
        port))

(defun please-send-me-blockchain (host port server)
  "Send message: Please send me the complete blockchain you have."
  (send :request-all 
        ;; the data I send: my host, my port. 
        (list :host (server-host server) :port (server-port server))
        host
        port))

(defun i-have-a-new-block (host port)
  "Send to the peer: I have a new block... here it is!"
  (send :new-block
        ;; the last block in my blockchain
        (last-block-on-blockchain *my-blockchain*)
        host
        port))

;;---------------------- SERVER ------------------------------
;; Server receives messages and performs actions accordingly.

(defun hi-message-handler (obj stream)
  (format stream "Client says Hi. Data: \"~A\"~%" obj))

(defun request-all-message-handler (obj stream server)
  "Handle the message that requests the entire blockchain."
  ;; obj contains the host and the port.
  (let ((host (getf obj :host))
        (port (getf obj :port)))
    ;; reply with a request-all-response message
    (when (and host port)
      (format stream "Remote peer ~A : ~D wants my blochain.~%" host port)
      (send :request-all-response ;here's my reply!
            ;; the whole blockchain
            (server-blockchain server)
            ;; to the peer that sent that message.
            host
            port
            ))))

(defun request-all-response-message-handler (obj stream server)
  "Handle the message that brings the blockchain from another peer."
  ;; we validate the blockchain and see if the latest block is newer than ours.
  ;; here we'll use the shortcutting AND operator to circumvent
  ;; the need for a lot of nested IFs
  (let*
      ((is-blockchain                   ; is it a blockchain object? 
         (eql (class-of obj)
              (find-class 'blockchain)))        (is-verified         ; is it a blockchain object, and verified?
         (and is-blockchain (blockchain-ok obj)))
       (last-block-there (last-block-on-blockchain obj))
       (last-block-here (last-block-on-blockchain (server-blockchain server))))

    ;; handle scenarios where one of the chains is blank
    (cond
      ((null last-block-there) (format stream "Received blank blockchain.~%"))
      ((null last-block-here) ;; we are empty
       ;; install in our server as the new blockchain
       (when is-verified
         (format stream "Received a blockchain and we have no blocks. Installing.~%")
         (setf (server-blockchain server) obj)))
      (t                      ; case when both blockchain have blocks!
       (let* ((has-newer-block ; is it a blockchain object, verified, and has a higher block id?
                (and is-verified
                     (> (block-id last-block-there)
                        (block-id last-block-here))))
              ;; verify that my shorter chain is contained in 
              ;; the longer chain
              (makes-sense
                (and has-newer-block
                     (verify-blockchain-against (server-blockchain server) obj))))
         ;; print message according to each failed validation.
         
         (cond
           ((not is-blockchain) (format stream "Not a blockchain!~%"))
           ((not is-verified) (format stream "Received invalid blockchain!~%"))
           ((not has-newer-block) (format stream "Blockchain is not longer than ours.~%"))
           ((not makes-sense) (format stream "Blockchain doesn't contain mine. FRAUD! ~%"))
           ;; all checks passed
           (t (format stream "Received a longer blochain!~%")
              ;; installing in our server as the new blockchain
              (setf (server-blockchain server) obj))))
       ))))

(defun message-handler (msg stream server)
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
           (request-all-message-handler obj stream server))
          (:request-all-response
           (request-all-response-message-handler obj stream server))
          (otherwise
           (format stream "Received an unimplemented operation: ~A~%" operation))))
      ;; not a good type of msg
      (format stream "Invalid data type of message: ~A~% "
              (type-of msg))))


(defun server-function (stream stdout server)
  "Function that handles incoming packets."
  (format stdout "Myself on ~A:~D receiving connection from ~A:~D ~%"
          (server-host server)
          (server-port server)
          usocket:*remote-host*
          usocket:*remote-port*)
  (let ((o (read stream nil 'eof))) ; what we got from the stream
    (cond
      ((eql o 'eof) (format stdout "EOF!"))
      (t (format stdout "Received object: ~A~%" o)
         ;; handle the message
         (message-handler o stdout server)
         ))))

(defun create-legochain-server (&key (host *default-host*)
                                     (port *default-port*)
                                     (blank-blockchain T))
  "Create a legochain server using sockets and accept connections on port number X. Return the server object... 
By default the server has a blank blockchain"
  (let ((s (make-instance 'server
                          :host host
                          :port port)))
    ;; create a blank blockchain
    (setf (server-blockchain s)
          (start-my-blockchain blank-blockchain))
    ;; get the two different values obtained from calling usocket:socket-server
    (multiple-value-bind (thread socket)
        (usocket:socket-server host port
                               ;; server handler function
                               #'server-function 
                               ;; arguments to our function, besides stream:
                               ;; stdout and my server object
                               (list *standard-output* s) 
                               :in-new-thread T ;IMPORTANT!
                               :multi-threading nil ; don't open a thread for every incoming packet.
                               :element-type *el-type*)
      (setf (server-thread s) thread
            (server-socket s) socket))
    ;; return the server object
    s
    ))

(defmethod close-server ((s server))
  (portable-threads:kill-thread (server-thread s))
  (usocket:socket-close (server-socket s)))


;; --------------- test servers- -----

(defun servers-test (port1 port2)
  (let ((s1 (create-legochain-server :port port1
                                     :blank-blockchain T))
        (s2 (create-legochain-server :port port2
                                     :blank-blockchain nil)))
    ;; s1 asks s2 for its (longer) blockchain
    (please-send-me-blockchain *default-host*
                               7002
                               s1)
    ;; now again
    (please-send-me-blockchain *default-host*
                               7002
                               s1)
    ;; now s2 asks s1
    (please-send-me-blockchain *default-host*
                               7001
                               s2)

    (close-server s1)
    (close-server s2)
    ))

