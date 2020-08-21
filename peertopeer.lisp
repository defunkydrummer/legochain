(in-package :legochain)

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
  '(:request-all              ; request complete blockchain from peer
    :request-all-response     ; peer replies with the whole blockchain
    :get-latest               ; obtain latest block
    :get-latest-response      ; reply containing the latest block
    :new-block                ; this message contains a newly created block
    :hi                       ; say 'hi (for test purposes)
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

(defun send-message-to-stream (&key operation object to-stream)
  "Create a message to send through the socket, based on operation and the data. This also encodes object to data, using conspack."
  (assert (member operation *operations*))
  ;; message is: operation . data
  (print
   `(,operation . ,(encode-data object))
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
;; ------------------ end protocol -----------------------------


;; ----------------- STATUS -----------------------------
;; Server status (a class)
;; This contains the status of our server

(defclass server ()
  ((host       :initarg :host
               :initform *default-host*
               :reader server-host
               :documentation "Vector with IP of this server."
               :type simple-array)
   (port       :initarg :port
               :initform *default-port*
               :reader server-port
               :documentation "Port of this server."
               ;; can be an integer from 0 to 65535, we tell Lisp.
               :type (integer 0 65535))
   (socket     :accessor server-socket
               :documentation "The socket server of this server."
               :type usocket:stream-server-usocket)
   (thread     :accessor server-thread
               :documentation "The thread where the server runs.")
   (blockchain :accessor server-blockchain
               :documentation "The blockchain of this server."
               :type blockchain)
   (peers      :accessor server-peers
               :documentation "A list of peers"
               :type cons))
  (:documentation "Server status, config, and data."))



;; ----------------- CLIENT part -----------------------------
;; Client sends messages to other peers' servers

(defun send (msg data host port &key (retries 2)
                                     (sleep 1))
  "Send a message to a specific peer.
Retries: Number of times to try again if connection is refused...
Sleep: time to sleep before retry."
  (handler-case
      (usocket:with-connected-socket
          (socket (usocket:socket-connect host
                                          port
                                          :element-type *el-type*
                                          :timeout 10 ;doesn't work on windows
                                          ))
        (send-message-to-stream :operation msg
                                :object data
                                :to-stream (usocket:socket-stream socket))
        ;; now!
        (force-output (usocket:socket-stream socket)))
    ;; if can't connect
    (usocket:connection-refused-error (e)
      (declare (ignore e))
      (when (> retries 0)
        ;; wait...
        (sleep sleep)
        ;; try again
        (send msg data host port :retries (1- retries)
                                 :sleep sleep)))))

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

(defun i-have-a-new-block (host port block)
  "Send to the peer: I have a new block... here it is!"
  (send :new-block
        block
        host
        port))

;;---------------------- SERVER part  ------------------------------
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
              (find-class 'blockchain)))
       (is-verified         ; is it a blockchain object, and verified?
         (and is-blockchain (verify obj)))
       (last-block-there
         (last-block obj))
       (last-block-here
         (last-block (server-blockchain server))))

    ;; handle scenarios where one of the chains is blank
    (cond
      ((null last-block-there) (format stream "Received blank blockchain.~%"))
      ((null last-block-here) ;; we are empty
       ;; install in our server as the new blockchain
       (when is-verified
         (format stream "Received a blockchain and we have no blocks. Installing.~%")
         (setf (server-blockchain server) obj)))
      (t                       ; case when both blockchains have blocks!
       (let* ((has-newer-block ; is it a blockchain object, verified, and has a higher block id?
                (and is-verified
                     (> (block-id last-block-there)
                        (block-id last-block-here))))
              ;; Chain "makes sense" when my shorter chain is contained in the longer chain.
              (makes-sense
                (and has-newer-block
                     (verify-against (server-blockchain server) obj))))
         ;; print message according to each failed validation.
         (cond
           ((not is-blockchain)   (format stream "Not a blockchain!~%"))
           ((not is-verified)     (format stream "Received invalid blockchain!~%"))
           ((not has-newer-block) (format stream "Blockchain is not longer than ours.~%"))
           ((not makes-sense)     (format stream "Blockchain doesn't contain mine. FRAUD! ~%"))
           ;; all checks passed
           (t (format stream "Received a longer blochain! Installing.~%")
              ;; installing in our server as the new blockchain
              (setf (server-blockchain server) obj))))))))

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
          (:hi                    (hi-message-handler obj stream))
          (:request-all           (request-all-message-handler obj stream server))
          (:request-all-response  (request-all-response-message-handler obj stream server))
          (otherwise              (format stream "Received an unimplemented operation: ~A~%" operation))))
      ;; else: not a good type of msg
      (format stream "Invalid data type of message: ~A~% " (type-of msg))))


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
         (message-handler o stdout server)))))

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
                               :multi-threading nil ;no thread for each request 
                               :element-type *el-type*)
      (setf (server-thread s) thread
            (server-socket s) socket))
    ;; return the server object
    s))

(defmethod close-server ((s server))
  (portable-threads:kill-thread (server-thread s))
  (usocket:socket-close (server-socket s)))


;; --------------- test peer-to-peer interaction -----

(defun servers-test (port1 port2)
  (let* ((s1 (create-legochain-server :port port1
                                     :blank-blockchain T))
        (s2 (create-legochain-server :port port2
                                     :blank-blockchain nil)))
    
    ;; s1 asks s2 for its (longer) blockchain
    (format t "S1 asks S2 for its longer blockchain.~%")
    (please-send-me-blockchain *default-host*
                               port2
                               s1)

    ;; now again
    (sleep 5)
    (format t "S1 asks S2 for its longer blockchain, again.~%")
    (please-send-me-blockchain *default-host*
                               port2
                               s1)                                       


    ;; now S1 will have a new block
    (format t "Adding new block to S1~%")
    (add-data (server-blockchain s1) "More data")
    (sleep 5)

    ;; now s2 asks s1
    (format t "S2 asks S1 for its  blockchain.~%")
    (please-send-me-blockchain *default-host*
                               port1
                               s2)    
    (sleep 10) ;before closing the server...

    (close-server s1)
    (close-server s2)
    ;; list payloads of all chains
    (print (list-payloads (server-blockchain s1)))
    (print (list-payloads (server-blockchain s2)))))
