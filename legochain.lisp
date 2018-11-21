(defpackage :legochain
  (:use :cl))

(in-package :legochain)

(defclass blockchain ()
  ((blocks :initarg :blocks
           :initform nil
           :documentation "The blockchain itself, a list of blocks."))
  (:documentation "A blockchain."))

(defparameter *dummy-value*
  (conspack:encode "This block has a dummy value. Congratulations!"))

(defparameter *mining-difficulty* 4
  "Number of zeroes the hash must start with to satisfy the difficulty condition when mining.")

(defclass block ()
  ((id :initarg :id
       :documentation "Id of the current block.")
   (previous-hash :initarg :previous-hash)
   ;; (this-hash :initarg this-hash
   ;;            :documentation "Hash value of the current block.")
   (timestamp :initarg :timestamp)
   (nonce-value :initarg :nonce-value
                :documentation "Nonce value for this block.")
   (payload :initarg :payload
            :initform *dummy-value*
            :documentation "The block's usable (payload) data."
            :type (or null (simple-array (unsigned-byte 8)))))
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
  (cl-ppcre:scan-to-strings *regex-difficulty* h))

(defun mine-new-block (&key previous-hash payload id)
  "Mine a new block for the data "
  ;; a terrible loop that will keep the computer busy...
  (let ((b nil) ;the final block
        (nonce-value 0))                            
    (loop
      do
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
      until (hash-complies-with-difficulty
             (compute-hash b)))
    ;;and we return the block thus mined
    b
    ))



(defun startup ()
  "Some startup tasks."
  ;; compile REGEX
  (unless *regex-difficulty*
    (setf *regex-difficulty*
          (cl-ppcre:create-scanner
           (regex-for-difficulty *mining-difficulty*)))))


