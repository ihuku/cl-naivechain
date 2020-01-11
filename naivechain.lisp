(defpackage naivechain
  (:use :cl)
  (:export :*api-port* :start-api-server :*p2p-port* :start-p2p-server))

(in-package :naivechain)

(defstruct (naiveblock (:conc-name nil))
  index prevhash timestamp data hash)

(defun make-genesis-block ()
  (make-naiveblock
    :index 0
    :prevhash "0"
    :timestamp 1465154705
    :data "my genesis block!!"
    :hash "816534932c2b7154836da6afc367695e6337db8a921823784c14378abed4f7d7"))

;; New block will be added at the head of chain.
(defvar *blockchain* (list (make-genesis-block)))

(defun get-latest-block ()
  (first *blockchain*))

(defun sha256 (str)
  (ironclad:byte-array-to-hex-string
    (ironclad:digest-sequence
      :sha256
      (ironclad:ascii-string-to-byte-array str))))

(defun calculate-hash (index prevhash timestamp data)
  ;; concatenate 4 fields into a single string, and calculate sha256
  (sha256 (format nil "~A~A~A~A" index prevhash timestamp data)))

(defun calculate-hash-for-block (block)
  (with-slots (index prevhash timestamp data) block
    (calculate-hash index prevhash timestamp data)))

(defun valid-block-p (new-block previous-block)
  (cond
    ((not (= (index new-block) (1+ (index previous-block))))
     (format t "invalid index") nil)
    ((not (equal (prevhash new-block) (hash previous-block)))
     (format t "invalid previous hash") nil)
    ((not (equal (hash new-block) (calculate-hash-for-block new-block)))
     (format t "invalid new block") nil)
    (t t)))

(defun add-block (new-block)
  (when (valid-block-p new-block (get-latest-block))
    (push new-block *blockchain*)))

(defun generate-next-block (data)
  (let* ((latest-block (get-latest-block))
         (index (1+ (index latest-block)))
         (prevhash (hash latest-block))
         (timestamp (get-universal-time)))
    (make-naiveblock
      :index index
      :prevhash prevhash
      :timestamp timestamp
      :data data
      :hash (calculate-hash index prevhash timestamp data))))

(defvar *p2p-port* 6001)

(defvar *peers* '())

(defstruct (peer (:conc-name nil)) host port)

(defun add-peer (host port)
  (push (make-peer :host host :port port) *peers*))

(defun start-p2p-server ()
  (swank:create-server :port *p2p-port* :dont-close t :style :spawn))

(defun send-message (message peer)
  (swank-client:with-slime-connection (connection (host peer) (port peer))
    (swank-client:slime-eval message connection)))

(defun broadcast-message (message)
  (mapc (lambda (to) (send-message message to)) *peers*))

(defvar *api-port* 3001)

(defun start-api-server ()
  (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port *api-port*)))

(hunchentoot:define-easy-handler (blocks-handler :uri "/blocks") ()
  (setf (hunchentoot:content-type*) "text/plain")
  (format nil "~A~%" (reverse *blockchain*)))

(hunchentoot:define-easy-handler (mine-block-handler :uri "/mine-block") (data)
  (add-block (generate-next-block data))
  (broadcast-message `(setf *blockchain* (quote ,*blockchain*)))
  (setf (hunchentoot:content-type*) "text/plain")
  (format nil "~A~%" (first *blockchain*)))

(hunchentoot:define-easy-handler (peers-handler :uri "/peers") ()
  (setf (hunchentoot:content-type*) "text/plain")
  (format nil "~A~%" *peers*))

(hunchentoot:define-easy-handler (add-peer-handler :uri "/add-peer") (host port)
  (let ((port (parse-integer port)))
    (broadcast-message `(add-peer ,host ,port))
    (add-peer host port)
    (setf (hunchentoot:content-type*) "text/plain")
    (format nil "~A~%" *peers*)))
