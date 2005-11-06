(in-package :sasl)

(defparameter mechanisms
  '(("PLAIN" . plain)
    ("DIGEST-MD5" . digest-md5))
  "Alist of mappings from SASL mechanism names to class names.")

(defun get-mechanism (name)
  "Return the class name for the mechanism called NAME, or nil if not implemented."
  (cdr (assoc name mechanisms :test 'string=)))

(defclass client ()
  ((authc-id :initarg :authentication-id
	     :accessor authc-id)
   (authz-id :initarg :authorization-id
	     :accessor authz-id
	     :initform nil)
   (password :initarg :password
	     :accessor password
	     :type (or string function))

   (service :initarg :service
	    :accessor service)
   (host :initarg :host
	 :accessor host)
   (serv-name :initarg :serv-name
	      :accessor serv-name
	      :initform nil)

   (mechanism-name :reader mechanism-name
		   :allocation :class))
  (:documentation "Abstract base class for SASL clients using various mechanisms."))

(defgeneric client-step (client server-input)
  (:documentation "Perform a step in the SASL authentication.
SERVER-INPUT is a byte array containing the response from the
server, or NIL if the client should start the exchange, or the
keyword :SUCCESS if the server reported successful
authentication.

Returns a byte array to be sent in response to the server,
or :SUCCESS if the client should consider authentication
successful, or :FAILURE if the client should consider
authentication failed.  Obeying this result is important, as some
mechanisms provide mutual authentication."))

(defun get-password (password)
  (etypecase password
    (string
     password)
    (function
     (funcall password))))

;; arch-tag: b9a83c50-39ec-11da-9ea5-000a95c2fcd0
