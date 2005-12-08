(in-package :sasl)

(defparameter mechanisms
  '(("PLAIN" . plain)
    ("DIGEST-MD5" . digest-md5)
    ("ANONYMOUS" . anonymous))
  "Alist of mappings from SASL mechanism names to class names.")

(defun get-mechanism (name)
  "Return the class name for the mechanism called NAME, or nil if not implemented."
  (cdr (assoc name mechanisms :test 'string=)))

(defclass client ()
  ((authc-id :initarg :authentication-id
	     :accessor authc-id
	     :documentation "The authentication id.
This is the user whose credentials you are providing to the server.")
   (authz-id :initarg :authorization-id
	     :accessor authz-id
	     :initform nil
	     :documentation "The authorization id.
This is the user you want to act as.  You don't need to provide it
unless it is different from the authentication id.")
   (password :initarg :password
	     :accessor password
	     :type (or string function)
	     :documentation "The password.
This is either a string, or a function taking no arguments and
returning the password.")

   (service :initarg :service
	    :accessor service
	    :documentation "The service name.
Common values include \"xmpp\" and \"imap\".")
   (host :initarg :host
	 :accessor host
	 :documentation "The hostname of the service.")
   (serv-name :initarg :serv-name
	      :accessor serv-name
	      :initform nil
	      :documentation "The specific server you are connecting to
\(if different from the hostname).")

   (mechanism-name :reader mechanism-name
		   :allocation :class))
  (:documentation "Abstract base class for SASL clients using various mechanisms."))

(defgeneric client-step (client server-input)
  (:documentation "Perform a step in the SASL authentication.
SERVER-INPUT is a byte vector containing the response from the
server, or NIL if the client should start the exchange, or the
keyword :SUCCESS if the server reported successful
authentication.

Returns a byte vector to be sent in response to the server,
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
