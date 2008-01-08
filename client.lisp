(in-package :sasl)
      
(defparameter mechanisms
  '(("DIGEST-MD5" digest-md5)
    ("PLAIN" plain :cleartext)
    ("ANONYMOUS" anonymous :anonymous))
  "List of SASL mechanisms in order of preference.
Each element is a list describing a mechanism.  The first item is
the name of the mechanism.  The second item is the class
implementing it.  The remaining items are properties of the
mechanism:

:CLEARTEXT    Password is sent in clear text or with weak encryption
:ANONYMOUS    Mechanism negotiates anonymous access")

(defun get-mechanism (name)
  "Return the class name for the mechanism called NAME, or nil if not implemented."
  (second (assoc name mechanisms :test 'string=)))

(defun choose-mechanism (available &key (cleartext nil) (anonymous nil))
  "Choose an SASL mechanism from AVAILABLE.
Return the class implementing the mechanism, or nil if no mechanism
is suitable.
AVAILABLE is a list of strings, naming the mechanisms offered by the
server.
If CLEARTEXT is true, accept mechanisms that send passwords in clear
text or with weak encryption.
If ANONYMOUS is true, use mechanisms that acquire anonymous access."
  (let ((implemented
	 (remove-if-not #'(lambda (mechanism) (member (first mechanism) available :test #'string=))
			mechanisms)))
    (unless cleartext
      (setf implemented
	    (remove-if #'(lambda (mechanism) (member :cleartext (cddr mechanism)))
		       implemented)))

    (flet ((anonymous-p (mechanism)
	     (member :anonymous (cddr mechanism))))
      (setf implemented
	    (remove-if (if anonymous
			   (complement #'anonymous-p)
			   #'anonymous-p)
		       implemented)))

    (second (first implemented))))

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
