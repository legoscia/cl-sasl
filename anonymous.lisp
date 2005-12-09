(in-package :sasl)

(defclass anonymous (client)
  ((trace :initform ""
	  :initarg :trace
	  :accessor trace-data
	  :documentation "Trace information.
This may be an e-mail address, or an opaque token that the
client's system administrator can interpret.  It can also
be an empty string (the default).")

   ;; internal variable
   (state :initform :start
	  :accessor state)
   (mechanism-name :initform "ANONYMOUS"))

  (:documentation "Client-side implementation of the SASL ANONYMOUS mechanism,
as specified in RFC 2245 and/or draft-ietf-sasl-anon-05.  It is supposed to
always conform to the latter, and to conform to both if the trace information
only contains printable ASCII characters or is empty."))

(defmethod client-step ((c anonymous) server-input)
  (declare (ignore server-input))
  (ecase (state c)
    (:start
     (setf (state c) :sent)
     (string-to-utf8 (trace-data c)))
    (:sent
     :success)))

;; arch-tag: 45e47574-6834-11da-8b94-000a95c2fcd0
