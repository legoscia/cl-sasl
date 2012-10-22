(in-package :sasl)

(defclass plain (client)
  (
   ;; internal variable
   (state :initform :start
	  :accessor state)

   (mechanism-name :initform "PLAIN"))
  (:documentation "Client-side implementation of the SASL PLAIN mechanism,
as specified in RFC 2595, section 6."))

(defmethod client-step ((c plain) server-input)
  (declare (ignore server-input))
  (ecase (state c)
    (:start
     (setf (state c) :sent)
     (concatenate '(vector (unsigned-byte 8))
		  (string-to-utf8 (or (authz-id c) "")) '(0)
		  (string-to-utf8     (authc-id c))     '(0)
		  (string-to-utf8     (get-password (password c)))))
    (:sent
     :success)))

;; arch-tag: c5a9643e-39ec-11da-9ea5-000a95c2fcd0
