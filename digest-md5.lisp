(in-package :sasl)

(defclass digest-md5 (client)
  ((realm :initarg :realm
	  :accessor realm
	  :initform nil)

   (state :initform :start
	  :accessor state)
   (nonce :accessor nonce)
   (cnonce :accessor cnonce)
   (digest-uri-value :accessor digest-uri-value)

   (mechanism-name :initform "DIGEST-MD5"))
  (:documentation "Client-side implementation of the SASL DIGEST-MD5 mechanism,
as specified in RFC 2831."))

(defmethod client-step ((c digest-md5) server-input)
  (ecase (state c)
    (:start
     ;; The server goes first, so wait if no input yet
     (if (null server-input)
	 #()
       ;; XXX: we assume that the challenge is pure ASCII.  correct?
       (let ((challenge (parse-challenge (map 'string #'code-char server-input))))

	 ;; If we know what realm we want, make sure we get it:
	 (if (realm c)
	     (unless (find (cons "realm" (realm c))
			   challenge :test #'equal)
	       (error "Realm ~A not offered by server" (realm c)))
	   ;; Else, just get the first one
	   (setf (realm c) (cdr (assoc "realm" challenge :test #'string=))))

	 (setf (nonce c) (cdr (assoc "nonce" challenge :test #'string=)))
	 ;; This is recommended to contain at least 64 bits of entropy
	 (setf (cnonce c) (format nil "~A" (random #x10000000000000000)))
	 (setf (digest-uri-value c)
	       (apply #'concatenate 'string (service c) "/"
		      (host c)
		      (if (or (null (serv-name c))
			      (string= (host c) (serv-name c)))
			  ()
			(list "/" (serv-name c)))))
	 (setf (state c) :sent)

	 ;; XXX: obey charset directive
	 (string-to-utf8
	  (apply #'concatenate 'string
		 "username=\"" (authc-id c) "\","
		 "realm=\"" (realm c) "\","
		 "nonce=\"" (nonce c) "\","
		 "cnonce=\"" (cnonce c) "\","
		 "nc=00000001,"
		 "qop=auth,"
		 "digest-uri=\"" (digest-uri-value c) "\","
		 "charset=utf-8,"
		 "response=" (response-value c t)
		 (when (authz-id c)
		   (list ",authzid=\"" (authz-id c) "\"")))))))
    (:sent
     ;; XXX: we assume that the challenge is pure ASCII.  correct?
     (let ((challenge (parse-challenge (map 'string #'code-char server-input))))
       (if (string= (cdr (assoc "rspauth" challenge :test #'string=))
		    (response-value c nil))
	   (progn
	     (setf (state c) :success)
	     #())
	 :failure)))
    (:success
     (if (eql server-input :success)
	 :success
       :failure))))

(defmethod response-value ((c digest-md5) request)
  (response (authc-id c) (authz-id c) (realm c) (get-password (password c))
	    (digest-uri-value c) (nonce c) (cnonce c) "00000001" "auth" request))

(defun response (authc-id authz-id realm password digest-uri nonce cnonce nc qop request)
  (labels ((c (&rest strings) (apply #'concatenate 'string strings))
	   (to-bytes (maybe-string)
		     (if (stringp maybe-string)
			 (string-to-latin1-or-utf8 maybe-string)
		       maybe-string))
	   (c-b (&rest maybe-strings) (apply #'concatenate 
					     '(vector
					       (unsigned-byte 8))
					     (map 'list #'to-bytes
						  maybe-strings)))
	   (h (string) (ironclad:digest-sequence :md5 (to-bytes string)))
	   (kd (k s) (h (c k ":" s)))
	   (hex (hash) (md5sum-to-hex hash)))
    (let ((a1 (if authz-id
		  (c-b (h (c authc-id ":" realm ":" password))
		       ":" nonce ":" cnonce ":" authz-id)
		(c-b (h (c authc-id ":" realm ":" password))
		     ":" nonce ":" cnonce)))
	  (a2 (let ((prefix (if request
				"AUTHENTICATE:"
			      ":"))
		    (suffix (if (string= qop "auth")
				""
			      ":00000000000000000000000000000000")))
		(c prefix digest-uri suffix))))
      (hex (kd (hex (h a1))
	       (c nonce ":" nc ":"
		  cnonce ":" qop ":" (hex (h a2))))))))
      
(defun md5sum-to-hex (md5sum)
  "Convert MD5SUM, a vector of 16 bytes, to a string of 32 lowercase hex digits."
  (string-downcase 
   (apply #'concatenate 'string
	  (map 'list (lambda (n)
		       ;; Note the zero-padded hex digits!  Forgetting that
		       ;; gives bugs that are hard to find.
		       (format nil "~2,'0X" n))
	       md5sum))))

(defun parse-challenge (challenge &optional (start 0) accumulated)
  "Parse CHALLENGE and return it as an alist.
Start at index START."
  (if (>= 0 (- (length challenge) start))
      accumulated
    ;; Directives are letters only - find the equal sign.
    (let* ((equal-sign (position #\= challenge :start start))
	   (directive (subseq challenge start equal-sign))
	   comma-position value)
      (unless equal-sign
	(error "Couldn't parse challenge - missing equal sign"))
      (if (not (eql (elt challenge (1+ equal-sign)) #\"))
	  ;; token - just find the comma
	  (progn
	    (setf comma-position (position #\, challenge :start equal-sign))
	    (setf value 
		  (string-trim '(#\Space #\Tab #\Newline #\Linefeed)
			       (subseq challenge (1+ equal-sign) comma-position))))
	;; quoted value - watch for backslash escaping
	(loop for index upfrom (+ 2 equal-sign)
	      until (eql (elt challenge index) #\")
	      do
	      (when (eql (elt challenge index) #\\)
		(incf index))
	      (push (elt challenge index) value)
	      finally 
	      (setf value
		    (concatenate 'string (nreverse value)))
	      (setf comma-position (1+ index))))

      (let ((new-entry (cons directive value)))
	(if (null comma-position)
	    (cons new-entry accumulated)
	  (parse-challenge challenge (1+ comma-position) (cons new-entry accumulated)))))))

(defun string-to-latin1-or-utf8 (string)
  "Convert STRING to ISO 8859-1 if possible, else to UTF-8.
Return a byte vector."
  (if (every #'in-latin1-p string)
      (map '(vector (unsigned-byte 8))
	   #'char-code
	   string)
    (string-to-utf8 string)))

;; arch-tag: bda651ac-39ec-11da-9ea5-000a95c2fcd0
