(in-package :sasl)

;;; These functions make assumptions about CHAR-CODE.  See the README
;;; file.

(defun string-to-utf8 (string)
  "Convert STRING to UTF-8.  Return a vector of unsigned-bytes."
  ;; Use built-in function on CLISP.
  #+clisp (ext:convert-string-to-bytes string 'charset:utf-8)
  #-clisp (string-to-utf8-lisp string))

(defun string-to-utf8-lisp (string)
  "Convert STRING to UTF-8.  Return a vector of unsigned-bytes."
  (let (result)
    (loop for c across string
	  do
	  (let ((code (char-code c)))
	    (cond
	     ((<= code #x7f)
	      ;; 0xxxxxxx
	      (push code result))
	     
	     ((<= code #x7ff)
	      ;; 110xxxxx 10xxxxxx
	      (push (logior #b11000000
			    (ash (logand #b11111000000 code)
				 -6))
		    result)
	      (push (logior #b10000000
			    (logand #b111111 code))
		    result))

	     ((<= code #xffff)
	      ;; 1110xxxx 10xxxxxx 10xxxxxx
	      (push (logior #b11100000
			    (ash (logand #b1111000000000000 code)
				 -12))
		    result)
	      (push (logior #b10000000
			    (ash (logand #b111111000000 code)
				 -6))
		    result)
	      (push (logior #b10000000
			    (logand #b111111 code))
		    result))

	     ((<= code #x10ffff)
	      ;; 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
	      (push (logior #b11110000
			    (ash (logand #b111000000000000000000 code)
				 -18))
		    result)
	      (push (logior #b10000000
			    (ash (logand #b111111000000000000 code)
				 -12))
		    result)
	      (push (logior #b10000000
			    (ash (logand #b111111000000 code)
				 -6))
		    result)
	      (push (logior #b10000000
			    (logand #b111111 code))
		    result))

	     (t
	      (error "Unknown code point: ~a" code)))))
    (let ((length (length result)))
      (make-array length
		  :element-type '(unsigned-byte 8)
		  :initial-contents (nreverse result)))))

(defun in-latin1-p (char)
  "Return non-nil if CHAR is in the ISO 8859-1 character set."
  (<= (char-code char) #xff))

;; arch-tag: 36cd3748-4939-11da-b980-000a95c2fcd0
