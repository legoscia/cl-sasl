(in-package :cl-user)

(defpackage :sasl
  (:use :cl)
  (:export
   :get-mechanism
   :client :client-step :mechanism-name
   :authc-id :authz-id :password :service :host :serv-name
   :plain
   :digest-md5 :realm))

;; arch-tag: c23ff10a-39ec-11da-9ea5-000a95c2fcd0
