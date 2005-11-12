;;; -*- mode: lisp; -*-

(defpackage sasl-system
  (:use :cl :asdf))

(in-package :sasl-system)

(defsystem sasl
  :description "SASL library"
  :version "0.1"
  :licence "BSD-style"
  :author "Magnus Henoch <henoch@dtek.chalmers.se>"
  :depends-on (:ironclad)

  :components
  ((:file "packages")
   (:file "util" :depends-on ("packages"))
   (:file "client" :depends-on ("packages" "util"))
   (:file "plain" :depends-on ("client"))
   (:file "digest-md5" :depends-on ("client"))))

;; arch-tag: cb9cefaa-39ec-11da-9ea5-000a95c2fcd0
