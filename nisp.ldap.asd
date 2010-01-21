(in-package :cl-user)
(defpackage #:nisp.ldap-system
  (:use :cl :asdf))

(in-package #:nisp.ldap-system)

(defsystem :nisp.ldap
  :version "0.0.12"
  :author "James S <dev@nixeagle.org>"
  :license "GPLv2 or later"
  :depends-on (:trivial-ldap :nisp.util :iterate :closer-mop)
  :components
  ((:file "ldap")))

(defsystem :nisp.8b-ldap
  :version "0.0.12"
  :depends-on (:trivial-ldap :nisp.util :iterate :nisp.ldap)
  :components
  ((:file "8b-ldap")))