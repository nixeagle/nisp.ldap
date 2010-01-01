(in-package :cl-user)
(defpackage #:nisp.ldap-system
  (:use :cl :asdf))

(in-package #:nisp.ldap-system)

(defpackage #:nisp.ldap
  (:use :cl))

(defsystem :nisp.ldap
  :version "0.0.4"
  :author "James S <dev@nixeagle.org>"
  :license "GPLv2 or later"
  :depends-on (:illusion :nistilities :trivial-ldap)
  :components
  ((:file "ldap")))