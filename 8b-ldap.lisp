(in-package :cl-user)
(defpackage #:nisp.8b-ldap
  (:use :cl :nisp.ldap))

(in-package :nisp.8b-ldap)

;;; For eighthbit we use ssh tunnels to LDAP, so make sure this matches
;;; up with your local machine name. This may _not_ be localhost!
(defvar *default-host* "localhost"
  "Location of ldap.")

(defvar *default-port* (the integer 2242)
  "Where to look for ldap.

This is not the default LDAP port, but we default to 2242 for ssh
tunnels to LDAP.")

(defvar *user* ""
  "User to interact with LDAP as.

Defaults to an empty string which means we are anonymous.")

(defvar *pass* ""
  "Password to auth to LDAP with.

Defautls to an empty string which means no pass.")

(defvar *root-base* "dc=eighthbit,dc=net"
  "All 8b LDAP things are under this base.")

(defun make-8b-ldap (&optional (user "") (pass "")
                     (base ""))
  "Make an ldap object for 8b's ldap.

We default to anon bind.

Note that the base should be defined as a concat of base and
*root-base*."
  (ldap:new-ldap :host *default-host*
                 :port *default-port*
                 :user user
                 :pass pass
                 :base (concatenate 'string *root-base* base)))

(setf (getf *connections* :anon) (make-8b-ldap))

(defparameter *ldap* (make-8b-ldap))
(defparameter *anon-ldap* (make-8b-ldap "" ""))