(defpackage #:nisp.ldap
  (:use :cl))

(in-package :nisp.ldap)

;;; Needs to be moved to a seperate utility file or something.
(defmacro while (test &body body)
  "While, like in most other languages."
  `(do ()
       ((not ,test))
     ,@body))

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

(defparameter *connections* '()
  "Property list of connections to LDAP.")

;;; Load an optional config file, the lack of this should not cause this
;;; program to become unusable.
(load "config.lisp" :if-does-not-exist nil)

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

(defun make-ldap (ldap-or-keyword
                  &optional (connections *connections*))
  "Return an LDAP object, so long as input is an ldap object or a
  keyword referincing an ldap object."
  (if (typep ldap-or-keyword 'ldap:ldap)
      ldap-or-keyword
      (getf connections ldap-or-keyword)))

(defparameter *ldap* (make-8b-ldap))
(defparameter *anon-ldap* (make-8b-ldap "" ""))

(defmacro with-ldap (ldap &body body)
  "Execute BODY in the context of LDAP bound to the ldap server."
  `(prog2
       (ldap:bind ,ldap)
       (progn ,@body)
     (ldap:unbind ,ldap)))

(defun strip-newlines (string &optional (replace-char nil))
  "Given a string, remove all newlines.

This is very irc specific where lines need to be all on one line.

Note that the newline is not replaced by a space!"
  (coerce
   (loop for char in (coerce string 'list)
      when (and replace-char (eq char #\Newline)) collect replace-char
      unless (eq char #\Newline) collect char)
   'string))

(defun print-single-entry (search-string &key (ldap *anon-ldap*)
                           attrs)
  (strip-newlines
   (ldap:ldif
    (with-ldap ldap
      (ldap:search ldap search-string
                   :attributes attrs)
      (ldap:next-search-result ldap)))
   #\ ))


(defun list-search-results (search-string &optional (ldap *anon-ldap*))
  "List of entries from a search."
  (with-ldap ldap
    (ldap:search ldap search-string))
  (let (result)
    (while (ldap:results-pending-p ldap)
      (push (ldap:next-search-result ldap) result))
    (nreverse (cdr result))))


