(defpackage #:nisp.ldap
  (:use :cl)
  (:export :*connections* #:with-ldap #:get-single-entry
           #:one-line-ldif #:make-ldap #:describing-ldif-search))

(in-package :nisp.ldap)

;;; Needs to be moved to a seperate utility file or something.
(defmacro while (test &body body)
  "While, like in most other languages."
  `(do ()
       ((not ,test))
     ,@body))


(defvar *connections* '()
  "Property list of connections to LDAP.")

;;; Load an optional config file, the lack of this should not cause this
;;; program to become unusable.



(defun make-ldap (ldap-or-keyword
                  &optional (connections *connections*))
  "Return an LDAP object, so long as input is an ldap object or a
  keyword referincing an ldap object."
  (if (typep ldap-or-keyword 'ldap:ldap)
      ldap-or-keyword
      (getf connections ldap-or-keyword)))



(defmacro with-ldap (ldap-or-keyword &body body)
  "Execute BODY in the context of LDAP bound to the ldap server."
  ;; I think this is fine to do instead of using gensyms. I'll consult
  ;; on lisp on this later.
  (let ((ldap (gensym)))
    `(let ((,ldap (make-ldap ,ldap-or-keyword)))
       (prog2
           (ldap:bind ,ldap)
           (progn ,@body)
         (ldap:unbind ,ldap)))))

(defun describing-ldif-search (search-string &optional (ldap :anon))
  "Development helper that prints a description of all search matches to
standard output in ldif form."
  (mapc #'(lambda (x) 
            (princ (ldap:ldif x))) 
        (list-search-results search-string (make-ldap ldap))))


(defun strip-newlines (string &optional (replace-char nil))
  "Given a string, remove all newlines.

This is very irc specific where lines need to be all on one line.

Note that the newline is not replaced by a space!"
  (coerce
   (loop for char in (coerce string 'list)
      when (and replace-char (eq char #\Newline)) collect replace-char
      unless (eq char #\Newline) collect char)
   'string))

(defun get-single-entry (search-string &key (ldap :anon)
                         attrs)
  "Get a single trivial-ldap:entry object by binding and searching."
  (with-ldap ldap
      (ldap:search (make-ldap ldap) search-string
                   :attributes attrs)
      (ldap:next-search-result (make-ldap ldap))))

(defgeneric one-line-ldif (entry)
  (:documentation "ldif on one line.")
  (:method ((entry ldap:entry))
    (strip-newlines (ldap:ldif entry) #\ )))

(defun print-single-entry (search-string &key (ldap :anon)
                           attrs)
  (strip-newlines
   (ldap:ldif
    (get-single-entry search-string :ldap (make-ldap ldap) :attrs attrs))
   #\ ))

(defun list-search-results (search-string &optional (ldap :anon))
  "List of entries from a search."
  (let ((ldap (make-ldap ldap)))
    (with-ldap ldap
      (ldap:search ldap search-string)
      (let (result)
        (while (ldap:results-pending-p ldap)
          (push (ldap:next-search-result ldap) result))
        (nreverse (cdr result))))))