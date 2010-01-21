(in-package :cl-user)

(defpackage #:nisp.ldap
  (:use :cl :iterate :nisp.util-types)
  (:shadow :delete :search)
  (:shadowing-import-from :closer-mop
                          :defmethod)
  (:export :*connections* #:with-ldap #:get-single-entry
           #:one-line-ldif #:make-ldap #:describing-ldif-search

           ;; Generics to add your own "hooks" onto.  These are there
           ;; for convience only, and likely will not exist in the same
           ;; form 3 months from now as the way these are implemented is
           ;; far from ideal. (These are done as around methods
           ;; specializing on (T T), which means nobody else can write
           ;; an around method to specialize on (T T). I'd vastly prefer
           ;; adding another method selection :hook or similar.
           #:compute-ldap #:compute-filter
           ))

(in-package :nisp.ldap)


(defvar *connections* '()
  "Property list of connections to LDAP.")

;;; Load an optional config file, the lack of this should not cause this
;;; program to become unusable.

(defclass abstract-filter ()
  ()
  ;; This feels right, but not positive how subclassing is going to work
  ;; yet.
  (:documentation "Base of all ldap filters."))

(defclass message ()
  ()
  ;; I think this is what trivial-ldap is missing that does not feel
  ;; "right". Messages are taken in as raw strings not as a CLOS class I
  ;; can select on.
  (:documentation "Base of all ldap messages."))

(defclass modification-state ()
  ((modified-p :type boolean
               :reader modified-p
               :initform nil))
  (:documentation "Represents a modified state."))

(defclass modification-time ()
  ((modified-time :reader modification-time
                  )))

(defclass dn (modification-state)
  ((dn :type string
       :reader dn
       :initarg :dn))
  (:documentation "!!!")
  (:default-initargs :dn ""))

(defmethod (setf dn) ((value string)
                      (object dn))
  (setf (slot-value object 'modified-p) t)
  (setf (slot-value object 'dn) value))

(defclass rdn (dn)
  ()
  (:documentation "!!!"))

(defclass entry (trivial-ldap:entry)
  ((dn :type dn
       :reader dn
       :initarg :dn)
   (rdn :type rdn
       :reader rdn
       :initarg :rdn))
  (:documentation "Basic LDAP entry.

Mostly used for method selection apart from trivial-ldap."))


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
   (iter (for char :in (coerce string 'list))
         (when (and replace-char (eq char #\Newline))
           (collect replace-char into out))
         (unless (eq char #\Newline)
           (collect char into out))
         (finally (return out)))
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
        (iter (until (ldap:results-pending-p ldap))
          (push (ldap:next-search-result ldap) result))
        (nreverse (cdr result))))))

(defgeneric compute-filter (type &rest args)
  (:documentation "Compute an LDAP filter to do searches with.")
  (:method (type &key)
    "Return TYPE unchanged ignoring ARGS."
    type))

(defmethod compute-filter ((filter (eql :all)) &key)
  "Matches all objects."
  "(objectClass=*)")

(defgeneric compute-ldap (ldap &rest args)
  (:documentation "Make or select LDAP.")
  (:method (ldap &rest args)
    (declare (ignore args))
    "Return LDAP ignoring ARGS."
    ldap))

(defmethod trivial-ldap:search :around ((ldap t) (filter t) &key)
  "Search LDAP for all records under specified base."
  (call-next-method (compute-ldap ldap) (compute-filter filter)))
