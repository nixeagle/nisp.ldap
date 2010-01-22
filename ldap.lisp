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

(defgeneric (setf modification-time) (object))
(defgeneric (setf modification-state) (state object))
(defgeneric (setf modification) (object))
(defclass modification-state ()
  ((modification-state :type boolean
                       :reader modification-state 
                       :initform nil))
  (:documentation "Represents a modified state."))

(defclass modification-time ()
  ;; Not fully implemented yet. Just prototyping.
  ((modification-time :reader modification-time 
                      :initform (get-universal-time)))
  (:documentation "Represents time of last modification"))

(defclass modification (modification-state modification-time)
  ()
  (:documentation "Represents objects that remember modification data."))

(defmethod (setf modification-time) ((object modification-time))
  "Set the modification time of OBJECT to now."
  ;; It makes no sense to allow any other value then "now" for modified
  ;; time.
  (setf (slot-value object 'modification-time) (get-universal-time)))
(defmethod (setf modification-state) ((state (eql nil))
                                      (object modification-time))
  (declare (ignore state object)))
(defmethod (setf modification-state) ((state t)
                                      (object modification-time))
  (declare (type boolean state)
           (ignore state))
  (if (next-method-p)
      (values (setf (modification-time) object) (call-next-method))
      (setf (modification-time) object)))


(defmethod (setf modification-state) ((state t)
                                      (object modification-state))
  "Set modification state of OBJECT to STATE."
  ;; We allow setting to false here as it is a legit operation to do on
  ;; discarding changes, saving changes and so on.
  (declare (type boolean state))
  (setf (slot-value object 'modification-state) state)
  (if (next-method-p)
      (values state (call-next-method))
      state))

(defmethod (setf modification-state) ((state t) (object modification))
  "Set both modification time and state of OBJECT."
  (declare (type boolean state))
  (call-next-method))

(defclass base-mixin ()
  ()
  (:documentation "LDAP objects where the concept of `base' makes sense."))

(defclass rdn-mixin ()
  ()
  (:documentation "LDAP objects where `rdn' makes sense."))

(defgeneric base (ldap-object)
  (:documentation "Return the path to the LDAP-OBJECT.

`base' in ldap speak is the same meaning as pwd in a shell.")
  (:method ((ldap base-mixin))
    (error "Redefine `base' on the subclass of base-mixin.")))

(defgeneric rdn (ldap-object)
  (:documentation "Return the rdn of the LDAP-OBJECT.

`rdn' is roughly equivalent to a filename in *nisp. They are unique to
the `base' they are in.")
  (:method ((ldap rdn-mixin))
    (error "Redefine `rdn' on the subclass of rdn-mixin.")))

(defclass abstract-base (base-mixin)
  ((base :type string 
         :reader base
         :initarg :base
         :documentation "LDAP base path: equivalent to 'ls' on *nix."))
  (:documentation "Represents LDAP base path.")
  (:default-initargs :base ""))

(defclass abstract-rdn (rdn-mixin)
  ((rdn :type string 
        :reader rdn
        :initarg :rdn))
  (:documentation "!!!")
  (:default-initargs :rdn ""))

(defclass base (abstract-base modification)
  ())
(defclass rdn (abstract-rdn modification)
  ())

(defclass dn (rdn-mixin base-mixin modification-state)
  ((rdn :type rdn :initarg :rdn)
   (base :type base :initarg :base))
  (:documentation "LDAP Distinguished Name"))

(defmethod (setf dn) ((value string)
                      (object dn))
  (setf (modification-state object) t))

(defclass entry (trivial-ldap:entry)
  ((dn :type dn
       :initarg :dn)
   (rdn :type rdn
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


(defun strip-newlines (string &optional (replace-char #\Space))
  "Given a string, remove all newlines.

This is very irc specific where lines need to be all on one line.

Note that the newline is not replaced by a space!"
  (iter (for char :in-string string)
        (collect (if (char= char #\Newline)
                     replace-char
                     char) :result-type string)))

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
      (iter (repeat 1000000)
            (until (ldap:results-pending-p ldap))
            (collect (ldap:next-search-result ldap))))))

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
