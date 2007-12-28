;;;; CLEWS: User handling functionality
;;;; Copyright (C) 2003-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; $Id: user.lisp,v 1.3 2007/07/26 08:53:00 willijar Exp $

(in-package :clews)

(defclass user (inet.acl:user property-holder)
  ((email :type cons :initform (list nil) :initarg :email :accessor email
          :documentation "cons of display name and email address"))
  (:documentation "A generic user record for web applications."))

(defun make-email(name address)
  (cons name address))

(defgeneric display-name(entity)
  (:documentation "return the display name of an entity or nil if none")
  (:method((entity list)) (car entity))
  (:method ((user user))
    (or (display-name (email user))
        (let ((firstname (property user :firstname))
              (lastname (property user :lastname)))
          (if lastname
              (with-output-to-string(os)
                (when firstname (write-sequence firstname os) (write-char #\space os))
                (when lastname (write-sequence lastname os)))
              (username user))))))

#| auto set email address
(map-dictionary
 #'(lambda(id u)
     (print
     (setf (email u)
      (make-email
       (when (property u :lastname) (display-name u))
       (if (not (find #\@ (username u)))
           (format nil "~A@aston.ac.uk" (username u))
           (username u)))))
     (setf (get-dictionary id aston::*user-source*) u))
 aston::*user-source*)
|#

(defgeneric email-address(entity)
  (:documentation "Return the mail address of entity")
  (:method((entity list)) (cdr entity))
  (:method((entity user)) (email-address (email entity))))

(define-condition no-such-user-condition (error)
  ((username :initarg :username))
  (:report (lambda (condition stream)
	     (format stream "No  User ~S in user source"
               (slot-value condition 'username)))))

(defclass user-source()
  ((roles :reader role-membership :initform (make-hash-table) ))
  (:documentation "Mixin to provided added user/role handling
for a dictionary user source"))

(defun clear-role-membership(username role-membership)
  "Clears username from the roles of a role-memberhsip hashtable"
  (maphash
   #'(lambda(role members)
       (let ((members (delete username members :test #'equal)))
	 (if members
	     (setf (gethash role role-membership) members)
	     (remhash role role-membership))))
   role-membership))

(defun add-role-membership(username user role-membership)
  "Add username to user roles a role-memberhsip hashtable"
  (dolist (role (roles user))
    (push username (gethash role role-membership))))

(defmethod initialize-instance :after ((users user-source) &key)
  "Read in roles membership"
  (map-dictionary
   #'(lambda(username user)
       (add-role-membership username user (role-membership users)))
   users))

(defmethod roles((users user-source))
  (dictionary-keys (role-membership users)))

(defmethod (setf get-dictionary) :after (user username (users user-source)
					      &optional default)
  (declare (ignore default))
  (clear-role-membership username (role-membership users))
  (add-role-membership username user (role-membership users)))

(defmethod rem-dictionary :after(username (users user-source))
  (clear-role-membership username (role-membership users)))

(defgeneric get-users(userspec source)
  (:documentation "return a list of usernames from a usersource
dictionary which match a given userspec"))

(defmethod get-users((username string) (source dictionary))
  (when (get-dictionary username source)  (list username)))

(defmethod get-users((userspec list) (source dictionary))
  (mapcan #'(lambda(userspec) (get-users userspec source)) userspec))

(defmethod get-users((role symbol) (source dictionary))
  (let ((users '()))
    (map-dictionary #'(lambda(username user)
			(when (has-role user role) (push username users)))
		    source)
    users))

(defmethod get-users((role symbol) (source user-source))
  (copy-list (gethash role (role-membership source))))

(defclass simple-pwd-source(dictionary)
  ((source :type dictionary :initarg :source
	   :reader source))
  (:documentation "Password source associated with a simple user
source - passwords stored in :password property of user"))

(defun make-simple-pwd-source(source)
  (make-instance 'simple-pwd-source :source source))

(defmethod get-dictionary(key (self simple-pwd-source) &optional default )
  (multiple-value-bind (user found-p)
			(get-dictionary key (source self) default)
	(values (when found-p (property user :pwd))
		found-p)))

(defmethod (setf get-dictionary) (new-value key (self simple-pwd-source)
					    &optional default)
  (declare (ignore default))
  (let ((user (or (get-dictionary key (source self))
		  (make-instance 'user :username key))))
    (setf (property user :pwd) new-value)
    (setf (get-dictionary key (source self)) user)))

