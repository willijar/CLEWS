;;;; CLEWS Base class for applications and plugins
;;;; Copyright (C) 2002-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; $Id: component.lisp,v 1.3 2007/07/08 11:20:33 willijar Exp $

(in-package :clews)

(defgeneric id(component)
  (:documentation "Return a unique id for an object"))

(defclass component (access-controlled-entity)
  ((id :initarg :id :type symbol :reader id
       :documentation "Unique id for this component - used as key for
application specific properties for users - should be unique across
all applications") )
  (:default-initargs
      :acl '((:view . (:all))
             (:manage . (:manager))))
  (:documentation
   "Abstract Base Class for clews components - applications or plugins"))

(defmethod  has-permission :around(action (component component) &optional user)
  (or (call-next-method)
      (unless (typep user 'user)
        (let ((email-address (email-address user)))
          (and email-address (has-permission action component email-address))))))

(defgeneric user-component-properties(component user &key defaults)
  (:documentation "Return a property-subset in user properties for
given component creating it if necessary")
  (:method(component user &key defaults)
    (property-subset user (id component) :defaults defaults)))

(declaim (inline  user-preference))

(defun user-preference(key component user &optional default)
  "Return a user preference relating to component - checks user
   properties, then component properties, otherwise returns default"
  (property  (property user (id component)) key default))

(defgeneric user-component-preferences(component user)
  (:documentation "Method should return the user preferences list
 (suitable for make-form) for the given component or nil if none available"))

(defmethod user-component-preferences((component component) user)
  (declare (ignore user))
  nil)