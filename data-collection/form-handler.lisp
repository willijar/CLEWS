;;;; Data collection Form handler base classes
;;;; Copyright (C) 2002-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL) 
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; $Id: form-handler.lisp,v 1.1 2005/03/10 19:56:27 willijar Exp $

(in-package :clews.data-collection)

(defclass form-handler(access-controlled-entity)
  ((storage :initarg :storage :reader storage
	    :documentation "Object used to store data submitted by the form")
   (title :initarg :title :type string :reader title
	  :documentation "Title to be used with this form")
   (description :initarg :description :type string :reader description
		:initform nil
		:documentation "Short string description of this form")
   (instructions :initarg :instructions :type string :reader instructions
		 :initform nil
		 :documentation "Instructions to users of this form")
   (analysis-fields :initarg :analysis-fields :initform
		    '()
		    :type list :reader analysis-fields
		    :documentation "a-ist of fieldnames and types for
 data which is to grouped for analysis purposes") )
  (:default-initargs 
   :acl '((:view . (:all))
	  (:analyse . (:admin))))
  (:documentation "Base class for all form handlers"))
  
(defgeneric get-form(form-handler request)
  (:documentation "return form object based on a dataform object,
a username and a request"))

(defgeneric get-data(form-handler request)
  (:documentation "On the basis of the request returns the default data"))

(defgeneric get-extra-data(form-handler request)
  (:documentation "return any extra calculated data to be stored with the
     form data"))

(defgeneric can-submit-data(form-handler request)
  (:documentation "returns true if given user is allowed to submit data"))
  
(defgeneric submit-form-data(data form-handler request)
  (:documentation "Stores data for a form handler - returns success"))

(defgeneric can-analyse-data(form-handler request)
  (:documentation "returns true if given user is allowed to analyse data"))

(defgeneric get-analysis-form(form-handler request)
  (:documentation
   "returns form element to create analysis query for theis form"))

(defgeneric get-analysis-data(form-handler query)
  (:documentation "Return data analysis form for this form"))

(defmethod get-data(form-handler request)
  (declare (ignore form-handler request))
  nil)

(defmethod get-data((form-handler form-handler) request)
  "By default returns the last submission in storage to this user"
  (let ((user (remote-user request)))
    (when user
      (first (last
	      (data-store-retrieve `(:username ,(username user))
				   (storage form-handler)))))))
  
(defmethod has-submitted((form-handler form-handler) username)
  (when (data-store-retrieve `(:username ,username) (storage form-handler))
    t))

(defmethod can-submit-data(form-handler request)
  (has-permission :view form-handler (remote-user request)))

(defmethod can-analyse-data(form-handler request)
  (has-permission :analyse form-handler (remote-user request)))

(defmethod submit-form-data(data (form-handler form-handler) request)
  (data-store-store
   (append data (get-extra-data form-handler request))
   (storage form-handler))
  t)

(defmethod get-extra-data((form-handler form-handler) request)
  `(:username ,(username (remote-user request))
    :role ,(string (first (inet.acl::roles (remote-user request))))
    :timestamp ,(get-universal-time)))
  
(defmethod get-analysis-form((form-handler form-handler) request)
  (declare (ignore request))
    (let ((data (data-store-retrieve nil (storage form-handler))))
      (make-form
       (cons '(:fmt :text "Output Format" :type (string)
	       :markup ((mcq :style :dropdown) "html" "tex" "pdf"))
	     (mapcar
	      #'(lambda(item)
		  (list
		   (car item)
		   :text (string (car item))
		   :type (cdr item)
		   :markup (cons '(mcq :style :dropdown)
				 (mapcar #'(lambda(item) (cons item item))
					 (remove-duplicates
					  (mapcar #'(lambda(record)
						      (getf record (car item))) data)
					  :test #'equal)))))
	      (analysis-fields form-handler))))))

(defmethod get-analysis-data((form-handler form-handler) query)
  (data-store-retrieve
   (when (and (analysis-fields form-handler) query)
     (mapcan #'(lambda(field) (list (first field) (getf query (first field))))
	   (analysis-fields form-handler)))
   (storage form-handler)))

(defclass static-form (form-handler)
  ((form
    :initarg :form :type (or form list)
    :documentation "The static form object to be used")
   (multiple-submit-p
    :type boolean :initarg :multiple-submit :initform nil
    :reader multiple-submit-p
    :documentation "If true user can submit more than once"))
  (:documentation "A simple static form of some type"))

(defmethod get-form((form-handler static-form) request)
  (declare (ignore request))
  (slot-value form-handler 'form))

(defmethod can-submit-data((form-handler static-form) request)
  (and (call-next-method form-handler request)
       (or (multiple-submit-p form-handler)
	   (not (has-submitted form-handler
			       (username (remote-user request)))))))

(defclass vote (form-handler)
  ((choices
    :initarg :choices :type list :initform '("Yes" "No")
    :reader choices
    :documentation "A list of choices for the user to pick"))
  (:documentation "A simple poll form handler"))

(defmethod get-form((form-handler vote) request)
  (declare (ignore request))
  (let ((idx -1))
    (make-instance
     'clews.form::form :persist nil
     :elements (list
		(apply #'make-instance
		       (append '(clews.form::form-element 'mcq)
			     (mapcar
			      #'(lambda(choice) (cons (incf idx) choice))
			      (choices form-handler))))))))

(defmethod can-submit-data((form-handler vote) request)
  (and (call-next-method form-handler request)
       (not (has-submitted form-handler
			   (username (remote-user request))))))