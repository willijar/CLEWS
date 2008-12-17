;;;; CLEWS Form handling
;;;; Copyright (C) 2002-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; Generic API for form and form data handling
;;;; $Id: form.lisp,v 1.4 2007/07/16 07:28:24 willijar Exp willijar $

(in-package :clews.form)

(defgeneric elements(form)
  (:documentation "List of form elements which make up this form"))

(defgeneric markup-form(form &optional data disabled element-markup)
  (:documentation
   "Returns the markup representation of the given form,
optionally using either request data or saved data.
Form may be disabled (i.e. for display purposes only.
Element-wrapper, given element markup andf an error message, should output the combined markup."))

(defgeneric form-data(form request)
  (:documentation "Returns a p-list of the values from the form
corresponding to the form elements. The keys are the element names and
the values are the validated value or a condition if it was
invalid. Returns as a second value a condition if the form was
invalid"))

(defgeneric get-form-element(name form)
  (:documentation
   "Returns the (first) element object with given name in form"))

(defgeneric validate-element-data(element data)
  (:documentation "returns data and condition values.  If valid value
will be converted to internal format and condition will be nil.  If
invalid value will be original data string and c9ondition specify
reason data is invalid"))

(defgeneric actions(form)
  (:documentation "Return a list of possible names and values returned
by the forms submit buttons"))

(defgeneric form-attrs(form)
  (:documentation "the attrs for a form element"))

(defgeneric form-method(form)
  (:documentation "Return whether a form is using the :get or :post methods"))

(defgeneric submitted-action(form request)
  (:documentation  "Return submission name for submission action"))

;;; General methods

(defmethod get-form-element(name form)
  (find name (elements form) :test #'equal :key #'name))

(defmethod validate-element-data(element (data string))
  (handler-case (parse-input (datatype element) data)
    (invalid-input (condition)
      (values data condition))))

(defmethod form-data (form (request request))
  "Validates elements and constructs a list of names, values/errors"
  (multiple-value-bind (data condition)
      (form-data
       form
       (mapcan
        #'(lambda(element)
            (let ((name (name element)))
              (list name
                    (let ((data (form-values name request)))
                      (unless (cdr data) (setf data (car data)))
                      (when data
                        (handler-case
                            (parse-input (datatype element) data)
                          (invalid-input (condition) condition)))))))
        (elements form)))
    (values (append (when (name form) (list :form (name form))) data)
            condition)))

(defmethod form-data (form (data list))
  "Assumes data elements have already been validated - returns data and
   condition"
  (declare (ignore form))
  (values data (find-if #'(lambda(s) (typep s 'condition)) data)))

(defmethod form-data (form (data (eql nil)))
  (let ((defaults
            (mapcan
             #'(lambda(entry) (list (name entry) (default-value entry)))
             (elements form))))
    (when defaults (form-data form defaults))))

;;; methods for form based on a markup template
(defmethod elements((form list))
  (let ((elements '()))
    (process-markup-when #'is-form-element-p
                         form
                         #'(lambda(element) (push element elements)))
    elements))

(defmethod actions((form list))
  (let ((actions '()))
    (process-markup-when
     #'(lambda(element)
         (multiple-value-bind (tag attr) (split-markup element)
           (and (eq tag 'input)
                (equalp (string (getf attr :type)) "submit"))))
     form
     #'(lambda(element)
         (multiple-value-bind (tag attr) (split-markup element)
           (declare (ignore tag))
           (push  (cons (getf attr :name) (getf attr :value)) actions))))
    actions))

(defmethod form-attrs(form)
  (process-markup-when
   #'(lambda(element)
       (multiple-value-bind (tag attr) (split-markup element)
         (when (eq tag 'form) (return-from form-attrs attr))))
   form
   #'(lambda(e)(declare (ignore e)))))

(defmethod markup-form ((form list) &optional data (disabled nil)
                        (element-markup #'element-markup))
  (let ((data (if (typep data 'request) (form-data form data) data)))
    (first (substitute-markup-when
            #'is-form-element-p
            form
            #'(lambda(element)
                (let* ((value-in
                        (getf data (name element) (default-value element)))
                       (error-msg
                        (when (typep value-in 'condition)
                          (princ-to-string value-in)))
                       (value-out
                        (if (typep value-in 'condition)
                            (jarw.parse::value value-in)
                            value-in)))
                  (funcall element-markup element
                           value-out
                           disabled error-msg)))
            :as-list t))))

(defun make-form-record(prefix things &key (as-list t))
  "Adds prefix to the names of things"
  (substitute-markup-when
   #'is-form-element-p
   things
   #'(lambda(element)
       (multiple-value-bind (tag attrs content) (split-markup element)
         (join-markup
          tag
          (let ((attrs (copy-list attrs))
                (name (getf attrs :name)))
            (setf (getf attrs :name)
                  (intern (concatenate 'string (string prefix) name) :keyword))
            attrs)
          content)))
   :as-list as-list))

(defun get-form-record(prefix data)
  "Return data elements prefixed by prefix"
  (let ((prefix (string prefix))
        (len (length prefix)))
    (flet ((is-record-element-p(name)
             (let ((name (string name)))
               (and (> (length name) len)
                    (string-equal prefix name :end2 len)))))
      (loop for a on data by #'cddr
            when (is-record-element-p (car a))
            append (list
                    (intern (subseq (string (car a)) len) :keyword)
                    (cadr a))))))

;;; form object as a list of form elements consistently presented
(defclass form ()
  ((name :initarg :name :reader name
         :initform nil
         :documentation "Unique id for this form")
   (title :initarg :title :initform nil :reader title
          :documentation "Title for this form - useful to users")
   (elements :type list :initarg :elements :reader elements
             :documentation
             "List of form elements which make up this form")
   (form-attrs :initarg :form-attrs :reader form-attrs
               :initform '(:method "POST" :action ".")
               :documentation
               "The (default) form tag attributes to be used")
   (actions :initarg :actions :reader actions
            :initform '(("Submit" . "Submit"))
            :documentation
            "List of pairs of (default) names and values for submit buttons"))
  (:documentation "Class representing forms"))


(defmethod form-method(form)
  (let ((method (getf (form-attrs form) :method :get)))
    (if (stringp method) (intern method :keyword) method)))

(defun make-form(elements &key
                 (name nil)
                 (form-attrs '(:method "POST"))
                 (actions '(("submit" . "Submit"))))
  (apply #'make-instance
         (cons 'form
               (list
                :name name
                :form-attrs form-attrs
                :actions actions
                :elements
                (mapcar
                 #'(lambda(element)
                     (apply #'make-instance
                            (if (listp element)
                                `(form-element :name ,(first element)
                                  ,@(rest element))
                                `(form-element :name ,element))))
                 elements)))))

(defmethod markup-form((form form) &optional data (disabled nil)
                       (element-markup #'element-markup))
  (multiple-value-bind (data condition) (form-data form data)
    (let ((elements
           (cons
            'OL
            (mapcar
             #'(lambda(element)
                 (let* ((value-in (getf data (name element)
                                        (default-value element)))
                        (error-msg (when (typep value-in 'condition)
                                     (princ-to-string value-in)))
                        (value-out (if (typep value-in 'condition)
                                       (jarw.parse::value value-in)
                                       value-in)))
                   (cons 'li
                         (funcall element-markup element
                                  value-out disabled error-msg))))
             (elements form)))))
      (if disabled
          elements
          `((FORM ,@(form-attrs form))
            ,@(when condition
                    '(((P :class "error")
                       "Please correct the error(s) shown below")))
            ,elements
            ((P :align "right")
             ,@(mapcar #'(lambda(action)
                           `((input :type "submit"
                              :name ,(car action)
                              :value ,(cdr action))))
                       (actions form))))))))

(defmethod submitted-action(form request)
  "Return submission name for submission action"
  (when request
    (some
     #'(lambda(action)
         (equal (car (form-values (car action) request)) (cdr action)))
     (actions form))))

;;; registration system for forms - a global form namespace
(defvar *forms* (make-hash-table :test #'equal))

(defun register-form(name rest)
  (setf (gethash name *forms*) rest))

(defun defform(name &rest rest)
  "Make a persistant form"
  (let ((form (apply #'make-form (nconc rest (list :name name)))))
    (setf (gethash name *forms*) form)))

(defgeneric find-form(form-name)
  (:documentation "Look up a form in the registered forms")
  (:method (form-type) (gethash form-type *forms*))
  (:method ((form-data list)) (gethash (getf form-data :form) *forms*)))

;; string form types
(defmethod elements((form string)) (elements (find-form form)))
(defmethod actions((form string)) (actions (find-form form)))
(defmethod markup-form((form string) &optional data (disabled nil)
                       (element-markup #'element-markup))
  (markup-form (find-form form) data disabled element-markup))
(defmethod markup-form((form (eql nil)) &optional data (disabled nil)
                       (element-markup #'element-markup))
  (markup-form (find-form data) data disabled element-markup))
(defmethod form-method((form string)) (form-method (find-form form)))
(defmethod submitted-action((form string) request)
  (submitted-action (find-form form) request))
(defmethod form-data ((form string) data)
  (form-data (find-form form) data))

;;helper function for using forms
(defun do-form(data &optional form disabled-p updatefunc request)
  "Return form markup. Data can be the actual data or a function to
return it.  If edit-p is false present only view. update func is
called with validated data return from form. request is http request
object"
  (labels ((data()
             (if (functionp data) (funcall data) data))
           (form() (typecase form
                     (null (find-form (data)))
                     (function (funcall form))
                     (atom (find-form form))
                     (t form))))
    (let ((form (form)))
      (when form
        (if (and (not disabled-p) (submitted-action form request))
            (multiple-value-bind(form-data condition)
                (form-data form request)
              (unless condition (funcall updatefunc form-data))
              `(div
                ,(when condition
                       `((p :class :error)
                         "Form Submission Not Accepted. Please Correct."
                         ,condition))
                ,(markup-form (form) (if condition request (data))
                              (if (not condition) :text))))
            (markup-form form (data) disabled-p))))))

(defun do-form-with-confirmation
    (&key data form handler request
     (on-success "Form submission Successful.")
     (on-failure "For submission not accepted due to an error in
     your input. Please correct and try again."))
  "Return markup handling a single form markupelement , with a confirmation
message if submitted data successfully accepted.

Keyword arguments:
data - the initial data property list or a function of no arguments
       which returns it.
form - the form designator.
       Can be either the form itself, a function of no arguments to return it,
       a name of a registered form or if nil the registered form for the data.
       May be called more than once.
handler - function called to process the data submitted from the form.
       It should return the accepted data for display.
request - the http request object
on-success - confirmation message designator, a string, markup sexp
       or function which takes the data as an argument.
       Called only if handler is successful.
on-failure - message designator called if data submission no successful."
  (let* ((data
          (typecase data
            (function (funcall data))
            (t data)))
         (form
          (typecase form
            (null (find-form data))
            (function (funcall form))
            (atom (find-form form))
            (t form))))
      (when form
        (if (submitted-action form request)
            (multiple-value-bind(data condition)
                (form-data form request)
              (unless condition
                (handler-case
                    (setf data (funcall handler data))
                  (error(c) (setf condition c))))
              (let ((msg (if condition on-failure on-success)))
                (if (functionp msg)
                    (funcall msg data)
                    `(div
                      ,(if (stringp msg) `(p ,msg) msg)
                      (hr)
                      ,(when condition
                             `((p :class :error) ,condition))
                      (markup-form form data (not condition))))))
            (markup-form form data)))))
