;;;; CLEWS: data collection application
;;;; Copyright (C) 2002-2005 Dr. John A.R. Williams
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; $Id: data-collection-application.lisp,v 1.1 2006/12/03 10:50:50 willijar Exp willijar $

(in-package :clews.data-collection)

(defclass data-collection-application(application)
  ((forms :type list :initarg :forms :initform '() :accessor forms
     :documentation "a-list of form handlers objects"))
  (:documentation "Form Data Collection application"))

(defmethod add-form-handler(id form-handler app)
  (setf (forms app)
	(cons (cons id form-handler)
	      (delete id (forms app) :key #'car :test #'equal))))

(defmethod response-handler((app data-collection-application) request rest)
  (let* ((parts (split-string (first (split-string rest 2 #(#\?))) :count 3
			      :delimiter '(#\\ #\/ #\.)))
	 (form (cdr (assoc (first parts) (forms app) :test #'equal)))
	 (action (second parts))
	 (fmt (third parts)))
  (if form
      (if (string-equal action "analysis")
	  (return-from response-handler
	    (analysis-handler app request form))
	  (let ((markup (form-response-handler app request form)))
	    (cond ((string-equal fmt "tex")
		   (make-instance 'response
				  :status 200
				  :content-type "text/x-tex"
				  :content (with-output-to-string(os)
					     (latex os markup))))
		  ((string-equal fmt "pdf")
		   (make-instance 'response
				  :status 200
				  :content-type "application/pdf"
				  :content (with-output-to-string(os)
					     (pdf os markup))))
		  (t markup))))
     (home-handler app request rest))))

(defmethod home-handler((app data-collection-application) request rest)
  "Provide a directory of forms"
  (declare (ignore  rest))
  (let ((user (remote-user request)))
    `(html
      (head (title "Data Collection Directory for " ,(username user)))
      (body
       (h1 "Directory of Forms to complete for " ,(username user))
       (ul
	,@(or
	  (mapcan
	   #'(lambda(entry)
	       (let ((id (car entry)) (form-handler (cdr entry)))
		 (when (can-submit-data form-handler request)
		   (list
		    `(li ((a :href ,id) ,(title form-handler))
			 (br) ,(description form-handler))) )))
	   (forms app))
	  '((li "No forms available at this time"))))
       (h1 "Directory of Forms for analysis for " ,(username user))
       (ul
	,@(or
	  (mapcan
	   #'(lambda(entry)
	       (let ((id (car entry)) (form-handler (cdr entry)))
		 (when (can-analyse-data form-handler request)
		   (list
		    `(li ((a :href ,(concatenate 'string id "/analysis"))
			  ,(title form-handler))
			 (br) ,(description form-handler))) )))
	   (forms app))
	  '((li "No forms available for analysis at this time")))) ))))

(defmethod form-response-handler((app data-collection-application)
				 request form-handler)
  (unless (can-submit-data form-handler request)
    (return-from form-response-handler
      (cons :not-found "Form not available.")))
  (let ((form (get-form form-handler request)) )
    `(html
      (head (title ,(title form-handler)))
      (body
       (h1 ,(title form-handler))
       ,@(multiple-value-bind (data condition) (form-data form request)
	   (if (and (submitted-action form request) (not condition))
	       (progn
		 (submit-form-data data form-handler request)
		 (list '(P "Your submission as follows has been successfully received. Thank you. "
			   ((a :href ".") "Back to list of available forms"))
		       (markup-form form data t)))
	     (list
	      (when condition
		`((P :class "error") "Submission unsuccessful." ,condition))
	      (list 'P (description form-handler))
	      (instructions form-handler)
	      (list 'OL
		    (markup-form
		     form
		     (if (submitted-action form request)
			 data (get-data form-handler request)))))))))))

(defmethod analysis-handler((app data-collection-application)
			    request form-handler)
  (unless (can-analyse-data form-handler request)
    (return-from analysis-handler
      (cons :not-found "Form not available.")))
  (let* ((query (get-analysis-form form-handler request))
	 (fmt (car (form-values  "FMT" request)))
	 (analysis-markup
	  (form-analysis-markup
	   (get-form form-handler request)
	   (get-analysis-data
	    form-handler
	    (when (and query (form-values "submit"  request))
		       (form-data query request))))))
    (cond ((string-equal fmt "tex")
	   (make-instance
	    'response
	    :status 200
	    :content-type "text/x-tex"
	    :content (with-output-to-string(os)
		       (latex
			os
			`((section :title ,(format nil "Analysis for ~A"
						   (title form-handler)))
			  ,analysis-markup)))))
	  ((string-equal fmt "pdf")
	   (make-instance
	    'response
	    :status 200
	    :content-type "application/pdf"
	    :content (with-output-to-string(os)
		       (pdf os `(document (title ,(format nil "Analysis for ~A"
						   (title form-handler)))
			  (body ,analysis-markup))))))
	  (t
	   `(html
	     (head (title ,(title form-handler)))
	     (body
	      (h1  "Analysis for " ,(title form-handler))
	      ,@(when query
		  `((p "Select the fields of the data subset below to be analysed")
			,(markup-form
			  query (when (form-values "submit" request ) request)
			  nil)
			(hr)))
	      , analysis-markup) )))))

(defmethod load-forms(path)
  (mapc #'(lambda(fname)
	    (write-log :load-form "~%Loading form file~% ~S~%"
		       fname)
	    (load fname :verbose nil))
	(directory (merge-pathnames path "*.lisp"))))
