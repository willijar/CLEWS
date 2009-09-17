;;;; $Id: applications.lisp,v 1.1 2003/03/22 13:31:55 willijar Exp $

(in-package :aston)

(eval-when (:compile-toplevel :load-toplevel)
  (let ((user-source (make-simple-user-source
                      (translate-logical-pathname "www:applicants;"))))
    (defvar *applications*
      (make-instance
       'aston-applications:aston-applications
       :user-dictionary user-source
       :pwd-source (make-simple-pwd-source user-source)
       :applications (make-filesystem-dictionary
                      (translate-logical-pathname "www:applications;")
                      :lazy-writes-p nil :cache-lifetime 3600)
       :properties '((:title . "Postgraduate Applications")
                     (:email . "eepg@aston.ac.uk")
                     (:signature . "Postgraduate Applications (Masters Programmes)
School of Engineering and Applied Science
Aston University"))))))

(defmethod render-page ((app (eql *applications*)) stream markup)
  (html stream (apply-aston-style markup)))

#+nil(defun time-field(item)
  (when item
    (date-utilities:parse-time   (subseq item 0 (min (length item) 19))
				 :default-seconds 0
				 :default-minutes 0
				 :default-hours 1
				 :default-day 1
				 :default-month 1 )))

#+nil(defun boolean-field(item)
  (when (string-equal item "t") t))

#+nil(defun do-address-field(item application)
  (multiple-value-bind (property-name prefix)
      (values-list (rest (assoc (second item)
				'(("home" :personal :home)
				  ("correspond" :personal :correspond)
				  ("ref2" :referees :ref2)
				  ("ref1" :referees :ref1)
				  ("fees" :application :fee-payer))
				:test #'string-equal)))
    (let ((p (property application property-name)))
      (loop for a in '(:name :addr1 :addr2 :city :state
		       :zip :country :phone :fax :email
		       :correspond :modified)
	    for b in (cddr item)
	    do (setf
		(getf p (intern (concatenate 'string (string prefix)
					     "-" (string a)) :keyword))
		b))
      (setf (property application property-name) p))))

#+nil(let* ((db (db-connect "applications"))
       (store (make-filesystem-dictionary
	       (translate-logical-pathname "www:applications;")
	       :lazy-writes-p nil)))
       (dolist (data-in (clsql::query "select * from application;"
				      :database db))
	 (let ((application (list nil))
	       (id (first data-in)))
	   (flet ((process-entry(entry value)
		   (when value
		    (let* ((property (second entry))
			   (subproperty (third entry))
			   (func (fourth entry))
			   (value (if (and value func)
				      (funcall func value) value)))
		      (when property
			(setf (property application property)
			      (if subproperty
				  (let ((p (property application property)))
				    (setf (getf p subproperty) value)
				    p)
				value)))))))
	     (loop for value in data-in ;; do main application table
       for entry in
       `((appl-id :id nil
		  ,(lambda(item) (format nil "~7,'0D" (parse-integer item))))
	 (username :username)
	 (created :status :started ,#'time-field)
	 (programme :application :programme
		    ,(lambda(item)
			(cdr
			 (assoc
			  item
			  '(("MSc in Telecommunications Technology" . :telecoms)
			    ("MSc in Internet Technology" . :internet)
			    ("MRes in Photonics Networks" . :photonics))
			  :test #'string-equal))))
	 (funding :application :funding
		  ,(lambda(item)
		      (car
		       (assoc
			item
			'(("Research Council - Fees Only" . :fees)
			  ("Research Council - Fees and Maintenance" . :full)
			  ("Employer" . :employer)
			  ("Self" . :self)
			  ("Family" . :family)
			  ("Overseas government or official body" . :official)
			  ("Other" . :other))
			:test #'string-equal))))
	 (funding_guaranteed :application :funding-guaranteed
			     ,#'(lambda(f) (when f t)))
	 (project :application :project)
	 (publicity :application :publicity)
	 (lit_req :status :lit-req ,#'time-field)
	 (lit_sent :status :lit-sent ,#'time-field)
	 (rcvd :status :rcvd ,#'time-field)
	 (ack :status :acknowledged ,#'time-field)
	 (decision-made :status :decision-made ,#'time-field)
	 (interview :status :interview ,#'time-field)
	 (refs_req :status :refs-req ,#'time-field)
	 (refs_rcvd :status :refs-check ,#'time-field)
	 (accm_form_sent :status :accm-sent ,#'time-field)
	 (accm_form_rcvd :status :accm-rcvd ,#'time-field)
	 (basis_of_entry :status :decision-basis)
	 (qualification_check :status :qual-check ,#'time-field)
	 (reject :status :accept ,#'not)
	 (notes :status :notes)
	 (sun nil :sun)
	 (sship_form_sent :status  ,#'time-field)
	 (sship_form_rcvd :status  ,#'time-field)
	 (start_date :application :start-date
		     ,#'(lambda(value)
			  (time-field
			   (concatenate 'string
					value
					"/10/01 00:00") ))))
       do (process-entry entry value))
 ;; do personal table
 (loop for value in
       (first (clsql::query
	(format nil "select * from personal where appl_id=~A;" id)
	:database db))
       for entry in
       `((appl_id nil)
	 (title :personal :title)
	 (firstname :personal :firstname)
	 (initials :personal :initials)
	 (lastname :personal :lastname)
	 (photo nil)
	 (nationality :personal :nationality)
	 (married :personal :married ,#'boolean-field)
	 (female :personal :female ,#'boolean-field)
	 (dateofbirth :personal :date-of-birth ,#'time-field)
	 (cv_url :personal :cv-url))
       do (process-entry entry value))
 (loop for value in
       (first (clsql::query
	(format nil "select * from qualifications where appl_id=~A;" id)
	:database db))
       for entry in
       `((appl_id nil)
	 (subject :qualifications :degree-subject)
	 (date :qualifications :degree-date ,#'time-field)
	 (award :qualifications :degree-award)
	 (class :qualifications :degree-class)
	 (university :qualifications :university)
	 (country :qualifications :country)
	 (alternative :qualifications :alternative-education)
	 (professional :qualifications :professional)
	 (english :qualifications :english)
	 (english_score :qualifications :english-score)
	 (english_date :qualifications :english-date ,#'time-field))
       do (process-entry entry value))
 (dolist (address (clsql::query
		   (format nil "select * from addresses where appl_id=~A;" id)
		   :database db))
   (do-address-field address application))
 (setf (get-dictionary (property application :id) store)
       application)))))
