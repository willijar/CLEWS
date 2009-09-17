;;;; $Id: forms.lisp,v 1.1 2003/10/12 10:56:41 willijar Exp $
;;;; Copyright (C) 2003 Dr. John A.R. Williams, J.A.R.Williams@blueyonder.co.uk

;;;; This file is part of the Common Lisp Applications EWAS application

;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2 of the License, or
;;;; (at your option) any later version.

;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.

;;;; You should have received a copy of the GNU General Public License
;;;; along with this program; if not, write to the Free Software
;;;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA
;;;; or retreive a copy from http://www.gnu.org/licenses/gpl.txt

;;;; Applications processing forms

(in-package :aston-applications)

(defun get-forms(application)
  (declare (ignore application))
  '((:personal . #'personal-form)
    (:application . #'application-form)
    (:qualifications . #'qualifications-form)
    (:referees . #'referee-form)))

(defun get-form(form-name application &key which)
  (if which
      (cadr (member form-name
		    (if (eq which :next)
			(get-forms application)
		      (reverse (get-forms application)))
		    :key #'car))
    (assoc form-name (get-forms application))))
  
(defun address-form(prefix &key with-name)
  `((table :border 0)
    ,(mapcan
      #'(lambda(item)
	  (when item
	    `((tr ((th :align "right") ,(first item))
		  ((td :align "left")
		   ,(multiple-value-bind (tag attr content)
			(split-markup (third item))
		      (join-markup
		       tag
		       (nconc
			(list :name
			      (intern
			       (concatenate
				'string (string prefix) "-"
				(string (second item)))
			       :keyword))
			attr)
		       content)))))))
      `(,(when with-name
	  '("Name:" :name
	    ((input :size 40 :maxlength 60
		   :datatype (string :min-length 2 :max-length 60)))))
	("Address line 1:" :addr1 
	 ((input :size 40 :maxlength 60
		:datatype (string :min-length 2 :max-length 60))))
	 ("Address line 2:" :addr2
	  ((input :size 40 :maxlength 60
		 :datatype (string :max-length 60))))
	 ("City :" :city ((input :size 25)))
	 ("State/Province/Region :" :state ((input :size 25)))
	 ("Zip/Postcode:" :zip
	  ((input :size 15 :maxlength 20 :datatype (string :max-length 20))))
	 ("Country:" :country ((mcq :style :dropdown) ,+countries+))
	 ("Phone Number:" :phone ((input :size 15 :maxlength 20)))
	 ("Email Address:" :email ((input :size 25 :maxlength 40)))))))

(defun personal-form(application)
  (declare (ignore application))
  `((section :name :personal :title "Personal Details")
    ((table :border 0)
     (tr (th "Title") (th "First Name") (th "Initials") (th "Last Name"))
     (tr (td ((mcq :style :dropdown :name :title) "Mr" "Ms"))
	 (td ((input :size 10 :name :firstname
		     :datatype (string :min-length 2))))
	 (td ((input :size 10 :name :initials)))
	 (td ((input :size 10 :name :lastname
		     :datatype (string :min-length 2))))))
    ((table :border 0)
     (tr ((td :width "50%") (b "Nationality:")
	  ((mcq :style :dropdown :name :nationality) ,+countries+))
	 ((td :width "50%") (b "Date of Birth (yyyy/mm/dd):")
	  (input :size 10 :name :date-of-birth :datatype date))))
    (p)
    ((table :border 0)
     (tr ((td :width "50%")
	  ((section :title "Home Address")
	   ,(address-form :home)))
	 ((td :width "50%")
	  ((section :title "Correspondence Address")
	   (p "(if different from home address)")
	   ,(address-form :correspond)))))
    (p "Please indicate any disability or additional which we should be made aware off")
    (p
     ((maq :name :disability)
      (nil . "I have no additional needs requirement")
      ("Dyslexia / other specific learning difficulty ")
      ("Blind / Partially sighted")
      ("Deaf / Hearing impaired")
      ("Wheelchair user / mobility difficulties")
      ("Personal care requirements")
      ("Mental health difficulty")
      ("Disability or medical condition that cannot be seen e.g. epilepsy, diabetes ")
      ("Other"))
     (br)
     "If Other please state: "
     ((input :name :disability-other)))))

(defun application-form(application)
  (declare (ignore application))
  `((section :name :application :title "Application Details")
    (p "Which programme are you applying for, and for which start date" (br)
       ((mcq :style :dropdown :name :programme :datatype symbol)
	(:telecoms .  "MSc in Telecommunications Technology")
	(:photonics . "MRes in Photonics Network Systems")
	(:internet . "MSc in Internet technology"))
       ((mcq :style :dropdown  :name :start-date :datatype date)
	(3274128000 . "Oct 2003")
	(3274214400 . "Oct 2004")))
    (p "Where would you prefer to complete your project"
       ((mcq :name :project :datatype symbol)
	"Industry" "University")
       ". Please note that we cannot guarantee a placement in industry")
    (p "Where did you find out about this programme?"
       ((mcq :name :publicity :datatype string)
	"Prospectus" "World Wide Web" "Personal Contact" nil)
       ". If Other where "
       (input :name :publicity))
    ((section :title "Finance")
     (p "Note that only applicants ordinarily resident in Great Britiain are
eligible for Research Council funding of maintenance and fees, and only
applicants ordinarily resident in EU countries are eligible for research
council funding of fees. If you have another source of funding,
you will need to complete a financial guarantee form before you can be
registered on the course.")
     (p "How will you finance yourself at Aston?"
	(mcq (:fees . "Research Council - Fees Only")
	     (:full . "Research Council - Fees and Maintenance")
	     (:employer . "Employer")
	     (:self . "Self")
	     (:family . "Family")
	     (:official . "Overseas government or official body")
	     (:other . "Other"))
     (p "Is the funding guaranteed?"
	((mcq :name :funding-guaranteed :datatype boolean) ((nil . "No") (t . "yes"))))
     (p "If funding is already guaranteed please enter the full name and address of your fee-payer below."
	((section :title "Fee Payers Details")
	 (input :size 25 :name :fee-payer-name
		:datatype (string :min-length 2))
	 ,(address-form :fee-payer)))))))
	
(defun qualifications-form(application)
  (declare (ignore application))
  `((section :name :qualifications :title "Qualifications")
   ((section :title "Qualifying Degree")
    (p "If you hold a degree or equivalent please give details below. This should be the qualification most relevant for studying on this program.")
    (table
     (tr (th "Degree Subject")
	 (td ((input :name :degree-subject :datatype (string :word-count 1)))))
     (tr (th "Award")
	 (td ((mcq :style :dropdown :name :degree-award)
	      "BEng Hons" "BEng" "BSc Hons"
	      "BSc" "MEng" "MSc" "Other")))
     (tr (th "Degree Class, Score or GPA")
	 (td ((input :name :degree-class :datatype (string :word-count 1)))))
     (tr (th "Date Awarded (or expected) (YYYY/MM/DD)")
	 (td ((input :name :degree-date :size 10 :datatype date))))
     (tr (th "University")
	 (td ((input :name :university :datatype (string :word-count 1)))))
     (tr (th "Country")
	 (td ((mcq :style :dropdown :value "UK"
		   :name :country ,+countries+))))))
   ((section :title "Alternative Education")
    (p "If you do not hold a degree, please provide
comprehensive information about your technical education,
to include dates and places of attendance,
examinations passed and the marks or grades awarded to you :")
    ((textarea :name :alternative-education :cols 60 :rows 5
	       :type :structured-text)))
   ((section :title "Professional Experience and Qualifications")
    (p " Enter details of professional experience and qualifications, including dates where appropriate:")
    ((textarea :name :professional :cols 60 :rows 5
	       :type :structured-text)))
   ((section :title "English")
    (p "Please indicate you knowledge/training of english. If 
you studied English as a foreign language give the date and test score")
    (p ((mcq :style :dropdown :name :english :value "First Language")
	"First Language"  "TOEFL" "ELTS" "ELTS (Computer)" "GCSE")
       " Score: " ((input :name :english-date :size 5 :datatype number))
       " Test Date (YYYY/MM/DD]:  "
       ((input :name :english-score :size 5 :datatype date))))))

(defun referee-form(application)
  (declare (ignore application))
  `((section :name :referees :titles "Referees")
    (p "Please give the names and details of two referees who can
comment on your academic or technical abilities (e.g. tutor or head of
department where you did your first degree or manager where you gained
relevant experience). At least one of your referees should be from
the institution where you took your first degree.  Provide
institutional email addresses if at all possible as this will speed up
the processing of the application.")
    (table
     (tr (td ((section :title "Main Referee")
	      ((input :name :ref1-name :size 25
		      :datatype (string :word-count 1)))
	      ,(address-form :ref1)))
	 (td ((section :title "Second Referee")
	      ((input :name :ref2-name :size 25
		      :datatype (string :word-count 1)))
	      ,(address-form :ref2)))))))

(defconstant +status-checks+
'((:STARTED . "Application Started")
  (:LIT-REQ . "Literature Requested")
  (:LIT-SENT . "Literature Sent")
  (:RCVD . "Application Received/Completed")
  (:TRANS-REQ . "Transcripts Requested")
  (:TRANS-RCVD . "Transcripts Received")
  (:QUAL-CHECK . "Qualifications Check")
  (:REFS-REQ . "References Requested")
  (:REF1-RCVD . "Reference 1 Received")
  (:REF2-RCVD . "Reference 2 Received")
  (:REFS-CHECK . "References  Checked")
  (:INTERVIEW . "Interviewed")
  (:DECISION-MADE . "Decision Made")
  (:SSHIP-OFFERED . "Studentship Offered")
  (:FIN-RCVD . "Financial Guarantee Received")
  (:ENROLE-PACK . "Enrolement Pack Sent")
  (:ACCM-SENT . "Accomodation Form Sent")
  (:ACCM-RCVD . "Accomodation Form Received")
  (:ENROLED . "Enroled"))
  "A list of dated checkpoints for the application process")