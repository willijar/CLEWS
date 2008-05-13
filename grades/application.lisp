;;; $Id: application.lisp,v 1.1 2006/09/08 06:29:41 willijar Exp $
;;; Copyright (c) 2004 Dr John A.R. Williams <J.A.R.Williams@aston.ac.uk>.
;;; Main class and handlers for the publications-manager class
;;; part of the EWAS Application for Managing Student Projects

;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 2
;;; of the License, or (at your option) any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

(in-package :ewas.grades)

(defclass grades-manager (ewas-application)
  ((db :type source :initarg :source :reader db
       :documentation "Database source"))
  (:default-initargs
    :id :grades
    :properties '((:title . "Grades") )
    :acl '((:view . (:student :tutor :admin))
	   (:edit . (:tutor :admin)
	    (:admin . (:admin))))
    (:documentation "Class for grade collation and management system")))

(defmethod published-methods :around ((app grades-manager))
  `(,@(call-next-method)
      ("/student/" ,#'student-grades-handler :stage :response :match :prefix)
      ("/staff/" ,#'staff-marking-handler :stage :response :match :exact)
      ("/module/" ,#'module-handler :stage :response :match :prefix)
      ("/assessment/" ,#'assessment-handler :stage :response :match :prefix)
      ("/spreadsheet/"
       ,#'spreadsheet-handler :stage :response :match :prefix)))

(defmethod response-handler((app grades-manager) request rest)
  "Redirect top level depending on permissions")

(defun format-percentage(num)
  (format nil "~,1F%" num))

(defun format-timestamp(utime)
  (multiple-value-bind (se mi ho da mo ye dw dst)
      (decode-universal-time utime)
    (declare (ignore se))
    (let ((month-name (aref port:+month-names+ (1- mo)))
	  (week-day-name (aref port:+week-days+ dw)))
      (format nil "~A, ~2,'0d ~a ~4d ~2,'0d:~2,'0d:~2,'0d"
	      week-day-name da month-name ye ho mi se))))

(defun average-mark(marks &optional
		    (mark-key #'mark)
		    (weight-key
		     #'(lambda(m)
			 (if (mark m)
			     (let ((a (assessment m)))
			       (* (percentage a) (credits (module a))))
			     0))))
  (let ((mark-total 0)
	(weight-total 0))
    (dolist(mark marks)
      (let ((w (funcall weight-key mark)))
	(incf mark-total (* w (funcall mark-key mark)))
	(incf weight-total w)))
    (if (= 0 weight-total)
	0
	(/ mark-total weight-total))))

(defmethod student-grades-handler((app grades-manager) request rest)
  "Redirect top level depending on permissions"
  (let ((is-tutor-p (has-permission :tutor app)))
  (multiple-value-bind(student-username form)
      (if is-tutor-p
	  (let ((form `(form
			((mcq :style :dropdown :name :username)
		     ,@(sort
			(mapcar #'car (field-values app 'students '(username)))
			#'string>))
			((input :type :submit :value "Submit")))))
	    (values  (getf (form-data form request) :username)
		     (markup-form form request)))
	  (username *current-user*))
    `(html
      (head (title "Grades"))
      (body
       ((section :title "Grades"))
       ,@form
       ,(when student-username
	  (let* ((student (car (records (db app)
					'student 'username student-username)))
		 (marks
		  (sort (marks student) #'string<
			:key #'(lambda(mark) (moduleid (assessment mark))) ))
		 (module-marks
		  (sort (module-marks student) #'string< :key #'moduleid) )
		 (examboard-decisions (sort (examboard-decisions student)
					    #'< :key #'start)))
	  `((section
	     :title (format nil "~A ~A (~A)" (firstname student) (lastname student) (studentid student)))
	    ,(when (suspended student)
		`((p :class :error)
		   ,(if is-tutor-p
			"The marks for this student must NOT be
given out as the student is on the suspension list. The reason should
be given in the notes."
			"Your grades are unavailable until a
financial, disciplinary or suspension matter has been
resolved. Contact the programme director if you are unsure why this is
so.")))
	    ,(when (and is-tutor-p (notes student))
	       `((p :class :error) "Notes: " ,(notes student)))
	    (p "Please note that until any marks or grades shown here
are for indication only and may be subject to moderation either by
internal moderators or by the examination board.")
	    ((section :title "Assessments")
	     ((table :cellspacing 3)
	      (tr (th "Module") ((th :colspan 2) "Assessment")
		  (th "Type") (th "Weighting (%)") (th "Deadline")
		  (th "Release Date")
		  (th "Mark (%)"))
	      ,@(mapcar
		 #'(lambda(mark)
		     (let ((assessment (assessment mark)))
		       `(tr
			 (td ,(moduleid assessment))
			 (td ,(assessmentid mark))
			 (td ,(title assessment))
			 (td ,(type assessment))
			 (td ,(format-percentage (percentage assessment)))
			 (td ,(format-timestamp (deadline assessment)))
			 (td ,(format-timestamp (release-date assessment)))
			 (td ,(if (or is-tutor-p (feedback-available-p mark))
				  (format-percentage (mark mark)) "-")))
			 ,(when is-tutor-p
			    `(td ,(note mark))) )))
		 marks)
	      (tr ((td :colspan 6 :align :right) "Average to date")
		  (td (format-percentage (average-mark marks))) )))
	    ((section :title "Module Final Marks")
	     ((table :cellspacing 3)
	      (tr ((th :colspan 2) "Module") 
		  (th "Pass Mark (%)") (th "Mark (%)") (th "Credits"))
	      ,@(mapcar
		 #'(lambda(mark)
		     (let ((module (module mark)))
		       `(tr
			 (td ,(moduleid module))
			 (td ,(title module))
			 (td ,(pass-mark module))
			 (td ,(format-percentage (mark mark)))
			 (td  ,(if (>= (mark mark) (pass-mark module))
				   (credits module)
				   0)
				   "/"
				   ,(credits module))
			 ,(when is-tutor-p
			    `(td ,(note mark))))))
		 module-marks)
	      (tr ((td :colspan 4 :align :right) "Accumulated Credits")
		  (td (reduce #'+
			      (mapcan
			       #'(lambda(m)
				   (when (and (mark m)
					      (>= (mark m)
						  (pass-mark (module m))))
				     (list (credits (module m)))))
			       module-marks)))))
	     (p "Condoned or referred modules are awarded the pass
mark only. Repeat or referred exams must be taken at the first
available opportunity (usually the following year) and may take a
different format.")
	     (p "180 Credits are required for an MSc, 120 Credits for
a PgD and 60 for a PgC."))
	    ((section :title "Examboard Decisions")
	     ((table :cellspacing 3)
	      ,@(mapcar
		 #'(lambda(d)
		     (let ((e (examboard d)))
		       `(tr (td ,(start e))
			    ,@(if (or is-tutor-p (< (finish e) (get-universal-time)))
				  `(((td ,(action d))
				     (td ,(argument d))
				     (td ,(notes d))))
				  `((td "Not Available until "
					(format-timestamp (finish e))))))))
		 examboard-decisions))
	     (dl
	      (dt "Award")
	      (dd "indicates that the board has decided to recomment
the award of a qualification.")
	      (dt "Proceed")
	      (dd "means that students can proceed with their course
or project, but there may possibly be repeat or referred exams
required which will be indicated.")
	      (dt "Withdraw")
	      (dd "means that the examboard is requesting the student
withdraw from the program.")))))))))))
	       

(defmethod staff-marking-handler((app grades-manager) request rest)
  "Redirect top level depending on permissions")
  
(defmethod module-handler((app grades-manager) request rest)
  "Redirect top level depending on permissions")

(defmethod assessment-handler((app grades-manager) request rest)
  "Redirect top level depending on permissions")

(defmethod spreadsheet-handler((app grades-manager) request rest)
  "Redirect top level depending on permissions")