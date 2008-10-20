;;; $Id: project.lisp,v 1.2 2005/03/10 20:14:01 willijar Exp $
;;; Copyright (c) 2004 Dr John A.R. Williams <J.A.R.Williams@aston.ac.uk>.
;;; Student project and assessment handling
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

(in-package :clews.projects)

(defstruct assessment
  (form nil) ; form id
  (weight 1 :type (real 0 1)) ; relative weighting
  (assessor nil) ; who is to do this assessment
  (allocated 0 :type unsigned-byte)  ; when allocated
  (submissions nil :type list)) ; list of submissions in reverse order

(defmethod assessment-submission((a assessment))
  (first (assessment-submissions a)))

(defclass project()
  ((project-id :reader project-id :initarg :id
	       :documentation "Unique id for the project")
   (description :accessor description
		:initarg :description
		:documentation "Description data for this project")
   (student :accessor student
	    :initform nil
	    :documentation "The student taking this project")
   (supervisors :accessor supervisors
		:initform nil
		:initarg :supervisors
		:documentation "List of supervisors for this project")
   (start-date :accessor start-date
	       :initform nil
	       :documentation "Universal time when project started")
   (deadline-date :accessor deadline-date
	     :initform nil
	     :documentation "Universal time for deadline")
   (submission-date :accessor submission-date
	      :initform nil
	      :documentation "Universal time when submitted")
   (locked-p :accessor locked-p :initform nil
	     :Documentation "When true project description and assessments cannot be changed")
   (notes :accessor notes
	  :initform ""
	  :documentation "Notes on this project")
   (changes :accessor changes
	    :initform nil
	    :documentation "List of pairs of userid and time when
changes are made")
   (assessments :accessor assessments
		:initform nil
		:documentation "List of assessment data for this project")))

(defmethod print-object((p project) stream)
  (print-unreadable-object (p stream :type t :identity t)
      (princ (project-id p) stream)))

(defmethod title((p project))
  (getf (description p) :title))

(defun project<(a b)
  "Project comparison function ordering by status"
  (cond ((not (student a)))
	((not (student b)) nil)
	((not (start-date a)))
	((not (start-date b)) nil)
	((< (start-date a) (start-date b)))
	((not (submission-date a)))
	((not (submission-date b)) nil)
	((< (submission-date a) (submission-date b)))
	((not (deadline-date a)))
	((not (deadline-date b)))
	((< (deadline-date a) (deadline-date b)))))
	 
(defmethod time-spent((p project))
  (if (start-date p)
      (- (or (submission-date p) (get-universal-time)) (start-date p))))

(let ((status-fields
       '(student supervisors start-date deadline-date
	 submission-date locked-p notes)))
  (defmethod status-data((p project))
    (mapcan #'(lambda(field)
		(when (slot-value p field)
		  (list  (intern (string field) :keyword)
			 (slot-value p field))))
	    status-fields))
  (defmethod (setf status-data)(value (p project))
    (dolist(field status-fields)
      (let ((v (getf value (intern (string field) :keyword) :missing)))
	(unless (eq v :missing)
	  (setf (slot-value p field) v))))))

(defun status-string(p)
  (cond
    ((not (student p)) "Unallocated")
    ((submission-date p)
     (format nil "Submitted ~A" (format-time nil (submission-date p)
					     :fmt :date-only)))
    ((and (start-date p)
	  (> (start-date p) (get-universal-time)))
     (format nil "Started ~A ~:[~;due ~A~]"
	     (format-time nil (start-date p):fmt :date-only)
	     (deadline-date p)
	     (format-time nil (deadline-date p):fmt :date-only)))))

(defmethod assessment(id (p project))
  (find id (assessments p) :key #'assessment-form))

(defmethod (setf assessment)(data id (p project))
  (setf (cdr (assoc id (assessments p))) data))

(defmethod project-mark((p project))
  (let ((sum 0)
	(weights 0))
    (dolist(a (assessments p))      
      (let ((w (assessment-weight a)))
	(incf sum (* w (mark-form-data (assessment-submission a))))
	(incf weights w)))
    (when (> weights 0) (/ sum weights))))
