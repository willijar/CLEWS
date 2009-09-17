;;;; $Id: admin.lisp,v 1.1 2003/10/12 10:53:16 willijar Exp $
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

;;;; Base Adaptive Tutor class - implementation

(in-package :aston-applications)

(defmethod admin-handler((app aston-applications) request rest)
  (let ((user (request-user request))
	(path (first (split-string rest 1 '(#\?)))))
    (unless (has-permission user :admin app)
      (return-from admin-handler
	(request-send-error
	 request 403 "You are forbidden to administrate applications.")))
    `(html
      (head (title ,(title app) " Admininstration"))
      (body
       ,(if (> (length path) 0)
	     (admin-status-page app request path)
	  (admin-summary-page app request) )))))

(defun admin-summary-page(app request)
  (let* ((url (request-url request))
	 (reverse (first (url-query-param url "reverse")))
	 (start (first (url-query-param url "start")))
	 (count (if (url-query-param url "start")
		    (parse-integer (first (url-query-param url "start")))
		  40))
	 #+nil(filter-param (first (url-query-param url "filter")))
	 (filter-func #'(lambda(application)
			  (declare (ignore application))
			  't)))
    `((section
	 :title ,(concatenate 'string (title app) " Summary of Applications "))
      ((table :border 1)
	(tr (th "Application") (th "Name") (th "Current Status") (th "Date"))
	,@(mapcar
	   #'(lambda(application)
	       (multiple-value-bind(description date)
		   (application-final-status application)
		 `(tr (td ((a :href ,(property application :id ))
			    ,(property application :id )))
		      (td ,(applicant-name application))
		      (td ,description)
		      (td ,(if date (format-date nil date :fmt :short) "-")))))
	   (get-application-set app
				:reverse reverse
				:start start
				:count count
				:filter filter-func))))))

(defun admin-status-page(app request id)
  (let* ((application (application app id))
	 (status (property application :status)))
    (when (request-data request "submit")
      (dolist (item +status-checks+)
	(let ((key (car item)))
	  (if (getf status key)
	      (unless (request-data request key)(remf status key))
	    (when (request-data request key)
	      (setf (getf status key) (get-universal-time))))))
      (dolist (key '(:notes :decision-basis))
	(let ((value (request-data request key)))
	  (if (> (length value) 0)
	      (setf (getf status key) value)
	    (remf status key))))
      (let ((decision (request-data request :decision)))
	(unless (getf status :decision-made)
	  (setf (getf status :decision-made) (get-universal-time)))
	(cond ((string= decision "Accept")
	       (setf (getf status :accept) t))
	      ((string= decision "Reject")
	       (setf (getf status :accept) nil))
	      (t (remf status :decision-made)
		 (remf status :accept))))
      (setf (property application :status) status)
      (setf (get-dictionary id (applications app)) application))
    `((section
       :title ,(concatenate 'string 
			    "Application " id
			    " " (applicant-name application)))
      ((a :href ,(concatenate 'string "..//" id)) "View Application")
      ((form :method "POST" :action ,(urlstring (request-url request)))
      ((table)
       (tr (th "") (th "Item") (th "Checked"))
       ,@(mapcar #'(lambda(key)
	   (let ((date (getf status (car key))))
	  `(tr (td ((input :type "checkbox" :name ,(car key)
			   ,@(when date (list :checked t)))))
	       (td ,(cdr key))
	       (td ,(when date (format-date nil date :fmt :short))))))
		 +status-checks+)
       (tr  ((td :colspan 3)
	     ((section :title "Decision ")
	      ((mcq :name :decision :style :dropdown
		    :value ,(if (getf status :accept) "Accept"
			      (if (getf status :decision-made) "Reject"
				"None"))
		    :disabled ,(getf status :decision-made))
			      "Accept" "Reject" "None")
	      (p "Basis of Decision (seen by the applicant)")
	      ((textarea :name :decision-basis :cols 40 :rows 5
			 :value ,(getf status :decision-basis))))))
       (tr  ((td :colspan 3)
	     ((section :title "Notes (NOT seen by the applicant)")
	      ((textarea :name :notes :cols 40 :rows 5
			 :value ,(getf status :notes))))))
       (tr ((td :colspan 3 :align "right")
	    ((input :type "submit" :name "submit" :value "Update Status")))))
      ))))
