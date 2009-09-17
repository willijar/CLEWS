;;;; $Id: applications.lisp,v 1.1 2003/10/12 10:55:45 willijar Exp $
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

;;;; Aston Applications Application Implementation

(in-package :aston-applications)

(defclass aston-applications (ewas-application)
  ((applications :type dictionary
		 :reader applications
		 :initarg :applications
		 :documentation "Applications indexed by application id")
   (lastid :type integer
	   :accessor last-id
	   :initform 0
	   :documentation
	   "The Last id allocated - next application will be 1+ this") )
  (:default-initargs :id :aston-applications
		     :acl '((:view . (:all))
			    (:apply . (:applicant :admin))
			    (:referee . (:admin :referee))
			    (:admin . (:admin)))
		     :default-properties
		     '((:title . "Applications")		       
		       (:signature . "Applications")))
  (:documentation "Postgraduate Application Class"))

(defmethod initialize-instance :after ((app aston-applications) &key)
  "Install the server handlers for the Applications application"
  (setf (last-id app)
	(reduce #'max
		(or (mapcar #'parse-integer
			    (dictionary-keys (applications app))) (list 0 0))))
  ;top level is registration
  (add-application-handler
   app "/register" #'registration-handler :stage :response :match :exact)
  (add-application-handler
   app "/register" nil :stage :authentication :match :exact)
  (add-application-handler
   app "/register" nil :stage :authorization :match :exact)
  (add-application-handler
   app "/admin/" #'admin-handler :stage :response :match :prefix)
  (add-application-handler
   app "/referee/" #'referee-handler :stage :response :match :prefix))

(defmethod application((app aston-applications) id)
  (get-dictionary id (applications app)))

(defmethod application((app aston-applications) (id integer))
  (get-dictionary (format nil "~7,'0D" id) (applications app)))

(defmethod (setf application) (new-value (app aston-applications) id)
  (setf (get-dictionary id (applications app)) new-value))

(defmethod (setf application) (new-value (app aston-applications) (id integer))
  (setf (get-dictionary (format nil "~7,'0D" id)
			(applications app)) new-value))
	 
(defun new-application(app &optional username)
  "Adds a new application and returns its id"
  (let ((id (format nil "~7,'0D" (incf (last-id app)))))
    (setf (get-dictionary id (applications app))
	  `((:id . ,id) (:username . ,username)))
    id))

(defun send-mail(app to subject body )
  "Send message and possibly a new password to given user
if they have an email address - returns confirmational markup"
  (let ((to (if (stringp to) to (property to :email))))
    (when to
      (format t "Email~%From:~A~%To: ~A~%Subject: ~A~%~%~A~%-- ~%~A"
	      (property app :email)
	      to subject body
	      (property app :signature))
      #+nil(email::send-mail (property app :email) to subject body
			     :signature (property app :signature)))))

(defmethod title(app) (property app :title))

(defun get-application-set(app &key
			       (reverse 't)
			       start
			       (count 40)
			       (filter #'(lambda(app) (declare (ignore app)) 't)) )
  "Return a set of count applications for which filter is true
starting at start. If reverse it 't then go from most recent first"
  (let ((keys (sort
	       (if start
		   (delete-if
		    (if reverse
			#'(lambda(item) (string< item start))
		      #'(lambda(item) (string> item start)))
		    (dictionary-keys (applications app)))
		 (dictionary-keys (applications app)))
	       (if reverse #'string< #'string>) )))
    (loop for key in keys
	  for application = (application app key)
	  when (funcall filter application) collect application into results
	  when (>= (length results) count) return results
	  finally (return results))))
	  