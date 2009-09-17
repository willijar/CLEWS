;;;; $Id: referee.lisp,v 1.1 2003/10/12 10:56:57 willijar Exp $
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

(in-package :aston-applications)

(defmethod referee-handler((app aston-applications) request)
  (let* ((user (request-user request))
	 (application (get-dictionary (property user :app-id)
				      (applications app))))
    (unless (has-permission user :referee app)
      (return-from referee-handler
	(request-send-error
	 request 403 "You are forbidden to referee applications.")))
    `(html
      (head (title ,(title app) " Referee"))
      (body
       ((section
	 :title ,(concatenate 'string 
			      " Reference for " (applicant-name application)))
	(p "Hello there")  )))))