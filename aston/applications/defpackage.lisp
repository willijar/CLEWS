;;;; $Id: defpackage.lisp,v 1.1 2003/10/12 10:56:23 willijar Exp $
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

(in-package :cl-user)

(defpackage :aston-applications
  (:documentation
   "Aston Postgraduate applications system")
  (:use :cl :ewas :dictionary :markup)
  (:import-from :ewas-form markup-form form-data date
		+countries+ +uk-countries+ +eu-countries+)
  (:import-from :ewas users authentication-cookie)
  (:import-from :jarw-lib split-string format-date)
  (:import-from :acl has-permission)
  (:import-from :httpd merge-url request-data request-header url-path
		request-send-error request-url request-user urlstring
		request request-cookie request-redirect cookie url-query-param)
  (:import-from :jarw-properties property)
  (:export aston-applications))

