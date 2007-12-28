;;;; Additional authentication support for CLEWS
;;;; Copyright (C) 2002-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; $Id: authentication.lisp,v 1.1 2006/07/19 10:29:17 willijar Exp $

(in-package :clews)

(defmethod stored-credentials(username (authenticator dictionary))
  (get-dictionary username authenticator))

(defmethod (setf stored-credentials)(value username
				     (authenticator dictionary))
  (setf (get-dictionary username authenticator) value))

(defclass password-file-authenticator(encrypted-authenticator
				      dictionary:userfile-dictionary
				      access-controlled-entity)
  ()
  (:documentation "Authenticator using passwords stored in a file
using crypt"))