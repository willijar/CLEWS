;;;; $Id: configuration.lisp,v 1.18 2005/03/10 19:54:25 willijar Exp $
;;;; Module Specification Handling
;;;; Copyright (C) 2002-2006 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details

(in-package :clews.grades)

;; patch fullname so it looks up user source for staff.

(defmethod fullname((username string))
  (let ((student (first (student-records *db* 'username username))))
    (if student (fullname student)
        (let ((user (get-dictionary username aston::*user-source*)))
          (if user (display-name user) username)))))

(defmethod fullname-link((username string))
  (let ((student (first (student-records *db* 'username username))))
    (if student
        (fullname student)
        (let ((user (get-dictionary username aston::*user-source*)))
          (if (and user (has-role user :staff))
              (format nil " <xref linkend=~S/> " username)
              username)))))

