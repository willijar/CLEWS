;; $Id: knowledge.lisp,v 1.1 2006/07/31 07:10:37 willijar Exp $
;; Methods pertaining to users knowledge
;; Copyright (C) 2003-2006 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of CLEWS: Assessment

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;;

;;; Code:

(in-package :clews.assessment)

(deftype knowledge ()
  "base knowledge is implemented as an alist"
  'list)

(declaim (inline started completed (setf started) (setf completed)))

(defun started(knowledge) (property knowledge :started))

(defun completed(knowledge) (property knowledge :completed))

(defun (setf started) (started knowledge)
  (setf (property knowledge :started) started))

(defun (setf completed) (completed knowledge)
  (setf (property knowledge :completed) completed))

(defun timetaken(knowledge)
  "Return time taken taken from given knowledge record (in secs)"
  (let ((completed (completed knowledge))
        (started (started knowledge)))
    (if started
        (if completed (- completed started)
            (- (get-universal-time) started))
        0)))

