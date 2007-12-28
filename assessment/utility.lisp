;; $Id: utility.lisp,v 1.1 2006/07/31 07:14:21 willijar Exp willijar $
;; Utilities for assessments
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

;;; date handling

(deftype datespec ()
  "Date specifications are either null or integers"
  '(or null integer))

;; date helper functions

(defun datespec(date)
  "Dates may be specified as a string or a universal time but are converted
   to a universal time"
  (ctypecase date
    (integer date)
    (string (parse-time date))
    (null nil)))

(declaim (inline datestring))

(defun datestring(ut) (when ut (format-time nil ut :fmt :short)))

(defun short-datestring(utime)
  "Decode the universal time UTIME and return an RFC-822-format string
  using UT rather than the local timezone"
  (when utime
    (multiple-value-bind (se mi ho da mo)
        (decode-universal-time utime 0)
      (declare (fixnum mi ho da mo) (ignore se))
      (format nil "~2,'0d ~a ~2,'0d:~2,'0d"
              da (aref port:+month-names+ (1- mo)) ho mi))))

(define-condition assessment-condition(condition)
  ((reason :reader reason
           :initarg :reason
           :initform nil))
  (:report  (lambda (condition stream)
              (let ((reason (reason condition)))
              (cond
                ((stringp reason) (write-string reason stream))
                ((rest reason) (apply #'format (cons stream reason))))))))

;; generic condition for invalid access to an assessment
(define-condition invalid-assessment-access(assessment-condition)
  ())

(define-condition invalid-assessment-definition(assessment-condition)
  ())

(defvar *check-assessment-access* t
  "If true an error will be thrown if the access rules are broken.")

(defun access-assert-func(predicate knowledge assessment)
  (multiple-value-bind (check-p reason)
      (funcall predicate knowledge assessment)
    (unless check-p
      (signal 'invalid-assessment-access :reason reason))))

(defmacro access-assert(predicate knowledge assessment)
  `(when *check-assessment-access*
    (access-assert-func ,predicate ,knowledge ,assessment)))

(defmacro opt(keyword value)
  (let ((v (gensym)))
    `(let ((,v ,value))
      (when ,v
        (list ,keyword ,v)))))

(defun assessment-status-table(knowledge assessment)
  `((table :class "assessment-status")
    ,@(mapcar
       #'(lambda(item)
           (when (cdr item)
             `(tr ((th :align "right") ,(car item))
               (td ,(cdr item)))))
       (assessment-status-long knowledge assessment))))

(defun assessment-student-status(knowledge assessment &optional (path ""))
  "List of markup describing the  this assessment"
  `(,(assessment-status-table knowledge assessment)
    (table
     ,(when (assessment-attempt-p knowledge assessment)
            `(tr (td ((a :href ,(strcat path "assessment")) "Start assessment."))
              (td ,@(when-bind (reason (assessment-should-not-attempt-reason
                                        knowledge assessment))
                               (list reason "Only click the link to start
this assessment if you are sure you know what you are doing. "))
               ,(when (and (timelimit knowledge assessment)
                           (not (completed knowledge)))
                      "The assessment has a timelimit so do not
start it until your you are ready to complete it in the given
time."))))
     ,(when (assessment-feedback-p knowledge assessment)
            `(tr (td ((a :href ,(strcat path "feedback" ))
                      "Feedback on your answers")))))))


(defun countdown-html(remaining)
  "Returns javascript to display countdown for remaining seconds"
  (format nil "
<form name=\"countdown\">Time remaining
<input type=\"text\" name=\"clock\" align=\"right\" size=5 readonly style=\"color:red;text-align:right;border-style:none;\"> minutes
</form>
<script language=javascript>
<!--
endTime=new Date()
function clockTick()
{
currentTime = new Date();
document.countdown.clock.value = Math.round((endTime-currentTime)/60000+~D);
setTimeout(\"clockTick()\", 10000);
}
clockTick();
-->
</script>" remaining))