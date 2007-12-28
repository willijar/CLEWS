;;;; CLEWS: Standalone Assessment Serialization
;;;; Copyright (C) 2003-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL) 
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; $Id: storage.lisp,v 1.1 2006/08/08 08:18:44 willijar Exp $

(in-package :clews.assessment)

(defmacro opt(keyword value)
  (let ((v (gensym)))
    `(let ((,v ,value))
       (when ,v
	 (list ,keyword ,v)))))
  
(defmethod initargs nconc ((obj assessment))
  `(,@(opt :acl (acl obj))
    ,@(opt :description (description obj))
    ,@(opt :answers (let ((entries nil))
		      (maphash #'(lambda(k v) (push (cons k v) entries))
			       (slot-value obj 'knowledge))
		      entries))))

(defmethod initargs nconc ((obj scheduled))
  `(,@(opt :deadline-date (datestring (default-deadline-date obj)))
    ,@(opt :feedback-date (datestring (default-feedback-date obj)))
    ,@(opt :strict-deadline (strict-deadline-p obj))
    ,@(opt :multiple-attempt (multiple-attempt-p obj))))

(defmethod initargs nconc ((obj ephemeral))
  `(,@(opt :start-date (datestring (default-start-date obj)))
    ,@(opt :end-date (datestring (default-end-date obj)))))

(defmethod initargs nconc ((obj timelimited))
  `(,@(opt :timelimit (default-timelimit obj))
    ,@(opt :strict-timelimit (strict-timelimit-p obj))))

(defmethod initargs nconc ((obj questionnaire))
  `(:questions ,(question-specifications obj)
    ,@(opt :randomise-questions (randomise-questions-p obj))
    ,@(opt :set-no-questions (slot-value obj 'set-no-questions))
    ,@(opt :no-questions-counted (slot-value obj 'no-questions-counted))))

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
		   ,(when (timelimit knowledge assessment)
		      '" The assessment has a timelimit so do not
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
#|
(defun assessment-markup(knowledge assessment request)
  (multiple-value-bind (access-p reason)
	(assessment-attempt-p knowledge assessment)
    (unless access-p (throw 'response (cons :forbidden reason))))
  `(,(assessment-status-table knowledge assessment)
     ,(when (time-remaining knowledge assessment)
	`((div :escape nil :align "center")
	  ,(countdown-html (/ (time-remaining knowledge assessment) 60))))
     ,(when-bind (reason (assessment-should-not-attempt-reason
			  knowledge assessment))
		 (list '(p :class :error) reason
		       "Only submit if you are sure you know what
you are doing."))
     ,(assessment-attempt-markup knowledge assessment request)
     ,(when (assessment-feedback-p knowledge assessment)
	`(p ((a :href "feedback")
	     "View Feedback on your assessment submission ")))))


(defun feedback-handler(knowledge assessment request)
  (let* ((user-data (user-component-properties tutor user))
	 (knowledge (knowledge user-data concept))
	 (assessment (assessment concept)))
      (multiple-value-bind (access-p reason)
	  (assessment-feedback-p knowledge assessment)
	(unless access-p (throw 'response (cons :forbidden reason))))
      (values
       `(html
	(head (title "Assessment feedback for " ,(concept-title concept)))
	(body ((section :title ,(concept-title concept))
	       ,(assessment-status-table knowledge assessment)
	       ,(assessment-feedback-markup knowledge assessment request)
	       #|,(when (assessment-reset-p knowledge assessment)
		  '(p ((a :href "reset") "Reset this assessment")))|#
	       (p "Back to " ((a :href ,(concatenate 'string "../" (concept-id concept)))
			      ,(concept-title concept)) " topic" ))))
      nil t)))

|#