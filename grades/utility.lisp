;; $Id: utility.lisp,v 1.1 2006/09/08 06:34:45 willijar Exp willijar $
;; Utilities
;; Copyright (C) 2006 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of CLEWS Grades

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;;

;;; Code:

(in-package :clews.grades)

(let ((connected (make-hash-table :test #'equal)))
  (defun db-connect(db)
    "open a local db connection"
    (let* ((args (or (gethash db connected) (cl-user::db db)))
	   (con (clsql:connect
		 args
		 :database-type :postgresql-socket :if-exists :old)))
      (when con (setf (gethash db connected) args))
      con)))

(defvar *db* (db-connect "MSc") "The database")

(defun format-percentage(num &optional (places 0) (%-p t))
  "Return a percentage value formatted for user output (default 0 places)"
  (when (and num (< num 1) (> num -1)) (setf num (* num 100)))
  (if num
      (if (= places 0)
          (format nil "~D~@[%~]" (round num) %-p)
          (format nil (format nil "~~,~DF~@[%~]" places %-p) num))
      (when %-p "%")))

(defun format-timestamp(utime)
  "Return a user frendly date/time format"
  (if utime
      (multiple-value-bind (se mi ho da mo ye dw)
	  (decode-universal-time utime 0)
	(declare (ignore se))
	(let ((month-name (aref port:+month-names+ (1- mo)))
	      (week-day-name (aref port:+week-days+ dw)))
	  (format nil "~A, ~2,'0d ~a ~4d ~2,'0d:~2,'0d"
		  week-day-name da month-name ye ho mi)))
      "-"))

(defun format-datestamp(utime)
  (if utime
      (multiple-value-bind (se mi ho da mo ye)
	  (decode-universal-time utime 0)
	(declare (ignore se mi ho))
	(let ((month-name (aref port:+month-names+ (1- mo))))
	  (format nil "~2,'0d ~a ~4d" da month-name ye)))
      "-"))

(defun average-mark(marks &key
		    (ignore-nil t)
		    (ignore-0 nil)
		    (mark #'mark)
		    (weight
		     #'(lambda(m)
             (if (mark m)
                 (let ((a (assessment m)))
                   (* (weighting a) (credits (module a))))
                 0))))
  "Given a set of mark entities return the weighted average.
The keywords
:mark is the function which when applied to an entity returns the mark
:weight is the function which when applied to an entity returns the weight
:ignore-nil if true do not count marks if they are nil (default t)
:ignore-0 if true do not count marks if they are 0 (default nil)

It is generally assumed that if a mark value is nil it has not been
entered yet. If it is 0 then probable the student didn't attend."
  (let ((mark-total 0)
        (weight-total 0))
    (dolist(entity marks)
      (let ((w (funcall weight entity))
            (m (funcall mark entity)))
        ;;(unless (null m) (incf mark-total (* w (funcall mark entity))))
        (when (cond ((null m) (not ignore-nil))
                    ((< m 0.5) (not ignore-0))
                    (t))
          (incf mark-total (* w m))
          (incf weight-total w))))
    (if (= 0 weight-total)
        0
        (/ mark-total weight-total))))

(defun group-by(records function &key (test #'equal))
  "Given a set of records split it into groups, grouped by given
function using test (default #'equal). Returns an alist, cars are
distinct values of function, cdr is list of records with that value"
  (let ((groups (make-hash-table :test test))
	(results nil))
    (dolist(record records)
      (push record (gethash (funcall function record) groups)))
    (maphash #'(lambda(k v)
		   (declare (ignore k))
		   (push v results))
	       groups)
    results))


(defun find-biggest(list &key (key #'identity) (test #'>))
  "Find the biggest item in a list. :key and :test have their usual meeanings.
returns the item with the biggest value of :key and the corresponding value"
  (when list
    (let* ((bigun (car list))
	   (bigval (funcall key bigun)))
      (dolist(item (cdr list))
	(let ((val (funcall key item)))
	  (when (funcall test val bigval)
	    (setf bigval val
		  bigun item))))
      (values bigun bigval))))

;;; aggregate functions

(defun group-limit(records function &key (test #'>))
  (let ((max (funcall function (first records)))
	(biggest (first records)))
    (dolist(record (cdr records))
      (let ((v (funcall function record)))
	(when (funcall test v biggest)
	  (setf max v
		biggest record))))
    (values biggest max)))

;; fix float output type - nil -> null
;; maybe should change type to mark
(deftype percentage() '(or null (real 0 100)))
(deftype timestamp() '(or null integer))

(defun percentage=(a b)
  "Compare two percentages"
  (or (eql a b)
      (and (numberp a) (numberp b)
	   (eql (coerce a 'float) (coerce b 'float)))))

(defmethod clsql-sys::database-output-sql-as-type
    ((type (eql 'percentage)) val database db-type)
  (declare (ignore database db-type))
  (if val
      (format nil "~,6F" (float val))
      'clsql-sys::NULL))

(defmethod clsql-sys::read-sql-value
    (val (type (eql 'percentage)) database db-type)
  (declare (ignore database db-type))
  (etypecase val
    (string (float (read-from-string val)))
    (float val)
    (number (float val))))

(defmethod clsql-sys::database-output-sql-as-type
    ((type (eql 'timestamp)) val database db-type)
  "Encode a timestamp from universal-time to sql timestamp"
  (if val
    (multiple-value-bind(second minute hour date month
				year day daylight-p zone)
	(decode-universal-time val 0)
      (declare (ignore day daylight-p))
      (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D~@D"
	      year month date hour minute second zone))
    'clsql-sys::NULL))

(defmethod clsql-sys::read-sql-value
    (Val (type (eql 'timestamp)) database db-type)
   "Convert a postgresql date or timestamp string to universal time. dates are represented by their last second"
  (unless (eq 'clsql-sys::NULL val)
    (cl-ppcre:register-groups-bind
   (year month day hour minute second zone)
   ("(\\d{4})-(\\d{2})-(\\d{2})(?: (\\d{2}):(\\d{2}):(\\d{2})(?:(?:\\.\\d*)*((?:\\+|-)\\d*))?)?" val)
   (apply #'encode-universal-time
	  (append (mapcar #'parse-integer
		  `(,(or second "59") ,(or minute "59") ,(or hour "23")
		     ,day ,month ,year))
		  (list (if zone (- (parse-integer zone)) 0)))))))

(defun do-form(data &optional form disabled-p updatefunc request)
  "Return form markup. Data can be the actual data or a function to
return it.  If edit-p is false present only view. update func is
called with validated data return from form. request is http request
object"
  (labels ((data()
             (if (functionp data) (funcall data) data))
           (form() (typecase form
                     (null (find-form (data)))
                     (function (funcall form))
                     (atom (find-form form))
                     (t form))))
    (let ((form (form)))
      (when form
        (if (and (not disabled-p) (submitted-action form request))
            (multiple-value-bind(form-data condition)
                (form-data form request)
              (unless condition (funcall updatefunc form-data))
              `(div
                ,(when condition
                       `((p :class :error)
                         "Form Submission Not Accepted. Please Correct."
                         ,condition))
                ,(markup-form (form) (if condition request (data))
                              (if (not condition) :text))))
            (markup-form form (data) disabled-p))))))

(defun academic-year(&optional (utime (get-universal-time)))
  (let* ((decoded-time (multiple-value-list (decode-universal-time utime)))
	 (year (nth 5 decoded-time))
	 (month (nth 4 decoded-time)))
  (if (< month 10) (1- year) year)))

(defmethod clsql-sys::database-output-sql-as-type ((type (eql 'list)) val database db-type)
  (declare (ignore database db-type))
  (let ((*print-circle* t)
        (*print-array* t)
        (*print-pretty* nil)
        (*package* #.(find-package :cl)))
    (let ((escaped (prin1-to-string val)))
      (clsql-sys::substitute-char-string escaped #\Null " "))))

(defmethod clsql-sys::read-sql-value(val (type (eql 'list)) database db-type)
   (declare (ignore type database db-type))
   (let ((*package* #.(find-package :cl)))
     (read-from-string val)))


#|
(dolist(a (clews.grades::assessment-records clews.grades::*db*))
          (when (clews.grades::feedback-form a) (setf (clews.grades::feedback-form a) (clews.grades::feedback-form a)) (clews.grades::update-records-from-instance a)))

(defmethod clsql-sys::read-sql-value(val (type (eql 'list)) database db-type)
   (declare (ignore type database db-type))
     (read-from-string val))

;; new project dissertation form
((FORM :METHOD :POST :TITLE "Project Dissertation Report" :FORM-MARK
  (LET ((FIELDS (REMOVE :APPROPRIATENESS CLEWS.FORM::FIELDS)))
    (* (CLEWS.FORM::VALUE :APPROPRIATENESS)
       (CLEWS.FORM::WEIGHTED-SUM FIELDS))))
 (OL
  (LI
   (B
    "Appropriateness of work for a Masters level project (multiplicative term):")
   ((MCQ :NAME :APPROPRIATENESS :WEIGHTING 10 :VALUE 75 :DATATYPE
     (INTEGER :MAX 100))
    (100 . "Appropriate for a Masters. (x 100%)")
    (80 . "Marginally appropriate a Masters. (x 80%)")
    (50 . "This dissertation is inappropriate for a Masters. (x 50%)"))
   (P
    "If this work is not 100% you must provide justification for you decision in the comments."))
  (LI
   (B
    "Evaluation of how much of this is the students own work (multiplicative term):")
   ((MCQ :NAME :OWNWORK :WEIGHTING 10 :VALUE 75 :DATATYPE
     (INTEGER :MAX 100))
    (100
     . "This appears to be entirely the students own work and is properly referenced. (x 100%)")
    (80
     . "There are signs of inappropriate referencing in this work. (x 80%)")
    (50 . "There is significant plagiarism in this work (x50%)")
    (0
     . "There are signs that a significant proportion of the work reported is not the students own work or that data has been falsified. (x 0%)"))
   (P
    "If the evaluation is less than 100% provide comments on the issues. If less than 80% please notify the programme director as disciplinary procedures may need to be invoked."))
  (LI (B "Quality of writing (English language) (5%):")
   ((MCQ :NAME :READABILITY :WEIGHTING 5 :VALUE 75 :DATATYPE
     (INTEGER :MAX 100))
    (100 . "Highly readable. Well written and easy to read. (100)")
    (75 . "Readable. (75)")
    (50 . "Readable. However, the writing could be improved to help
  the reader understand what the author is describing. (50)")
    (40 . "Difficult to read. Needs rewriting to make the point of
  the dissertation clear. (40)")
    (0 . "Impossible to read. Should be rejected on the basis of
  writing alone. (0)")))
  (LI (B "Clarity (including organisation of material) (5%):")
   ((MCQ :NAME :CLARITY :WEIGHTING 5 :VALUE 75 :DATATYPE
     (INTEGER :MAX 100))
    (100 . "A well-structured exposition of the material that is easy
  to understand (100).")
    (75
     . "The dissertation is organised and clear. No real problems. (75)")
    (50 . "There are concepts or results that are unclear or the
  organisation of the paper needs revision. (50)")
    (25 . "Both the organisation of the dissertation and its clarity
  are poor and need to be revised to be acceptable. (25)")
    (0 . "Haphazard organisation and unclear concepts make this
  dissertation impossible to understand. Reject and suggest a complete
  rewriting. (0)")))
  (LI (B "Conciseness (length relative to substance) (5%):")
   ((MCQ :NAME :CONCISENESS :WEIGHTING 5 :VALUE 70 :DATATYPE
     (INTEGER :MAX 100))
    (100 . "The dissertation is sufficiently long enough to describe
  the research, but is not wordy. (100)")
    (70 . "The length of the dissertation is reasonable. It could be
  improved somewhat. (70)")
    (40 . "The dissertation is too wordy and needs to be cut to be
  effective. (40)")
    (40 . "There are insufficient details for this to be considered an
    accurate description of the work carried out. (40)")
    (0 . "This dissertation is either too long and needs to be cut
  drastically or too short to be of any use. It should be failed and
  resubmitted. (0)")))
  (LI (B "References to literature (10%):")
   ((MCQ :NAME :REFERENCES :WEIGHTING 10 :VALUE 75 :DATATYPE
     (INTEGER :MAX 100))
    (100 . "A strong, comprehensive reference list. Can't be improved
  upon. (100)")
    (75 . "Good reference list. (75)")
    (50 . "Weak reference list. Needs additional papers to be
  complete. (50)")
    (30 . "Poor reference list. There are insufficient papers to
  support the current research. (30)")
    (0 . "The reference list is missing major papers that are
  required to place the current research in a correct context. (0)")))
  (LI (B "Relevance of figures (5%):")
   ((MCQ :NAME :FIGURES :WEIGHTING 5 :VALUE 75 :DATATYPE
     (INTEGER :MAX 100))
    (100 . "Excellent graphics that illuminate the text. (100)")
    (75
     . "The graphics are appropriate to the text and its contents. (75)")
    (50 . "The figures could use revision to increase comprehension
  or readability. (50)")
    (30 . "The figures are poorly drawn and will require revision to
  be useful. (30)")
    (0 . "Lacks figures to make the text comprehensible, or the
  figures are so poorly drawn as to be useless or figures copied from
  other source with no referencing or evidence of copyright permission. (0)")))
  (LI
   (B
    "Originality (if not original, cite prior publications in comments section): (5%)")
   ((MCQ :NAME :ORIGINALITY :WEIGHTING 5 :VALUE 60 :DATATYPE
     (INTEGER :MAX 100))
    (100 . "Novel contribution of fundamental importance. (100)")
    (80 . "New work. I know of no comparable effort. (80)")
    (60 . "Derivative work, but provides new results. (60)")
    (40 . "This manuscript is very similar to the work of others. (40)")
    (0 . "This has been done before. The manuscript should be
  rejected. (0)")))
  (LI
   (B "Significance of results (degree of contribution to field) (5%):")
   ((MCQ :NAME :SIGNIFICANCE :WEIGHTING 5 :VALUE 50 :DATATYPE
     (INTEGER :MAX 100))
    (100 . "This is a major advance in this field. (100)")
    (80 . "Advances the field. (80)")
    (50 . "A modest advance that may lead to additional work. (50)")
    (40 . "No one will care about the work in this dissertation. (40)")
    (0
     . "The results are trivial and the dissertation should be rejected. (0)")))
  (LI (B "Technical accuracy of results (20%):")
   ((MCQ :NAME :ACCURACY :WEIGHTING 20 :VALUE 75 :DATATYPE
     (INTEGER :MAX 100))
    (100 . "The dissertation is accurate. It cannot be faulted on its
  methods, analysis, or conclusions. (100)")
    (75 . "The dissertation is accurate, but its methods, analysis, or
  conclusions could be improved. (75)")
    (50 . "There is minor inaccuracy in this dissertation that must
  be corrected. (50)")
    (25 . "There is major inaccuracy in this dissertation that must
  be corrected. (25)")
    (0 . "There are sufficient inaccuracies in this dissertation that
  it should be rejected. (0)")))
  (LI
   (B "Rigour (analytical or practical, whichever is appropriate) (10%):")
   ((MCQ :NAME :RIGOUR :WEIGHTING 10 :VALUE 75 :DATATYPE
     (INTEGER :MAX 100))
    (100 . "Well derived or argued dissertation. (100)")
    (75 . "Provides sufficient rigour in the manuscript that the
  results appear to be reasonable and accurate.")
    (50 . "Needs to provide a better argument in places. (50)")
    (25 . "Extremely sloppy methods or analysis. (25)")
    (0 . "Lacks any rigour whatsoever. The results cannot be
  substantiated based on the arguments given here. (0)")))
  (LI (B "Detail level of procedures outlined (10%):")
   ((MCQ :NAME :DETAIL :WEIGHTING 10 :VALUE 75 :DATATYPE
     (INTEGER :MAX 100))
    (100 . "The details in this dissertation are numerous so that it is
  easy for me to understand the importance of the results and the
  techniques by which they were arrived at. (100)")
    (75 . "The details in this manuscript are sufficient to permit me
  to understand the results and the techniques. (75)")
    (50 . "The manuscript lacks some details so that I cannot be
  certain that the results are correct. (50)")
    (25 . "There are a number of details missing and they must be
  included to be able to justify the results. (25)")
    (0 . "The manuscript contains so few details that it is
  impossible to judge its worth. It should be rejected. (0)")))
  (LI (B "Substantiation of conclusions (10%):")
   ((MCQ :NAME :CONCLUSIONS :WEIGHTING 10 :VALUE 50 :DATATYPE
     (INTEGER :MAX 100))
    (100 . "If I performed the same work, I believe I would reach the
  same conclusions. (100)")
    (75 . "If I performed the same work, I am fairly confident I
  would reach the same conclusions. (75)")
    (50 . "If I performed the same work, I might reach the same
  conclusions, but I have some doubts. (50)")
    (25 . "If I performed the same work, I doubt I would reach the
  same conclusions. (25)")
    (0 . "The manuscript does not substantiate the conclusions stated
  in this paper. The dissertation should be rejected. (0)")))
  (LI
   (P (B " Comment")
    " justifying the above assessment and indicating improvements which could have been made.")
   ((TEXTAREA :ROWS 20 :COLS 100 :STRUCTURED-TEXT T :NAME :COMMENT
     :DATATYPE (STRING :WORD-COUNT 10)))))
 (P ((INPUT :TYPE :SUBMIT :NAME "Preview" :VALUE "Preview Mark"))
  ((INPUT :TYPE :SUBMIT :NAME "Submit" :VALUE "Submit Report and Mark"))))
|#