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

;; (let ((connected (make-hash-table :test #'equal)))
;;   (defun db-connect(db)
;;     "open a local db connection"
;;     (let* ((args (or (gethash db connected) (cl-user::db db)))
;; 	   (con (clsql:connect
;; 		 args
;; 		 :database-type :postgresql-socket :if-exists :new)))
;;       (when con (setf (gethash db connected) args))
;;       con)))

(defun db-connect(db)
    "open a local db connection"
    (clsql:connect
 		 (cl-user::db db)
 		 :database-type :postgresql-socket :if-exists :new))

(defvar *db* (db-connect "MSc") "The database")

(defun reset-db()
  (setq *db* (db-connect "MSc"))
  (setf (slot-value aston::*grades* 'db) *db*)
  (setf (slot-value aston::*msc-projects* 'db) *db*)
)

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
