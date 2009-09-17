;;;; Aston Web Applications
;;;; Copyright (C) 2002-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; $Id: adaptive-tutor.lisp,v 1.15 2005/03/10 19:54:25 willijar Exp $

(in-package :aston)

(defvar *tutorials*
  (make-instance
   'clews.articles:tutorial-collection
   :id :tutorials
   :acl '((:view . (:all))
          (:edit . ("willijar"))
          (:admin . ("willijar")))
   :class 'clews.articles:tutorial-article
   :path (translate-logical-pathname #p"clews:tutorials;")
   :file-type "rst"))

(defvar *tutorial-app*
  (make-instance
   'clews.articles:clews-tutorial
   :id :tutorial-app
   :articles *tutorials*
   :pwd-source *pwd-source*
   :user-dictionary *user-source*
   :acl '((:view . (:all))
          (:edit . ("willijar"))
          (:admin . ("willijar")))))

(defun ensure-goal(goal group)
  (dolist (username (clews:get-users group *user-source*))
    (let* ((user (get-dictionary username *user-source*))
           (goals (clews.articles::goals *tutorials* user)))
      (unless (member goal goals :test #'equalp)
        (setf (clews.articles::goals *tutorials* user)
              (cons goal goals))
        (setf (get-dictionary username *user-source*) user)))))

(defun email(to title content)
  (format t "~A ~S~%~%~A" to title content)
  (inet.rfc2821::send-mail
        "J.A.R.Williams@aston.ac.uk"
        to
        title
        content))


(defun send-tutorial-report-email(user nodays)
  (email
   (username user)
   (format nil "~D Day Tutorial Reminders" nodays)
   (clews.articles::deadlines-report-text *tutorials* user nodays)))


(defun tutorials-reports(nodays)
  (map-dictionary
   #'(lambda(username user)
       (multiple-value-bind(txt rep)
           (clews.articles::deadlines-report-text *tutorials* user nodays)

         (when rep
           (email
            username
            (format nil "~D Day Tutorial Reminders" nodays)
            (format nil "Dear ~A,

Enclosed is your regular reminder report for the online assessments at
http://heisenberg.aston.ac.uk:8080/tutorials/

~A"
                    (clews.grades::fullname username)
                    txt)))))
   *user-source*)
  (terpri))

(defun student-deadlines-reports(nodays)
  (maphash
   #'(lambda(username data)
       (email
        username
        (format nil "~D Day Assessment Deadline Reminder" nodays)
        (format nil
                "Dear ~A,

Enclosed is your reminder report for assessments due in the
next ~D days.

~{~A ~A: ~A ~A (~A)~%~}
--
EE Masters Assessment System~%"
                (clews.grades::fullname username)
                nodays
                (mapcan
                 #'(lambda(record)
                     (setf (first record)
                           (jarw.parse:format-output '(date :fmt :short)
                                                     (first record)))
                     record)
                 data))) )
   (clews.grades::student-deadlines nodays)))

(defun staff-deadlines-reports(nodays)
  (maphash
   #'(lambda(username data)
       (email
        username
        (format nil "~D Day Assessment Deadline Reminder" nodays)
        (format nil
                "Enclosed is your reminder report for student
assessments due in the next ~D days. Once the student submits an
assessment you must enter the submission date using the online marking
form to have them removed from the list. Each student has also been
sent this information.

~{~8A ~A ~A: ~A ~A (~A)~%~}
--
EE Masters Assessment System"
                nodays
                (mapcan
                 #'(lambda(record)
                     (setf (second record)
                           (jarw.parse:format-output '(date :fmt :short)
                                                     (second record)))
                     record)
                 data))) )
   (clews.grades::staff-deadlines nodays)))

;; date 2007-11-10 23:59  3403727940
;; interval 1 week 604800

(defun weekly-reports()
  (format t "----------------------------------------------------~%")
  (tutorials-reports 14)
  ;(staff-deadlines-reports 14)
  ;(student-deadlines-reports 14)
  -3600)

;; write function to determine date of next one
;; maybe

(defun next-weekend(&optional (now (get-universal-time)))
    (do ((d 3403727940 (+ d 604800)))
        ((> d now) d)))

;;  (jarw.cron:schedule (next-weekend) #'weekly-reports))

(defun date(str) (jarw.parse:parse-input 'jarw.parse:date str))

(defun find-user-tutorial-records(username tutorial &optional (test #'identity))
  (with-open-file(is (merge-pathnames username #p"/home/willijar/www/clews/tutorials/users/") :direction :input)
    (do ((rec (read is nil nil) (read is nil nil))
         (results nil))
        ((not rec) (nreverse results))
      (when (and (equal (first rec) tutorial) (funcall test rec))
        (push rec results)))))

;; e.g. (find-user-tutorial-records "dodiakn" "phase-and-group-delay" #'(lambda(rec) (< (third rec) (date "2009-01-24 21:03"))))