;; Various Latex reports
;; Copyright (C) 2008 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of CLEWS Grades

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;;

;;; Code:

(in-package :clews.grades)

(defun latex-summary-table(os entries)
  (write-line "\\begin{tabular}{r@{ : }l}" os)
  (dolist(entry entries)
    (format os "{\\bf ~A} & " (first entry))
    (markup::latex-escape os (format nil "~A" (second entry)))
    (write-line "\\\\" os))
  (write-line "\\end{tabular}" os))


(defmethod latex(os (mark mark) &optional (level "section*") dummy)
  (declare (ignore dummy))
  (let ((assessment (assessment mark))
        (student (student mark)))
    (update-instance-from-records assessment)
    (format os "\\~A{~A (~A):~A (~A)\\\\~A (~A)}"
            level
            (title (module assessment))
            (moduleid assessment)
            (title assessment)
            (assessmentid assessment)
            (fullname student)
            (studentid student))
    (latex-summary-table
     os
     `(("Raw Mark" ,(format-percentage (mark mark) 1))
       ("Late Penalty" ,(format-percentage (late-penalty mark)))
       ("Attempt" ,(attempt mark))
       ("Revision" ,(revision mark))
       ("Modified By" ,(modified-by mark))
       ("Modified" ,(format-timestamp (modified mark)))
       ("Submitted" ,(format-timestamp (submission-date mark)))
       ("Deadline" ,(format-timestamp (deadline-date mark)))
       ,@(when (note mark) `(("Note" ,(note mark))))))
    (when (feedback mark)
      (latex os (markup-form (feedback-form mark) (feedback mark))))))

(defmethod latex(os (project project)
                 &optional (level "section*") (include-marks-p t))
  (format os "\\~A{~A}~%" level (title project))
  (when (description project)
    (let ((*section-level* 3))
    (latex os
           (markup-form (find-form (description project))
                        (description project)))))
  (latex-summary-table
   os
   `(,@(let ((student (student project)))
         (when student
           `(("Allocated to"
              ,(format nil "~A (~A)" (fullname student) (studentid student))))))
     ("Supervisor(s)" ,(supervisors project))
     ("Start Date" ,(format-timestamp (start-date project)))
     ("Submission Deadline" ,(format-timestamp (deadline-date project)))
     ("Submission Date" ,(format-timestamp (submission-date project)))))
  (unless include-marks-p (return-from latex))
  (dolist(mark (marks project))
    (update-instance-from-records mark)
    (write-line "\\cleardoublepage" os)
    (terpri os)
    (latex os mark "subsection*")))

(defun mark-completed-p(marks id)
  (let ((m (find id marks :key #'assessmentid)))
    (when m (mark m))))

(defun projects-reports(path students)
  (with-open-file(os path :direction :output :if-exists :supersede :if-does-not-exist :create)
    (write-line "\\documentclass{article}
\\include{jarw-macros}
\\smallmargins
\\begin{document}
" os)
    (dolist(s students)
      (let ((project (project s)))
        (when project
          (latex os project)
          (write-line "\\pagebreak" os))))
    (terpri os)
    (write-line "\\end{document}" os)))
