;; $Id: application.lisp,v 1.2 2006/09/04 08:45:20 willijar Exp willijar $
;; Web Assessment Application handlers
;; Copyright (C) 2002-2006 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of CLEWS Assessments

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;;

;;; Code:

(in-package :clews.assessments)

;; knowledge domain data stored in a hash of string ids and concept objects
;; this means that concepts can be referred to before the actual
;; concept-assoc is created

(defclass assessment-application(application)
  ((assessments :initarg :assessments
		:reader assessments
		:documentation "Dictionary of Assessments"))
  (:default-initargs :id :assessments)
  (:documentation "Assessments Application"))

(defun display-title(name)
  (nsubstitute-if #\space #'(lambda(c) (or (eql c #\-) (eql c #\_)))
                  (string-capitalize name)))

(defun has-assessment-permission(role assessment app
                                 &optional (user *current-user*))
  "Assessments either have their own permissions or else inherit permissions
from the app"
  (if (inet.acl:acl assessment)
      (has-permission role assessment user)
      (has-permission role app user)))

(defmethod initialize-instance :after((app assessment-application)
                                      &key assessment-path
                                      &allow-other-keys)
  (unless (slot-boundp app 'assessments)
    (setf (slot-value app 'assessments)
          (make-instance 'serializable-dictionary
                         :directory assessment-path
                         :package 'clews.assessment))))


(declaim (inline get-assessment (setf get-assessment) get-knowledge (setf get-knowledge)))
(defun get-assessment(name app) (get-dictionary name (assessments app)))
(defun (setf get-assessment)(value name app)
  (setf (get-dictionary name (assessments app)) value))
(defun get-knowledge(username assessmentname app)
  (or (knowledge username (get-assessment assessmentname app))
      (list nil)))
(defun (setf get-knowledge)(value username assessmentname app)
  (let ((assessment (get-assessment assessmentname app)))
    (setf (knowledge username assessment) value)
    (setf (get-assessment assessmentname app) assessment)))

(defun short-datestring(utime)
  "Decode the universal time UTIME and return an RFC-822-format string
  using UT rather than the local timezone"
  (when utime
    (multiple-value-bind (se mi ho da mo)
        (decode-universal-time utime 0)
      (declare (fixnum mi ho da mo) (ignore se))
      (format nil "~2,'0d ~a ~2,'0d:~2,'0d"
              da (aref port:+month-names+ (1- mo)) ho mi))))

(defmethod response-handler((app assessment-application) request rest)
  (let* ((path (subseq rest 0 (position #\? rest)))
         (parts (split-string  path 3 #(#\/ #\.) :remove-empty-subseqs t))
         (assessment (when (first parts)
                       (if (get-assessment (first parts) app)
                           (first parts)
                           (throw 'response :not-found))))
         (action (second parts))
         (student
          (if (third parts)
              (if (has-permission '(:tutor :admin) app)
                  (or (get-dictionary (third parts) (users app))
                      (throw 'response :not-found))
                  (throw 'response :forbidden))
              *current-user*)))
    (cond
      ((not assessment) (home-handler app request))
      ((or (not action) (equal action "status"))
       (assessment-status-handler app request assessment student))
      ((equal action "attempt")
       (assessment-attempt-handler app request assessment student))
      ((equal action "feedback")
       (if student
           (assessment-feedback-handler app request assessment)
           (mark-handler app request assessment student)))
      ((equal action "marks")
       (marks-summary-handler app request assessment))
      (t (throw 'response :not-found)))))

(defun home-handler(app request)
  (declare (ignore request))
  (unless (has-permission '(:student :tutor :admin) app)
    (throw 'response :forbidden))
  (let ((studentname (username *current-user*))
        (not-started nil)
        (not-completed nil)
        (no-feedback nil)
        (feedback nil)
        (tutors nil))
    ;; collate assessments and knowledge by category for this student
    (map-dictionary
     #'(lambda(k a)
         (when (has-assessment-permission '(:tutor :admin) a app)
           (push (cons k a) tutors))
         (when (has-assessment-permission :student a app)
           (let ((knowledge (get-knowledge studentname k app)))
             (cond
               ((not (started knowledge))
                (push (cons k knowledge) not-started))
               ((not (completed knowledge))
                (push (cons k knowledge) not-completed))
               ((assessment-feedback-p knowledge a)
                (push (cons k knowledge) feedback))
               (t (push (cons k knowledge) no-feedback))))))
     (assessments app))
    `(html
      (head (title "Directory of Assessments"))
      (body
       (navbar  ((("."  "Home")
                  ("summary" "Summary")
                  ("preferences/" "Preferences")))
        :on-url ".")
       ((section :title ,(format nil "Directory of Assessments for ~S"
                                 (display-name *current-user*)))
        ((table :border 0)
         ,@(when
            tutors
            `((tr ((th :colspan 5 :id "#tutors")
                   (h2 "Tutor's assessments")))
              (tr ((th :colspan 2) "Assessment")
               ((th  :width "15%") "Deadline")
               ((th  :width "15%") "Feedback")
               ((th :width "30%") "Submissions"))
              ,@(mapcar
                 #'(lambda(rec)
                     (let ((name (car rec))
                           (a (cdr rec)))
                       `(tr ((td :colspan 2)
                             ((a :href ,(format nil "~A/marks" name))
                              ,(display-title name)))
                         (td ,(short-datestring (default-deadline-date a)))
                         (td ,(short-datestring (default-feedback-date a)))
                         ((td :align :right)
                          ,(hash-table-size (slot-value a 'knowledge))))))
                 (sort tutors #'>
                       :key #'(lambda(rec)
                                (or (default-deadline-date (cdr rec)) 0))))))
         (tr ((th :colspan 5 :id "#incomplete")
              (h2 "Started but not completed")))
         (tr ((th :colspan 2) "Assessment")
             ((th :width "15%") "Started")
             ((th  :width "15%")"Deadline")
             ((th :width "30%") "Notes"))
         ,@(mapcar
            #'(lambda(rec)
                (let*((name (car rec))
                      (knowledge (cdr rec))
                      (assessment (get-assessment name app)))
                  `(tr (td ((a :href ,(format nil "~A/status" name))
                            ,(display-title name)))
                    (td ,(description  assessment))
                    (td ,(short-datestring (started knowledge))
                     (td ,(or
                           (short-datestring (deadline-date knowledge assessment))
                           "-"))
                     (td ,(assessment-should-not-attempt-reason
                           knowledge assessment))))))
            (sort not-completed
                  #'<
                  :key (lambda(rec) (started (cdr rec)))))
         (tr ((th :colspan 5 :id "#notstarted") (h2 "Not Started")))
         (tr ((th :colspan 2) "Assessment")
             ((th) "Deadline") ((th :colspan 2)"Notes"))
         ,@(mapcar
            #'(lambda(rec)
                (let*((name (car rec))
                      (knowledge (cdr rec))
                      (assessment (get-assessment name app)))
                  `(tr (td ((a :href ,(format nil "~A/status" name))
                            ,(display-title name)))
                    (td ,(description  assessment))
                    (td ,(or
                          (short-datestring (deadline-date knowledge assessment))
                          "None"))
                    ((td :colspan 2) ,(assessment-should-not-attempt-reason
                                       knowledge assessment)))))
            (sort not-started
                  #'(lambda(a b) (cond ((and a b) (< a b)) ((not b) t)))
                  :key (lambda(rec)
                         (deadline-date (cdr rec)
                                        (get-assessment (car rec) app)))))
         (tr ((th :colspan 5 :id "#feedback")
              (h2 "Completed, Feedback available")))
         (tr ((th :colspan 2) "Assessment") (th "Completed")
             ((th :colspan 2) "Notes"))
         ,@(mapcar
            #'(lambda(rec)
                (let* ((name (car rec))
                       (knowledge (cdr rec))
                       (assessment (get-assessment name app)))
                  `(tr (td ((a :href ,(format nil "~A/feedback" name))
                            ,(display-title name)))
                    (td  ,(description  assessment))
                    (td ,(short-datestring (completed knowledge)))
                    ((td :colspan 2) ,(assessment-should-not-attempt-reason
                                       knowledge assessment)))))
            (sort feedback
                  #'<
                  :key (lambda(rec) (completed (cdr rec)))))

         (tr ((th :colspan 5 :id "#nofeedback")
              (h2 "Completed but no feedback")))
         (tr ((th :colspan 2) "Assessment") (th "Completed") ((th :colspan 2) "Notes"))
         ,@(mapcar
            #'(lambda(rec)
                (let* ((name (car rec))
                       (knowledge (cdr rec))
                       (assessment (get-assessment name app)))
                  `(tr (td ((a :href ,(format nil "~A/status" name))
                            ,(display-title name)))
                    (td ,(description  assessment))
                    (td ,(short-datestring (completed knowledge)))
                    ((td :colspan 2)
                     ,(assessment-should-not-attempt-reason
                       knowledge assessment)))))
            no-feedback))) ))))

(defun assessment-status-handler(app request assessmentname
                                     &optional (student *current-user*))
  (declare (ignore request))
  (let ((assessment (get-assessment assessmentname app))
        (knowledge (get-knowledge (username student)
                                  assessmentname app)))
    (unless (has-assessment-permission :student assessment app student)
      (throw 'response :forbidden))
    `(html
      (head (title "Assessment Status"))
      (body
       (navbar
        ((("../"  "Home")
          ,@(when (assessment-attempt-p knowledge assessment)
                  (list (list
                         "attempt"
                         (if (started knowledge) "Continue Attempt" "Start"))))
          ,@(when (assessment-feedback-p knowledge assessment)
                  '(("feedback" "Feedback")))
          ,@(when (has-assessment-permission :tutor assessment app)
                  '(("marks" "Marks Summary")))
          ("../preferences/" "Preferences")))
        :on-url "status")
       ((section :title ,(format nil "Assessments Status of ~S for ~S"
                                 (display-title assessmentname)
                                 (display-name student)))
        ,(when (timelimit knowledge assessment)
               '(p "The assessment has a timelimit so do not start it until your are sure you are ready to complete it."))
        ,(assessment-status-table knowledge assessment)
        ,(when (assessment-attempt-p knowledge assessment)
               `(p
                 ,@(when-bind (reason (assessment-should-not-attempt-reason
                                       knowledge assessment))
                              (list reason " Only click the link to start
or continue this assessment if you are sure you know what you are doing. "))
                 ((a :href  "attempt")
                  ,(if (started knowledge) "Continue" "Start")
                  " the assessment.")))
        ,(when (assessment-feedback-p knowledge assessment)
               `(p ((a :href  "feedback" ) "Feedback on your answers"))))))))

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
value=Math.round((endTime-currentTime)/60000+~D);
if (value>0) {
document.countdown.clock.value = value
setTimeout(\"clockTick()\", 10000);
}
else
{
document.countdown.clock.value=\"None\";
}}
clockTick();

-->
</script>" remaining))

(defun assessment-attempt-handler(app request assessmentname
                                      &optional (student *current-user*))
  (let ((assessment (get-assessment assessmentname app))
        (knowledge (get-knowledge (username student) assessmentname app)))
    (unless (has-assessment-permission
             '(:student :tutor :admin) assessment app)
      (throw 'response :forbidden))
    (multiple-value-bind (access-p reason)
        (assessment-attempt-p knowledge assessment)
      (unless access-p (throw 'response (cons :forbidden reason))))
    (let ((markup
           `(html
             (head (title "Assessment Status"))
             (body
              (navbar  ((("../"  "Home")
                         ("status" "Status")
                         ,@(when (assessment-feedback-p knowledge assessment)
                                 '(("feedback" "Feedback")))
                         ("../preferences/" "Preferences")))
               :on-url "attempt")
              ((section :title ,(format nil "Assessment ~S for ~S"
                                        (display-title assessmentname)
                                        (display-name student)))
               ,(assessment-status-table knowledge assessment)
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
                           "View Feedback on your assessment submission ")))
               (p ((a :href "../") "Back to Assessment Directory")))))))
      (setf (get-knowledge (username student) assessmentname app)
            knowledge)
      (values markup nil t))))

(defun assessment-feedback-handler(app request assessmentname
                                   &optional (student *current-user*))
  (let ((assessment (get-assessment assessmentname app))
        (knowledge (get-knowledge (username student)
                                  assessmentname app)))
    (unless (has-assessment-permission '(:student :admin :tutor) assessment app)
      (throw 'response :forbidden))
    (multiple-value-bind (access-p reason)
        (assessment-feedback-p knowledge assessment)
      (unless access-p (throw 'response (cons :forbidden reason))))
    (values
     `(html
       (head (title "Assessment Feedback"))
       (body
        (navbar  ((("../"  "Home")
                   ,@(when (assessment-attempt-p knowledge assessment)
                           (list (list
                                  "attempt"
                                  (if (started knowledge) "Continue Attempt" "Start"))))
                   ("../preferences/" "Preferences")))
         :on-url "feedback")
        ((section :title
                  ,(format nil "Feedback on ~S for ~S"
                           (display-title assessmentname)
                           (display-name student)))
         ,(assessment-status-table knowledge assessment)
         ,(assessment-feedback-markup knowledge assessment request)
         (p ((a :href "../") "Back to Assessment Directory")))))
     nil t)))

(defun groups-form(app &optional extra-fields)
  (let* ((roles (sort (roles (users app)) #'string<)))
    (flet ((roles-maq(name)
             `((maq
                :name ,name
                :value ,(first (last roles))
                :style :dropdown
                :datatype symbol
                :size ,(min (length roles) 5))
               ,@roles)))
      `((form :method :post)
        ((table :cellspacing 5)
         (tr (th "Student Set") (th "Normalisation Set"))
         (tr ((td :align :center) ,(roles-maq :students))
          ((td :align :center) ,(roles-maq :normalisation))))
        ,@extra-fields
        ((input :type "submit" :name "Submit" :value "Analyse"))))))

(defun student-detail-cells(student)
  "markup of table cells describing a student"
  `((td ((a :href ,(strcat "feedback/" (username student)))
         ,(username student)))
    (td ,(property student :studentid))
    (td ,(property student :lastname)
     ,@(when-bind (f (property student :firstname))
                  (list ", " f)))))

(defun %fmt(v) "Output format for %" (when v (format nil "~,1F%" v)))

(defun marks-summary-handler(app request assessmentname)
  (let ((assessment (get-assessment assessmentname app)))
    (unless (has-assessment-permission :tutor assessment app)
      (throw 'response :forbidden))
    (flet ((userrecords(usernames)
             (mapcar #'(lambda(id) (get-dictionary id (users app)))
                     usernames)))
      (let* ((form (groups-form
                    app
                    `((p "Plagiarism - Distance Metric:"
                       ((input :name :distance :value 2 :size 2
                               :datatype (integer :min 0)))
                       " Time Interval (minutes):"
                       ((input :name :delay :value 30 :size 3
                               :datatype (number :min 0))))) ) )
             (form-data
              (if (submitted-action form request)
                  (setf (property (user-component-properties app
                                                             *current-user*)
                                  :groups-default)
                        (form-data form request))
                  (property (user-component-properties app *current-user*)
                            :groups-default)) )
             (students  (userrecords
                         (sort (get-users (getf form-data :students)
                                          (users app))
                               #'string<) ))
             (normalisation (or (userrecords
                                 (get-users (getf form-data :normalisation)
                                            (users app)))
                                students)))
        (values
         `(html
           (head (title "Marks Analysis"))
           (body
            (navbar  ((("./"  "Home")
                       ("status" "Status")
                       ,@(when (has-assessment-permission :tutor assessment app)
                               '(("marks" "Marks Summary")))
                       ("./preferences/" "Preferences")))
             :on-url "marks")
            ((section :title ,(format nil "~S marks summary"
                                      (display-title assessmentname)))
             ,(markup-form form form-data)
             ,@(when students
                     `(((section :title "Cohort")
                        ,(cohort-table-assessment app assessmentname
                                                  students normalisation))
                       ((section :title "Assessment Detail")
                        ,(cohort-table-assessment-detail
                          app assessmentname students normalisation))
                       ((section :title "Potential Plagiarism")
                        ,(cohort-table-plagiarism
                          app assessmentname students
                          (getf form-data :distance 2)
                          (getf form-data :delay 30) ))))
             (p ((a :href "../") "Back to Directory")))))
         nil t)))))

(defun mark-handler(app request assessmentname studentname)
  (let ((assessment (get-assessment assessmentname app))
        (knowledge (get-knowledge studentname  assessmentname app)))
    (unless (has-assessment-permission :tutor assessment app)
      (throw 'response :forbidden))
    (unless (started knowledge)
      (throw 'response
        (cons :not-found (format nil "~S has not started assessment ~S"
                                 studentname assessmentname))))
    (let ((markup
           `(html
             (head (title "Assessment Mark"))
             (body
              ((section :title ,(format nil "Mark Assessment ~S for ~S"
                                        (display-title assessmentname)
                                        studentname))
               ,(assessment-status-table knowledge assessment)
               ,(assessment-mark-markup knowledge assessment request)
               (p ((a :href "../../") "Back to Assessment Directory")))))))
      (setf (get-knowledge studentname assessmentname app) knowledge)
      (values markup nil t))))

(defun cohort-table-assessment(app assessmentname students
                               normalisation-students)
  "Present information relating to assessment - assessment mark"
  (let ((assessment (get-assessment assessmentname app))
        (row 0)
        (marks nil))
    `((table :cellspacing 0 :cellpadding 2)
      (caption "Table of assessment related information")
      ((tr :class :header)
       (th "Username") (th "Studentid") (th "Name")
       (th "Mark") (th "Time taken") (th "started") (th "Completed"))
      ,@(mapcar
         #'(lambda(student)
             (let* ((knowledge
                     (get-knowledge (username student) assessmentname app))
                    (rowclass (if (evenp (incf row)) :even :odd)))
               `((tr :class ,rowclass)
                 ,@(student-detail-cells student)
                 ((td :align "right")
                  ,(cond
                    ((not (completed knowledge)) "-")
                    ((clews.assessment::assessment-marked-p
                      knowledge assessment)
                     (let ((m (assessment-mark knowledge assessment)))
                       (when m (push m marks)
                             (%fmt (* 100 m)))))
                    ("Unmarked")))
                 ((td :align "right") ,(timetaken knowledge))
                 ((td :align "right")
                  ,(short-datestring (started knowledge)))
                 ((td :align "right")
                  ,(short-datestring (completed knowledge)))
                 ,(when-bind (status (assessment-count-p-reason
                                      knowledge assessment))
                             `((tr :class ,rowclass)
                               (td)
                               ((td :align "left" :colspan 9)
                                ,status))))))
         students)
      ((tr :class :header)
       (th "Averages")
       ((td :colspan 2))
       ((td  :align "right")  ,(%fmt (* 100 (mean marks))) )
       ((td :colspan 2)) )
      ((tr :class :header) (th "Weighting")
       (td ,(assessment-normalisation-weighting
             (mapcar #'(lambda(student)
                         (get-knowledge (username student) assessmentname app))
                     normalisation-students)
             assessment) )))))


(defun cohort-table-assessment-detail(app assessmentname
                                      students normalisation)
  (let ((assessment (get-assessment assessmentname app)))
    (flet ((knowledge-set(students)
             (mapcar #'(lambda(student)
                         (get-knowledge (username student) assessmentname app))
                     students)))
      (let* ((knowledge-set (knowledge-set students))
             (normalisation-set (or (knowledge-set normalisation)
                                    knowledge-set)))
        (assessment-detail-statistics knowledge-set assessment
                                      normalisation-set)))))

(defun cohort-table-plagiarism(app assessmentname students
                               &optional (distance 2) (delay 60))
  (let ((assessment (get-assessment assessmentname app))
        (completed (make-hash-table)))
    `(table
      (tr (th "Student1") (th "Student 2") (th "Distance Metric")
       (th "Interval (mins)"))
      ,@(mapcan
         #'(lambda(student1)
             (setf (gethash student1 completed) t)
             (let ((k1 (get-knowledge (username student1) assessmentname app)))
               (when (clews.assessment::completed k1)
                 (let ((candidates
                        (mapcan
                         #'(lambda(student2)
                             (unless (gethash student2 completed)
                               (let ((k2 (get-knowledge
                                          (username student2)
                                          assessmentname app)))
                                 (when (clews.assessment::completed k2)
                                   (let ((d (assessment-distance-metric
                                             k1 k2 assessment))
                                         (interval
                                          (min
                                           (abs (- (started k1) (started k2)))
                                           (abs (- (completed k2)
                                                   (completed k1))))))
                                     (when (and (<= interval (* 60 delay))
                                                (<= d distance))
                                       (list (list student2 d
                                                   (/ interval 60.0)))))))))
                         students)))
                   (when candidates
                     (mapcar
                      #'(lambda(candidate)
                          `(tr
                            (th ,(username student1))
                            (td ,(username (first candidate)))
                            ((td :align "right") ,(second candidate))
                            ((td :align "right")
                             ,(format nil "~,2f" (third candidate)) )))
                      candidates))))))
         students))))

(defun summary-handler(app request rest)
  (declare (ignore rest))
  (unless (has-permission '(:tutor :admin) app)
    (throw 'response :forbidden))
  (let* ((form
          (let ((roles (roles (users app)))
                (assessments))
            (map-dictionary
             #'(lambda(k a)
                 (when (has-assessment-permission
                        '(:tutor :admin) a app)
                   (push k assessments)))
             (assessments app))
            `((form :method :post)
              ((table :cellspacing 5)
               (tr (th "Student Set") (th "Assessment Set"))
               (tr
                (td ((maq
                      :name :students
                      :size ,(min (length roles) 5)
                      :style :dropdown
                      :datatype symbol)
                     ,@(sort roles #'string<)))
                (td ((maq
                      :name :assessments
                      :size ,(min (length assessments) 5)
                      :style :dropdown)
                     ,@(sort assessments #'string<)))))
              ((input :type "submit" :name "Submit" :value "Analyse")))))
         (form-data
          (if (submitted-action form request)
              (setf (property (user-component-properties app *current-user*)
                              :summary-default)
                    (form-data form request))
              (property (user-component-properties app *current-user*)
                        :summary-default)) )
         (students (sort (copy-list (get-users (getf form-data :students)
                                               (users app)))
                         #'string< :key #'username))
         (assessments (sort (getf form-data :assessments) #'string<)))
    `(html (head (title "Assessment Group Summary"))
      (body
       ((section :title  "Assessment Group Summary")
        (navbar  ((("./"  "Home")
                   ("summary" "Summary")
                   ("./preferences/" "Preferences")))
                 :on-url "summary")
        ,(markup-form form form-data)
        (table
         (tr (th "Student")
             (th "Avg")
             ,@(mapcar #'(lambda(a)
                           `((th :width ,(format nil "~,1F%" (/ 100 (+ 2 (length assessments)))))
                             ,(display-title a))) assessments)
             ,@(let ((assessments
                      (mapcar #'(lambda(a) (get-dictionary a (assessments app)))
                              assessments)))
                    (mapcar
                     #'(lambda(s)
                         (let* ((username (username s))
                                (marks
                                 (mapcar
                                  #'(lambda(assessment)
                                      (let ((k (clews.assessments::knowledge
                                                username assessment)))
                                        (if (clews.assessment::completed k)
                                            (clews.assessment::assessment-mark
                                             k assessment) 0)))
                                  assessments)))
                           `(tr ((th :align :right) ,username)
                             ((td :aligh :right)
                              ,(format nil "~,1F"
                                       (* 100 (jarw.math::mean marks))))
                             ,@(mapcar
                                #'(lambda(m)
                                    `((td :align :right)
                                      ,(if m
                                           (format nil "~,1F" (* 100 m))
                                           "X")))
                                marks))))
                     students)))))))))
