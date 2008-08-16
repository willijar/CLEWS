;; $Id: grades.lisp,v 1.2 2006/09/08 06:32:53 willijar Exp willijar $
;; Online assessments and marking.
;; Copyright (C) 2006 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords: CLEWS, Aston

;; This file is part of CLEWS Grades

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;;

;;; Code:

(in-package :clews.grades)

(defclass grades-manager (application)
  ((db  :initarg :db :reader db :documentation "Database source"))
  (:default-initargs
      :id :grades
    :acl '((:view . (:all))
           (:tutor . (:tutor :admin))
           (:student  . (:student))
           (:admin . (:admin))))
  (:documentation "Class for grade collation and management system"))

(defmethod published-methods ((app grades-manager) baseurl)
  (declare (ignore baseurl))
  `(,@(call-next-method)
    ("grades/" ,#'student-grades-handler :stage :response :match :prefix)
    ("transcript/" ,#'student-transcript-handler
     :stage :response :match :prefix)
    ("students/" ,#'students-directory-handler
     :stage :response :match :prefix)
    ("modules/" ,#'module-handler :stage :response :match :prefix)
    ("assessments/" ,#'assessment-handler :stage :response :match :prefix)
    ("spreadsheets/"
     ,#'spreadsheet-handler :stage :response :match :prefix)))

(defmethod menu((app grades-manager))
  `((("grades/" ,(if (has-permission '(:tutor :admin) app)
                     "Student Grades"
                     "Your Grades"))
     ("projects/" "Projects")
     ("students/" "Directory of Students")
     ,@(when (has-permission '(:tutor :admin) app)
             '(("assessments/" "Assessments and marking")
               ("spreadsheets/" "Examboard Spreadsheets"))))))

(defmethod response-handler((app grades-manager) request rest)
  "Redirect top level depending on permissions"
  (let ((top (let ((path (inet.uri:path (inet.uri:url request))))
               (subseq path 0 (mismatch path rest :from-end t)))))
    (cond
      ((has-permission '(:admin :tutor) app)
       (redirect request (concatenate 'string top "assessments/")))
      ((has-permission '(:student) app)
       (redirect request (concatenate 'string top "grades/")))
      (:forbidden))))

(defun marks-table(student &optional is-tutor-p)
  (let ((mark-groups
         (sort (mark-groups student)
               #'(lambda(a b)
                   (cond((string< (moduleid a) (moduleid b)))
                        ((string-equal (moduleid a) (moduleid b))
                         (> (weighting a) (weighting b)))))
               :key #'(lambda(g) (assessment (car g)))))
        (moduleid nil))
    `((table :cellspacing 3)
      (tr (th "Module") (th "Weighting (%)") ((th :colspan 2) "Assessment")
       (th "Deadline") (th "Mark") (th "Notes"))
      ,@(mapcar
         #'(lambda(mark-group)
             (let* ((mark (car mark-group))
                    (assessment (assessment mark)))
               `(tr
                 (td ,(unless (string= (moduleid assessment) moduleid)
                              (setf moduleid (moduleid assessment))))
                 ((td :align :right)
                  ,(format-percentage (weighting assessment)))
                 (td  ,(assessment-type assessment))
                 (td ,(title assessment))
                 (td  ,(format-timestamp (deadline-date mark)))
                 ((td  :align :right)
                  ,(cond
                    ((not (release-date mark)) "NA")
                    ((or is-tutor-p (feedback-available-p mark))
                     `((a :href ,(format nil "../assessments/~A/~A"
                                         (assessmentid mark)
                                         (studentid mark)))
                       ,(format-percentage
                         (when (mark mark) (+ (mark mark)
                                              (late-penalty mark))) 1)))
                    (t (format nil "Avail: ~A"
                               (format-timestamp
                                (release-date mark))))))
                 (td  ,(mark-status-message mark))
                 ,(when is-tutor-p `(td (em ,(note mark)))))))
         mark-groups)
      ,(when is-tutor-p
             `(tr ((td :colspan 5 :align :right) (em "Average"))
               ((td  :align :right)
                (em ,(format-percentage
                      (average-mark (current-marks student)) 1) )))))))

(defun module-marks-table(student &optional is-tutor-p)
  (let ((module-marks
         (sort (copy-list (current-module-marks (module-marks student)))
               #'string< :key #'moduleid) ))
    `((table :cellspacing 3)
      (tr ((th :colspan 2) "Module")
       (th "Mark") ((th) "Credits"))
      ,@(mapcar
         #'(lambda(mark)
             (let ((module (module mark)))
               `(tr
                 (td ,(moduleid module))
                 (td ,(title module))
                 ((td :align :right)
                  ((a :href ,(format nil "../modules/~A/~A"
                                     (moduleid mark)
                                     (studentid mark)))
                   ,(format-percentage (mark mark) 0)))
                 ((td :align :center)
                  ,(credits mark) "/" ,(credits module))
                 (td  ,(mark-status-message mark))
                                        ;,(when (> (attempt mark) 1) "Referred. ")
                                        ;,(when (condoned mark) "Condoned. "))
                 (td ,(when is-tutor-p `(em ,(note mark)))))))
         module-marks)
      (tr ((td :colspan 3 :align :right) "Accumulated Credits")
       ((td :align :center)
        ,(reduce #'+ (mapcar #'credits module-marks))
        "/"
        ,(reduce #'+ (mapcar #'(lambda(m) (credits (module m)))
                             module-marks)))))))

(defun examboard-decisions-table(student &optional is-tutor-p)
  (let ((examboard-decisions
         (sort (copy-list (examboard-decisions student)) #'<
               :key #'examboardid)))
    (dolist(d examboard-decisions)
      (when (> (revision d) 1)
        (setf examboard-decisions
              (delete-if #'(lambda(o) (and (= (examboardid o) (examboardid d))
                                           (= (revision o) (1- (revision d)))))
                         examboard-decisions))))
    `((table :cellspacing 3)
      ,@(mapcar
         #'(lambda(d)
             (let ((e (examboard d)))
               `(tr (td ,(format-timestamp (start e)))
                 ,@(if (or is-tutor-p
                           (and (finish e)
                                (< (finish e) (get-universal-time))))
                       `(((td :align :right) ,(action d))
                         (td ,(argument d))
                         (td ,(when is-tutor-p (notes d))))
                       `(((td :colspan 2) "Not Available until "
                          ,(format-timestamp (finish e))))))))
         examboard-decisions))))

(defmethod student-transcript-handler((app grades-manager) request rest)
  (unless (has-permission :tutor app) (throw 'response :forbidden))
  (let* ((sep (position #\. rest))
         (id (if sep (subseq rest 0 sep) rest))
         (type (if sep (intern (subseq rest (1+ sep)) :keyword) :pdf))
         (credits 0)
         (student
          (car
           (student-records
            (db app) (if (every #'digit-char-p id) 'studentid 'username) id))))
    (unless student
      (throw 'response (cons :not-found (format nil "Student ~A" id))))
    (let* ((decision
            (car (sort (copy-list (examboard-decisions student))  #'>
                       :key #'examboardid)))
           (module-mark-groups
            (sort (copy-list (module-mark-groups student))
                  #'string< :key #'(lambda(g) (moduleid (car g)))))
           (project (project student))
           (tex
            (with-output-to-string(os)
              (map 'nil
                   #'(lambda(s) (when s (write s :stream os :escape nil)))
                   `("\\documentclass[a4paper]{article}
\\usepackage{helvet}
\\renewcommand\\sfdefault{phv}%               use helvetica for sans serif
\\renewcommand\\familydefault{\\sfdefault}%    use sans serif by default
\\setlength{\\textheight}{24cm}
\\setlength{\\textwidth}{16cm}
\\setlength{\\oddsidemargin}{0cm}
\\begin{document}
\\flushleft
\\section*{Aston University\\\\
School of Engineering and Applied Science\\\\
Electronic Engineering}

\\vspace{2cm}
\\begin{tabular}{lcp{9cm}}
Name & : & " ,(fullname student) "\\\\
Programme & : & " ,(title (programme student)) "\\\\
Status & : & "
                     ,@(when decision
                             `(,(action decision) " "
                               ,@(when (argument decision) (list (argument decision)))
                               " (" ,(format-datestamp (finish (examboard decision))) ")"))
                     "\\end{tabular}

"
                     ,@(when (suspended student)
                             (list
                              (format nil "The marks for this student must {\\em NOT} be given out as the student is on the suspension list.

~@[\\subsection*{Notes}
~A~]

" (notes student))))
                     "\\vspace{1cm}
\\subsection*{Performance in Masters Modules}

\\begin{tabular}{lp{6cm}rrrl}
 & {\\em Module} & {\\em Mark \\%} & {\\em Credits} & {\\em Pass Mark} (\\%)\\\\"
                     ,@(mapcan
                        #'(lambda(mark-group)
                            (let ((mark (car mark-group)))
                              (list (moduleid mark) " & "
                                    (title (module mark)) " & "
                                    (cond
                                      ((mark mark)
                                       (format nil "~A & ~@[~A~]"
                                               (format-percentage (mark mark) 0 nil)
                                               (let ((mcredits (credits (module mark))))
                                                 (when (passedp mark)
                                                   (incf credits mcredits)
                                                   mcredits))))
                                      ((> (attempt mark) 1)
                                       "-- & --")
                                      (" Not Completed & --"))
                                    " & "
                                    (pass-mark (module mark))
                                    " & "
                                    (mark-status-message mark :publicp t)

                                    "\\\\
")))
                        module-mark-groups)
                     " & {\\em Total Credits Achieved }& & {\\em " ,credits "}
\\end{tabular}

"
                     ,@(when project
                             `("\\subsection*{Masters Project}
``"
                               ,(title project) "''\\\\"
                               ,@(let ((company (getf (description project) :location)))
                                      (when company (list company)))))
                     "
\\vspace{1cm}

All marks are expressed as a percentage. "
                     ,(if (< (year student) 2004)
                          "Condoned and referred modules are awarded 40\\% only."
                          "Referred modules are capped at the pass mark. From 2004 onwards the actual
mark for condoned modules will be shown however the modules are considered
as passed.")
                     "
Repeat or referred examinations are the following year and may take a
different format. Candidates offered referred or re-sit examinations will be
provided with a revised transcript if the total credit value is increased.

180 Credits are required for an MSc or MRes, 120 Credits for a PgD and
60 for a PgC.

\\vspace{1.5cm}

J.A.R.Williams (Dr)\\\\
Postgraduate Programme Director\\\\
Electronic Engineering\\\\
\\vspace{0.5cm}
"
                     ,(format-datestamp (get-universal-time))
                     "\\end{document}")))))
      (with-open-file(os #p"/home/willijar/tmp/test.pdf"
                         :if-does-not-exist :create
                         :if-exists :supersede
                         :element-type 'flexi-streams:octet
                         :direction :output)
        (pdf os tex))
      (case type
        (:tex (make-instance 'response :content-type "application/x-tex"
                             :content tex))
        (t (make-instance
            'response
            :content-type  "application/pdf"
            :body (pdf 'flexi-streams:octet tex)))))))

(defmethod student-grades-handler((app grades-manager) request rest)
  (let ((is-tutor-p (has-permission :tutor app))
        (clsql::*default-caching* nil))
    (declare (special clsql::*default-caching*))
    (multiple-value-bind(student-username form)
        (if is-tutor-p
            (let* ((form `((form :method :post :action "./")
                           ((mcq :style :dropdown :name :username)
                            ,@(student-usernames (db app)))
                           ((input :type :submit :value "Submit"))))
                   (username (or  (getf (form-data form request) :username)
                                  (when (> (length rest) 0) rest))))
              (values username (markup-form form (list :username username))))
            (username *current-user*))
      `(html
        (head (title "Grades"))
        (body
         (navbar ,(menu app) :on-url "grades/" :relative "../")
         ((section :title "Grades"))
         ,form
         ,(when student-username
                (let ((student
                        (or (car (student-records (db app)
                                                  'username student-username))
                            (car (student-records (db app)
                                                  'studentid student-username)))))
                  (unless student
                    (throw 'response
                      (cons :not-found
                            (format nil "There is no student ~S" student-username))))
                  `((section
                     :title ,(format nil "~A (~A)"
                                     (fullname student) (studentid student)))
                    (p (b ,(title (programme student)) ", " ,(year student)))
                    (p
                     ((img :src ,(format nil "http://www.ee.aston.ac.uk/images/people/~A.png" (username student))
                           :alt "No Photograph Available")))
                    ,(when (suspended student)
                           `((p :class :error)
                             ,(if is-tutor-p
                                  (format nil "The marks for this student must NOT be given out until a matter is resolved with the postgraduate office. ~A." (suspended student))
                                  "Your grades will be unavailable. Please contact the postgraduate office.")))
                    ,@(when is-tutor-p
                            `(,@(when (notes student)
                                      `(((p :class :error) "Notes: " ,(notes student))))
                              (p ((a :href ,(format nil "../transcript/~A"
                                                    (studentid student)))
                                  "View Transcript"))))
                    (p "Please note that until any marks or grades shown
here are for indication only and may be subject to moderation either
by internal moderators or by the examination board. You will be
formally notified of marks by letter after the appropriate examination
board.")
                    ((section :title "Assessments")
                     ,(marks-table student is-tutor-p))
                    ((section :title "Module Final Marks")
                     ,(module-marks-table student is-tutor-p))
                    ,(let ((p (project student)))
                          (when p
                            `((section :title "Project")
                              (p
                               ((a :href ,(format nil "../projects/~A/" (projectid p)))
                                ,(title p))
                               ,@(let ((location (getf (description p) :location)))
                                      (when location
                                        (list ", " location)))))))
                    ((section :title "Examboard Decisions")
                     ,(examboard-decisions-table student))
                    ((section :title "Nomenclature")
                     ((dl :compact :compact)
                      (dt "Award")
                      (dd "indicates that the board has decided to recommend
the award of a qualification.180 Credits are required for an MSc, 120 Credits for
a PgD and 60 for a PgC.")
                      (dt "Proceed")
                      (dd "means that students can proceed with their course
or project, but there may possibly be repeat or referred exams
required which will be indicated.")
                      (dt "Withdraw")
                      (dd "means that the examboard is requesting the student
withdraw from the program.")
                      (dt "Refer")
                      (dd "A module was failed and a second attempt is allowed
to one or more components of the module assessment. The referred mark
will be capped to the pass mark for the module.")
                      (dt "Repeat")
                      (dd "The previous attempt to one or more components of
the module assessment are put aside and can be retaken as if a first
attempt (i.e. the mark will NOT be capped.")
                      (dt "Penalty")
                      (dd "A late submission penalty is subtracted from the
assessment mark as per the school guidelines")
                      (dt "Condone")
                      (dd "The Board of Examiners, in consideration of a
student's overall performance, recommends that credit be awarded for
part of a programme in which the student has failed to satisfy the
assessment criteria, on the grounds that the positive aspects of the
overall performance outweigh the area of failure.Condoned modules
carry a credit value and the actual mark obtained by the student.")
                      ))))))))))

(defun integer-sequence(start end)
  (assert (<= start end))
  (do((count start (1+ count))
      (result))
     ((> count end) (nreverse result))
    (push count result)))

(defmethod students-directory-handler((app grades-manager) request rest)
  (let ((is-tutor-p (has-permission :tutor app)))
    (multiple-value-bind(programmeid year form)
        (let* ((this-year (academic-year))
               (form `((form :method :post)
                       "Programme: "
                       ((mcq :style :dropdown :name :programmeid
                         :datatype (string :nil-allowed t))
                        (nil "All")
                        ,@(mapcar
                           #'(lambda(p) (list (programmeid p) (title p)))
                           (programme-records (db app))))
                       " Year: "
                       ((mcq :style :dropdown :name :year :datatype integer)
                        ,@(nreverse (integer-sequence 1997 (1+ this-year))))
                       ((input :type :submit :value "View Cohort"))))
               (form-data (form-data form request)))
          (values (getf form-data :programmeid nil)
                  (or (getf form-data :year) this-year)
                  (markup-form form request)))
      `(html
        (head (title "Directory"))
        (body
         (navbar ,(menu app) :on-url "students/" :relative "../")
         ((section :title "Directory"))
         ,form
         ,(let ((students
                 (sort
                  (copy-list
                   (if programmeid
                       (student-records (db app)
                                        'programmeid programmeid 'year year)
                       (student-records (db app) 'year year)))
                  #'string< :key #'username)))
               `((section :title
                  ,(format nil "~A (~A)"
                           (or programmeid "All Programmes")
                           year))
                 (table
                  ,@(mapcar
                     #'(lambda(student)
                         `(tr
                           (td ((a :href ,(concatenate 'string "mailto:"
                                                       (email-address student)))
                                ,(fullname student)))
                           (td
                            ,(let ((award-decision
                                    (find "Award" (examboard-decisions student)
                                          :test #'string-equal
                                          :key #'action)))
                                  (when award-decision
                                    (format
                                     nil "~A (~A)"
                                     (argument award-decision)
                                     (elt (multiple-value-list
                                           (decode-universal-time
                                            (start (examboard award-decision))))
                                          5)))))
                           (td ,(when (or is-tutor-p
                                          (equal (username student)
                                                 (username *current-user*)))
                                      `((a :href ,(format nil "../grades/~A"
                                                          (username student)))
                                        "Grades")))
                           (td ,(when is-tutor-p
                                      `((a :href ,(format nil "../transcript/~A"
                                                          (username student)))
                                        "Transcript")))
                           (td ,(let ((p (project student)))
                                     (when p
                                       `((a :href ,(format nil "../projects/~A"
                                                           (projectid p)))
                                         ,(title p)))))
                           ))
                     students))
                 ,@(when students
                         `(((section :title "Mailing List")
                            (p ,(format nil "~A~{, ~A~}"
                                        (email-address (first students))
                                        (mapcar #'email-address
                                                (rest students))))))))))))))


(defmethod assessment-handler((app grades-manager) request rest)
  "Top levvel staff assessments display handler - parse rest and call
method as required"
  (let* ((parts (when (> (length rest) 0)
                  (split-string rest 3 '(#\/ #\? #\#)
                                :remove-empty-subseqs t)))
         (assessmentid (first parts))
         (studentid (if (has-permission '(:tutor :admin) app)
                        (second parts)
                        (studentid *current-user*)))
         (assessment
          (when assessmentid
            (or (car (assessment-records (db app) 'assessmentid assessmentid))
                (throw 'response
                  (cons :not-found
                        (format nil "Assessment ~A" assessmentid))))))
         (student
          (when studentid
            (or (car (student-records (db app) 'studentid studentid))
                (throw 'response
                  (cons :not-found (format nil "Student ~A" studentid)))))))
    (cond
      ((> (length parts) 3) (throw 'response :not-found))
      (student (if (and (not (has-permission '(:tutor :admin) app))
                             (suspended student))
                   (redirect request "../grades/" 302)
                   (assessment-feedback app request assessment student)))
      (assessment (assessment-mark-handler app request assessment))
      (t (assessments-directory app request)))))

(defmethod assessments-directory((app grades-manager) request)
  "Display a directory of all a assessments for current tutor"
  (unless (has-permission '(:tutor :admin) app) (throw 'response :forbidden))
  (let*((marks-due (tutors-marks-due (db app)))
        (user-assessments
         (reduce #'append
                 (mapcar #'assessments
                         (module-records (db app)
                                         'owner (username *current-user*)))))
        (year 0)
        (moduleid ""))
    (flet ((assessment-row(assessment)
             `(,@(unless (= year (year assessment))
                         (setf moduleid "")
                         `((tr ((th :colspan 6)
                                ,(setf year (year assessment))))))
               ,@(unless (string= moduleid (moduleid assessment))
                         `((tr (td ,(setf moduleid (moduleid assessment)))
                            ((td :colspan 5)
                             ((a :href ,(format nil "../modules/~A/~A/"
                                                (moduleid assessment) (year assessment)))
                              ,(title (module assessment)))))))
               (tr
                ((td :align :right)
                 ,(format-percentage (weighting assessment)))
                (td ,(assessment-type assessment))
                (td ((a :href ,(format nil "~D/"
                                       (assessmentid assessment)))
                     ,(title assessment)))
                ((td :align :left)
                 ,(second (assoc (assessmentid assessment)
                                 marks-due))))))
           (cmp(a b)
             (or (> (year a) (year b))
                 (and (= (year a) (year b))
                      (string< (moduleid a) (moduleid b))))))
      `(html
        (head (title "Assessments Directory"))
        (body
         (navbar ,(menu app) :relative "../")
         ((section :title ,(format nil "Assessments directory for ~S"
                                   (username *current-user*)))
          ((section :title "Your Modules")
           (table
            ,@(mapcan #'assessment-row
                      (sort (copy-list user-assessments) #'cmp))))
          ((section :title "Other Assessments")
           (table
            ,@(mapcan #'assessment-row
                      (sort
                       (set-difference
                        (assessment-records (db app) 'assessmentid
                                            (mapcar #'car marks-due))
                        user-assessments
                        :test #'=
                        :key #'assessmentid)
                       #'cmp))))))))))

(defun summary-table(items)
  `(table
    ,@(mapcar
       #'(lambda(a) `(tr ((th :align :right :valign :top) ,(first a))
                      (td ,(second a))))
       items)))

(defun assessment-summary(assessment)
  (summary-table
   `(("Module" ,(format nil "~A: ~A" (moduleid assessment)
                        (title (module assessment))))
     ("Year" ,(year assessment))
     ("Description" ,(title assessment))
     ("Type" ,(assessment-type assessment))
     ("Weighting" ,(format-percentage (weighting assessment)))
     ("Deadline" ,(format-timestamp (deadline-date assessment)))
     ("Release Results" ,(format-timestamp (release-date assessment)))
     ("Feedback Available"
      ,(if (feedback-form assessment)
           "Yes."
           "No. Assessors should contact the system administrator if they want to provide feedback online"))  )))

(defmethod assessment-mark-handler((app grades-manager) request assessment)
  "Display and allow marking for a current assessment for staff member"
  (unless (has-permission '(:tutor :admin) app) (throw 'response :forbidden))
  (let* ((title (format nil "~A:~A (~A)"
                        (moduleid assessment)
                        (title assessment)
                        (assessment-type assessment)))
         (clsql::*default-caching* nil)
         (dates-required
          (and (deadline-date assessment)
               (not (equalp (assessment-type assessment) "EXAM"))))
         (fields `((mark :datatype
                    (number :nil-allowed t :min 0 :max 100 :format "~,1F")
                    :size 3)
                   ,@(when dates-required
                           `((submission-date :datatype (date :fmt :short :nil-allowed t)
                              :size 17
                              :value ,(deadline-date assessment))
                             (deadline-date :datatype (date :fmt :short :nil-allowed t)
                              :size 17
                              ,@(unless (has-permission :admin app)
                                        '(:disabled :disabled)))))
                   (note :datatype string :size 22))))
    (declare (special clsql::*default-caching*))
    (flet ((fieldname (prefix studentid)
             (intern (concatenate 'string (string prefix) studentid)
                     :keyword)))
      `(html
        (head (title ,title))
        (body
         (navbar ,(menu app) :relative "../../")
         ((section :title ,title)
          ,(assessment-summary assessment)
          (p "The deadline for student submission may be overridden
on an individual basis by the programme director where an extension is
grantedq. If a deadline is set then submission dates should be set for
each student,the mark entered should be the raw mark and penalties
will automatically be applied to these if the submission is
late. Dates are entered in UTC in the format 'YYYY/MM/DD HH:MM'")
          (p "If a release date is set the marks and feedback will be
made available to the students after this date. Any changes made after
the release date must be by the programme director.")
          (p "If tutors wishes to use an online feedback form to
provide feedback to students please contact the system administrator
for advisement.")
          ,(do-form
            ;; data
            #'(lambda()
                (mapcan
                 #'(lambda(mark)
                     (mapcan #'(lambda(field)
                                 (list (fieldname (car field)
                                                  (studentid mark))
                                       (slot-value mark (car field))))
                             fields))
                 (current-marks assessment)))
            ;; form
            #'(lambda()
                `((form :method :post)
                  ((table :border 1 :cellpadding 0 :cellspacing 0)
                   (tr ((th :colspan 2) "Student") (th "Mark")
                    ,@(when dates-required
                            `((th "Submitted (if late)" (br) "(yyyy/mm/dd hh:mm)")
                              (th "Extension" (br) "(yyyy/mm/dd hh:mm)")))
                    (th "Note") (th "Penalty"))
                   ,@(mapcar
                      #'(lambda(mark)
                          (let ((disabled
                                 (unless (and (can-edit app mark)
                                              (not (feedback-form mark)))
                                   :disabled)))
                            `(tr
                              (td ((a :href ,(studentid mark))
                                   ,(studentid mark)))
                              (td ,(if (equalp (assessment-type assessment)
                                               "EXAM")
                                       (candidate (student mark))
                                       (fullname (student mark))))
                              ,@(mapcar
                                 #'(lambda(field)
                                     `((td :align :right :valign :top)
                                       ((input
                                         :name ,(fieldname (car field)
                                                           (studentid mark))
                                         :value ,(slot-value mark (car field))
                                         ,@(rest field)
                                         :disabled ,disabled))))
                                 fields)
                              ((td :align :right)
                               ,(format-percentage (late-penalty mark))))))
                      (sort (copy-list (current-marks assessment)) #'string<
                            :key #'(lambda(m) (username (student m))))))
                  ((input :type :submit :name :update-marks
                    :value "Update Marks"))))
            ;; disabled-p
            nil
            ;; updatefunc
            #'(lambda(data)
                (dolist(mark (current-marks assessment))
                  (when (can-edit app mark)
                    (apply
                     #'modify-mark
                     (cons mark
                           (mapcan
                            #'(lambda(f)
                                (let ((data (getf data
                                                  (fieldname (car f)
                                                             (studentid mark))
                                                  :none)))
                                  (unless (eq data :none)
                                    (list (intern (string (car f)) :keyword)
                                          data))))
                            fields)))))
                (slot-makunbound assessment 'marks))
            request)))))))

(defun mark-summary(app mark)
  (summary-table
   `(("Raw Mark" ,(format-percentage (mark mark) 1))
     ("Late Penalty" ,(format-percentage (late-penalty mark)))
     ("Attempt" ,(attempt mark))
     ("Revision" ,(revision mark))
     ("Modified By" ,(modified-by mark))
     ("Modified" ,(format-timestamp (modified mark)))
     ,(when (submission-date mark)
            `("Submitted" ,(format-timestamp (submission-date mark))))
     ("Deadline" ,(format-timestamp (deadline-date mark)))
     ,(when (has-permission '(:tutor :admin) app)
            `("Note" ,(note mark))))))

(defmethod assessment-feedback((app grades-manager) request
                               assessment student)
  "Display the assessment feedback form and other info for given student and
assessment"
  (let* ((clsql::*default-caching* nil)
         (marks (sort (mark-records (db app)
                                    'studentid (studentid student)
                                    'assessmentid (assessmentid assessment))
                      #'mark-priority>))
         (mark (car marks)))
    (declare (special clsql::*default-caching*))
    (unless mark
      (throw 'response
        (cons :not-found
              (format nil
                      "Mark for student ~S on assessment ~A not found."
                      (studentid student) (assessmentid assessment)))))
    ;; filter out latest revisions only for students
    (unless (has-permission '(:tutor :admin) app)
      (unless (feedback-available-p mark)
        (throw 'response
          (cons :forbidden
                (format nil
                        "Feedback for assessment ~A will not be made available."
                        (assessmentid assessment)))))
      (unless (string= (username *current-user*)
                       (username student))
        (throw 'response :forbidden))
      (setf marks
            (let ((rev 0))
              (mapcan #'(lambda(m) (unless (= rev (revision m))
                                     (setf rev (revision m))
                                     (list m)))
                      marks))))
    (let* ((student (student mark))
           (title (format nil "~A: \"~A\" mark for ~A (~A)"
                          (moduleid assessment)
                          (title assessment)
                          (fullname student)
                          (studentid student)))
           (feedback-form (feedback-form assessment))
           (preview-p (inet.http:form-values "Preview" request)))
      `(html
        (head (title ,title))
        (body
         (navbar ,(menu app) :relative "../../")
         ((section :title ,title)
          ,(assessment-summary assessment)
          ,(when (and (can-edit app mark)  (not (submission-date mark)))
              (do-form
                 (list :date (submission-date mark))
                `((form :method :post)
                   (table
                    (tr ((th :align :right :valign :top) "Submitted")
                        (td
                         ((input :datatype (date :fmt :short :nil-allowed t)
                                 :name :date)))
                        (td ((input :type :submit
                                    :name :enter-submission-date
                                    :value "Set Submission Date"))))))
                (or (not (can-edit app mark))  (submission-date mark))
                #'(lambda(data)
                    (let ((date (getf data :date)))
                      (when date
                        (setf (submission-date mark) date)
                        (update-records-from-instance mark))))
                request))          ,@(if (can-view app mark)
                `( ,(do-form
                    #'(lambda() (feedback (car marks)))
                    #'(lambda() `(div (hr)
                                  ,(mark-summary app (car marks))
                                  ,@(when preview-p
                                      `(((p :class :error)
                                         "This is a preview only. The calculated mark of "
                                         ,(format nil "~,1f"
                                                  (* 100 (form-mark
                                                          (form-data
                                                           feedback-form
                                                           request)
                                                          feedback-form)))
                                         "% has not been stored. You must go back and (press submit to finish the assessment and store the mark." )))

                                  ,feedback-form))
                    (unless (can-edit app mark) t)
                    #'(lambda(data)
                        (let ((m (form-mark data feedback-form)))
                            (let ((newmark
                                   (modify-mark
                                    mark
                                    :feedback data
                                    :mark (unless preview-p
                                            (if m (* 100 m) (mark mark))))))
                              (unless (eql newmark mark)
                                (setf (cdr marks) (cons (car marks) (cdr marks))
                                      (car marks) newmark)))))
                    request)

                  ,@(mapcar
                     #'(lambda(mark)
                         (do-form (feedback mark)
                           `(div (hr)
                             ,(mark-summary app mark) ,feedback-form) :text))
                     (cdr marks)))
                '((p "Feeback form for this assessment not available")))))))))

(defmethod module-handler((app grades-manager) request rest)
  "Top levvel modules display handler - parse rest and call
method as required. Takes moduleid and either a year or studentid"
  (let* ((parts (when (> (length rest) 0)
                  (split-string rest 3 '(#\/ #\? #\#)
                                :remove-empty-subseqs t)))
         (tutor-p (has-permission '(:tutor :admin) app))
         (moduleid (first parts))
         (subdir (second parts))
         (student
          (car (student-records (db app) 'studentid
                                (cond (tutor-p subdir)
                                      ((studentid *current-user*))
                                      (t (throw 'response :forbidden))))))
         (year (if (and tutor-p (not student))
                   (or (when subdir (parse-integer subdir :junk-allowed t))
                       (academic-year))
                   (year student)))
         (module (car (module-records (db app)
                                      'year year 'moduleid moduleid))))
    (if module
        (if student
            (if (and (not (has-permission '(:tutor :admin) app))
                     (suspended student))
                (redirect request "../grades/" 302)
                (module-mark-handler app module student))
            (module-report-handler app module))
        (redirect request "../assessments/"))))

(defun module-summary(app module)
  (summary-table
   `(("Module Code" ,(moduleid module))
     ("Title" ,(title module))
     ("Year" ,(year module))
     ("Credits" ,(credits module))
     ("Pass Mark" ,(format-percentage (pass-mark module)))
     ("Owner" ,(owner module))
     ("Assessments"
      (table
       ,@(mapcar
          #'(lambda(a)
              `(tr
                ((td :align :right) ,(format-percentage (weighting a)))
                (td ,(assessment-type a))
                (td ,(if (has-permission '(:tutor :admin) app)
                         `((a :href ,(format nil "../../assessments/~D"
                                             (assessmentid a)))
                           ,(title a))
                         (title a)))))
          (assessments module)))) )))

(defun module-mark-summary(app mark)
  (summary-table
   `(("Allocated Mark" ,(when (mark mark) (format-percentage (mark mark) 0)))
     ,@(when (has-permission '(:tutor :admin) app)
             `(("Calculated Mark"  ,(format-percentage (calculated-mark mark) 1))))
     ("Attempt" ,(attempt mark))
     ("Revision" ,(revision mark))
     ("Modified By" ,(modified-by mark))
     ("Modified" ,(format-timestamp (modified mark)))
     ("Condoned" ,(if (condonedp mark) "Yes" "No"))
     ,(when (has-permission '(:tutor :admin) app)
            `("Note" ,(note mark))))))

(defmethod module-mark-handler((app grades-manager) module student)
  "Display the module mark history for given student and
module"
  (unless (or (has-permission '(:tutor :admin) app)
              (string-equal (username student) (username *current-user*)))
    (throw 'response :forbidden))
  (let* ((marks (sort (module-mark-records (db app)
                                           'studentid (studentid student)
                                           'moduleid (moduleid module))
                      #'mark-priority>))
         (mark (car marks)))
    (unless mark
      (throw 'response
        (cons :not-found
              (format nil
                      "Module mark for student ~S on module ~A not found."
                      (studentid student) (moduleid module)))))
    ;; filter out latest revisions only for students
    (unless (has-permission '(:tutor :admin) app)
      (setf marks
            (let ((rev 0))
              (mapcan #'(lambda(m) (unless (= rev (revision m))
                                     (setf rev (revision m))
                                     (list m)))
                      marks))))
    (let ((title (format nil "~A:~A module mark for ~A (~A)"
                         (moduleid module)
                         (title module)
                         (fullname student)
                         (studentid student))))
      `(html
        (head (title ,title))
        (body
         (navbar ,(menu app) :relative "../../")
         ((section :title ,title)
          ,(module-summary app module)
          ,@(mapcar
             #'(lambda(mark) `(div (hr) ,(module-mark-summary app mark)))
             marks)))))))

(defmethod module-report-handler((app grades-manager) module)
  (unless (has-permission '(:tutor :admin) app)
    (throw 'response :forbidden))
  (let* ((clsql::*default-caching* nil)
         (assessments (sort (assessments module) #'> :key #'weighting))
         (module-marks
          (let((h (make-hash-table :test #'equal)))
            (dolist(mark (current-module-marks module))
              (setf (gethash (studentid mark) h) mark))
            h))
         (students
          (let ((markset  (current-module-marks module)))
            (dolist(a assessments)
              (setf markset (append markset (current-marks a))))
            (sort
             (student-records (db app) 'studentid
                              (delete-duplicates
                               (mapcar #'studentid markset)
                               :test #'string=))
             #'string< :key #'username)))
         (marks (mapcar
                 #'(lambda(assessment)
                     (let((h (make-hash-table :test #'equal)))
                       (dolist(mark (current-marks assessment))
                         (setf (gethash (studentid mark) h) mark))
                       h))
                 assessments))
         (title (format nil "~A:~A ~A"
                        (moduleid module) (title module) (year module))))
    (declare (special clsql::*default-caching*))
    `(html
      (head (title ,title))
      (body
       (navbar ,(menu app) :relative "../../")
       ((section :title ,title)
        ,(module-summary app module)
        (hr)
        ((table :border 1 :cellpadding 0 :cellspacing 0)
         (tr ((th :colspan 2) "Student")
             ,@(mapcar
                #'(lambda(a)
                    `((th)
                      ,(title a) (br)
                      ,(assessment-type a) (br)
                      ,(format-percentage (weighting a))))
                assessments)
             (th "Total")
             (th "Allocated Mark"))
         ,@(mapcar
            #'(lambda(student)
                (let ((module-mark
                       (gethash (studentid student) module-marks)))
                  `(tr
                    (td ,(studentid student))
                    (td ,(fullname student))
                    ,@(mapcar
                       #'(lambda(h)
                           (let ((mark (gethash (studentid student) h)))
                             `((td :align :right)
                               ,(when (and mark (> (attempt mark) 1)) "R")
                               ,(format-percentage
                                 (when mark (mark mark)) 1))))
                       marks)
                    ((td :align :right)
                     ,(format-percentage
                       (when module-mark (calculated-mark module-mark))
                       1))
                    ((td :align :right)
                     ,(format-percentage
                       (when module-mark (mark module-mark)) 1))
                    (td
                     ,@(when module-mark
                             (list
                              (when (> (attempt module-mark) 1) "Referred. ")
                              (when (condonedp module-mark) "Condoned. ")))))))
            students)
         (tr
          ((th :colspan 2 :align :right) "Averages")
          ,@(mapcar
             #'(lambda(a)
                 `((td :align :right)
                   (em
                    ,(format-percentage
                      (average-mark (current-marks a)) 1))))
             assessments)
          ((td :align :right)
           (em ,(format-percentage
                 (average-mark (current-module-marks module)
                               :mark #'calculated-mark
                               :weight #'(lambda(a) (declare (ignore a)) 1))
                 1)))
          ((td :align :right)
           (em ,(format-percentage
                 (average-mark (current-module-marks module)
                               :mark #'mark
                               :weight #'(lambda(a) (declare (ignore a)) 1))
                 1))))))))))


(defmethod spreadsheet-handler((app grades-manager) request rest)
  (unless (has-permission '(:tutor :admin) app)
    (throw 'response :forbidden))
  (multiple-value-bind(programmeid year form format expand)
      (let* ((this-year (academic-year))
             (form `((form :method :post)
                     "Programme: "
                     ((mcq :style :dropdown :name :programmeid
                       :datatype (string :nil-allowed t))
                      (nil "All")
                      ,@(mapcar
                         #'(lambda(p) (list (programmeid p) (title p)))
                         (programme-records (db app))))
                     " Year: "
                     ((mcq :style :dropdown :name :year :datatype integer)
                      ,@(nreverse (integer-sequence 1997 (1+ this-year))))
                     ((mcq :style :dropdown :name :format :value :html
                       :datatype symbol) :html :latex :update)
                     ((input :type :submit :value "View"))))
             (form-data (form-data form request))
             (saved (property (user-component-properties
                               app  *current-user*) :spreadsheet)))
        (setf (getf saved :format) :html)
        (loop for a on form-data by #'cddr
              unless (cadr a)
              do (setf (cadr a) (getf saved (car a))))
        (setf (property (user-component-properties
                         app  *current-user*) :spreadsheet)
              form-data)
        (values (getf form-data :programmeid nil)
                (or (getf form-data :year) this-year)
                (markup-form form form-data)
                (getf form-data :format :html)
                (query-values "expand" request)))
    (let ((title
           (format nil "Spreadsheet ~A ~A "
                   (title (car (programme-records (db app)
                                                  'programmeid programmeid)))
                   year)))
      (case format
        (:latex
         (make-instance
          'response
          :content-type "text/x-tex"
          :content
          (format nil "
\\documentclass[a4paper,landscape,10pt]{article}
\\usepackage[dvips]{color}
\\setlength{\\topmargin}{-1.5in}
\\setlength{\\oddsidemargin}{0in}
\\setlength{\\textwidth}{20cm}
\\setlength{\\textheight}{26cm}
\\begin{document}
\\pagestyle{plain}
\\section*{~A}

~A

\\flushright ~A
\\end{document}
% print using e.g. dvips -t landscape -x 600 filename.dvi
"
                  title
                  (multiple-value-bind(columns rows avg)
                      (spreadsheet-data (db app) programmeid year expand)
                    (spreadsheet-table format columns rows avg expand))
                  (format-timestamp (get-universal-time)))))
        (t
         `(html
           (head (title "Spreadsheet"))
           (body
            (navbar ,(menu app) :on-url "spreadsheets/" :relative "../")
            ((section :title ,title)
             ,form
             ,@(when programmeid
                     (multiple-value-bind(columns rows avg)
                         (spreadsheet-data (db app) programmeid year expand)
                       (spreadsheet-table format columns rows avg expand))))
            ((p :align :right) ,(format-timestamp (get-universal-time))))))))))

(defstruct spreadsheet-row
  (student nil :type (or student null))
  (marks nil :type list)
  (avg 0.0 :type real)
  (decision nil :type (or examboard-decision null)))

(defun spreadsheet-data(db programmeid year &optional expand)
  (let* ((students (student-records db 'programmeid programmeid 'year year))
         (assessments
          (delete-duplicates
           (mapcar #'assessment
                   (mapcan #'(lambda(s)
                               (update-instance-from-records s)
                               (copy-list (marks s)))
                           students))
           :key #'assessmentid))
         (modules
          (sort
           (delete-duplicates
            (mapcar #'module
                    (mapcan
                     #'(lambda(s)
                         (slot-makunbound s 'module-marks)
                         (dolist(m (module-marks s))
                           (unless (module m)
                             (format t "s=~A m=~S~%" s m)))
                         (copy-list (module-marks s)))
                     students))
            :key #'moduleid :test #'equal)
           #'string< :key #'moduleid))

;          (sort (copy-list
;                 (programme-modules
;                  (car (programme-records db 'programmeid programmeid))
;                  year))
;                #'string< :key #'moduleid))
         (mark-columns
          (mapcar
           #'(lambda(m)
               (if (member (moduleid m) expand :test #'string-equal)
                   (cons
                    m
                    (sort (mapcan
                           #'(lambda(a)
                               (when (and (equal (moduleid a) (moduleid m))
                                          (> (weighting a) 0)) (list a)))
                           assessments)
                          #'> :key #'weighting))
                   (list m)))
           modules))
         (current-marks
          (mapcar
           #'(lambda(entry)
               (append
                (list (mapcar #'update-instance-from-records (current-module-marks (car entry))))
                (mapcar #'(lambda(a) (mapcar #'update-instance-from-records (current-marks a))) (rest entry))))
           mark-columns))
         (students-rows
          (mapcar
           #'(lambda(s)
               (make-spreadsheet-row
                :student s
                :decision (last-examboard-decision s)))
           students))
         (averages
          (mapcar
           #'(lambda(m)
               (mapcar
                #'(lambda(marks)
                    (average-mark
                     marks
                     :ignore-nil t
                     :ignore-0 t
                     :mark #'(lambda(m) (or (mark m)
                                            (when (typep m 'module-mark)
                                              (calculated-mark m))))
                     :weight #'(lambda(m) (declare (ignore m)) 1)))
                m))
           current-marks)))
    ;; fill in marks for students and calculate overall average
    (dolist(s students-rows)
      (let ((studentid (studentid (spreadsheet-row-student s))))
        (setf (spreadsheet-row-marks s)
              (mapcar
               #'(lambda(entry)
                   (mapcar
                    #'(lambda(marks)
                        (find studentid marks
                              :key #'studentid :test #'string-equal))
                    entry))
               current-marks)))
      (setf (spreadsheet-row-avg s)
            (average-mark
             (delete-if #'null
                        (mapcar #'car (spreadsheet-row-marks s)))
             :ignore-nil nil
             :ignore-0 nil
             :mark #'(lambda(m)  (or (mark m) (calculated-mark m)))
             :weight #'(lambda(m)

                         (credits
                          (find (moduleid m) modules
                                :key #'moduleid :test #'string-equal))))))
    ;; sort in decreasing order
    (setf students-rows (sort students-rows #'> :key #'spreadsheet-row-avg))
    (values mark-columns students-rows averages)))

(defmethod spreadsheet-table((format (eql :html)) columns rows averages expand)
  (let ((footnotes nil)
        (footnote nil))
    (labels ((add-footnote(txt)
               (setf footnote (cons txt (delete txt footnote :test #'equalp))))
             (mark-entry(m)
               (if m
                   (let* ((modulep (typep m 'module-mark))
                          (v (or (mark m) (and modulep (calculated-mark m))))
                          (p (if modulep 0 (late-penalty m)))
                          (style
                           (format
                            nil
                            "{~:[color: red;~;~]~:[text-decoration: underline;~;~]}"
                            (or (passedp m)
                                (when modulep
                                  (>= v (pass-mark (module m)))))
                            (mark m)))
                          (f-p nil))
                     (when (note m) (add-footnote (note m)) (setf f-p t))
                     (when (> (attempt m) 1)
                       (add-footnote
                        (format nil "Referred~@[ (~,0F)~]" (previous-mark m)))
                       (setf f-p t))
                     (when (/= 0 p)
                       (add-footnote
                        (format nil "~D days late, penalty: ~D%"
                                (days-late m) p))
                       (setf f-p t))
                     `((td :align :right)
                       ,(when f-p `(sup ,(1+ (length footnotes))))
                       ((span  :style ,style)
                        ,(format-percentage (when v (+ p v)) 1 nil))))
                   '((td)))))
      `(((table :class "spreadsheet" :border 1 :cellpadding 0 :cellspacing 0)
         (tr (th "SUN") (th "Name")
          ,@(mapcar ;; module titles
             #'(lambda(entry)
                 (let* ((moduleid (moduleid (car entry)))
                        (expansions
                         (if (member moduleid expand :test #'string-equal)
                             (remove moduleid expand :test #'string-equal)
                             (cons moduleid expand))))
                   `((th :colspan ,(length entry)
                      :title ,(title (car entry)))
                     ((a :href
                       ,(format nil "?~{expand=~A&~}" expansions)
                       :title ,(title (car entry)))
                      ,moduleid))))
             columns)
          (th "Avg") ((th :colspan 2) "Decision"))
         (tr ((th :colspan 2))
          ,@(mapcan ;; a(> (attempt m) 1)ssessment titles
             #'(lambda(entry)
                 (append
                  (mapcar #'(lambda(a)
                              `((th :title ,(format nil "~D ~A"
                                                    (assessmentid a)
                                                    (title a)))
                                ,(assessment-type a)
                                " (" ,(format-percentage (weighting a))
                                ")"))
                          (rest entry))
                  (list '(th ""))))
             columns)
          ((th :colspan 3)))
         ,@(mapcar ;; rows
            #'(lambda(row)
                (let ((decision (spreadsheet-row-decision row)))
                  `(tr
                    (td ,(studentid (spreadsheet-row-student row)))
                    ((th :align :left)
                     ,(fullname (spreadsheet-row-student row)))
                    ,@(mapcan
                       #'(lambda(entry)
                           (setf footnote nil)
                           (let* ((m (car entry))
                                  (markup
                                   (append
                                    (mapcar
                                     #'(lambda(a) (mark-entry a))
                                     (rest entry))
                                    (list (mark-entry m)))))
                             (when footnote
                               (push (format nil "~{~A.~}"
                                             (reverse footnote))
                                     footnotes))
                             markup))
                       (spreadsheet-row-marks row))
                    (th ,(format-percentage (spreadsheet-row-avg row) 1 nil))
                    ,(if decision
                         `((td :title
                              ,(format nil "~D: ~A"
                                       (examboardid decision)
                                       (format-datestamp
                                        (start (examboard decision)))))
                          ,(action decision) " "
                          ,(argument decision) " "
                          ,(when (notes decision)
                                 (push (notes decision) footnotes)
                                 `(sup ,(length footnotes))) )
                         `(td "No examboard decision")))))
            rows)
         (tr
          ((th :colspan 2) "Cohort Averages")
          ,@(mapcan
             #'(lambda(m)
                 (mapcar
                  #'(lambda(avg)
                      `((th :align :right) ,(format nil "~,1F" avg)))
                  (append (rest m) (list (car m)))))
             averages)))
        ,@(let* ((footnotes (nreverse footnotes))
                 (s (ceiling (length footnotes) 3)))
                `((table
                   (tr
                    ,@(mapcar
                       #'(lambda(idx)
                           (let* ((start (* idx s))
                                  (end (min (+ start s) (length footnotes))))
                             (when (< start (length footnotes))
                               `((td :width "33%" :valign :top)
                                 ((ol :start ,(1+ start))
                                  ,@(mapcar
                                     #'(lambda(f) (list 'li f))
                                     (subseq footnotes start end )))))))
                           '(0 1 2))))))  ))))

(defmethod spreadsheet-table((format (eql :latex))
                             columns rows averages expand)
  (let ((footnotes nil)
        (footnote nil))
    (labels ((add-footnote(txt)
               (setf footnote (cons txt (delete txt footnote :test #'equalp))))
             (format-percentage(v) (format nil "~@[~,1F~]" v))
             (mark-entry(m)
               (if m
                   (let* ((modulep (typep m 'module-mark))
                          (v (or (mark m) (and modulep (calculated-mark m))))
                          (p (if modulep 0 (late-penalty m)))
                          (result (format-percentage (when v (+ p v))))
                          (f-p nil))
                     (when (not (mark m))
                       (setf result (format nil "\\overline{~A}" result)))
                     (when (note m) (add-footnote (note m)) (setf f-p t))
                     (when (> (attempt m) 1)
                       (add-footnote (format nil "Referred~@[ (~,1F)~]"
                                             (previous-mark m)))
                       (setf f-p t))
                     (when (/= 0 p)
                       (add-footnote
                        (format nil "~D days late, penalty: ~D\\%"
                                (days-late m) p))
                       (setf f-p t))
                     (when f-p
                       (setf result
                             (format nil "^{~D} ~A" (1+ (length footnotes))
                                     result)))
                     (setf result (format nil "$~A$" result))
                     (when (not (or (passedp m) (when modulep (>= v (pass-mark (module m))))))
                       (setf result (format nil "\\textcolor{red}{~A}" result)))
                     result)
                   " ")))
      (format
       nil
       "
\\begin{tabular}{||ll|~{|~{~*r~}~}||r||p{7cm}||}\\hline\\hline

% module headings
SUN & Name~%~{ ~A ~%~} & \\multicolumn{1}{c||}{Avg} & Decision\\\\\

% assessment headings
 & ~%~{~{& ~A~} &~%~} & & \\\\\\hline\\hline

% rows
~{ ~A & ~A~%~{& ~A~%~} & $~A$ & ~A ~A ~@[$^{~D}$~]\\\\~%~}\\hline\\hline

% averages
\\multicolumn{2}{||r||}{Cohort Averages}
~{ ~{& ~,1F~}~} & &\\\\
\\hline\\hline\\end{tabular}

% footnotes
\\begin{tabular}{p{8cm}p{8cm}p{8cm}}
~{~A~}\\\\
\\end{tabular}
"
       columns                          ; column defs
       (mapcar                          ; module headings
        #'(lambda(c)
            (format nil "& \\multicolumn{~D}{c|~:[~;|~]}{~A}"
                    (length c)
                    (eql c (car (last columns)))
                    (moduleid (car c))))
        columns)
       (mapcar                          ; assessment headings
        #'(lambda(c)
            (mapcar #'(lambda(a)
                        (format nil  "\\multicolumn{1}{p{3em}}{~A (~A)}"
                                (assessment-type a)
                                (format-percentage (weighting a))))
                    (rest c)))
        columns)
       (mapcan                          ; rows
        #'(lambda(row)
            (let ((decision (spreadsheet-row-decision row))
                  (student (spreadsheet-row-student row)))
              (list
               (studentid student)
               (fullname student)
               (mapcan
                #'(lambda(entry)
                    (setf footnote nil)
                    (let* ((m (car entry))
                           (markup
                            (append
                             (mapcar #'(lambda(a) (mark-entry a)) (rest entry))
                             (list (mark-entry m)))))
                      (when footnote
                        (push (format nil "~{~A.~}"
                                      (reverse footnote))
                              footnotes))
                      markup))
                (spreadsheet-row-marks row))
               (format-percentage (spreadsheet-row-avg row))
               (if decision (action decision) "No recommendation")
               (if decision (argument decision) "")
               (when (and decision (notes decision))
                 (push (notes decision) footnotes)
                 (length footnotes))) ))
        rows)
       (mapcar #'(lambda(m) (append (rest m) (list (car m)))) averages)
       (let* ((footnotes (nreverse footnotes))
              (s (ceiling (length footnotes) 3)))
         (flet((col(idx)
                 (let* ((start (* idx s))
                        (end (min (+ start s) (length footnotes))))
                   (when (< start (length footnotes))
                     (format nil
                             "\\begin{enumerate}
\\setlength{\\itemsep}{-\\parsep}
\\setcounter{enumi}{~D}
~{\\item ~A~%~}
\\end{enumerate}~%"
                             start
                             (subseq footnotes start end))))))
           (list (col 0) " & " (col 1) " & " (col 2))))
       ))))

(defmethod spreadsheet-table((format (eql :update))
                             columns rows averages expand)
  (dolist(row rows)
    (dolist(entry (spreadsheet-row-marks row))
      (let ((m (car entry)))
        (when (and m (not (mark m)) (> (calculated-mark m) 0)
                   (member (moduleid m) expand :test #'string-equal))
          (format t "Setting ~S <- ~S~%" m
                  (if (> (attempt m) 1)
                      (pass-mark (module m))
                      (calculated-mark m)))
          (setf (mark m)
                (if (> (attempt m) 1)
                    (pass-mark (module m))
                    (calculated-mark m)))
          (update-records-from-instance  m)))))
  (spreadsheet-table :html columns rows averages expand))

(defun student-deadlines(nodays &optional (year (academic-year)))
  "Returns a hashtable keyed by student username containing a list of
deadlines up to nodays ahead for that student. Each deadline is a list
of the deadline date, moduleid and title, assessment title and type."
  ;; cannot do by simple query as deadliens may be set in mark or assessment
  (let* ((modules (module-records *db* 'year year))
         (assessments (mapcan #'assessments modules))
         (now (get-universal-time))
         (marks (delete-if
                 #'(lambda(mark)
                     (or (not (deadline-date mark))
                         (submission-date mark)
                         (> (deadline-date mark) (+ now (* nodays 24 60 60)))))
                 (mapcan #'marks assessments)))
         (records (make-hash-table :test #'equal)))
    (dolist(mark marks)
      (push
       (list
        (deadline-date mark)
        (moduleid (module mark))
        (title (module mark))
        (title (assessment mark))
        (assessment-type (assessment mark)))
       (gethash (username (student mark)) records)))
    records))

(defun staff-deadlines(nodays &optional (year (academic-year)))
  "Returns a hashtable keyed by accessor name containing a list of
deadlines up to nodays ahead for that staff. Each deadline is a list
of the student username, deadline date, moduleid and title, assessment
title and type."
   (let* ((modules (module-records *db* 'year year))
          (assessments (mapcan #'assessments modules))
          (now (get-universal-time))
          (marks (delete-if #'(lambda(mark) (or (not (deadline-date mark))
                                                (submission-date mark)
                                                (> (deadline-date mark)
                                                   (+ now (* nodays 24 60 60)))))
                            (mapcan #'marks assessments)))
          (records (make-hash-table :test #'equal)))
    (dolist(mark marks)
      (when (student mark)
        (push
         (list
          (username (student mark))
          (deadline-date mark)
          (moduleid (module mark))
          (title (module mark))
          (title (assessment mark))
          (assessment-type (assessment mark)))
         (gethash (assessor (assessment mark)) records))))
      records))

;; '059930086'
