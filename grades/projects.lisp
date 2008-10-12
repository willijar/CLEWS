;; $Id: projects.lisp,v 1.1 2006/09/08 06:33:54 willijar Exp willijar $
;; Projects Application and Handling
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

(defclass projects-manager (application)
  ((title :initform "Projects" :initarg :title :reader title
          :documentation "Identifying title for these projects")
   (db :accessor db :initarg :db
       :documentation "Database holding projects")
   (description-form
    :reader description-form
    :initform "project-description"
    :initarg :description-form
    :documentation "Project Description Form")
   (moduleid :initarg :moduleid :reader moduleid :initform "EE4006"
             :documentation "Module associated with the projects")
   (student-url :reader student-url
                :initform nil :initarg :student-url
                :documentation "Function which given a user object will return
a url to the student details."))
  (:default-initargs
      :id :clews.projects
    :acl '((:view . (:all))
           (:student . (:student))
           (:supervisor . (:tutor :staff))
           (:tutor  . (:tutor :staff))
           (:admin . (:admin))))
  (:documentation "Class for project management system"))

(defmethod has-permission((action (eql :student)) (app projects-manager)
                          &optional (entity *current-user*))
 (let ((student
         (first (student-records (db app) 'username (username entity)))))
    (when student
      (if (suspended student)
        (throw 'response
          '(:forbidden . "You are on sanctions - please contact the office."))
        t))))

(defmethod assessor-role((p project) (a assessment))
  "Return the assessors role for given assessment on project"
  (cond ((or (string= (assessment-type a) "SUP")
             (string= (title a) "Dissertation 1")
             (string= (moduleid a) "EE4019"))
         (supervisors p))
        (t :tutor)))

(defmethod assessor-role((p project) (m mark))
  (assessor-role p (assessment m)))

(defmethod published-methods ((app projects-manager) baseurl)
  (declare (ignore baseurl))
  `(,@(call-next-method)
    ("add/" ,#'add-project-handler :stage :response :match :exact)
    ("choices" ,#'student-choices-handler :stage :response :match :exact)
    ("all" ,#'all-project-handler :stage :response :match :exact)
    ("delete/" ,#'delete-project-handler :stage :response :match :prefix)
    ("allocate/" ,#'allocate-projects-handler
     :stage :response :match :exact :display-plugins-p nil :role :admin)
    ("assessments/" ,#'allocate-assessments-handler
     :stage :response :match :exact :display-plugins-p nil :role :admin)
    ("scores/" ,#'allocate-scores-handler
     :stage :response :match :exact :display-plugins-p nil :role :admin)
    ("marks/" ,#'marks-handler
     :stage :response :match :prefix :display-plugins-p nil :role :admin)))

(defmethod student-anchor((app projects-manager) user)
  "Return the html anchor pointing to a students details"
  (if (student-url app)
      `((a :href ,(funcall (student-url app) user))
        ,(display-name user))
      (display-name user)))

(defmethod store-project (project (app projects-manager))
  "Store project, updating changes to project and user"
  (clsql:update-records-from-instance project)
  (dolist(m (marks project))
    (setf (deadline-date m) (deadline-date project)
          (submission-date m) (submission-date project))
    (clsql:update-records-from-instance m)))

(defmethod get-project((id integer) (app projects-manager))
  (car (project-records (db app) 'projectid id)))

(defmethod get-project((username string) (app projects-manager))
  (let ((student (car (student-records (db app) 'username username))))
    (when student (project student))))

(defmethod rem-project(project (app projects-manager))
  (when (studentid project)
    (error "Project Deletion attempted for allocated project ~A" project))
  (clsql:delete-instance-records project))

(defun user-choices(app role &optional (test #'identity))
  "Produce a list of users selections for an MCQ or MAQ"
  (sort (mapcan
         #'(lambda(username)
             (let ((user (get-dictionary username (users app))))
               (when (funcall test user)
                 (list (cons username (format nil "~A~:[ ~S~;~]"
                                              username
                                              (equal username (display-name user))
                                              (display-name user)))))))
         (delete-duplicates
          (etypecase role
            (list role)
            (symbol (get-users role app)))
          :test #'equal))
        #'string< :key #'car))

(defun studentid-choices(app &optional (test #'identity))
  (mapcan
   #'(lambda(student)
       (when (funcall test student)
         (list (cons (studentid student) (fullname student)))))
   (sort (student-records (db app) 'username
                          (get-users :student app))
         #'string< :key #'lastname)))

(defmethod status-form((app projects-manager) project)
  "Return the status form object for a project"
  `((form :method :post)
    (table
     (tr
      ((th :align :right) "Student")
      (td ((mcq :name :studentid :style :dropdown
                :datatype (string :nil-allowed t))
           (nil . "None")
           ,@(when (and project (studentid project))
                   (list (cons (studentid project) (fullname (student project)))))
           ,@(mapcar
              #'(lambda(s)
                  (cons (studentid s)
                        (format nil "~A (~A~A)"
                                (fullname s)
                                (programmeid s)
                                (year s))))
              (sort
               (students-without-projects (db app))
               #'string< :key #'username))))
      (td "Student to whom project is allocated"))
     (tr
      ((th :align :right :valign :top) "Supervisor(s)")
      (td ((maq :name :supervisors :size 5 :style :dropdown
                :datatype (string :nil-allowed t))
           ,@(user-choices app  :supervisor)))
      (td "List of supervisors and tutors allocated to this project"))
     (tr
      ((th :align :right) "Start Date")
      (td ((input :name :start-date :size 35
                  :value ,(jarw.parse:parse-time "2007-04-04 09:00")
                  :datatype (jarw.parse:date :nil-allowed t :fmt :short))))
      (td "Date student started project"))
     (tr
      ((th :align :right) "Submission Deadline")
      (td ((input :name :deadline-date :size 35
                  :value ,(jarw.parse:parse-time "2007-09-29 17:00")
                  :datatype (jarw.parse:date :nil-allowed t :fmt :short))))
      (td "Submission deadline for project including any extensions"))
     (tr
      ((th :align :right) "Submitted")
      (td ((input :name :submission-date :size 35
                  :datatype (jarw.parse:date :nil-allowed t :fmt :short) )))
      (td "Date student submitted project"))
     ,@(mapcan
        #'(lambda(module-mark)
            (when (can-view app module-mark)
              `(tr ((th :align "right")
                    ,(moduleid module-mark) " " ,(title (module module-mark)))
                   (td ,(format-percentage (mark module-mark))))))
        (module-marks project)))
    ((input :type :submit :name :submit :value "Update Status"))))

;;; permissions for current user
(defmethod can-add((app projects-manager) (class (eql 'project))
                   &optional (user *current-user*))
  (has-permission '(:supervisor :admin :tutor) app user))

(defmethod can-view(app (project project) &optional (user *current-user*))
  (or (has-permission '(:admin :tutor :student) app user)
      (string= (studentid project) (studentid user))
      (member (username user) (supervisors project) :test #'string=)))

(defmethod can-edit(app (project project) &optional (user *current-user*))
  (and (not (lockedp project))
       (let ((username (username user))
             (student (student project)))
         (or (has-permission :admin app user)
             (string= (studentid project) (studentid user))
             (and student (string= username (username student)))
             (member username (supervisors project) :test #'string=)))))

(defmethod can-view-project-status((app projects-manager) project
                                   &optional (user *current-user*))
  (or (has-permission '(:admin :tutor :supervisor) app user)
      (or (has-permission :student app user)
           (string= (studentid project) (studentid user)))))

(defmethod can-edit-project-status((app projects-manager) project
                                   &optional (user *current-user*))
  (and (not (lockedp project)) (has-permission :admin app user)))

(defmethod can-edit-assessor((app projects-manager) (mark mark)
                             &optional (user *current-user*))
  (and (has-permission :admin app user)
       (not (mark mark))))

(defmethod can-allocate-assessments((app projects-manager) project
                                    &optional (user *current-user*))
  (declare (ignore project))
  (has-permission :admin app user))

(defmethod can-delete(app (project project)
                      &optional (user *current-user*))
  (and (not (studentid project))
       (or (has-permission :admin app user)
           (member (username user) (supervisors project)
                   :test #'string=))))

(defmethod response-handler((app projects-manager) request rest)
  (let* ((args (split-string
                (let ((p (position #\? rest)))
                  (if p (subseq rest 0 p) rest))
                2 '(#\/) :remove-empty-subseqs t))
         (id (first args)))
    (cond ((and id (> (length id) 0)) (project-display-handler app request id))
          (t (home-handler app request rest)))))

(defmethod home-handler((app projects-manager) request rest)
  `(html
    (head
     (title ,(title app)))
    (body
     (navbar ,(menus app))
     ((section :title ,(format nil "~A ~A"
                               (display-name *current-user*)
                               (title app)))
      ,@(mapcar
         #'(lambda(func) (funcall func app request rest))
         (list #'tutor-home-handler
               #'supervisor-home-handler
               #'student-home-handler))))))

;; table format requested by David Web
(defun projects-table(projects &key
                      status (relative "")
                      (order #'project<))
  "Present a table of projects - either specified as a list of projects or
using a filter and sort order function"
  `(dl
    ,@(mapcan
       #'(lambda(p)
           `((dt  ,(if (student p) (fullname (student p)) "Unallocated")
              ", "
              ((a :href ,(format nil "~A~A/" relative (projectid p)))
               #\" ,(title p) #\") )
             (dd
              ,(getf (description p) :location)
              ". Supervised by " ,(join-strings (supervisors p) ", ")
              ". " ,(status-string p)
              ,@(let ((status (when status (funcall status p))))
                     (when (> (length status) 0) status)))))
       (sort (copy-list projects) order))))

(defmethod student-home-handler((app projects-manager) request rest)
  "Students home page - show their allocated project OR their priority
listing"
  (unless (has-permission :student app) (return-from student-home-handler))
  (let ((project (get-project (username *current-user*) app)))
    (when (and project
               (start-date project)
               (< (start-date project) (get-universal-time)))
      (return-from student-home-handler
        (throw 'response
          (redirect request (format nil "~A/" (projectid project))))))
    `(div
      ,(when project
             `((section :title "Your allocated Project")
               ((a :href ,(format nil "~A/" (projectid project)))
                ,(title project))))
      ,(project-choices-handler app request *current-user*))))

(defun project-active-p(p)
  (or (not (studentid p))
      (notevery #'mark (module-marks p))))

(defmethod tutor-home-handler((app projects-manager) request rest)
  (let* ((username (username *current-user*))
         (projects  (mapcan
                    #'(lambda(p) (when (and (active (student p))
                                 (some
                                  #'(lambda(m)
                                      (and (string= username (modified-by m))
                                           (not (mark m))))
                                  (marks p)))
                        (list p)))
                    (allocated-projects (db app)))))
    (unless projects (return-from tutor-home-handler))
    `((section :title "Projects to Assess")
      (p "List of projects for which there are open assessments that require completion by you.")
      ,(projects-table
        projects
        :status
        #'(lambda(p)
            `((table
               (tr ((th :colspan 3) "Assessment") (th "Mark")
                   (th "Assessor") (th "Deadline"))

 ,@(assessor-form-rows app p :relative "../"))))))))

(defmethod supervisor-home-handler((app projects-manager) request rest)
  (unless (has-permission :supervisor app)
    (return-from supervisor-home-handler))
  (let ((username (username *current-user*)))
    `((section :title "Supervision")
      (p "List of all projects you have or are supervising")
      ,(projects-table
        (sort
         (supervisors-project-records (db app) username)
         #'>
         :key #'(lambda(p) (if (studentid p) (year (student p)) 9999)))))))

(defmethod all-project-handler((app projects-manager) request rest)
  (declare (ignore request rest))
  (flet ((project<(a b)
           (let((sa (student a))
                (sb (student b)))
             (cond
               ((and sa sb) (string< (lastname sa) (lastname sb)))
               (t (< (projectid a) (projectid b)))))))
    (let (unallocated notsubmitted unmarked rest)
      (dolist(p (project-records (db app)))
        (cond ((not (studentid p))
               (push p unallocated))
              ((< (year (student p)) 2003)
               (push p rest))
              ((not (submission-date p))
               (push p notsubmitted))
              ((or (notevery #'mark (module-marks p)))
               (push p unmarked))
              (t (push p rest))))
      `(html
        (head (title "All Projects"))
        (body
         (navbar ,(menus app) :on-url "all")
         ((section :title "All Projects")
          ((section
            :title
            ,(format nil "~D Projects Submitted but Not Marked"
                     (length unmarked)))
           ,(projects-table unmarked :order #'project<))
          ((section
            :title ,(format nil "~D Projects Not Submitted"
                            (length notsubmitted)))
           ,(projects-table notsubmitted :order #'project<))
          ((section
            :title ,(format nil "~D Projects Not allocated"
                            (length unallocated)))
           ,(projects-table unallocated :order #'project<))
          ((section
            :title ,(format nil "~D Projects Completed and Marked"
                            (length rest)))
           ,(projects-table rest :order #'project<))))))))

(defun generate-projectid(app)
  (clsql:sequence-next "projectid" :database (db app)))

(defmethod menus((app projects-manager) &optional project)
  `((("../grades/" "Your Grades")
     ("."  "Your Projects")
     ,@(when (has-permission :student app)
             '(("choices" "Project Choices")))
     ,@(when (can-add app 'project) '(("add/" "Add a Project")))
     ,@(when project
             (let ((id (projectid project)))
               (list
                (when (can-view app project)
                  (list (format nil "~A/" id)
                        (format nil "View ~A" id)))
                (when (can-edit app project)
                  (list (format nil "~A/?edit=t" id)
                        (format nil "Edit ~A" id)))
                (when (can-delete app project)
                  (list (format nil "delete/~A" id)
                        (format nil "Delete ~A" id))) )))
     ,@(when (has-permission :admin app)
             '(("allocate/" "Allocate Projects")
               ("assessments/" "Allocate Assessments")
               ("scores/" "Enter Scores")
               ("marks/" "View Marks")
               ("all" "View All Projects")))
     ("preferences/" "Preferences") )))

(defmethod add-project-handler((app projects-manager) request rest)
  (unless (can-add app 'project)
    (throw 'response (cons :forbidden "Not allowed to add a project. Contact the system administrator if you think you should be allowed.")))
  (let* ((form (description-form app))
         (action (submitted-action form request)))
    (multiple-value-bind (form-data condition)
        (form-data form request)
      (if (and action (not condition))
          (let ((project
                 (make-instance 'project
                                :projectid (generate-projectid app)
                                :description form-data
                                :title (getf form-data :title)
                                :supervisors `(,(username *current-user*)))))
            (store-project project app)
            (redirect request (format nil "../~A/" (projectid project))))
          `(html
            (head
             (title "Add a Project"))
            (body
             (navbar ,(menus app) :on-url "add/" :relative "../")
             ((section :title "Add a Project")
              ,(when condition
                     `((p :class :error)
                       "Project not added, please correct the errors below"))
              ,(markup-form form (when action form-data)))))))))

(defun assessor-fieldname(mark)
  (intern (format nil "assessment-~A-~A" (studentid mark) (assessmentid mark))
          :keyword))

(defun assessor-choice-field(app project mark
                             &optional (enabled (can-edit-assessor app mark)))
  `((mcq
     :name ,(assessor-fieldname mark)
     :style :dropdown
     ,@(unless enabled (list :disabled :text))
     :value ,(modified-by mark))
    (nil . "None")
    ,@(user-choices app (assessor-role project mark))))

(defun assessor-form-rows(app project
                          &key
                          (relative "../../")
                          (marks (current-marks project)))
  (mapcar
   #'(lambda(m)
       `(tr
         (td
          ,(if (or (can-edit app m)
                   (and (can-view app m) (feedback m)))
               `((a :href
                  ,(format nil "~Aassessments/~A/~A"
                           relative
                           (assessmentid m)
                           (studentid m)))
                 ,(moduleid (assessment m)) ": " ,(title (assessment m)))
             (title (assessment m)))
          (td ,(assessment-type (assessment m)))
          ((td :align :right)
           ,(format-percentage (weighting (assessment m))))
          ((td :align :right)
           ,(format-percentage (mark m)))
          (td ,(assessor-choice-field app project m))
          (td ,(jarw.parse::format-output 'jarw.parse::date (deadline-date m))) )))
   marks))

(defun assessors-update(app data marks)
  (dolist(mark marks)
    (update-instance-from-records mark)
    (when (can-edit-assessor app mark)
      (let ((assessor (getf data (assessor-fieldname mark) :none)))
        (unless (eql assessor :none)
          (setf (modified-by mark) assessor)
          (update-records-from-instance mark))))))

(defun do-assessor-form(app project request &optional disabled)
  (let ((marks (mapcar #'update-instance-from-records (current-marks project))))
    (do-form
        #'(lambda()
            (mapcan
             #'(lambda(m) (list (assessor-fieldname m) (modified-by m)))
             marks))
      #'(lambda()
          `((form :method :post)
            (table
             (tr  ((th :colspan 2) "Assessment")
              (th "Weighting") (th "Mark") (th "Assessor") (th "Due"))
             ,@(assessor-form-rows app project :marks marks)
             (tr ((td :colspan 4 :align :center)
                  ((input :type :submit :name "Update Assessors"
                          :value "Update Assessors")))))))
      (when (or disabled (not (has-permission :admin app))) :text)
      #'(lambda(data) (assessors-update app data marks))
      request)))

(defmethod project-display-handler((app projects-manager) request projectid)
  (let ((project (get-project (parse-integer projectid :junk-allowed t) app))
        (edit-p (car (query-values "edit" request))))
    (unless project
      (throw 'response (cons :forbidden
                             (format nil "Project ~A not found" projectid))))
    (unless (can-view app project)
      (throw 'response
        (cons :forbidden
              (format nil "You are not allowed to view project ~A"
                      projectid))))
    `(html
      (head
       (title ,(format nil "Project ~A" projectid)))
      (body
       (navbar ,(menus app project) :relative "../" :on-url ,projectid)
       ((section :title ,(format nil "Project ~A" projectid))
        ((section :title "Description")
         ,(do-form
           #'(lambda()
               (let ((d (description project)))
                 (setf (getf d :title) (title project))
                 d))
           (if (description project)
               (find-form (description project))
               (description-form app))
           (unless (and edit-p (can-edit app project)) :text)
           #'(lambda(v)
               (setf (description project) v
                     (title project) (getf v :title))
               (update-records-from-instance project))
           request))
        ,(when (and (has-permission '(:admin :tutor :supervisor) app)
                    (not (studentid project)))
               `((section :title "Student Preferences")
                 ,(project-selections app (projectid project))))
        ,@(when (can-view-project-status app project)
                `(((section :title "Status")
                   ,(do-form
                     #'(lambda() (status-data project))
                     (status-form app project)
                     (unless (and edit-p (can-edit-project-status app project))
                       :text)
                     #'(lambda(v)
                         (setf (status-data project) v)
                         (store-project project app))
                     request))
                  ((section :title "Assessments")
                   ,(do-assessor-form app project request (not edit-p))) )))))))

(defmethod project-selections((app projects-manager) projectid)
  `(table
    (tr (th "No") (th "Student Name")
     (th "Username") (th "Allocated Project"))
    ,@(mapcar
       #'(lambda(r)
           `(tr
             (td ,(cdr r))
             (td ,(student-anchor app (car r)))
             (td ,(username (car r) ))
             (td ,(let ((p (get-project (username (car r)) app)))
                       (when p (title p))))))
       (sort
        (mapcan
         #'(lambda(username)
             (let* ((user (get-dictionary username (users app)))
                    (choices (property
                              (user-component-properties app user)
                              :choices))
                    (p (position projectid choices :test #'equal)))
               (when p (list (cons user p)))))
         (get-users :student app))
        #'< :key #'cdr))))

(defmethod delete-project-handler((app projects-manager) request projectid)
  (let ((project (get-project (parse-integer projectid :junk-allowed t) app)))
    (unless project (return-from delete-project-handler :not-found))
    (unless (can-delete app project)
      (return-from delete-project-handler
        (cons :forbidden
              (format nil "You are not allowed to delete project ~A"
                      projectid))))
    (let ((cmd (car (form-values "action" request))))
      (when (and cmd (not (string= cmd "Yes")))
          (redirect request (format nil "../~A/" projectid)))
      `(html
        (head (title "Delete Project " ,projectid))
        (body
         (navbar ,(menus app) :relative "../")
         (h1 "Delete Project " ,projectid)
         ,(if cmd
              (progn
                (rem-project project app)
                `(p  "Project " ,projectid " successfully deleted. "))
              `((form :method :post)
                (p ,(format
                     nil
                     "Are you sure you want to delete project ~A, ~S"
                     projectid (title project)) (br)
                 ((input :type :submit :name "action" :value "Yes"))
                 ((input :type :submit :name "action" :value "No"))))))))))

(defmethod student-choices-handler((app projects-manager) request rest)
  `(html
    (head
     (title ,(title app)))
    (body
     (navbar ,(menus app))
     ((section :title ,(format nil "~A ~A"
                               (display-name *current-user*)
                               (title app)))
      (div
       ,(project-choices-handler app request *current-user*))))))

(defmethod project-choices-handler((app projects-manager) request student)
  "Display choices for student"
  (let* ((projectid
          (let ((projectid (car (query-values "project" request))))
            (when projectid (parse-integer projectid  :junk-allowed t))))
         (move (let ((move (car (query-values "move" request))))
                 (when (and move projectid)
                   (parse-integer move :junk-allowed t))))
         (available (mapcar #'projectid (available-projects (db app))))
         (chosen  (mapcan
                   #'(lambda(id) (when (member id available) (list id)))
                   (property (user-component-properties app student)
                             :choices)))
         (row 0))
    (setf chosen (nconc chosen (set-difference available chosen)))
    (when (and move projectid (member projectid available))
      (let ((newpos
             (min (max (+ (position projectid chosen) move) 0)
                  (1- (length chosen)))))
        (setf chosen  (delete projectid chosen))
        (setf chosen
              (nconc (subseq chosen 0 newpos)
                     (list projectid)
                     (subseq chosen newpos))))
      (setf (property (user-component-properties app student) :choices)
            chosen)
      (setf (get-dictionary (username student) (users app)) student))
    (flet ((table-row(project)
             `(tr
               ((td :align :right) ,(incf row) ".")
               (td ((a :href ,(format nil "~A/" (projectid project)))
                    ,(title project)))
               (td ,(join-strings (supervisors project) ", "))
               (td ,@(mapcar
                      #'(lambda(m)
                          `((a :href ,(format nil "?project=~A&move=~D"
                                              (projectid project) m))
                            ,(format nil "~:[Up~;Down~]~D " (< 0 m)
                                     (abs m))))
                      '(1 -1 8 -8))))))
      `((section :title "Available Projects" )
        (p "Order the available projects listed below to reflect your
preferences")
        ((table :border 1 :celspacing 0 :class projects)
         (tr (th) (th "Title") (th "Supervisor(s)") (th "Action"))
         ,@(mapcar #'(lambda(id) (table-row (get-project id app))) chosen))))))

(defmethod allocate-projects-handler((app projects-manager) request rest)
  (unless (has-permission :admin app) (throw 'response :forbidden))
  (when (car (form-values "submit" request))
    (let* ((student
            (car (student-records
                  (db app) 'username  (car (form-values "username" request)))))
           (project (get-project (parse-integer
                                  (car (form-values "projectid" request)))
                                 app)))
      (when (and student project (not (studentid project)))
        (setf (studentid project) (studentid student))
        (update-records-from-instance project))))
  (let ((student-data
         (sort
          (mapcan
           #'(lambda(s)
               (let((user (get-dictionary (username s) (users app))))
                 (when user
                   (list (list user
                               (or (property (user-component-properties app user)
                                             :score)
                                   (average-mark (marks s)))
                               (property
                                (user-component-properties app user) :choices))))))
           (students-without-projects (db app)))
          #'(lambda(a b)
              (cond
                ((and (third a) (third b)) (> (second a) (second b)))
                ((third a) t)
                ((third b) nil)
                ((> (second a) (second b)))))))
        (available (available-projects (db app)))
        (allocated nil))
    `(html
      (head (title "Allocate Projects"))
      (body
       (navbar ,(menus app) :relative "../" :on-url "/allocate/")
       ((section :title "Allocate Projects")
        (p "Number indicates students priority - where there is none
the student has not made any selections.")
        (table
         (tr (th "Student") (th "Choices"))
         ,@(mapcar
            #'(lambda(data)
                (let* ((choices
                        (mapcan #'(lambda(choice)
                                    (let ((p (find choice available
                                                   :key #'projectid)))
                                      (when p (list p))))
                                (third data)))
                       (best (or
                              (find-if-not
                               #'(lambda(item) (member item allocated))
                               choices)
                              (find-if-not
                               #'(lambda(item) (member item allocated))
                               available))))
                  (push best allocated)
                  `(tr
                    (td ,(display-name (car data))
                     " (" ,(username (car data))")")
                    (td
                     ((form :method :post)
                      ((mcq :name "projectid" :style :dropdown
                            :value ,(when best (projectid best)))
                       (nil . "None")
                       ,@(mapcar
                          #'(lambda(choice)
                              (cons (projectid choice)
                                    (format nil "~@[~D.~] ~A"
                                            (position choice choices)
                                            (title choice) )))
                          available))
                      ((input :type :submit :name "submit"
                              :value "Allocate"))
                      ((input :type :hidden
                              :name "username"
                              :value ,(username (car data)))))))))
            student-data)))))))

(defun assessment-fieldname(id) (format nil "assessment-~D" id))

(defmethod allocate-assessments-handler((app projects-manager) request rest)
  (unless (has-permission :admin app) (throw 'response :forbidden))
  (let ((projects (mapcan
                   #'(lambda(project)
                       (when (and (notevery #'mark (module-marks project))
                                  (active (student project))
                                  (notevery #'mark (marks project)))
                         (update-instance-from-records project)
                         (list project)))
                   (allocated-projects (db app)))))
    `(html
      (head (title "Allocate Assessments"))
      (body
       (navbar ,(menus app) :relative "../" :on-url "/assessments/")
       ((section :title "Tutors Accumulative Workload")
        ,(tutor-workload-table app))
       ((section :title "Allocate Assessments")
        (p "Only projects which have an unallocated assessment and are not completed are listed here.")
        ,(do-form
          #'(lambda()
              (mapcan
               #'(lambda(p)
                   (mapcan #'(lambda(m)
                               (list (assessor-fieldname m) (modified-by m)))
                           (current-marks p)))
               projects))
          #'(lambda()
              `((form :method :post)
                ,(projects-table
                  projects
                  :relative "../"
                  :status
                  #'(lambda(project)
                      `((table
                         (tr  ((th :colspan 2) "Assessment")
                          (th "Weighting") (th "Mark") (th "Assessor"))
                         ,@(assessor-form-rows app project )))))
                ((input :type :submit
                  :name "Update Assessments"
                  :value "Update Assessments"))))
          nil
          #'(lambda(data)
              (dolist(p projects)
                (assessors-update app data (current-marks p))))
          request))))))

(let ((effort-weighting
       '((:supervised . 6)
         ("SUP" . 1)
         ("DIS" . 3)
         ("ORAL" . 1))))
  (defun tutor-workload(rec)
    (reduce
     #'+
     (mapcar
      #'(lambda(type)
          (let ((w (cdr type))
                (v (second (assoc (car type) (cdr rec) :test #'equalp))))
            (if (and w v) (* w v) 0)))
      effort-weighting))))

(defun inc-record(key record &optional (value 1))
  (let ((item (assoc key (cdr record) :test #'equalp)))
    (if item
        (incf (second item) value)
        (push (list key value) (cdr record)))))

(defun tutor-workload-table(app &optional (scorefunc #'tutor-workload))
  "Produce a table of tutors workload"
  (let* ((earliest (- (get-universal-time) (* 2 365 24 60 60))) ; 3 yrs ago
         (records
          (mapcan
           #'(lambda(username)
               (let ((stats (assessment-stats (db app) username
                                              :earliest earliest
                                              :module (moduleid app)
                                              :group-by "TYPE")))
                 (when stats (list (cons username stats)))))
           (get-users :tutor app)))
         (fields (list :supervised "SUP" "DIS" "ORAL" :total)))
    (dolist(project (allocated-projects (db app)))
      (when (and (or (not (submission-date project))
                     (> (submission-date project) earliest))
                 (supervisors project))
        (let ((v (/ 1 (length (supervisors project)))))
          (dolist(username (supervisors project))
            (let ((r (assoc username records :test #'equal)))
              (when r (inc-record :supervised r v)))))))
    (dolist(rec records)
      (push (list :total (funcall scorefunc rec)) (cdr rec)))
    `((table :border 1)
      (tr
       (th "Tutor") ,@(mapcar #'(lambda(f) `(th ,(string f))) fields))
      ,@(mapcar
         #'(lambda(rec)
             (let ((f (rest rec)))
               `(tr ((th :align :left) ,(first rec))
                 ,@(mapcar
                    #'(lambda(v)
                        `((td :align :right)
                          ,(second (assoc v f :test #'equalp))))
                    fields))))
         (sort records #'(lambda(a b)
                           (cond
                             ((not a) nil)
                             ((not b) t)
                             ((> a b))))
               :key #'(lambda(r) (second (assoc :total (cdr r)))))))))


(defmethod allocate-scores-handler((app projects-manager) request rest)
  (unless (has-permission :admin app) (throw 'response :forbidden))
  (let ((students (sort (get-users :student app) #'string<)))
    (when (car (form-values "submit" request))
      (dolist(username students)
        (let ((user (get-dictionary username (users app)))
              (score (let ((v (car (form-values username request))))
                       (if (> (length v) 0) (parse-number v) 0))))
          (setf (property (user-component-properties app user) :score) score)
          (setf (get-dictionary username (users app)) user))))
    `(html
      (head (title "Allocate Scores"))
      (body
       (navbar ,(menus app) :relative "../" :on-url "/scores/")
       ((section :title "Allocate Scores")
        (p "Enter integer scores below. Students with the highest score get first
choice in project allocations")
        ((form :method :post)
         (table
          (tr (th "Student") (th "Score"))
          ,@(mapcar
             #'(lambda(username)
                 (let ((user (get-dictionary username (users app))))
                   `(tr
                     (td ,(display-name user))
                     (td ((input
                           :name ,username
                           :value ,(or (property
                                        (user-component-properties app user)
                                        :score)
                                       (let ((s (car (student-records (db app)
                                                                      'username username))))
                                         (when s
                                           (average-mark (marks s))))
                                       0)))))))
             students))
         ((input :type :submit :name "submit" :value "Update Scores"))))))))

(defmethod marks-handler((app projects-manager) request rest)
  (unless (has-permission :tutor app) (throw 'response :forbidden))
  (let* ((role (when-bind(role (car (query-values :role request)))
                 (intern (string-upcase role) :keyword))))
    `(html
      (head (title "Project Marks"))
      (body
       (navbar ,(menus app) :on-url "marks/" :relative "../")
       ((section :title ,(if role (strcat "Marks for " role) "Marks"))
        ((form :method :get)
         (p "Students: "
            ((mcq :style :dropdown :name :role :value ,(string-downcase role))
             ,@(sort (mapcar #'string-downcase (roles (users app))) #'string> ))
            " format: " ((mcq :name :format :style :dropdown) "html")
            ((input :name :submit :type :submit :value "Get Marks"))))
        (hr)
        ,@(when
           role
           (let* ((students
                   (mapcan
                    #'(lambda(username)
                        (let ((rec (car (student-records (db app) 'username username))))
                          (when rec (list rec))))
                    (sort (get-users role (users app)) #'string<)))
                  (assessments
                   (sort
                    (delete-duplicates
                     (mapcan
                      #'(lambda(student)
                          (when (project student)
                            (mapcar #'assessment (marks (project student)))))
                      students)
                     :key #'assessmentid)
                    #'< :key #'weighting)))
             `(((table :border 1)
                (tr (th "Student")
                 ,@(mapcar #'(lambda(a) `((th :width "10%") ,(title a)))
                           assessments)
                 (th "Mark (%)") (th "Project"))
                ,@(mapcar
                   #'(lambda(student)
                       (let ((project (project student)))
                         `(tr
                           (td ,(fullname student))
                           ,@(if project
                                 `(,@(mapcar
                                      #'(lambda(a)
                                          `((td :align :right)
                                            ,(let ((m (find (assessmentid a)
                                                            (marks project)
                                                            :key #'assessmentid)))
                                                  (if m
                                                      (if (feedback m)
                                                          (format-percentage
                                                           (form-mark
                                                            (feedback m)
                                                            (feedback-form m)))
                                                          (modified-by m))))))
                                      assessments)
                                   ((td :align :right)
				    ,@(mapcar
				       #'(lambda(module-mark)
					   (when (can-view app module-mark)
					     (format nil "~A=~A; "
					     (moduleid module-mark)
					     (format-percentage
					      (mark module-mark)))))
				       (module-marks project)))
                                   (td ((a :href
                                           ,(format nil "../~A/"
                                                    (projectid project)))
                                        ,(title project))))
                                 `(((td :colspan ,(+ 2 (length assessments))
                                     :align :center)
                                    "No Project Allocated"))))))
                   students) )))) )))))

(register-form
 "project-description"
 '((form :name "project-description" :method "POST")
   ((section :title "Title")
    ((input :name :title :size 60)))
   ((section :title "Location")
    ((input :name :location :size 60 :value "Aston University")))
   ((section :title "Outline")
    (p (em "Give an outline of this project of at least 100 words (formatted using
restructured text))"))
    ((textarea :name :outline :structured-text t :cols 80 :rows 20
               :datatype (string :word-count 75))))
   ((section :title "Skills")
    (p (em "List the skills required for this project"))
    ((input :name :skills :size 60)))
   ((input :type :submit :name "Update" :value "Update Description"))))

(register-form
 "legacy-project-description"
 '((FORM :NAME "legacy-project-description" :METHOD "POST")
   ((SECTION :TITLE "Title") ((INPUT :NAME :TITLE :SIZE 60)))
   ((SECTION :TITLE "Location")
    ((INPUT :NAME :LOCATION :SIZE 60 :VALUE "Aston University")))
   ((SECTION :TITLE "Context")
    (P (EM "Give the background/context of this project"))
    ((TEXTAREA :NAME :CONTEXT :STRUCTURED-TEXT T :COLS 80 :ROWS 20 :DATATYPE
	       (STRING :WORD-COUNT 75))))
   ((SECTION :TITLE "Outline")
    (P
     (EM "Give an outline of this project of at least 100 words
(formatted using structured text))"))
    ((TEXTAREA :NAME :OUTLINE :STRUCTURED-TEXT T :COLS 80 :ROWS 20 :DATATYPE
	       (STRING :WORD-COUNT 75))))
   ((SECTION :TITLE "Skills")
    (P (EM "List the skills required for this project"))
    ((INPUT :NAME :SKILLS :SIZE 60)))
   ((INPUT :TYPE :SUBMIT :NAME "Update" :VALUE "Update Description"))))

#|
(setq *m* (car (module-records *db* 'year 2004 'moduleid "EE4006")))
(setq *dm*
      (apply #'list (mapcan #'(lambda(a)
					(when (string= (assessment-type a) "DIS")
					  (list a)))
			    (assessments *m*))))
(setq *do* (apply #'list (mapcan #'(lambda(a)
				     (when (string= (assessment-type a) "ORAL")
				       (list a)))
				 (assessments *m*))))

(defun get-assessments(moduleid year type)
  (apply
   #'list
   (mapcan
    #'(lambda(a)
	(when (string= (assessment-type a) type)
	  (list a)))
    (assessments (car (module-records *db* 'year year 'moduleid moduleid))))))

(defun do-stats(assessments &optional (remap #'identity) (rescale #'identity) set-marks)
  (let* ((counters (make-array 10 :element-type 'fixnum :initial-element 0))
	 (mark-records (reduce #'append (mapcar #'marks assessments)))
	 (data
	  (mapcan #'(lambda(m)
		      (when (feedback m)
			(list (funcall remap (feedback m)))))
		  mark-records))
	 (form (feedback-form (car assessments)))
	 (marks (funcall rescale
			 (mapcar #'(lambda(m)
				     (* 100 (form-mark m form)))
				 data)))
	 )
    (when set-marks
      (map 'nil #'(lambda(record mark)
		    (format t "~,1F -> ~,1F~%" (mark record) mark)
		    (setf (mark record) mark)
		    (update-records-from-instance record))
	   mark-records marks))
    (format t "marks=~A~%"(sort (copy-list marks) #'>))
    (dolist(mark marks)
      (incf (aref counters (floor (/ mark 10.0)))))
    (format t "Mean: ~,1F%~%StdDev: ~,1F%~%"
	     (jarw.math::mean marks)
	     (jarw.math::stddev marks))
    (dotimes(idx 10)
      (format t "~2D-~2D%: ~D~%"
	      (* 10 idx) (1- (* 10 (1+ idx))) (aref counters idx)))))


(defun set-marks(assessments &optional (remap #'identity)  target)
  (let* ((mark-records
	  (sort (reduce #'append (mapcar #'marks assessments))
			     #'string> :key #'studentid))
	 (old-mean 68.0d0)
	 (form (feedback-form (car assessments))))
    (format t "~D records~%" (length mark-records))
    (dolist(record mark-records)
      (if (feedback record)
	  (let* ((data (funcall remap (feedback record)))
		 (old-mark (* 100 (form-mark data form)))
		 (new-mark (car (rescale-marks2 (list old-mark) target old-mean))))
	    (format t "~A ~,1F -> ~,1F~%" (studentid record)
		    old-mark new-mark)
	    (setf (mark record) new-mark))
	  (progn
	    (format t "~A ~,1F -> nil~%" (studentid record)
		    (mark record)  nil)
	    (setf (mark record) nil)))
      (update-records-from-instance record))))

(defun find-missing(assessments)
  (flet ((mark-report(msg assessment mark)
	   (format t "~20A: ~3D ~4A ~8A (~D) from ~A~%"
		   msg
		   (assessmentid assessment)
		   (assessment-type assessment)
		   (username (student mark))
		   (studentid mark)
		   (modified-by mark))))
  (dolist(assessment assessments)
    (dolist(mark (marks assessment))
      (cond
	((not (feedback mark))
	 (mark-report "Missing Feedback" assessment mark))
	((not (mark mark))
	 (mark-report "Missing mark???" assessment mark)))))))

(defun remap1(data)
  (mapcar #'(lambda(v) (if (eq v 20) 0 v)) data))

(defun rescale-marks(marks target)
  (flet ((logscale(m) (log (/ m (- 100 m)) 10))
	 (unlogscale(a)
	   (let ((u (expt 10 a)))
	     (* 100.0 (/ u (1+ u))))))
  (let* ((mean (jarw.math::mean marks))
	 (logshift (- (logscale target) (logscale mean))))
    (mapcar #'(lambda(m) (unlogscale (+ logshift (logscale m))))
	    marks))))

(defun rescale-marks2(marks target &optional (mean (jarw.math::mean marks)))
  (flet ((logscale(m) (log m 10))
	 (unlogscale(a) (expt 10 a)))
    (let* ((logshift (- (logscale target) (logscale mean))))
      (mapcar #'(lambda(m) (unlogscale (+ logshift (logscale m))))
		marks))))

(defun rescale-mark(mark target mean)
  (* mark (/ target mean)))


(defun set-mark(studentid &key (examboardid 18) (moduleid "EE4006")
                late plagiarise)
  (let* ((student (car (student-records *db* 'studentid studentid)))
         (module-mark (find moduleid (current-module-marks student)
                            :key #'moduleid :test #'string-equal)))
    (unless (find examboardid (examboard-decisions student) :key #'examboardid)
      (error "Student ~A not in examboard ~D" student examboardid))
    (unless module-mark
      (error "No module ~A for student ~A" moduleid student))
    (when (mark module-mark)
      (warn "Mark already set to ~A" (mark module-mark)))
    (when plagiarise
      (format t "Applying 10% penalty for plagiarism~%")
      (dolist(mark (current-marks module-mark))
        (if (mark mark)
            (progn
              (setf (mark mark) (- (mark mark) 10))
              (setf (note mark) "-10\% Penalty for minor plagiarism applied."))
            (setf (note mark) "-10\% Penalty for minor plagiarism to be applied."))
        (update-records-from-instance mark)))
                                        ;(setf (mark module-mark) (calculated-module-mark mark))))
    (setf (mark module-mark) (calculated-mark module-mark))
    (update-records-from-instance module-mark)
    (mark module-mark)))

|#