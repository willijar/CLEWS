;;;; Main class and handlers for the project-manager class
;;;; Copyright (C) 2004-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL) 
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; $Id: projects.lisp,v 1.5 2005/03/10 20:14:01 willijar Exp $

(in-package :clews.projects)

(defclass project-manager (application)
  ((title :initform "Projects" :initarg :title :reader title
	  :documentation "Identifying title for these projects")
   (project-source
    :type dictionary :reader projects :initarg :project-source
    :documentation "Source of projects")
   (description-form
    :reader description-form
    :initform "project-description"
    :initarg :description-form
    :documentation "Project Description Form")
   (assessments
    :reader assessments
    :initform
    `((:project-supervisor 15/100 ,#'supervisors)
      (:project-oral 15/100 :tutor)
      (:project-dissertation-1 35/100 :tutor)
      (:project-dissertation-2 35/100 :tutor))
    :initarg :assessments
    :documentation "List of assessment specs - each a list of key,
weighting and a spec for who can be allocated")
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

(defmethod published-methods ((app project-manager) baseurl)
  (declare (ignore baseurl))
  `(,@(call-next-method)
      ("add/" ,#'add-project-handler :stage :response :match :exact)
      ("delete/" ,#'delete-project-handler :stage :response :match :prefix)
      ("allocate/" ,#'allocate-projects-handler
       :stage :response :match :exact :display-plugins-p nil :role :admin)
      ("assessments/" ,#'allocate-assessments-handler
       :stage :response :match :exact :display-plugins-p nil :role :admin)
      ("scores/" ,#'allocate-scores-handler
       :stage :response :match :exact :display-plugins-p nil :role :admin)
      ("marks/" ,#'marks-handler
	:stage :response :match :prefix :display-plugins-p nil :role :admin)))

(defmethod filtered-projects((app project-manager) func)
  (let ((list))
    (map-dictionary
     #'(lambda(k v)
	 (declare (ignore k))
	 (when (funcall func v) (push v list)))
     (projects app))
    (nreverse list)))

(defmethod available-projects((app project-manager))
  (filtered-projects app #'(lambda(p) (not (student p)))))

(defmethod student-anchor((app project-manager) user)
  "Return the html anchor pointing to a students details"
  (if (student-url app)
      `((a :href ,(funcall (student-url app) user))
	,(display-name user))
      (display-name user)))

(defmethod store-project (project (app project-manager))
  "Store project, updating changes to project and user"
  (push (cons (username *current-user*) (get-universal-time))
	(changes project))
  (let* ((studentname (student project))
	 (id (project-id project)))
    (if studentname
	(let ((student (get-dictionary studentname (users app)))
	      (old-project (get-project studentname app)))
	  (unless (eql old-project project)
	    (setf (property (user-component-properties app student) :projectid)
		  (project-id project)
		  (get-dictionary studentname (users app))
		  student)
	    (when old-project
	      (setf (student old-project) nil)
	      (setf (get-dictionary (project-id old-project) (projects app))
		    old-project)
	      (store-project old-project app))))
	(dolist(studentname (get-users :student app))
	  (let ((student (get-dictionary studentname (users app))))
	    (when (equalp (property (user-component-properties app student)
				 :projectid) id)	      
	      (setf (property (user-component-properties app student)
			      :projectid) nil)
	      (setf (get-dictionary studentname (users app)) student)))))
    (setf (get-dictionary (project-id project) (projects app)) project)))

(defmethod get-project(id (app project-manager))
  "Retreive project by project id or username"
  (or (get-dictionary id (projects app))
      (let ((student (get-dictionary id (users app))))
	(when student
	  (get-dictionary
	   (property (user-component-properties app student)
		     :projectid) (projects app))))))

(defmethod rem-project(project (app project-manager))
  (unless (not (student project))
    (error "Project Deletion attempted for allocated project ~A" project))
  (rem-dictionary (project-id project) (projects app)))

(defmethod init-assessments((p project) (app project-manager))
  (assert (null (assessments p)))
  (setf (assessments p)
	(mapcar #'(lambda(rec)
		    (list (first rec)
			  :form (second rec)
			  :weight (third rec)))
		(assessments app))))

(defun user-choices(app role &optional (test #'identity))
  "Produce a list of users selections for an MCQ or MAQ"
  (sort (mapcan
	 #'(lambda(username)
	     (let ((user (get-dictionary username (users app))))
	       (when (funcall test user)
		 (list (cons username (display-name user))))))
	 (get-users role app))
	#'string> :key #'car))

(defmethod status-form((app project-manager))
  "Return the status form object for a project"
  `((form :method :post)
    (table
     (tr
      ((th :align :right) "Student")
      (td ((mcq :name :student :style :dropdown
		:datatype (string :nil-allowed t))
	   (nil . "None")
	   ,@(user-choices app :student)))
      (td "Student to whom project is allocated"))
     (tr
      ((th :align :right) "Locked")
      (td ((boolean :name :locked-p)))
      (td "If true description and assessments cannot be changed"))
     (tr
      ((th :align :right :valign :top) "Supervisor(s)")
      (td ((maq :name :supervisors :size 5 :style :dropdown
		:datatype (string :nil-allowed t))
	  ,@(user-choices app  :supervisor)))
      (td "List of supervisors and tutors allocated to this project"))
     (tr
      ((th :align :right) "Start Date")
      (td ((input :name :start-date :size 35
		  :value ,(ext:parse-time "2005-04-04 09:00")
		  :datatype (date :nil-allowed t :fmt :rfc2822))))
      (td "Date student started project"))
     (tr
      ((th :align :right) "Submission Deadline")
      (td ((input :name :deadline-date :size 35
		  :value ,(ext:parse-time "2005-09-30 17:00")
		  :datatype (date :nil-allowed t :fmt :rfc2822))))
      (td "Submission deadline for project including any extensions"))
     (tr
      ((th :align :right) "Submitted")
      (td ((input :name :submission-date :size 35
		  :datatype (date :nil-allowed t :fmt :rfc2822) )))
      (td "Date student submitted project"))
     (tr
      ((th :align :right :valign :top) "Notes")
      (td ((textarea :name :notes :structured-text t :cols 40 :rows 4))))
     #+nil(tr
      (th
     ,@(mapcar
	#'(lambda(a)
	    `(tr
	      (th ,(second a))
	      (td ((mcq :style :dropdown :name ,(first a))
		   ,@(user-choices app :tutor)))))
	(assessments app))
     )))
    ((input :type :submit :name :submit :value "Update Status"))))

;;; permissions for current user
(defmethod can-add-project((app project-manager)
			   &optional (user *current-user*))
  (has-permission '(:supervisor :admin :tutor) app user))

(defmethod can-view-project((app project-manager) project
			    &optional (user *current-user*))
  (or (has-permission '(:admin :tutor :student) app user)
      (member (username user) (supervisors project)
	      :test #'string=)))

(defmethod can-edit-project-description((app project-manager) project
					&optional (user *current-user*))
  (and (not (locked-p project))
       (let ((username (username user)))
	 (or (has-permission :admin app user)
	     (string= username (student project))
	     (member username (supervisors project) :test #'string=)))))

(defmethod can-edit-project-status((app project-manager) project
				   &optional (user *current-user*))
  (has-permission :admin app user))

(defmethod can-allocate-assessments((app project-manager) project
				   &optional (user *current-user*))
  (has-permission :admin app user))

(defmethod can-delete-project((app project-manager) project
			      &optional (user *current-user*))
  (and (not (student project))
       (or (has-permission :admin app user)
	   (member (username user) (supervisors project)
		   :test #'string=))))

(defmethod can-view-assessment((app project-manager) project assessment)
  (declare (ignore assessment))
  (or (has-permission :admin app) (has-permission :tutor app)
      (string= (student project) (username *current-user*))))

(defmethod can-edit-assessment((app project-manager) project assessment)
  (or (has-permission :admin app)
      (and (not (locked-p project))
       (string= (assessment-assessor assessment) (username *current-user*)))))

(defmethod response-handler((app project-manager) request rest)
  (let* ((args (split-string
		(let ((p (position #\? rest)))
		  (if p (subseq rest 0 p) rest))
		2 '(#\/) :remove-empty-subseqs t))
	 (id (first args))
	 (assessment (second args)))
    (cond (assessment (assessment-display-handler app request id assessment))
	  ((and id (> (length id) 0)) (project-display-handler app request id))
	  (t (home-handler app request rest)))))

(defmethod home-handler((app project-manager) request rest)
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
	 (list #'supervisor-home-handler
	       #'tutor-home-handler
	       #'admin-home-handler
	       #'student-home-handler))))))

(defun projects-table(app &key (projects nil)
		      (filter #'identity)
		      (status #'status-string)
		      (order #'project<))
  "Present a table of projects - either specified as a list of projects or
using a filter and sort order function"
  (flet ((fullname(username)
	   (when username (display-name
			   (get-dictionary username (users app))))))
    (let* ((projects (or projects (sort (filtered-projects app filter) order)))
	   (loc-p (and projects
		       (getf (description (first projects)) :location))))
  `((table :border "1" :cellspacing "0" :class"projects")
    (tr (th "Title") (th "Student") (th "Supervisor(s)")
     ,(when loc-p '(th "Location"))
     (th "Status"))
    ,@(mapcar
       #'(lambda(p)
	   `(tr
	     (td ((a :href ,(format nil "~A/" (project-id p)))
		  ,(title p)))
	     (td ,(fullname (student p)))
	     (td ,(join-strings (supervisors p) ", "))
	     ,(when loc-p `(td ,(getf (description p) :location)))
	     (td ,(funcall status p) )))
       projects)))))

(defmethod student-home-handler((app project-manager) request rest)
  "Students home page - show their allocated project OR their priority
listing"
  (unless (has-permission :student app) (return-from student-home-handler))
  (let ((project (get-project (username *current-user*) app)))
    (when (and project
	       (start-date project)
	       (< (start-date project) (get-universal-time)))
      (return-from student-home-handler
	(redirect request (format nil "~A/" (project-id project)))))
	`(div
	  ,(when project
	     `((section :title "Your allocated Project")
	       ((a :href ,(format nil "~A/" (project-id project)))
		,(title project))))
	  ,(project-choices-handler app request *current-user*))))

(defmethod tutor-home-handler((app project-manager) request rest)
  (unless (has-permission :tutor app) (return-from tutor-home-handler))
  (let ((username (username *current-user*)))
  `((section :title "Projects to Assess")
    (p "Your due assessments will appear here for students in the the 2004
cohorts onwards. Please use the "
     ((a :href "http://www.ee.aston.ac.uk/teaching/pg/staff/marking")
      " previous marking system ") " for students in previous cohorts.")
    ,(projects-table
      app
      :filter
      #'(lambda(p)
	  (find username (assessments p)
		:key #'assessment-assessor
		:test #'equal))
      :status
      #'(lambda(p)
	  `(p
	    ,@(mapcan
	       #'(lambda(a)
		   (when (equal username (assessment-assessor a))
		   `(((a :href ,(format nil "~A/~A"
					   (project-id p)
					   (assessment-form a)))
			  ,(if (assessment-submissions a) "Edit " "Add ")
		     ,(title (find-form (assessment-form a)))))))
	       (assessments p))))))))
		   
(defmethod admin-home-handler((app project-manager) request rest)
  (unless (has-permission :admin app) (return-from admin-home-handler))
  `((section :title "All Projects")
    ,(projects-table app)))

(defmethod supervisor-home-handler((app project-manager) request rest)
  (unless (has-permission :supervisor app)
    (return-from supervisor-home-handler))
  (let ((username (username *current-user*)))
    `((section :title "Supervision")
      ,(projects-table app
	:filter
	#'(lambda(p) (member username (supervisors p) :test #'equal))))))

(defun generate-project-id(app)
  (let* ((keys (dictionary-keys (projects app))))
    (format nil "~5,'0D"
	    (if keys
		(let ((biggest (first keys)))
		  (dolist(key (rest keys))
		    (when (string> key biggest) (setf biggest key)))
		  (1+ (parse-integer biggest)))
		1))))

(defmethod menus((app project-manager) &optional project)
  `((("."  "Home")
     ,@(when (can-add-project app) '(("add/" "Add")))
     ,@(when project
	     (let ((id (project-id project)))
	     (list
	      (when (can-view-project app project)
		(list (format nil "~A/" id)
		      (format nil "View ~A" id)))
	      (when (can-edit-project-description app project)
		(list (format nil "~A/?edit=t" id)
		      (format nil "Edit ~A" id)))
	      (when (can-delete-project app project)
		(list (format nil "delete/~A" id)
		      (format nil "Delete ~A" id))) )))
     ,@(when (has-permission :admin app)
	     '(("allocate/" "Allocate Projects")
	       ("assessments/" "Allocate Assessments")
	       ("scores/" "Enter Scores")
	       ("marks/" "View Marks")))
     ("preferences/" "Preferences") )))

(defmethod add-project-handler((app project-manager) request rest)
  (unless (can-add-project app) (throw 'response :forbidden))
  (let* ((form (description-form app))
	 (action (submitted-action form request)))
    (multiple-value-bind (form-data condition)
	(form-data form request)
      (if (and action (not condition))
	(let ((project-id (generate-project-id app)))
	  (store-project
	   (make-instance
	    'project
	    :id project-id
	    :description form-data
	    :supervisors `(,(username *current-user*)))
	   app)
	  (redirect request (format nil "../~A/" project-id)))
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

(defun do-form(data &optional form disabled-p updatefunc request)
  "Return form markup. Data can be the actual data or a function to
return it.  If edit-p is false present only view. update func is
called with validated data return from form. request is http request
object"
  (flet ((getdata() ; memoize data
	   (if (functionp data) (setf data (funcall data)) data)))
    (let ((form (typecase form
		  (null (find-form (getdata)))
		  (atom (find-form form))
		  (t form))))
      (if (and (not disabled-p) (submitted-action form request))
	  (multiple-value-bind(form-data condition)
	      (form-data form request)
	    (unless condition (funcall updatefunc form-data))
		`(div
		  ,(when condition
		    '((p :class :error)
		      "Form Submission Not Accepted. Please Correct Below"))
		  ,(markup-form form form-data (not condition))))
	  (markup-form form (getdata) disabled-p)))))

(defmethod assessment-display-handler((app project-manager) request project-id
				      assessment-id)
  (let* ((project (get-project project-id app))	 
	 (assessment-id (intern (string-upcase assessment-id) :keyword))
	 (assessment
	  (when project  (assessment assessment-id project))))
    (unless (and project assessment
		 (can-view-assessment app project assessment))
      (throw 'response :forbidden))
    `(html
      (head
       (title ,(format nil "~A:  ~A: ~A" assessment-id
		       project-id (title project))))
      (body
       (navbar ,(menus app project) :relative "../")
       ((section :title ,(format nil " ~A: ~A"
				 project-id (title project)))
	(table
	 (tr
	  ((td :width "50%" :valign :top)
	   ,(do-form (description project) nil :text))
	  ((td :valign :top :width "50%")
	   ((section :title ,(title (find-form (assessment-form assessment))))
	   ,(do-form
	     #'(lambda() (assessment-submission assessment))
	     (assessment-form assessment)
	     (unless (can-edit-assessment app project assessment) :text)
	     #'(lambda(data)
		 (setf (getf data :submitted) (get-universal-time)
		       (getf data :submitted-by) (username *current-user*))
		 (push data (assessment-submissions assessment))
		    (store-project project app))
	     request)	   
	   ,(let ((s (assessment-submission assessment)))
		 (when s
		   `(p ,(format nil "Completed ~A by ~S. Mark ~D"
				(format-time nil (getf s :submitted))
				(getf s :submitted-by)
				(mark-form-data s))))))))))))))

(defmethod project-display-handler((app project-manager) request project-id)
  (format t "Project id=~S~%" project-id)
  (let ((project (get-project project-id app))
	(edit-p (car (query-values "edit" request)))
	(message nil))
    (unless (and project (can-view-project app project))
      (throw 'response :forbidden))
    (let ((incomplete-assessments
	   (mapcan
	    #'(lambda(a)
		(unless (assessment-submissions a) (list a)))
	    (assessments project))))
    (flet ((display-form(form datafunc permission updatefunc)
	     (let ((data)
		   (edit-p (and edit-p permission)))
	       (if (and edit-p (submitted-action form request))
		   (multiple-value-bind(form-data condition)
		       (form-data form request)
		     (setq data request
			   message condition)
		     (unless condition
		       (funcall updatefunc form-data project)
		       (store-project project app)
		       (setq message "Project Updated Successfully")))
		   (setq data (funcall datafunc project)))
	       (markup-form form data (unless edit-p :text))))
	   (edit-assessment-link(a)
	     `((a :href ,(format nil "../~A/~A"
				 (project-id project) (assessment-form a)))
	       ,(if (assessment-submission a) "Edit " "Add ")
	       ,(title (find-form (assessment-form a))))))
    `(html
      (head
       (title ,(format nil "Project ~A: ~S" project-id (title project))))
      (body
       (navbar ,(menus app project) :relative "../" :on-url ,project-id)
       ((section :title ,(format nil "Project ~A: ~S " project-id
				 (title project)))
	,(when message `((p :class :error) ,message))
	((section :title "Description")
	 ,(display-form (find-form (description project))
			 #'description
			 (can-edit-project-description app project)
			 #'(lambda(v p)  (setf (description p) v)))
	 (p (b "Last Update: ")
	    ,@(let ((c (first (changes project))))
		   (list (car c) " " (format-time nil (cdr c))))))
	((section :title "Status")
	 ,(display-form (status-form app)
			#'status-data
			(can-edit-project-status app project)
			#'(lambda(v p)
			    (setf (status-data p) v)))
	 ,(if incomplete-assessments
	     `((section :title "Incomplete Assessments")
	       (table
		(tr  (th "Assessor")(th "Assessment") (th "Weighting"))
	       ,@(mapcar
		  #'(lambda(a)
		      `(tr
			(td ,(assessment-assessor a))
			(td ,(if (can-edit-assessment app project a)
				 (edit-assessment-link a)
				 (title (find-form (assessment-form a)))))
			((td :align :right)  ,(assessment-weight a))))
		  incomplete-assessments)))
	     `(p (b "Overall Mark: ") ,(project-mark project))))
	,(when (and (has-permission '(:admin :tutor :supervisor) app)
		    (not (student project)))
	       `((section :title "Student Preferences")
		 ,(project-selections app (project-id project))))
       ,(unless (= (length incomplete-assessments)
		   (length (assessments project)))
		`((section :title "Completed Assessments")
		  ,@(mapcar
		     #'(lambda(a)
			 (when-bind (s (assessment-submission a))
			    `((section
			       :title ,(title (find-form (assessment-form a))))
			      ,(markup-form (find-form s) s :text)
			      (p (b "Mark: ") ,(mark-form-data s)
			       " (x " ,(assessment-weight a) " weighting)")
			      ,(when (can-edit-assessment app project a)
				     (edit-assessment-link a)))))
		     (assessments project)))) )))))))

(defmethod project-selections((app project-manager) project-id)  
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
			 (p (position project-id choices :test #'equal)))
		    (when p (list (cons user p)))))
	    (get-users :student app))
	   #'< :key #'cdr))))

(defmethod delete-project-handler((app project-manager) request project-id)
  (let ((project (get-project project-id app)))
    (unless (and (can-delete-project app project) project)
      (return-from delete-project-handler :forbidden))
    (let ((cmd (car (form-values "action" request))))
      (when (and cmd (not (string= cmd "Yes")))
	(return-from delete-project-handler
	  (redirect request (format nil "../~A/" project-id))))
    `(html
      (head (title "Delete Project " ,project-id))
      (body
       (navbar ,(menus app) :relative "../")
       (h1 "Delete Project " ,project-id)
       ,(if cmd
	    (progn
	      (rem-project project app)
	      `(p  "Project " ,project-id " successfully deleted. "))
	    `((form :method :post)
	      (p ,(format
		  nil
		  "Are you sure you want to delete project ~A, ~S"
		  project-id (title project)) (br)
	       ((input :type :submit :name "action" :value "Yes"))
	       ((input :type :submit :name "action" :value "No"))))))))))

(defmethod project-choices-handler((app project-manager) request student)
  "Display choices for student"
  (let* ((projectid  (car (query-values "project" request)))
	 (move (let ((move (car (query-values "move" request))))
		(when (and move projectid)
		  (parse-integer move :junk-allowed t))))
	 (available (available-projects app))
	 (chosen  (mapcan
		       #'(lambda(id)
			   (let ((p (get-project id app)))
			     (when (member p available) (list p))))
		       (property (user-component-properties app student)
				 :choices)))
	 (row 0))
    (setf chosen (nconc chosen (set-difference available chosen)))
    (when (and move projectid)
      (let ((project (get-project projectid app)))
	(when project
	  (let ((newpos
		 (min (max (+ (position project chosen) move) 0)
		      (1- (length chosen)))))
	    (setf chosen  (delete project chosen))
	    (setf chosen
		  (nconc (subseq chosen 0 newpos)
			 (list project)
			 (subseq chosen newpos))))))
      (setf (property (user-component-properties app student) :choices)
	    (mapcar #'project-id chosen))
      (setf (get-dictionary (username student) (users app)) student))
    (flet ((table-row(project)
	       `(tr
		 ((td :align :right) ,(incf row) ".")
		 (td ((a :href ,(format nil "~A/" (project-id project)))
		      ,(title project)))
		 (td ,(join-strings (supervisors project) ", "))
		 (td ,@(mapcar
			#'(lambda(m)
			    `((a :href ,(format nil "?project=~A&move=~D"
						(project-id project) m))
			      ,(format nil "~:[Up~;Down~]~D " (< 0 m)
			       (abs m))))
			'(1 -1 8 -8))))))
      `((section :title "Available Projects" )
       (p "Order the available projects listed below to reflect your
preferences")
       ((table :border 1 :celspacing 0 :class projects)
	(tr (th) (th "Title") (th "Supervisor(s)") (th "Action"))
	,@(mapcar #'table-row chosen))))))

(defmethod allocate-projects-handler((app project-manager) request rest)
  (unless (has-permission :admin app) (throw 'response :forbidden))
  (when (car (form-values "submit" request))
    (let* ((user (get-dictionary (car (form-values "username" request))
				 (users app)))
	   (project (get-project (car (form-values "projectid" request)) app)))
	   (when (and user project (not (student project)))
	     (setf (student project) (username user))
	     (store-project project app))))
    (let ((students
	   (mapcar
	    #'car
	    (sort
	     (mapcan
	      #'(lambda(username)
		  (when (not (get-project username app))
		    (let ((user (get-dictionary username (users app))))
		      (list
		       (list
			user
			(or (property (user-component-properties app user) :score)
			    0)
			(property
			 (user-component-properties app user) :choices))))))
	      (get-users :student app))
	     #'(lambda(a b)
		 (cond
		   ((and (third a) (third b)) (> (second a) (second b)))
		   ((third a) t)
		   ((third b) nil)
		   ((> (second a) (second b))))))))
	  (available (available-projects app))
	  (allocated nil))
      `(html
	(head (title "Allocate Projects"))
	(body
	 (navbar ,(menus app) :relative "../" :on-url "/allocate/")
	 ((section :title "Allocate Projects")
	  (p "Number indicates students priority - where there is none
the student has not made any selections")
	  (table
	   (tr (th "Student") (th "Choices"))
	   ,@(mapcar
	      #'(lambda(user)
		  (let* ((choices
			  (mapcan #'(lambda(choice)
				      (let ((p (get-project choice app)))
					(when p (list p))))
				  (property
				   (user-component-properties app user) :choices)))
			 (best (or
				(find-if-not
				 #'(lambda(item) (member item allocated))
				 choices)
				(find-if-not
				 #'(lambda(item) (member item allocated))
				 available))))
		    (push best allocated)
		    `(tr
		      (td ,(display-name user) " (" ,(username user)")")
		      (td
		       ((form :method :post)
			((mcq :name "projectid" :style :dropdown
			      :value ,(when best (project-id best)))
			 (nil . "None")
			 ,@(mapcar
			    #'(lambda(choice)
				(cons (project-id choice)
				      (format nil "~@[~D.~] ~A"
					      (position choice choices)
					      (title choice) )))
			    available))
			((input :type :submit :name "submit" :value "Allocate"))
			((input :type :hidden
				:name "username" :value ,(username user))))))))
	      students)))))))

(defmethod allocate-assessments-handler((app project-manager) request rest)
  (unless (has-permission :admin app) (throw 'response :forbidden))
  (when (car (form-values "submit" request))
    (flet ((get-assessor(form)
	     (let ((assessor (car (form-values (string form) request))))
	       (if (string= assessor "None") nil assessor))))
      (let* ((projectid (car (form-values "project" request)))
	     (project (get-project projectid app)))
	(when project
	  (unless (assessments project)
	    (setf (assessments project)
		  (mapcar #'(lambda(spec)
			      (make-assessment :form (first spec)
					       :weight (second spec)))
			  (assessments app))) )
	  (dolist(assessment (assessments project))
	    (unless (assessment-submissions assessment)
	      (setf (assessment-assessor assessment)
		    (get-assessor (assessment-form assessment)))))
	  (store-project project app)))))
  `(html
    (head (title "Allocate Assessments"))
    (body
     (navbar ,(menus app) :relative "../" :on-url "/assessments/")
     ((section :title "Tutors Accumulative Workload")
      ,(tutor-workload-table app))
     ((section :title "Allocate Assessments")
      ((table :border "1" :cellspacing "0" :class"projects")
       (tr ((th :width "30%") "Title")
	   (th "Student") (th "Status") (th "Assessors"))
       ,@(mapcar
	  #'(lambda(p)
	      `(tr
		(td ((a :href ,(format nil "../~A/" (project-id p)))
		     ,(title p)))
		(td ,(display-name (get-dictionary (student p) (users app))))
		(td ,(status-string p))
		((td :align :center)
		 ((form :method :post)
		  ((input :type :hidden
			  :name "project" :value ,(project-id p)))
		  (table
		   ,@(flet
		      ((user-mcq(id &optional value disabled)
			  `((mcq
			     :name ,id
			     :style :dropdown
			     :disabled ,disabled
			     :value ,(or value "None"))
			    "None"
			    ,@(let ((spec
				     (third (assoc id (assessments app)))))
				   (sort (cond
				     ((functionp spec)
				      (funcall spec p))
				     ((get-users spec app))
				     ((get-users :tutor app)))
					 #'string<)))))
		      (if (assessments p)
			  `((tr
			     ,@(mapcar
				#'(lambda(a)
				    `(th ,(title (find-form
						  (assessment-form a)))
				      " (" ,(assessment-weight a) ")"))
				(assessments p)))
			    (tr
			     ,@(mapcar
				#'(lambda(a)
				    `(td ,(if (assessment-submission a)
					      `(p ,(assessment-assessor a)
						" (" ,(mark-form-data (assessment-submission a)) ")")
					      (user-mcq
					       (assessment-form a)
					       (assessment-assessor a) ))))
				(assessments p))))
			  `((tr
			     ,@(mapcar #'(lambda(a)
					   `((th :align :center)
					     ,(title (find-form (first a)))
					     " (" ,(second a) ")" ))
				       (assessments app)))
			    (tr ,@(mapcar #'(lambda(a)
					      `((td :align :center)
						,(user-mcq (first a))))
					  (assessments app)))))))
		  ((input :type "submit" :name "submit" :value "Allocate"))))))
	  (sort
	   (filtered-projects
	    app
	    #'(lambda(project)
		(and (student project)
		     (or (not (assessments project))
			 (notevery #'assessment-submissions
				   (assessments project))))))
	   #'project<)))))))

(let ((effort-weighting
       '((:supervised . 6)
	 (:project-supervisor . 0)
	 (:project-oral . 1)
	 (:project-dissertation-1 . 3)
	 (:project-dissertation-2 . 3)
	 (:beng-dissertation-moderator . 3)
	 (:ug-oral . 1)
	 (:beng-dissertation-supervisor . 3)
	 (:meng-dissertation-moderator . 3)
	 (:meng-dissertation-supervisor . 3))))
  (defun tutor-workload(rec)
    (reduce #'+ (mapcar
		 #'(lambda(c) (* (cdr c) (getf rec (car c) 0)))
		 effort-weighting))))
	  
  
(defun tutor-workload-table(app &optional (scorefunc #'tutor-workload))
  "Produce a table of tutors workload"
  (let ((records nil)
	(completed nil)	
	(fields (list :supervised)))
    (labels((inc-entry(username key &optional (value 1))
	    (pushnew key fields)
	    (let ((rec (assoc username records :test #'equal)))
	      (unless rec
		(setf rec (cons username nil))
		(setf records (cons rec records)))
	      (incf (getf (cdr rec) key 0) value)))
	  (inc-completed(username key &optional (value 1))
	    (let ((rec (assoc username completed :test #'equal)))
	      (unless rec
		(setf rec (cons username nil))
		(setf completed (cons rec completed)))
	      (incf (getf (cdr rec) key 0) value))))
      (dolist(project (filtered-projects app #'student))
	(dolist(name (supervisors project))
	  (inc-entry name :supervised
		     (/ 1 (length (supervisors project)))))
	(dolist(a (assessments project))
	  (inc-entry (assessment-assessor a)  (assessment-form a))
	  (when (assessment-submissions a)
	    (inc-completed (assessment-assessor a)
			   (assessment-form a))))))
    (setf fields (nconc fields (list :total)))
    (dolist(rec records)
      (setf (getf (cdr rec) :total) (funcall scorefunc (cdr rec))))
    (dolist(rec completed)
      (setf (getf (cdr rec) :total) (funcall scorefunc (cdr rec))))
    `((table :border 1)
      (tr
       (th "Tutor") ,@(mapcar #'(lambda(f) `(th ,(string f))) fields))
      ,@(mapcar
	 #'(lambda(rec)
	     (let ((f (cdr rec))
		   (c (cdr (assoc (car rec) completed))))
	       `(tr (th ,(display-name
			  (get-dictionary (car rec) (users app))))
		 ,@(mapcar
		    #'(lambda(v) `
			((td :align :center)
			 ,(getf f v) ,(when (getf c v)
					    " (" (getf c v) ")")
			 )) fields))))
	 (sort records #'> :key #'(lambda(r) (getf (cdr r) :total)))))))

(defmethod allocate-scores-handler((app project-manager) request rest)
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
       (table (tr (th "Student") (th "Score"))
	      ,@(mapcar
		 #'(lambda(username)
		     (let ((user (get-dictionary username (users app))))
		       `(tr
			 (td ,(display-name user))
			 (td ((input
			       :name ,username
			       :value ,(property
				       (user-component-properties app user)
				       :score)))))))
		 students))
       ((input :type :submit :name "submit" :value "Update Scores"))))))))

(defmethod marks-handler((app project-manager) request rest)
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
	  ((mcq :style :dropdown :name :roleadd/ :value ,(string-downcase role))
	   ,@(mapcar #'string-downcase (roles (users app))))
	  " format: " ((mcq :name :format :style :dropdown) "html")
       ((input :name :submit :type :submit :value "Get Marks"))))
      (hr)
      ,@(when
	 role
	 (let ((records
		  (mapcar
		   #'(lambda(s) (cons s (get-project s app)))
		   (sort (get-users role (users app)) #'string<)))
		(assessments nil))
	   (dolist(r records)
	     (when (cdr r)
	       (dolist(a (assessments (cdr r)))
		 (pushnew (assessment-form a) assessments))))
	      `(((table :border 1)
		 (tr (th "Student")
		  ,@(mapcar #'(lambda(a) `((th :width "10%")
					   ,(title (find-form a))))
			    assessments)
		  (th "Mark (%)") (th "Project"))
		 ,@(mapcar
		    #'(lambda(r)
			(let* ((username (car r))
			       (user (get-dictionary username (users app)))
			       (project (cdr r)))
			`(tr			  
			  ,@(if project
				`((td
				   ((a :href ,(format nil "../~A/"
						      (project-id project)))
				    ,(display-name user)))
				  ,@(mapcar 
				     #'(lambda(a)
					 `((td :align :right)
					   ,(mark-form-data
					     (assessment-submission
					      (find a (assessments project)
						    :key #'assessment-form)))))
				     assessments)
				  ((td :align :right)
				   ,(when project
					  (format nil "~4,1f"
						  (* 100 (project-mark project)))))
				  (td ,(when project (title project))))
				`((td ,(display-name user))
				  ((td :colspan ,(+ 2 (length assessments))
				   :align :center)
				   "No Project Allocated"))))))
		    records) )))) )))))