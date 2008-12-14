;; $Id: database.lisp,v 1.2 2007/04/26 07:30:18 willijar Exp willijar $
;; Database API
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


(clsql:locally-enable-sql-reader-syntax)

(defvar *refresh* t)
(defvar *caching* nil)

(defgeneric cached-slots(entity)
  (:documentation "Return a list of the cached field to be unbound when instance is updated from record")
  (:method(entity) nil))

(defgeneric refresh-instance(entity)
  (:documentation "Refresh instacne when updated from record")
  (:method(entity)
    (mapcar #'(lambda(slot) (slot-makunbound entity slot))
            (cached-slots entity))))

(def-view-class view-with-cached-slots()
  ()
  (:documentation "A view class with some cached slots to be invalidated when instance is updated from record"))

(defmethod update-instance-from-records
    ((view view-with-cached-slots)
     &key (database (clsql-sys::view-database view)))
  (refresh-instance view)
  (call-next-method))

(defmacro def-view-lookup((accessor (name thisclass)) &key
                          class using (home-key using)
                          (foreign-key using)
                          set (slot accessor))
  "Macro to overwite readers for view-class joins to overcome caching problem"
  (flet ((keytest(h f) `([= [slot-value ',class ',f]
                         (slot-value ,name ',h)])))
    (let ((theset (gensym)))
      `(defmethod ,accessor((,name ,thisclass))
        "Automatically generated view lookup"
        (or (and (slot-boundp ,name ',slot) (slot-value ,name ',slot))
            (setf (slot-value ,name ',slot)
                  (let ((,theset
                         (clsql:select
                          ',class
                          :database (clsql-sys::view-database ,name)
                          :flatp t
                          :caching *caching*
                          :refresh *refresh*
                          :where
                          ,@(if (listp home-key)
                                `([and ,@(mapcan #'keytest
                                                 home-key foreign-key)])
                                (keytest home-key foreign-key)))))
                    ,(if set theset `(car ,theset)))))))))

;;; general access control API
(defgeneric can-view(app item &optional user)
  (:documentation "Return true if user can view item through app")
  (:method :around(app item &optional (user *current-user*))
	   (call-next-method app item user))
  (:method(app item &optional user)
    (declare (ignore app item user))
    t))

(defgeneric can-edit(app item &optional user)
  (:documentation "Return true if user can edit item through app")
  (:method :around(app item &optional (user *current-user*))
	   (call-next-method app item user))
  (:method(app item &optional user)
    (declare (ignore item))
    (has-permission '(:admin) app user)))

(defgeneric can-add(app class &optional user)
  (:documentation "Return true if user can edit item through app")
  (:method :around(app class &optional (user *current-user*))
	   (call-next-method app class user))
  (:method(app class &optional user)
    (declare (ignore class))
    (has-permission '(:admin) app user)))

(defgeneric can-delete(app item &optional user)
  (:documentation "Return true if user can edit item through app")
  (:method :around(app item &optional (user *current-user*))
	   (call-next-method app item user))
  (:method(app item &optional user)
    (declare (ignore app item user))
    nil))

(def-view-class programme()
  ((programmeid :accessor programmeid :type (string 3) :initarg :programmeid
		:documentation "Two letter code identifying programme")
   (award :accessor award :type (string 5) :initarg :award
	  :documentation "Award associated with this programme")
   (title :accessor title :type (string 4) :initarg :title
	  :documentation "Programme title"))
  (:base-table programmes))

(defmethod print-object((programme programme) stream)
  (print-unreadable-object (programme stream :type t :identity t)
     (format stream "~S"  (title programme))))

(def-view-class student (view-with-cached-slots)
  ((studentid :accessor studentid :type (string 9) :initarg :studentid
	      :documentation "Unique Student ID (SUN)"
	      :db-kind :key :db-constraints :not-null)
   (title :accessor title :type (string 4) :initarg :title
	  :documentation "Name title")
   (firstname :accessor firstname :type (string 25) :initarg :firstname
	      :documentation "First name in full")
   (initials :accessor initials :type (string 8) :initarg :initials
	     :documentation "Initials, with no deperation")
   (lastname :accessor lastname :type (string 25) :initarg :lastname
	     :documentation "Last name in Full")
   (year :accessor year :type integer :initarg :year :initform 2005
	 :documentation "Year of entry to programme")
   (programmeid :accessor programmeid :type (string 3) :initarg :programme
		:initform "TT"
		:documentation "Two letter code identifying programme")
   (username :accessor username :type (string 16) :initarg :username
	     :documentation "Students login username")
   (nationality :accessor nationality :type (string 50) :initarg :nationality
		:void-value nil)
   (active :accessor active :type boolean :initform t
	   :documentation "True if student still on books")
   (suspended :accessor suspended :type string
	      :void-value nil
	      :documentation "If suspended - reason why")
   (candidate :accessor candidate :type (string 7))
   (modified  :type timestamp :accessor timestamp)
   (date-of-birth :type timestamp :accessor date-of-birth
		  :initarg :date-of-birth
		  :void-value nil
		  :column "dateofbirth")
   (notes :accessor notes :type (string) :void-value nil)
   (programme :db-kind :virtual :type programme)
   (marks :db-kind :virtual :type list)
   (module-marks :db-kind :virtual :type list)
   (examboard-decisions  :db-kind :virtual :type list)
   (project :db-kind :virtual :type project))
  (:base-table students))

(defmethod cached-slots((student student))
  '(marks module-marks examboard-decisions project))

(def-view-lookup(programme (student student))
    :class programme :using programmeid :set nil)

(def-view-lookup(marks (student student))
    :class mark :using studentid :set t)

(def-view-lookup(module-marks (student student))
   :class module-mark :using studentid :set t)

(def-view-lookup(examboard-decisions (student student))
    :class examboard-decision :using studentid :set t)

(def-view-lookup(project (student student))
    :class project :using studentid :set nil)


(defmethod last-examboard-decision((student student))
  (let ((last nil))
    (dolist(d (examboard-decision-records
               (clsql-sys::view-database student)
               'studentid (studentid student)))
      (when (or (not last)
                (and (action d)
                     (or (> (finish (examboard d)) (finish (examboard last)))
                         (and (= (finish (examboard d))
                                 (finish (examboard last)))
                              (> (revision d) (revision last))))))
        (setf last d)))
    last))

(defmethod email-address((student student))
  (concatenate 'string (username student) "@aston.ac.uk"))

(defmethod print-object((student student) stream)
  (print-unreadable-object (student stream :type t :identity t)
     (format stream "~S ~A ~A (~A)"
	     (username student)
	     (programmeid student)
	     (year student)
	     (studentid student))))

(defmethod fullname((student student))
  (format nil "~A~@[, ~A~]~@[ ~A~]~@[ (~A)~]"
	  (lastname student)
	  (firstname student)
	  (initials student)
	  (title student) ))

(def-view-class module()
  ((moduleid :accessor moduleid :type (string 6) :initarg :moduleid
	     :db-kind :key :db-constraints :not-null)
   (title :accessor title :type (string 50) :initarg :title)
   (credits :accessor credits :type integer :initarg :credits)
   (level :accessor level :type integer
          :initarg :level :initform 4)
   (stage :accessor stage :type integer :initarg :stage :initform 1)
   (teaching_period :accessor teaching-period :type integer
                    :initarg :teaching-period :initform 1)
   (owner :accessor owner :type (string 50) :initarg :owner)
   (year :accessor year :type integer :initarg :year :db-kind :key)
   (pass-mark :accessor pass-mark :type number
	      :void-value 50 :column "pass_mark")
   (assessments :db-kind :virtual :type list)
   (module-marks :type list :reader module-marks
                 :db-kind ::join
                 :db-info (:join-class module-mark
                           :home-key (moduleid year)
                           :foreign-key (moduleid year))))
  (:base-table modules))

(def-view-lookup(assessments (m module))
    :class assessment :using (moduleid year) :set t)

(defmethod print-object((module module) stream)
  (print-unreadable-object (module stream :type t :identity t)
     (format stream "~A ~A"
	     (moduleid module)
	     (year module))))

(defun module-cmp(a b)
  "Module comparison function for sorting"
  (or (> (year a) (year b))
      (string< (moduleid a) (moduleid b))))

(def-view-class assessment()
  ((assessmentid :accessor assessmentid :type integer :initarg :assessmentid
		 :db-kind :key :db-constraints :not-null)
   (moduleid :accessor moduleid :type (string 6) :initarg :moduleid
	     :db-kind :key :db-constraints :not-null)
   (title :accessor title :type (string 50) :initarg :title)
   (percentage :accessor weighting :type float :initarg :weighting
	       :column "percentage")
   (assessor :accessor assessor :type (string 50) :initarg :accessor)
   (type :accessor assessment-type :type (string 4)
         :initarg type :initform "EXAM")
   (year :accessor year :type integer :initarg :year)
   (deadline-date :type timestamp :accessor deadline-date :initarg :deadline
		  :column "deadline_date")
   (release-date :type timestamp :accessor release-date :initarg :release-date
		 :column "release_date")
   (feedback-form :accessor feedback-form :type list :initarg :feedback-form
		  :column "feedback_form")
   (status :type (string 10) :accessor status :initarg :status
	   :initform "Compulsory")
   (requirements :type string :accessor requirements :initarg :requirements
                 :void-value "" :initform "")
   (module :db-kind :virtual :type module)
   (marks :db-kind :virtual :type list))
  (:base-table assessments))

(def-view-lookup(marks (a assessment))
  :class mark :using assessmentid :set t)

(def-view-lookup(module (a assessment))
    :class module :using (moduleid year) :set nil)

(defmethod feedback-available-p((assessment assessment))
  (and (release-date assessment)
       (< (release-date assessment) (get-universal-time))))

(defmethod print-object((assessment assessment) stream)
  (print-unreadable-object (assessment stream :type t :identity t)
     (format stream "~A ~A ~A ~,1F% ~A"
	     (assessmentid assessment)
	     (moduleid assessment)
	     (year assessment)
       (weighting assessment)
	     (assessment-type assessment))))

(def-view-class with-revisions()
  ((revision :reader revision :type integer :initform 1 :initarg :revision
	     :db-kind :key :db-constraints :not-null)
   (modified :accessor modified :initarg :modified
	     :type timestamp :initform (get-universal-time))
   (modified-by :accessor modified-by :type (string 50) :initarg :modified-by
		:initform (when *current-user* (username *current-user*))
		:column "modified_by"))
  (:documentation "A mix in class allowing for storing revisions to an
entity"))

(defmethod can-edit(app (item with-revisions) &optional user)
  (or (has-permission :admin app user)
      (string= (username user) (modified-by item))))

(def-view-class mark-base(with-revisions view-with-cached-slots)
  ((attempt :reader attempt :type integer :initarg :attempt :initform 1
	    :db-kind :key :db-constraints :not-null))
  (:documentation "A mix in class for mark type entities - with an
attempt field as well as with revisions"))

(def-view-class mark(mark-base)
  ((assessmentid :reader assessmentid :type integer :initarg :assessmentid
		 :db-kind :key :db-constraints :not-null)
   (studentid :reader studentid :type (string 9) :initarg :studentid
	      :db-kind :key :db-constraints :not-null)
   (mark :initform nil :accessor mark :type percentage :initarg :mark)
   (note :initform nil :accessor note :type string :initarg :note)
   (feedback
    :accessor feedback :type list :initarg :feedback :initform nil
    :documentation "List of feedback from the assessment feedback form")
   (deadline-date :initarg :deadline-date :accessor deadline-date
		  :type timestamp :initform nil
		  :column "deadline_date")
   (release-date :type timestamp :accessor release-date :initarg :release-date
		 :column "release_date")
   (submission-date :initarg :submission-date
		    :type timestamp :accessor submission-date :initform nil
		    :column "submission_date")
   (student :db-kind :virtual :type student)
   (assessment :db-kind :virtual :type assessment))
  (:base-table marks))

(defmethod feedback-form((m mark))
  (feedback-form (assessment m)))

(def-view-lookup(student (m mark))
    :class student :using studentid :set nil)

(def-view-lookup(assessment (m mark))
    :class assessment :using assessmentid :set nil)

(defmethod module((m mark))
  (module (assessment m)))

(defmethod release-date((mark mark))
  (or (slot-value mark 'release-date) (release-date (assessment mark))))

(defmethod feedback-available-p((mark mark))
  (and (release-date mark)
       (< (release-date mark) (get-universal-time))))

(defmethod modify-mark((oldmark mark) &key
                       (mark (mark oldmark))
                       (note (note oldmark))
                       (feedback (feedback oldmark))
                       (deadline-date (slot-value oldmark 'deadline-date))
                       (submission-date (submission-date oldmark))
                       (modified-by (if *current-user*
                                        (username *current-user*)
                                        (modified-by oldmark)))
                       (attempt (attempt oldmark)) &allow-other-keys)
  (when (and
         (percentage= (mark oldmark) mark)
         (equal (note oldmark) note)
         (equalp (feedback oldmark) feedback)
         (eql (deadline-date oldmark) deadline-date)
         (eql (attempt oldmark) attempt)
         (eql (submission-date oldmark) submission-date))
    ;; no changes made
    (return-from modify-mark oldmark))
  (let ((newmark
         (if (or (not (= (attempt oldmark) attempt))
                 (and (mark oldmark) (feedback-available-p oldmark)))
             ;; create a new record
             (make-instance 'mark
                            :assessmentid (assessmentid oldmark)
                            :studentid (studentid oldmark)
                            :attempt attempt
                            :revision (if (= (attempt oldmark) attempt)
                                          (1+ (revision oldmark))
                                          1)
                            :modified-by modified-by
                            :modified (get-universal-time)
                            :mark mark
                            :note note
                            :feedback feedback
                            :deadline-date deadline-date
                            :submission-date submission-date)
             (progn
               (setf (mark oldmark) mark
                     (note oldmark) note
                     (feedback oldmark) feedback
                     (deadline-date oldmark) deadline-date
                     (submission-date oldmark) submission-date)
               oldmark))))
    (update-records-from-instance newmark)
    newmark))

(defmethod can-view(app (mark mark) &optional user)
  (or (has-permission '(:admin :tutor :supervisor) app user)
      (and (has-permission :student app user)
           (string= (studentid mark) (studentid user)))))

(defmethod can-edit(app (mark mark) &optional user)
  (or (has-permission :admin app user)
      (and (not (and (mark mark) (feedback-available-p mark)))
	   (or (string= (username user) (modified-by mark))
	       (string= (username user) (assessor (assessment mark)))
	       (string= (username user) (owner (module (assessment mark))))))))

(defmethod deadline-date((mark mark))
  (or (slot-value mark 'deadline-date) (deadline-date (assessment mark))))

(defmethod (setf deadline-date)((val integer) (mark mark))
  (setf (slot-value mark 'deadline-date) val))

(defun working-days-late(deadline &optional (date (get-universal-time)))
  "Calculate the integer number of working days between a deadline and a
date. It is assumed that both fall on a working day."
  (flet((day-of-week(date)
	  (elt (multiple-value-list
			   (decode-universal-time date)) 6)))
    (when (and deadline date (< deadline date))
      (multiple-value-bind(weeks leftover)
          (floor (/ (- date deadline) (* 60 60 24 7)))
        (+ (* weeks 5) (ceiling (* 7 leftover))
           (cond
             ((> (day-of-week date) 4) ; at weekend
              (- 5 (day-of-week date)))
             ((< (day-of-week date) (day-of-week deadline)) -2)
             (0)))))))

(defmethod days-late((mark mark))
  "If a submission for a mark is late return the number of days late"
  (when (submission-date mark)
    (working-days-late (deadline-date mark) (submission-date mark))))

(defmethod major-work-p((assessment assessment))
  "Return true if is to be considered a major piece of work according
to school guidelines - more than 5 credits"
  (>= (/ (* (weighting assessment) (credits (module assessment))) 100)  5))

(defmethod late-penalty((mark mark))
  "Late penalty in % according to school guidelines of Jan 04
For major works 5% for first day + 1% per extra day late
For minor works 10% per day late"
  (let ((days-late (days-late mark))
	(major-p (major-work-p (assessment mark))))
    (if days-late
        (+ (if major-p -5 -10)
           (* (1- (ceiling days-late)) (if major-p -1 -10)))
        0)))

(defmethod print-object((mark mark) stream)
  (print-unreadable-object (mark stream :type t :identity t)
     (format stream "~A ~S ~A ~:[Attempt ~D~;~]"
	     (assessmentid mark)
	     (studentid mark)
	     (mark mark)
	     (< (attempt mark) 2)
	     (attempt mark))))

(defmethod (setf feedback)((feedback list) (mark mark))
  (call-next-method))
#|
  (setf (slot-value mark 'feedback) feedback)
  (when (and feedback (feedback-form (assessment mark)))
    (setf (mark mark)
	  (form-mark (car feedback) (feedback-form (assessment mark))))))
|#
(def-view-class module-mark(mark-base)
  ((moduleid :accessor moduleid :type (string 6) :initarg :moduleid
	     :db-kind :key :db-constraints :not-null)
   (year :accessor year :type integer :initarg :year :db-kind :key)
   (studentid :accessor studentid :type (string 9) :initarg :studentid
	   :db-kind :key :db-constraints :not-null)
   (mark :accessor mark :type percentage :void-value nil :initarg :mark)
   (note :accessor note :type string :initarg :note)
   (marks :type list :db-kind :virtual
	  :documentation
	  "The assessment marks which contribute to this module mark")
   (student :reader student
	  :db-kind :join
	  :db-info (:join-class student
		    :home-key studentid
		    :foreign-key studentid
		    :set nil))
   (module :db-kind :join :reader module
           :db-info (:join-class module
                     :home-key (moduleid year)
                     :set nil
                     :foreign-key (moduleid year))))
  (:base-table module_marks))

(defmethod cached-slots((m module-mark))
  '(marks))

(def-view-lookup(module (module-mark module-mark))
    :class module :using (moduleid year) :set nil)

(defmethod instance-refreshed((m module-mark))
  (mapcar #'(lambda(slot) (slot-makunbound m slot))
          '(marks)))

(defmethod can-view(app (mark module-mark) &optional user)
  (or (has-permission '(:admin :tutor :supervisor) app user)
      (and (has-permission :student app user)
           (string= (studentid mark) (studentid user)))))

(defmethod print-object((mark module-mark) stream)
  (print-unreadable-object (mark stream :type t :identity t)
     (format stream "~A ~A ~A ~:[Attempt ~D~;~]"
	     (moduleid mark)
	     (studentid mark)
	     (mark mark)
	     (< (attempt mark) 2)
	     (attempt mark))))

(defmethod passedp((mark module-mark))
  (let ((v (mark mark)))
    (and v (or (>= v (pass-mark (module mark)))
               (search "condone" (note mark) :test #'equalp)))))

(defmethod passedp((mark mark))
  (and (mark mark)
       (>= (mark mark) (pass-mark (module  mark)))))

(defmethod condonedp(mark)
  (search "Condone" (note mark) :test #'char-equal))

(defmethod referredp(mark)
  (> (attempt mark) 1))

(defmethod repeatp(mark)
  (search "Repeat" (note mark) :test #'char-equal))

(defmethod mark-status-message(mark &key (publicp nil))
  (format nil "~:[~;Referred. ~]~:[~;Condoned. ~]~:[~;Repeat. ~]~@[Late Penalty: ~A%.~]"
	  (referredp mark)
	  (condonedp mark)
	  (unless publicp (repeatp mark))
	  (when (and (typep mark 'mark) (< (late-penalty mark) 0))
	    (late-penalty mark)) ))

(defmethod credits((mark module-mark))
  "The credit value acheived with this module mark"
  (if (or (passedp mark) (condonedp mark))
      (credits (module mark))
      0))

(defmethod assessments((module-mark module-mark))
  "Return the list of assessments that contribute to a module-mark"
  (assessments (module module-mark)))

(defmethod calculated-mark((module-mark module-mark))
  "Calculated mark uses the most recent assessment marks"
  (slot-makunbound module-mark 'marks)
  (slot-makunbound (student module-mark) 'marks)
  (average-mark
   (mapcar
    #'(lambda(group)
         (or (find-if #'mark group) (car group)))
    (mark-groups module-mark))
   :ignore-nil nil
   :ignore-0 nil
   :weight #'(lambda(m) (weighting (assessment m)))
   :mark #'(lambda(m) (+ (or (mark m) 0) (late-penalty m)))))

(defmethod calculated-mark((mark mark))
  (let ((form (feedback-form (assessment mark))))
    (if (and form (> (length (feedback mark)) 3))
        (* 100.0 (form-mark (feedback mark) form))
        (mark mark))))

(def-view-class examboard()
  ((examboardid :accessor examboardid :type integer :db-kind :key
                :initarg :examboardid)
   (start :type timestamp :accessor start :initarg :start)
   (finish :type timestamp :accessor finish :initarg :finish)
   (decisions :db-kind :virtual :type list))
  (:base-table examboards))

(def-view-lookup(decisions (examboard examboard))
    :class examboard-decision :using examboardid :set t)

(defmethod print-object((examboard examboard) stream)
  (print-unreadable-object (examboard stream :type t :identity t)
     (format stream "~A (~A)"
	     (examboardid examboard)
	     (format-timestamp (start examboard)))))

(def-view-class examboard-decision(with-revisions)
  ((examboardid :accessor examboardid :type integer :db-kind :key
		:initarg :examboardid)
   (studentid :accessor studentid :type (string 9) :initarg :studentid
	   :db-kind :key :db-constraints :not-null)
   (student :accessor student
	  :db-kind :join
	  :db-info (:join-class student
		    :home-key studentid
		    :foreign-key studentid
		    :set nil))
   (revision :accessor revision :type integer :db-kind :key)
   (action :accessor action :type (string 20) :initarg :action)
   (argument :accessor argument :type (string 100) :initarg :argument)
   (notes :accessor notes :type (string) :initarg :note)
   (examboard :accessor examboard
	  :db-kind :join
	  :db-info (:join-class examboard
		    :home-key examboardid
		    :foreign-key examboardid
		    :set nil)))
  (:base-table examboard_decisions))

(defmethod print-object((decision examboard-decision) stream)
  (print-unreadable-object (decision stream :type t :identity t)
    (format stream "~A ~A \"~A ~A\""
            (examboardid decision)
            (studentid decision)
            (action decision)
            (argument decision))))

(def-view-class project(view-with-cached-slots)
  ((projectid :accessor projectid :type integer  :db-kind :key
              :initarg :projectid)
   (studentid :accessor studentid :type (string 9)
              :void-value nil :initarg :studentid :initform nil)
   (moduleids :type string :initarg :moduleid :column "moduleid")
   (title :accessor title :type (string 255) :initarg :title)
   (student :accessor student
            :db-kind :join
            :db-info (:join-class student
                                  :home-key studentid
                                  :foreign-key studentid
                                  :set nil))
   (description :accessor description :type list
                :initarg :description :initform nil)
   (supervisors :accessor supervisors :type list :initarg :supervisors
                :initform nil)
   (start-date  :type timestamp :accessor start-date :initform nil)
   (deadline-date :type timestamp :accessor deadline-date :initform nil)
   (submission-date :type timestamp :accessor submission-date :initform nil)
   (module-marks :db-kind :virtual :type list))
  (:base-table projects))

(defmethod cached-slots((p project))
  '(module-marks))

(defgeneric moduleids(project)
  (:documentation "Return the moduleids associated with this project")
  (:method((project project))
    (let ((s (slot-value project 'moduleids)))
      (when s (split-string s)))))

(defgeneric (setf moduleids)(values project)
  (:documentation "Set the moduleids associated with this project")
  (:method((values list) (project project))
     (setf (slot-value project 'moduleids) (join-strings values))))

(defgeneric module-marks(project)
  (:documentation "Return the module marks associated with a project"))

(defmethod module-marks((p project))
  (when (and (student p) (moduleids p)q)
    (if (slot-boundp p 'module-marks)
        (slot-value p 'module-marks)
        (setf (slot-value p 'module-marks)
              (mapcan
               #'(lambda(moduleid)
                   (let ((m
                          (first
                           (current-module-marks
                            (clsql:select
                             'module-mark
                             :database (clsql-sys::view-database p)
                             :caching nil
                             :flatp t
                             :where [and [= [slot-value 'module-mark 'studentid]
                             (studentid p)]
                             [= [slot-value 'module-mark 'moduleid]
                             moduleid]])))))
                     (when m (list m))))
               (moduleids p))))))

(defmethod assessments((p project))
  (mapcar #'assessments (module-marks p)))

(defmethod marks((p project))
  (reduce #'append (mapcar #'current-marks (module-marks p))))

(defmethod lockedp((p project))
  "Return t if project cannot be modified - if all module marks have been
given for the project"
  (and (module-marks p)
       (every #'mark (module-marks p))))

(let ((status-fields
       '(studentid supervisors start-date deadline-date
         submission-date)))
  (defmethod status-data((p project))
    (mapcan #'(lambda(field)
                (when (slot-value p field)
                  (list (intern (string field) :keyword)
                        (slot-value p field))))
            status-fields))
  (defmethod (setf status-data)(value (p project))
    (dolist(field status-fields)
      (let ((v (getf value (intern (string field) :keyword) :missing)))
        (unless (eq v :missing)
          (setf (slot-value p field) v))))))

(defun status-string(p)
  (cond
    ((not (student p)) )
    ((submission-date p)
     (format nil "Submitted ~A" (format-timestamp (submission-date p))))
    ((and (start-date p)
          (> (start-date p) (get-universal-time)))
     (format nil "Started ~A ~:[~;due ~A~]"
             (format-timestamp (start-date p))
             (deadline-date p)
             (format-timestamp (deadline-date p))))))

(defmethod print-object((project project) stream)
  (print-unreadable-object (project stream :type t :identity t)
    (format stream "~A ~@[(~A)~]"
            (projectid project)
            (when (slot-boundp project 'studentid) (studentid project)))))

(defmethod programme-modules((programme programme) year)
  (let ((moduleids
         (or
          (clsql:query
           (format nil "select moduleid from programme_modules where programmeid='~A' and year=~S" (programmeid programme) year)
           :flatp t
           :database (clsql-sys::view-database programme)
           )
          (clsql:query
           (format nil "select distinct moduleid from module_marks left join students using (studentid) where programmeid='~A' and students.year=~S" (programmeid programme) year)
           :flatp t
           :database (clsql-sys::view-database programme)
           ))))
    (clsql:select
     'module
     :database (clsql-sys::view-database programme)
     :flatp t
     :caching nil
     :where [and [= [slot-value 'module 'year] year]
     [in [slot-value 'module 'moduleid] moduleids]])))

(defmethod marks((module-mark module-mark))
  "Return the list of (assessment) marks that contribute to a module-mark"
  (if (slot-boundp module-mark 'marks)
      (slot-value module-mark 'marks)
      (setf (slot-value module-mark 'marks)
            (mapcan
             #'(lambda(m)
                 (when (equal (moduleid (assessment m)) (moduleid module-mark))
                   (list m)))
             (marks  (student module-mark))))))

(defmethod module-mark((m mark))
  (car (current-module-marks
        (clsql:select
         'module-mark
         :database (clsql-sys::view-database m)
         :caching nil
         :flatp t
         :where [and [= [slot-value 'module-mark 'studentid]
                        (studentid m)]
                [= [slot-value 'module-mark 'moduleid]
                   (moduleid (assessment m))]]))))

;;; The following methods return records from the corresponding table in the
;;; source. Criteria mat be specified as a p-list of slot names in the records
;;; and the value which must match.

(let ((grades-package *package*)) ;; ensure symbols are read in this package
  (defun simple-query(db table &optional criteria)
    (let ((*package* grades-package))
      (apply #'clsql:select
             `(,table
               :database ,db
               ,@(when criteria
                       `(:where
                         ,(clsql:sql-and
                           (loop for a on criteria by #'cddr
                                 collect
                                 (cond
                                   ((null (cadr a))
                                    [null [slot-value table (car a)]])
                                   ((listp (cadr a))
                                    [in [slot-value table (car a)] (cadr a)])
                                   (t
                                    [= [slot-value table (car a)] (cadr a)]))))))
               :flatp t
               :caching nil)))))

;;; accessors for a grades database

(defmethod student-records(db &rest criteria)
  (simple-query db 'student criteria))

(defmethod module-records(db &rest criteria)
  (simple-query db 'module criteria))

(defmethod assessment-records(db &rest criteria)
  (simple-query db 'assessment criteria))

(defmethod mark-records(db &rest criteria)
  (simple-query db 'mark criteria))

(defmethod module-mark-records(db &rest criteria)
  (simple-query db 'module-mark criteria))

(defmethod examboard-records(db &rest criteria)
  (simple-query db 'examboard criteria))

(defmethod examboard-decision-records(db &rest criteria)
  (simple-query db 'examboard-decision criteria))

(defmethod project-records(db &rest criteria)
  (simple-query db 'project criteria))

(defmethod programme-records(db &rest criteria)
  (simple-query db 'programme criteria))

(defmethod previous-mark((mark module-mark))
  (when (> (attempt mark) 1)
    (let ((m
	   (car (module-mark-records
		 (clsql-sys::view-database mark)
		 'moduleid (moduleid mark)
		 'studentid (studentid mark)
		 'attempt (1- (attempt mark))))))
      (when m (mark m)))))

(defmethod previous-mark((mark mark))
  (when (> (attempt mark) 1)
    (let ((m
	   (car (mark-records
	   (clsql-sys::view-database mark)
	   'assessmentid (assessmentid mark)
	   'studentid (studentid mark)
	   'attempt (1- (attempt mark))))))
      (when m (mark m)))))

(defmethod student-usernames(db)
  "Return the list of all student usernames in alphabetic order"
  (mapcan
   #'(lambda(item) (when (car item) item))
   (clsql:select [username]
		 :from [students]
		 :order-by [username]
		 :database db)))

(defmethod available-projects(db)
  (clsql:select 'project
		:database db
		:caching nil
		:where [null [slot-value 'project 'studentid]]
		:flatp t))

(defmethod allocated-projects(db)
  (clsql:select 'project
		:database db
		:caching nil
		:where [not [null [slot-value 'project 'studentid]]]
		:flatp t))

(defun project<(a b)
  "Project comparison function ordering by status"
  (cond ((not (student a)))
        ((not (student b)) nil)
        ((not (start-date a)))
        ((not (start-date b)) nil)
        ((< (start-date a) (start-date b)))
        ((not (submission-date a)))
        ((not (submission-date b)) nil)
        ((< (submission-date a) (submission-date b)))
        ((not (deadline-date a)))
        ((not (deadline-date b)))
        ((< (deadline-date a) (deadline-date b)))))

(defmethod time-spent((p project))
  (if (start-date p)
      (- (or (submission-date p) (get-universal-time)) (start-date p))))

(defmethod assessors-project-records(db username)
  "Return a list of project records where the user is an assessor"
  (let ((ids (clsql::query
	       (format nil "select distinct projectid from projects
left join (marks left join assessments using (assessmentid)) as foo
using (studentid,moduleid) where modified_by='~A';" username)
	       :database db
	       :flatp t)))
    (when ids
      (clsql:select
       'project
       :database db
       :flatp t
       :caching nil
       :where [in [slot-value 'project 'projectid] ids]))))

(defmethod supervisors-project-records(db username)
  (clsql:select
   'project
   :database db
   :flatp t
   :caching nil
   :refresh t
   :where [like [slot-value 'project 'supervisors]
                (format nil "%~S%" username)]))

(defmethod studentid((user user))
  (or (when (stringp (property user :studentid)) (property user :studentid))
      (setf (property user :studentid)
	    (car (clsql:select [studentid]
			       :from [students]
			       :where [= [username] (username user)]
			       :flatp t)))))

(declaim (inline mark-version>))
(defun mark-priority>(a b)
  (when a
    (or (null b)
	(> (attempt a) (attempt b))
	(and (= (attempt a) (attempt b))
	     (> (revision a) (revision b))))))

(defgeneric mark-groups(entity)
  (:documentation
   "Given of marks grouped by student and assessment, with each group
sorted by mark-priority>")
  (:method((marks list))
    (mapcar
     #'(lambda(set) (sort set #'mark-priority>))
     (group-by marks #'(lambda(m) (cons (studentid m) (assessmentid m))))))
  (:method(entity)
    (mark-groups (marks entity)))
  (:method((student student))
    (mapcar
     #'(lambda(set) (sort set #'mark-priority>))
     (group-by (marks student) #'assessmentid)))
  (:method((assessment assessment))
    (mapcar
     #'(lambda(set) (sort set #'mark-priority>))
     (group-by (marks assessment) #'studentid)))
  (:method((mark mark))
    (list (sort (mark-records (clsql-sys::view-database mark)
			      'studentid (studentid mark)
			      'assessmentid (assessmentid mark))
		#'mark-priority>)))
  (:method((module-mark module-mark))
    (mapcar
     #'(lambda(set) (sort set #'mark-priority>))
     (group-by (marks module-mark) #'assessmentid)))
  (:method((mark module-mark)) (mark-groups (marks mark))))

(defgeneric module-mark-groups(entity)
  (:documentation
   "Given of marks grouped by student and assessment, with each group
sorted by mark-priority>")
  (:method((marks list))
    (mapcar
     #'(lambda(set) (sort set #'mark-priority>))
     (group-by marks #'(lambda(m) (cons (studentid m) (moduleid m))))))
  (:method(entity)
    (mark-groups (module-marks entity)))
  (:method((student student))
    (mapcar
     #'(lambda(set) (sort set #'mark-priority>))
     (group-by (module-marks student) #'moduleid)))
  (:method((module module))
    (mapcar
     #'(lambda(set) (sort set #'mark-priority>))
     (group-by (module-marks module) #'studentid)))
  (:method((mark mark)) (mark-groups (module-marks mark))))

(defgeneric current-marks(entity)
  (:documentation "Return the Current marks for a given entity")
  (:method(entity) (mapcar #'car (mark-groups entity))))

(defgeneric current-module-marks(entity)
  (:documentation "Return the Current marks for a given entity")
  (:method(entity) (mapcar #'car (module-mark-groups entity))))

(defmethod tutors-marks-due(db &optional (username (username *current-user*)))
  "Return a list of assessmentids and number of due marks for tutor with
given username"
  (clsql::query
   (format nil
	    "select assessmentid,count(*) from (marks left join assessments using (assessmentid)) left join modules using (moduleid,year) where mark isnull and (modified_by='~A' or assessor='~:*~A' or owner='~:*~A') group by assessmentid"
	    username)
    :database db))

(defmethod students-without-projects(db)
  (student-records db 'studentid
		   (clsql::query
		    "select studentid from students left join projects using (studentid) where not username isnull and not programmeid isnull and not year isnull and projects.studentid isnull"
		    :database db :flatp t)))

(defmethod assessment-stats(db username &key module year earliest (group-by "assessmentid"))
  "Return a list of assessmentids and counts for marks modified by username
for given module and year"
  (format t "select ~A,count(*) from marks left join assessments using (assessmentid) where modified_by='~A' ~@[and moduleid='~A'~] ~@[and year in (~{~A~})~]
~@[and modified>'~A'~] group by ~A"
	   group-by
	   username
	   module
	   (if (listp year) year (list year))
	   (when earliest (format-datestamp earliest))
	   group-by)
  (clsql::query
   (format nil
	   "select ~A,count(*) from marks left join assessments using (assessmentid) where modified_by='~A' ~@[and moduleid='~A'~] ~@[and year in (~{~A~})~]
~@[and modified>'~A'~] group by ~A"
	   group-by
	   username
	   module
	   (if (listp year) year (list year))
	   (when earliest (format-datestamp earliest))
	   group-by)
   :database db))

(defmethod unallocated-assessments(db module &key year)
  "Returtn list of assessmentid and studentid where assessment has not been set."
  (clsql::query
   (format nil
	   " select studentid,assessmentid from marks left join students using (studentid) left join assessments using (assessmentid) where moduleid='~A' ~@[and assessments.year in (~{~A~})~] and modified_by is null;"
	   module
	   (if (listp year) year (list year)))
   :database db))

(defun decode-refer-args(db studentid itemid)
  (let* ((student (or (car (student-records db 'studentid studentid))
                      (car (student-records db 'username studentid)))))
    (declare (special clsql::*default-caching*))
    (unless student (error "Student ~A not found" studentid))
    (let ((mark (when (integerp itemid)
                  (car (current-marks
                        (mark-records db
                                      'studentid (studentid student)
                                      'assessmentid itemid)))))
          (module-mark
           (when (stringp itemid)
             (car (current-module-marks
                   (module-mark-records db
                                        'studentid (studentid student)
                                        'moduleid itemid))))))
      (cond
        (mark (setf module-mark (module-mark mark)))
        (module-mark
         (setf mark
               (find "EXAM" (current-marks (marks module-mark))
                     :test #'equalp
                     :key #'(lambda(m)
                              (assessment-type (assessment m)))))
         (unless mark
           (format t "Unable to find examination assessment for module ~S"
                   (moduleid module-mark))))
        (t (error "Unable to find records to refer ~S in ~S"
                  studentid itemid)))
      (when (and (mark module-mark)
                 (>= (mark module-mark) (pass-mark (module module-mark))))
        (error "Module mark of ~D is more than the pass mark of ~D for module ~A"
               (mark module-mark)
               (pass-mark (module module-mark))
               (moduleid module-mark)))
      (values mark module-mark))))

(defun refer(db studentid itemid &key deadline)
  "Given a student username or id and a module or assessmentid update
the mark and module-mark records to a referred status. Signals an
error if state is incorrect"
  (multiple-value-bind(mark module-mark)
      (decode-refer-args db studentid itemid)
    (unless (> (attempt module-mark) 1)
      (let ((m (make-instance 'module-mark
                              :moduleid (moduleid module-mark)
                              :studentid (studentid module-mark)
                              :mark nil
                              :year (year module-mark)
                              :attempt 2
                              :revision 1)))
        (update-records-from-instance m)
        (print m)))
    (when (and mark (< (attempt mark) 2))
      (let ((m (make-instance 'mark
                              :assessmentid (assessmentid mark)
                              :studentid (studentid mark)
                              :attempt 2
                              :mark nil
                              :revision 1)))
        (when deadline
          (setf (deadline-date m)
                (jarw.parse:parse-input 'jarw.parse:date deadline)))
        (update-records-from-instance m)
        (print m)))
    (terpri)))

(defun repeat(db studentid itemid)
  "Given a student username or id and a module or assessmentid update
the mark and module-mark records to a referred status. Signals an
error if state is incorrect"
  (multiple-value-bind(mark module-mark)
      (decode-refer-args db studentid itemid)
    (unless (not (mark module-mark))
      (let ((m (make-instance 'module-mark
		      :moduleid (moduleid module-mark)
		      :studentid (studentid module-mark)
		      :attempt (attempt module-mark)
		      :mark nil
		      :revision (1+ (revision module-mark))
		      :note "Repeat"
		      :modified (get-universal-time))))
      (update-records-from-instance m)
      (print m)))
    (unless (not (mark mark))
      (let ((m (make-instance 'mark
		      :assessmentid (assessmentid mark)
		      :studentid (studentid mark)
		      :attempt (attempt mark)
		      :mark nil
		      :revision (1+ (revision mark))
		      :note "Repeat"
		      :modified (get-universal-time))))
      (update-records-from-instance m)
      (print m)))))

(defun condone(db studentid moduleid)
  (let ((student (or (car (student-records db 'studentid studentid))
		     (car (student-records db 'username studentid)))))
    (unless student (error "Student ~A not found" studentid))
    (let ((module-mark
	   (car (current-module-marks
		 (module-mark-records db
				      'studentid (studentid student)
				      'moduleid moduleid)))))
      (unless module-mark
	(error "Module ~S for student ~S not found" moduleid studentid))
      (when (mark module-mark)
	(when  (>= (mark module-mark) (pass-mark (module module-mark)))
	  (error "Module mark of ~D is more than the pass mark of ~D for
module ~A"
		 (mark module-mark)
		 (pass-mark (module module-mark))
		 (moduleid module-mark)))
	(when (< (mark module-mark) 30)
	  (error "Module mark of ~D is less than 30 for module ~A"
		 (mark module-mark)
		 (moduleid module-mark))))
      (unless (condonedp module-mark)
	(update-records-from-instance
	 (make-instance 'module-mark
			:moduleid (moduleid module-mark)
			:studentid (studentid module-mark)
			:attempt (attempt module-mark)
			:revision (1+ (revision module-mark))
			:mark (mark module-mark)
			:note "Condoned"
			:modified (get-universal-time)))
	(write-line "Condoned Module Mark entry added")))))

(defmethod clone-mark((mark module-mark) &key (attempt (attempt mark))
                      (revision (revision mark)) note)
   (assert (not (and (= attempt (attempt mark)) (= revision (revision mark))))
	  (revision attempt)
	  "Either revision (~D) or attempt (~D) must be different in a cloned mark" revision attempt)
  (let ((m (make-instance 'module-mark
		      :moduleid (moduleid mark)
          :year (year mark)
		      :studentid (studentid mark)
		      :attempt attempt
		      :mark nil
		      :revision revision
		      :note note
		      :modified (get-universal-time))))
    (update-records-from-instance m)))

(defmethod clone-mark((mark mark) &key (attempt (attempt mark))
		      (revision (revision mark)) note)
     (assert (not (and (= attempt (attempt mark)) (= revision (revision mark))))
	  (revision attempt)
	  "Either revision (~D) or attempt (~D) must be different in a cloned mark" revision attempt)
  (let ((m (make-instance 'mark
			  :assessmentid (assessmentid mark)
			  :studentid (studentid mark)
			  :attempt attempt
			  :mark nil
			  :revision revision
			  :note note
			  :modified (get-universal-time))))
    (update-records-from-instance m)))

(defun post-examboard(programmeid year &key modules ignore-modules (db *db*))
  "Interactively deal with examboard outcomes for  given programme and year. If the modules list is given, only those modules will be dealt with in this pass."
  (princ
   "The input options are
c - condone a module
f - refer a module or assessment
i - ignore (no action)
p - repeat a module
a - accept calculated mark only
q - quit
"
   *query-io*)
  (flet((get-key(&optional (options "icfpa"))
          (let ((ans nil))
            (jarw.lib:while (not (find ans options))
              (format *query-io* "[~Aq]?" options)
              (finish-output *query-io*)
              (clear-input *query-io*)
              (setf ans (char (read-line *query-io*) 0))
              (when (eql ans #\q) (return-from post-examboard)))
            ans)))
    (let ((students (sort (copy-list (student-records db
                                                      'programmeid programmeid
                                                      'year year))
                          #'string< :key #'username)))
      (dolist(student students)
        (format *query-io* "[~A] (~8A) ~A~%"
                (studentid student)
                (username student)
                (fullname student))
        (dolist(m (sort (copy-list (current-module-marks student))
                        #'string< :key #'moduleid))
          (when (and (not (mark m))
                     (or (not modules)
                         (member (moduleid m) modules :test #'string=))
                     (or (not ignore-modules)
                         (not (member (moduleid m) ignore-modules
                                      :test #'string=))))
            (format *query-io* " ~A: ~,1f "
                    (moduleid m) (or (mark m) (calculated-mark m)))
            (finish-output *query-io*)
            (let ((cm (calculated-mark m))
                  (pm (pass-mark (module m))))
              (cond
                ((mark m))
                ((>= cm pm)
                 (setf (mark m) (if (> (attempt m) 1) pm cm))
                 (format *query-io* " -> ~,1F" (mark m))
                 (update-records-from-instance m))
                ((>= (+ 1 cm) pm)
                 (setf (mark m) pm)
                 (update-records-from-instance m)
                 (format *query-io* " -> ~,1F" (mark m)))
                (t
                 (let ((option (get-key)))
                   (ecase option
                     (#\a ;; accept
                      (setf (mark m) cm)
                      (update-records-from-instance m))
                     (#\c ;; condone
                      (setf (mark m) cm)
                      (setf (note m) "Condoned")
                      (update-records-from-instance m)
                      (format *query-io* " -> ~,1F Condoned" cm))
                     (#\i) ;; ignore
                     (#\f ;; refer
                      (setf (mark m) cm)
                      (setf (note m) "Referred")
                      (update-records-from-instance m)
                      (clone-mark m :attempt (1+ (attempt m)))
                      (format *query-io* " -> ~,1F Referred~%" cm)
                      (dolist(mark (marks m))
                        (format *query-io* "  ~3D ~5A: ~5,1F~@[+~,1F penalty~]"
                                (assessmentid mark)
                                (assessment-type (assessment mark))
                                (mark mark)
                                (late-penalty mark))
                        (when (or (not (mark mark)) (< (mark mark) pm))
                          (setf (note mark) "Referred")
                          (update-records-from-instance mark)
                          (clone-mark mark :attempt (1+ (attempt m)))
                          (format *query-io* " Referred"))
                        (terpri *query-io*)
                        (finish-output *query-io*)))
                     (#\p ;; repeat
                      (setf (mark m) cm)
                      (update-records-from-instance m)
                      (clone-mark m :revision (1+ (revision m)) :note "Repeat")
                      (format *query-io* " -> ~,1F Repeat~%" (calculated-mark m))
                      (dolist(mark (marks m))
                        (format *query-io* "  ~3D ~5A: ~5,1F~@[+~,1F penalty~]"
                                (assessmentid mark)
                                (assessment-type (assessment mark))
                                (mark mark)
                                (late-penalty mark))
                        (when (or (not (mark mark)) (< (mark mark) pm))
                          (update-records-from-instance mark)
                          (clone-mark mark :revision (1+ (revision m))
                                      :note "Repeat")
                          (format *query-io* " Repeat"))
                        (terpri *query-io*)
                        (finish-output *query-io*))) ))))
              (terpri *query-io*)
              (finish-output *query-io*) )))))))

#|


(setq *ms* (mapcan #'(lambda(m) (multiple-value-bind(a regs) (cl-ppcre:scan-to-strings "Repeat[^\\d]*([0-9\\.]*)[^\\d]*([0-9\\.]*)" (note m)) (when a (list (list regs m)))))
(mark-records *db*)))

(dolist(rec *ms*)
  (let* ((m (second rec))
	 (marks (reverse (mapcan #'(lambda(m) (when m (list m))) (map 'list #'(lambda(s) (when (> (length s) 0) (parse-number  s))) (first rec)))))
	 (revision 1)
	 (vm (mark m)))
       (setf (mark m) (pop marks)
	     marks (nconc marks (list vm)))
       (update-records-from-instance m)
       (dolist(v marks)
	 (when v
	 (let ((newmark (make-instance 'mark
			    :assessmentid (assessmentid m)
			    :studentid (studentid m)
			    :attempt 1
			    :revision (incf revision)
			    :modified-by "willijar"
			    :modified (get-universal-time)
			    :mark v)))
	   (update-records-from-instance newmark))))))

(defun do-updates(func list)
  (dolist(rec list)
    (let ((studentid (car rec)))
      (dolist(id (cdr rec))
	(funcall func *db* studentid id)))))

(setq *repeats*
      '(("039923280" "EE403A" "EE403B")
	("049925188" "EE403A")))

(setq *condones*
      (sort
      '(("049940767" "EE401B")
	("039906755" "EE404A")
	("049925188" "EE401B")
	("049935109" "EE401B")
	("039902458" "EE4008")
	("049939211" "EE401A")
	("049915617" "EE403B" "EE404A")
	("039939937" "EE403A")
	("039943174" "EE401A")
	("049924778" "EE4012"))
      #'string< :key #'car))

(mapcar
 #'(lambda(rec)
     (let ((studentid (car rec)))
       (mapcar
	#'(lambda(moduleid)
	    (mapcar
	     #'(lambda(mark)
		 (list studentid moduleid (mark mark) (note mark) (calculated-mark mark)))
	    (module-mark-records *db* 'studentid studentid 'moduleid moduleid)))
	(cdr rec))))
 *condones*)

(setq *refers*
      '(("049940767" "EE403A" "EE403B" "EE404B" 161)
	("029900697" "EE401A" "EE4009")
	("049928617" "EE4008" "EE4009" "EE401A")
	("049928813" "EE403A")
	("049934618" "EE403B")
	("039943174" "EE401B" "EE403B" "EE404A")
	("049908642" "EE4008")
	("049925188" "EE401B")))


|#


(clsql:locally-disable-sql-reader-syntax)

