;;;; Group assessment entity -*- Lisp -*- Copyright (C) 2008
;;;; John A.R. Williams <J.A.R.Williams@jarw.org.uk> Released under
;;;; the GNU General Public License (GPL) See
;;;; <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;

(in-package :clews.groups)

(defclass group()
  ((groupname :type string :initarg :name :reader groupname)
   (tutors :type list :initform nil :reader tutors :initarg :tutors
           :documentation "List of tutors usernames for this group")
   (students :type list :initform nil :reader students :initarg :students
             :documentation "List of student usernames")
   (group-data :type list :initform nil
               :documentation "plist of group related data for this group")
   (student-data :type hash-table :initform (make-hash-table :test #'equal)
                 :documentation "plists of data for individuals in this group")
   (views :type list :allocation :class
          :documentation "The list of available views"))
  (:documentation "Class representing an assessment group"))

(defmethod has-permission((action (eql :tutor))
                          (entity group)
                          &optional (user *current-user*))
  (member (username user) (tutors entity) :test #'equal))

(defmethod has-permission((action (eql :student))
                          (entity group)
                          &optional (user *current-user*))
  (member (username user) (students entity) :test #'equal))

(defgeneric student-data(key username entity)
  (:documentation "Return some student data")
  (:method(key (username string) (group group))
    (getf (gethash username (slot-value group 'student-data)) key)))

(defgeneric (setf student-data)(value key username entity)
  (:documentation "Set some student data")
  (:method(value (key symbol) (username string) (group group))
    (setf (getf (gethash username (slot-value group 'student-data)) key)
          value)))

(defgeneric group-data(key entity)
  (:documentation "Return the group data for given name on entity")
  (:method((key symbol)(group group))
    (getf (slot-value group 'group-data) key)))

(defgeneric (setf group-data)(value key entity)
  (:method(value (key symbol) (group group))
    (setf (getf (slot-value group 'group-data) key) value)))

(defclass group-assessment(command-processor)
  ((command-log :type pathname :initarg :command-log :reader command-log
                :documentation "Pathname where command log is stored")
   (tutors :type list :initform nil :reader tutors :initarg :tutors
           :documentation "List of global tutors")
   (groups :type list :initform nil :reader groups
           :documentation "List of group data")
   (group-criteria
    :type list :reader group-criteria
    :documentation "alist of group criteria labels and descriptions")
   (group-assessment-form
    :type list :initform nil :initarg :group-assessment-form
    :reader group-assessment-form
    :documentation "Tutors assessment form for the group work")
   (individual-assessment-form
    :type list :initform nil :initarg :individual-assessment-form
    :reader individual-assessment-form
    :documentation "Tutors assessment form for the individual work")
   (mark-calculation :initarg :mark-calculation
                     :type function
                     :initform
                     #'(lambda(&key group peer self individual)
                         (declare (ignore self))
                         (* 0.5 (+ individual
                                   (* group (/ (first peer)
                                               (reduce #'+ peer))))))
                     :documentation "Function which calculates an
individuals mark - it should take the following keyword arguments
group, peer, individual which correspond to the groups mark, the group
peer review marks (theirs is first), self and individual mark
respectively and return an overall mark. All marks are in range
0-100."))
  (:documentation "Class to manage a group assessment"))

(defmethod shared-initialize :before ((assessment group-assessment) slot-names
                                      &key (groups nil groups-p)
                                      &allow-other-keys)
  (when groups-p
    (setf (slot-value assessment 'groups)
          (mapcar
           #'(lambda(g)
               (make-instance 'group
                              :name (first g)
                              :students (second g)
                              :tutors (third g)))
           groups))))

;; access control
(defmethod has-permission((action (eql :tutor))
                          (entity group-assessment)
                          &optional (user *current-user*))
  (member (username user) (tutors entity) :test #'equal))

;;  accessors
(defun group(groupname assessment)
  "Given a group return the group object"
  (find groupname (groups assessment) :test #'equal :key #'groupname))

(defun students-group(studentname assessment)
  "Given a students name return the appropriate group object"
  (find studentname (groups assessment) :key #'students
   :test #'(lambda(a b) (member a b :test #'equal))))

(defmethod student-data((key symbol) (username string)
                        (entity group-assessment))
    (student-data key username (students-group username entity)))

(defmethod (setf student-data)(value (key symbol) (username string)
                               (entity group-assessment))
  (setf (student-data key username (students-group username entity)) value))

(defun peer-review(assessor assesse group)
  (cdr (assoc assesse (student-data :reviews assessor group) :test #'equal)))

(defun (setf peer-review)(review assessor assesse group)
  (let ((old (assoc assesse (student-data :reviews assessor group)
                    :test #'equal)))
    (if old
        (setf (cdr old) review)
        (setf (student-data :reviews assesse group)
              (acons assesse review (student-data :reviews assessor group))))))

;; helper functions
(defun assessments-criteria-confirmed-p(group)
  (let ((confirmed-by (group-data :weights-confirmed group)))
    (every #'(lambda(s) (member s confirmed-by :test #'equal))
           (students group))))

(defun peer-reviews-completed-p(group &optional assessor)
  "Return whether the group (or optionally a student in the group)
have completed all their peer reviews"
  (if assessor
      (let ((reviews (student-data :reviews assessor group)))
        (every #'(lambda(s) (member s reviews :test #'equal :key #'car))
               (students group)))
      (every #'(lambda(s) (peer-reviews-completed-p group s))
             (students group))))

(defun group-assessments-completed-p(assessment)
  (every #'(lambda(group) (group-data :group group)) (groups assessment)))

(defun individual-assessments-completed-p(assessment)
  (every
   #'(lambda(group)
       (every
        #'(lambda(student)
            (student-data :individual student group))
        (students group)))
   (groups assessment)))

;; external assessment commands - to be called using execute-command

(defcommand set-group-assessment-data
    (assessment &key (tutor (username *current-user*)) groupname data)
  (:documentation "Set the assessment form data for a group")
  (:check
   (declare (ignore data))
   (let ((group (group groupname assessment)))
     (and group
          (or (has-permission :tutor assessment)
              (and (has-permission :tutor group)
                   (equal (username *current-user*) tutor))))))
  (:action
   (let ((group (group groupname assessment)))
     (setf (group-data :group-assessment group) `(:tutor ,tutor ,@data))))
  (:undo
   (declare (ignore data tutor))
   (let* ((group (group groupname assessment))
          (olddata (group-data :group-assessment group)))
     #'(lambda(assessment)
         (declare (ignore assessment))
         (setf (group-data :group group) olddata)))))

(defcommand set-individual-assessment-data
    (assessment &key (tutor (username *current-user*)) student data)
  (:documentation "Set the individuals for assessment data")
  (:check
   (declare (ignore data))
   (let ((group (students-group student assessment)))
     (and group
          (or (has-permission :tutor assessment)
              (and (has-permission :tutor group)
                   (equal (username *current-user*) tutor))))))
  (:action
   (let ((group (students-group student assessment)))
     (setf (student-data :individual student group) `(:tutor ,tutor ,@data))))
  (:undo
   (declare (ignore data tutor))
   (let* ((group (students-group student assessment))
         (olddata (student-data :individual student group)))
     #'(lambda(assessment)
         (declare (ignore assessment))
         (setf (student-data :individual student group) olddata)))))

(defcommand confirm-group-criteria-weights
    (assessment &key (student (username *current-user*)))
  (:documentation "Confirm the groups criteria selection")
  (:check
   (let ((group (students-group student assessment)))
     (and group
          (group-data :weights group)
          (or (has-permission :tutor assessment)
              (has-permission :tutor group)
              (and (has-permission :student group)
                   (equal (username *current-user*) student))))))
  (:action
   (let ((group (students-group student assessment)))
     (setf (group-data :weights-confirmed group)
           (nunion (group-data :weights-confirmed group) (list student))))))

(defcommand set-group-criteria-weights
    (assessment
     &key data
     (groupname (let ((g (students-group (username *current-user*)
                                         assessment)))
                  (when g (groupname g)))))
  (:documentation "Set the group criteria weights - remove confirmations")
  (:check
   (declare (ignore data))
   (let ((group (group groupname assessment)))
     (and
      group
      (or (has-permission :tutor assessment)
          (has-permission '(:tutor :student) group))
      (not (assessments-criteria-confirmed-p group)))))
  (:action
   (let ((group (group groupname assessment)))
     (setf (group-data :weights group) data
           (group-data :weights-confirmed group) nil)
     (when (has-permission :student group)
       (confirm-group-criteria-weights group
                                       :student (username *current-user*)))
     data))
  (:undo
   (declare (ignore data))
   (let* ((group (group groupname assessment))
          (weights (group-data :weights group))
          (weights-confirmed (group-data :weights-confirmed group)))
     #'(lambda(assessment)
         (declare (ignore assessment))
         (setf (group-data :weights group) weights
               (group-data :weights-confirmed group) weights-confirmed)))))

(defcommand set-peer-review(assessment &key assessor assessee data)
  (:documentation "Set review of assessee by assessor")
  (:check
   (declare (ignore data))
   (let ((group (students-group assessor assessment)))
     (and group
          (or (has-permission :tutor assessment)
              (has-permission :tutor group)
              (and (has-permission :student group)
                   (not (peer-reviews-completed-p group))
                   (member assessee (students group) :test #'equal))))))
  (:action
   (let ((group (students-group assessor assessment)))
     (setf (peer-review assessor assessee group) data)))
  (:undo
   (declare (ignore data))
   (let* ((group (students-group assessor assessment))
          (olddata (peer-review assessor assessee group)))
     #'(lambda(assessment)
         (declare (ignore assessment))
         (setf (peer-review assessor assessee group) olddata)))))

(defun criteria-weights-form(assessment &optional (submit "Submit"))
  `((form :name :criteria-weights :method :post)
    (table
     (tr (th "Weight (1-20)") (th "Criteria"))
     ,@(mapcar
        #'(lambda(c)
            `(tr (td ((input :name ,(car c) :value "10"
                             :format (integer :min 1 :max 2)
                             :size 5)))
                 (td ,(cdr c))))
        (group-criteria assessment))
     (tr ((td :colspan 2 :align :center)
          ((input :type :submit :value ,submit)))))))

(defun peer-review-form(assessment assesse)
  (let* ((group (students-group assesse assessment))
         (weights (group-data :weights group)))
  `((form :name :peer-review :method :post)
    (table
     (tr (th "Mark (1-20)") (th "Weighting") (th "Criteria"))
     ,@(mapcar
        #'(lambda(c)
            `(tr (td ((input :name ,(car c) :value "10"
                             :format (integer :min 1 :max 2)
                             :size 5)))
                 ((td :align :right) ,(getf (car c) weights))
                 (td ,(cdr c))))
        (group-criteria assessment))
     (tr ((td :colspan 2 :align :center)
          ((input :type :submit :value "Submit Peer Review marks"))))))))

(defun group-mark(groupname assessment)
  "Return the group assessment makr for group in assessment"
  (form-mark (group-data :group-assessment (group groupname assessment))
             (group-assessment-form assessment)))

(defun individual-mark(student assessment)
  "Return the individual assessment mark for student in assessment"
  (let ((group (students-group student assessment)))
    (form-mark (student-data :individual student group)
               (individual-assessment-form  assessment))))

(defun peer-form-mark(group assessment form-data)
  "Return the weighted mark for peer review form data for a group in assessment"
  (let ((weights (group-data :weights group)))
    (/
     (reduce #'+
             (mapcar
              #'(lambda(c)
                  (let ((k (car c)))
                    (* (getf k form-data) (getf k weights))))
              (group-criteria assessment)))
     (reduce #'+ (mapcar #'(lambda(c) (getf (car c) weights))
                         (group-criteria assessment))))))

(defun peer-marks(student assessment)
  "Return the list of a groups peer marks with the students one first"
  (let* ((group (students-group student assessment))
         (students
          (cons student (remove student (students group) :test #'equal))))
    (mapcar
     #'(lambda(assessee)
         (mean
          (mapcar
           #'(lambda(assessor)
               (peer-form-mark group assessment
                               (peer-review assessor assessee group)))
           (rest students))))
         students)))

(defun self-mark(student assessment)
  (let* ((group (students-group student assessment)))
    (peer-form-mark group assessment (peer-review student student group))))

;;; views on this assessment

(defview set-group-criteria-weights(group-assessment)
  (:title "Set Peer review criteria weights")
  (:description "Set the weighting for the group for the various
  criteria you will be reviewing each other on.")
  (:check(assessment)
         (can-execute-command-p 'set-group-criteria-weights assessment))
  (:handler(assessment request)
    (let ((group (students-group (username *current-user*) assessment)))
      (list
       (do-form-with-confirmation
           :data (group-data :weights group)
           :form (criteria-weights-form assessment)
           :handler
           #'(lambda(data)
               (set-group-criteria-weights
                assessment :groupname (groupname group) :data data))
           :request request)))))

(defview confirm-group-criteria-weights(group-assessment)
  (:title "Confirm Peer Review criteria weights")
  (:description "Set the weighting for the group for the various
  criteria you will be reviewing each other on.")
  (:check(assessment)
         (can-execute-command-p 'confirm-group-criteria-weights assessment))
  (:handler(assessment request)
    (let ((group (students-group (username *current-user*) assessment)))
      `((p "Comfirm the group review criteria weightings below")
        ,(do-form-with-confirmation
            :data (group-data :weights group)
            :form `((form :method :post)
                    ((input :name :submit :type :submit :value "Confirm")))
            :handler
            #'(lambda(data)
                (confirm-group-criteria-weights assessment)
                data)
            :on-success "Group Criteria Review Weights Confirmed"
            :request request)
        (hr)
        ,(markup-form
          (criteria-weights-form assessment) (group-data :weights group) t)
        (hr)
        '(p "If you disagree with these criteria you may reset
        them" ((a :href "../set-group-criteria-weights") "here") "- note however all of your team will than have to
        reconfirm the new weights.")))))

(defview assess-group(group-assessment)
  (:title "Assess Groups Performance")
  (:description "Enter the group assessment data")
  (:check(assessment)
         (some #'(lambda(g)
                   (can-execute-command-p
                    'set-group-assessment-data assessment
                    :groupname (groupname g)))
               (groups assessment)))
  (:handler(assessment request)
      (let ((groupname (first (query-values :group request))))
        (list
         (when groupname
           (unless (can-execute-command-p
                    'set-group-assessment-data assessment :groupname groupname)
             (error 'permission-denied-error
                    :action "Assess"
                    :service groupname))
           (let ((group (group groupname assessment)))
             `((section :title ,(format "Group ~S" groupname))
               (ul ,@(mapcar #'(lambda(a) `(li ,a)) (students group)))
               ,(do-form-with-confirmation
                 :request request
                 :data (group-data :group-assessment group)
                 :form (group-assessment-form assessment)
                 :handler #'(lambda(data)
                              (set-group-assessment-data
                               assessment :groupname groupname :data data))))))
         `((section :title "Available group assessments")
           (p "Click on group name to enter group assessment for that group.")
           (table
            (tr (th "Group") (th "Mark") ("Assessor"))
            ,@(mapcan
               #'(lambda(group)
                   (let ((groupname (groupname group)))
                     (when (can-execute-command-p
                            'set-group-assessment-data assessment
                            :groupname groupname)
                       `((tr (td ((a :href ,(format "?group=~A" groupname ))
                                  ,groupname))
                             (td ,(group-mark groupname assessment))
                             (td (getf :tutor
                                       (group-data
                                        :group-assessment
                                        (group groupname assessment)))))))))
               (groups assessment))))))))

(defview assess-individual(group-assessment)
  (:title "Assess Individual Performance")
  (:description "Enter an individuals assessment data")
  (:check(assessment)
         (some #'(lambda(s)
                   (can-execute-command-p
                    'set-individual-assessment-data assessment :student s))
               (reduce #'append (mapcar #'students (groups assessment)))))
  (:handler(assessment request)
     (let* ((student (first (query-values :student request)))
            (groupname (groupname (students-group student assessment))))
        (list
         (when student
           (unless (can-execute-command-p
                    'set-individual-assessment-data assessment :student student)
             (error 'permission-denied-error
                    :action 'assess-individual
                    :service student))
           (let ((group (group groupname assessment)))
             `((section :title ,(format "Assess Student ~S in Group ~S"
                                        student groupname))
               (ul ,@(mapcar #'(lambda(a) `(li ,a)) (students group)))
               ,(do-form-with-confirmation
                 :request request
                 :data (student-data :individual student group)
                 :form (individual-assessment-form assessment)
                 :handler #'(lambda(data)
                              (set-individual-assessment-data
                               assessment :student student :data data))))))
         `((section :title "Available Assessments")
           (p "Click on student name to enter assessment for that individual.")
           (table
            (tr (th "Student") (th "Mark") ("Assessor"))
            ,@(mapcan
               #'(lambda(group)
                   (when (has-permission :tutor group)
                     (let ((groupname (groupname group)))
                       `((tr ((th :colspan 3) "Group " ,groupname))
                         ,(mapcan
                           #'(lambda(student)
                               `((tr
                                  (td ((a :href ,(format "?student=~A" student))
                                       student))
                                  (td ,(individual-mark student assessment))
                                  (td ,(getf
                                        (student-data :individual student group)
                                        :tutor)))))
                           (students group))))))
               (groups assessment))))))))


