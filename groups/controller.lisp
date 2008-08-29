;;;; Group assessment entity -*- Lisp -*- Copyright (C) 2008
;;;; John A.R. Williams <J.A.R.Williams@jarw.org.uk> Released under
;;;; the GNU General Public License (GPL) See
;;;; <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;

(in-package :clews.coursework)

(defclass coursework-controller(command-processor)
  ((tutors :type list :initform nil :reader tutors :initarg :tutors
           :documentation "The global tutors usernames")
   (students :type list :initform nil :initarg :students :reader students
             :documentation "The students usernames")
   (state :type hash-table :initform (make-hash-table :test #'equal)
          :documentation "plists of state data keyed by user entity name")
   (components
    :type list :initform nil :reader components
    :documnentation "The assessment components which make up this coursework"))
  (:documentation "Class to manage a coursework assessment"))

(defmethod shared-initialize :before
    ((controller coursework-controller) slot-names
     &key components &allow-other-keys)
  (setf (slot-value controller 'components)
        (mapcar #'(lambda(a) (apply #'make-instance a)) components)))

(defmethod has-permission((action (eql :tutor))
                          (entity coursework-controller)
                          &optional (user *current-user*))
  (member (username user) (tutors entity) :test #'equal))

(defgeneric state(key entityid assessments)
  (:documentation "Return a piece of state data for entity")
  (:method((key symbol) entityid (assessments group-assessments))
    (getf (gethash entityid (slot-value assessments 'data)) key)))

(defgeneric (setf state)(value key entityid assessments)
  (:documentation "Return a piece of state data for entity")
  (:method(value (key symbol) entityid (assessments group-assessments))
    (setf (getf (gethash entityid (slot-value assessments 'data)) key)
          value)))

(defgeneric component(componentid controller)
  (:documentation "Return a component by key")
  (:method(componentid controller)
    (find componentid (components controller) :test #'equal :key #'id)))

(defgeneric component-completed-p(component controller)
  (:method((component individual-assessment-component) controller)
    (let ((key (id component)))
      (every #'(lambda(student) (state key student controller))
             (students controller)))))

(defgeneric component-mark(student component controller)
  (:documentation "Return the mark for given component on student")
  (:method(entityid component controller)
    (form-mark (state (id component) entityid controller)
               (component-form component))))

(defclass group()
  ((id :initarg :id :reader id)
   (tutors :type list :initform nil :reader tutors :initarg :tutors
           :documentation "List of tutors usernames for this group")
   (students :type list :initform nil :reader students :initarg :students
             :documentation "List of student usernames"))
  (:documentation "Class representing an assessment group"))

(defmethod has-permission((action (eql :tutor))
                          (entity group)
                          &optional (user *current-user*))
  (member (username user) (tutors entity) :test #'equal))

(defmethod has-permission((action (eql :student))
                          (entity group)
                          &optional (user *current-user*))
  (member (username user) (students entity) :test #'equal))

(defclass group-coursework-controller(coursework-controller)
  ((groups :type list :initform nil :reader groups
           :documentation "The student groups")
   (group-criteria
    :type list :reader group-criteria
    :documentation "alist of group criteria labels and form elements")
   (mark-calculation
    :initarg :mark-calculation :type function
    :initform
    #'(lambda(&key group peer assessments)
        (declare (ignore self))
        (* 0.5 (+ (getf assessments :individual)
                  (* (getf group :assessments) (/ (first peer)
                                                  (reduce #'+ peer))))))
                     :documentation "Function which calculates an
individuals mark - it should take the following keyword arguments
group, peer, individual which correspond to the groups mark, the group
peer review marks (theirs is first), self and individual mark
respectively and return an overall mark. All marks are in range
0-100."))
  (:documentation "Class to manage a group assessment"))

(defmethod students((controller group-coursework-controller))
  (reduce #'append (groups controller) :key #'students))

(defmethod shared-initialize :before
    ((controller group-coursework-controller) slot-names
     &key (groups nil groups-p) &allow-other-keys)
  (when groups-p
    (setf (slot-value controller 'groups)
          (mapcar
           #'(lambda(g)
               (make-instance 'group
                              :name (first g)
                              :students (second g)
                              :tutors (third g)))
           groups))))

(defmethod component-completed-p((component group-assessment-component)
                                 controller)
  (let ((key (id component)))
    (every #'(lambda(groupid) (state key groupid controller))
           (mapcar #'id (groups controller)))))

;;  accessors
(defun group(groupid assessment)
  "Given a group return the group object"
  (find groupid (groups assessment) :test #'equal :key #'id))

(defun students-group(studentname assessment)
  "Given a students name return the appropriate group object"
  (find studentname (groups assessment) :key #'students
   :test #'(lambda(a b) (member a b :test #'equal))))

(defun peer-review(assessor assesse controller)
  (cdr (assoc assesse (state :peer-reviews assessor controller) :test #'equal)))

(defun (setf peer-review)(review assessor assesse controller)
  (let ((old (assoc assesse (state :peer-reviews assessor controller)
                    :test #'equal)))
    (if old
        (setf (cdr old) review)
        (push (cons assesse review)
              (state :peer-reviews assesse controller)))))

(defun assessments-criteria-confirmed-p(groupid assessment)
  (let ((confirmed-by (state :weights-confirmed groupid assessment)))
   (not (set-difference (students (group groupid assessment))
                        confirmed-by :test #'equal))))

(defun peer-reviews-completed-p(group assessment &optional assessor)
  "Return whether the group (or optionally a student in the group)
have completed all their peer reviews"
  (let ((group (if (typep group 'group) group (group group assessment))))
    (if assessor
        (let ((reviews (state :reviews assessor assessment)))
          (every #'(lambda(s) (member s reviews :test #'equal :key #'car))
                 (students group)))
        (every #'(lambda(s) (peer-reviews-completed-p group assessment s))
               (students group)))))

(defgeneric can-edit-component-data(controller entityid component
                                               &optional user)
  (:method(controller entityid component &optional (user *current-user*))
    (declare (ignore entityid))
    (has-permission :tutor controller user))
  (:method(controller entityid (component group-assessment-component)
           &optional (user *current-user*))
    (or (call-next-method)
        (let ((group (or (group entityid controller)
                         (students-group entityid controller))))
          (and group (has-permission :tutor group user))))))

(defgeneric can-view-component-data(controller entityid component
                                               &optional user)
  (:method(controller studentid component &optional (user *current-user*))
    (or (equal studentid (username user))
        (can-edit-component-data controller studentid component user)))
  (:method(controller entityid (component group-assessment-component)
           &optional (user *current-user*))
      (or (call-next-method)
          (let ((group (or (group entityid controller)
                           (students-group entityid controller))))
            (and group
                 (member (username user) (students group) :test #'equal))))))

;; external assessment commands - to be called using execute-command

(defcommand set-component-data
    (controller &key (tutor (username *current-user*))
                componentid entityid data)
  (:documentation "Set the assessment form data for a group")
  (:check
   (declare (ignore data tutor))
   (let ((component (component componentid controller)))
     (when component
       (can-edit-component-data controller entityid component))))
  (:action
   (setf (state componentid entityid controller) `(:tutor ,tutor ,@data)))
  (:undo
   (declare (ignore data tutor))
   (let* ((olddata (state componentid entityid controller)))
     #'(lambda(controller)
         (setf (state componentid entityid controller) olddata)))))

(defcommand set-group-criteria-weights
    (controller
     &key data
     (groupid (let ((g (students-group (username *current-user*)
                                         controller)))
                (when g (id g)))))
  (:documentation "Set the group criteria weights - remove confirmations")
  (:check
   (declare (ignore data))
   (let ((group (group groupid controller)))
     (and
      group
      (or (has-permission :tutor controller)
          (has-permission '(:tutor :student) group))
      (not (assessments-criteria-confirmed-p group)))))
  (:action
   (setf (state :weights groupid controller) data
         (state :weights-confirmed groupid controller) nil)
   (let ((group (group groupid controller)))
     (when (has-permission :student group)
       (confirm-group-criteria-weights groupid
                                       :student (username *current-user*)))
     data))
  (:undo
   (declare (ignore data))
   (let* ((weights (state :weights groupid controller))
          (weights-confirmed (state :weights-confirmed groupid controller)))
     #'(lambda(controller)
         (setf (state :weights groupid controller) weights
               (state :weights-confirmed groupid controller)
               weights-confirmed)))))

(defcommand confirm-group-criteria-weights
    (controller &key (student (username *current-user*)))
  (:documentation "Confirm the groups criteria selection")
  (:check
   (let* ((group (students-group student controller))
          (groupid (id group)))
     (and group
          (state :weights groupid controller)
          (or (has-permission :tutor controller)
              (has-permission :tutor group)
              (and (has-permission :student group)
                   (equal (username *current-user*) student))))))
  (:action
   (let* ((group (students-group student controller))
          (groupid (id group)))
     (pushnew student
              (state :weights-confirmed groupid controller) :test #'equal))))

(defcommand set-peer-review(controller &key assessor assessee data)
  (:documentation "Set review of assessee by assessor")
  (:check
   (declare (ignore data))
   (let ((group (students-group assessor controller)))
     (and group
          (or (has-permission :tutor controller)
              (has-permission :tutor group)
              (and (has-permission :student group)
                   (not (peer-reviews-completed-p group controller))
                   (member assessee (students group) :test #'equal))))))
  (:action
   (setf (peer-review assessor assessee controller) data))
  (:undo
   (declare (ignore data))
   (let* ((olddata (peer-review assessor assessee controller)))
     #'(lambda(controller)
         (setf (peer-review assessor assessee controller) olddata)))))

(defun criteria-weights-form(controller &optional (submit "Submit"))
  `((form :name :criteria-weights :method :post)
    (table
     (tr (th "Weight (1-20)") (th "Criteria"))
     ,@(mapcar
        #'(lambda(c)
            `(tr (td ((input :name ,(car c) :value "10"
                             :format (integer :min 1 :max 2)
                             :size 5)))
                 (td ,(cdr c))))
        (group-criteria controller))
     (tr ((td :colspan 2 :align :center)
          ((input :type :submit :value ,submit)))))))

(defun peer-review-form(controller assesse)
  (let* ((group (students-group assesse controller))
         (weights (state :weights (groupid group) controller)))
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
        (group-criteria controller))
     (tr ((td :colspan 2 :align :center)
          ((input :type :submit :value "Submit Peer Review marks"))))))))

(defun peer-form-mark(group controller form-data)
  "Return the weighted mark for peer review form data for a group in controller"
  (let ((weights (state :weights (groupid group) controller)))
    (/ (reduce #'+
               (mapcar
                #'(lambda(c)
                    (let ((k (car c)))
                      (* (getf k form-data) (getf k weights))))
                (group-criteria controller)))
       (reduce #'+ (mapcar #'(lambda(c) (getf (car c) weights))
                           (group-criteria controller))))))

(defun peer-marks(student controller)
  "Return the list of a groups peer marks with the students one first"
  (let* ((group (students-group student controller))
         (students
          (cons student (remove student (students group) :test #'equal))))
    (mapcar
     #'(lambda(assessee)
         (mean
          (mapcar
           #'(lambda(assessor)
               (peer-form-mark group controller
                               (peer-review assessor assessee controller)))
           (rest students))))
         students)))

(defun self-mark(student controller)
  (let ((group (students-group student controller)))
    (peer-form-mark group controller (peer-review student student controller))))

(defun link-to-assessment(controller componentid entityid
                          &optional (relative "../"))
  (if (can-execute-command-p
       'set-component-data controller
       :componentid c :entityid groupid)
      `((a :href ,(format nil "~A?component=~A&entity=~A"
                          relative componentid groupid))
        ,(let ((m (component-mark entityid componentid controller)))
              (if  (format "~,2D" m) "YES")))
      "NO"))

(defun assessments-table(controller componentids entityids &optional (relative "../"))
  `(table
    (tr (th )
        ,@(mapcar #'(lambda(c) `(th ,c)) componentids))
    ,@(mapcar
       #'(lambda(entityid)
           `(tr (th ,entityid)
                ,@(mapcar
                   #'(lambda(componentid)
                       `(td ,(link-to-assessment
                              controller componentid groupid relative)))
                   componentids)))
       entityids)))

;;; views on this controller
(defview enter-assessment(group-coursework-controller)
  (:title "Assessment")
  (:description "Enter or review an assessment")
  (:check(controller &key componentid entityid)
         (some
          #'(lambda(entityid)
              (some
               #'(lambda(componentid)
                   (can-execute-command-p
                    'set-component-data controller
                    :componentid componentid :entityid entityid))
               (if componentid
                   (list componentid)
                   (mapcar #'id (components controller)))))
          (if entityid
              (list entityid)
              (append (mapcar #'id (groups controller))
                      (students controller)))))
  (:handler(controller request)
      (let ((componentid
             (intern (first (query-values :component request)) :keyword))
            (entityid (first (query-values :entity request)))
            (groupids (mapcar #'id (groups controller)))
            (studentids (students controller))
            group-components individual-components)
        (dolist(c (components controller))
          (if (group-assessment-component-p c)
              (push (id c) group-components)
              (push (id c) individual-components)))
        (list
         (when (and componentid entityid)
           (unless
               (can-execute-command-p
                'set-component-data controller
                :componentid componentid :entityid entityid)
             (error 'permission-denied-error
                    :action "Assess"
                    :service componentid))
           (let* ((component (component componentid controller))
                  (group-p (group-assessment-component-p component)))
             `((section
                 :title ,(format nil "~S assessment for ~:[student~;group~] ~S"
                                 componentid group-p entityid))
                ,(do-form-with-confirmation
                  :request request
                  :data (state componentid entityid controller)
                  :form (component-form component)
                  :handler #'(lambda(data)
                               (set-component-data
                                controller
                                :componentid componentid
                                :entityid entityid
                                :data data)))
               (hr))))
         `((section :title "Group Components")
           ,(assessments-table controller group-components groupids))
         `((section :title "Individual Components")
           ,(assessments-table controller individual-components studentids))))))

(defview set-group-criteria-weights(group-coursework-controller)
  (:title "Set Peer review criteria weights")
  (:description "Set the weighting for the group for the various
  criteria you will be reviewing each other on.")
  (:check(controller)
         (can-execute-command-p 'set-group-criteria-weights controller))
  (:handler(controller request)
    (let ((group (students-group (username *current-user*) controller)))
      (list
       (do-form-with-confirmation
           :data (state :weights (groupid group) controller)
           :form (criteria-weights-form controller)
           :handler
           #'(lambda(data)
               (set-group-criteria-weights
                controller :groupid (groupid group) :data data))
           :request request)))))

(defview confirm-group-criteria-weights(group-coursework-controller)
  (:title "Confirm Peer Review criteria weights")
  (:description "Set the weighting for the group for the various
  criteria you will be reviewing each other on.")
  (:check(controller)
         (can-execute-command-p 'confirm-group-criteria-weights controller))
  (:handler(controller request)
    (let ((group (students-group (username *current-user*) controller)))
      `((p "Comfirm the group review criteria weightings below")
        ,(do-form-with-confirmation
            :data (group-data :weights group)
            :form `((form :method :post)
                    ((input :name :submit :type :submit :value "Confirm")))
            :handler
            #'(lambda(data)
                (confirm-group-criteria-weights controller)
                data)
            :on-success "Group Criteria Review Weights Confirmed"
            :request request)
        (hr)
        ,(markup-form
          (criteria-weights-form controller) (group-data :weights group) t)
        (hr)
        '(p "If you disagree with these criteria you may reset
        them" ((a :href "../set-group-criteria-weights") "here") "- note however all of your team will than have to
        reconfirm the new weights.")))))



