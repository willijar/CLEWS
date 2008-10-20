;;;; Group assessment entity -*- Lisp -*- Copyright (C) 2008
;;;; John A.R. Williams <J.A.R.Williams@jarw.org.uk> Released under
;;;; the GNU General Public License (GPL) See
;;;; <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;

(in-package :clews.coursework)

(defclass coursework(command-processor module)
  ((tutors :type list :initform nil :reader tutors :initarg :tutors
           :documentation "The global tutors usernames")
   (students :type list :initform nil :initarg :students :reader students
             :documentation "The students usernames")
   (state :type hash-table :initform (make-hash-table :test #'equal)
          :documentation "plists of state data keyed by user entity name")
   (components :type list :reader components
    :documentation "The assessment components which make up this coursework"))
  (:documentation "Class to manage a coursework assessment"))

(defmethod shared-initialize :before
    ((coursework coursework) slot-names
     &key components &allow-other-keys)
  (setf (slot-value coursework 'components)
        (mapcar #'(lambda(a) (apply #'make-instance a)) components)))

(defmethod has-permission((action (eql :tutor))
                          (entity coursework)
                          &optional (user *current-user*))
  (member (username user) (tutors entity) :test #'equal))

(defgeneric state(key entityid coursework)
  (:documentation "Return a piece of state data for entity")
  (:method((key symbol) entityid (coursework coursework))
    (getf (gethash entityid (slot-value coursework 'state)) key)))

(defgeneric (setf state)(value key entityid coursework)
  (:documentation "Return a piece of state data for entity")
  (:method(value (key symbol) entityid (coursework coursework))
    (setf (getf (gethash entityid (slot-value coursework 'state)) key)
          value)))

(defgeneric component(componentid coursework)
  (:documentation "Return a component by key")
  (:method(componentid coursework)
    (find componentid (components coursework) :test #'equal :key #'id)))

(defmethod component-completed-p((component individual-component)
                                 (coursework coursework))
    (let ((key (id component)))
      (every #'(lambda(student)
                 (component-completed-p component
                                        (state key student coursework)))
             (students coursework))))

(defmethod component-mark((coursework coursework) (componentid symbol)
                          (entityid string))
  (form-mark (state componentid entityid coursework)
             (component-form (component componentid coursework))))


(defclass group()
  ((id :initarg :id :reader id)
   (tutors :type list :initform nil :reader tutors :initarg :tutors
           :documentation "List of tutors usernames for this group")
   (students :type list :initform nil :reader students :initarg :students
             :documentation "List of student usernames"))
  (:documentation "Class representing an assessment group"))

(defmethod print-object((object group) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (princ (id object) stream)))

(defmethod has-permission((action (eql :tutor))
                          (entity group)
                          &optional (user *current-user*))
  (member (username user) (tutors entity) :test #'equal))

(defmethod has-permission((action (eql :student))
                          (entity group)
                          &optional (user *current-user*))
  (member (username user) (students entity) :test #'equal))

(defclass group-coursework(coursework)
  ((groups :type list :reader groups
           :documentation "The student groups")
   (group-criteria
    :type list :initarg :group-criteria :initform nil :reader group-criteria
    :documentation "alist of group criteria labels and form elements")
   (mark-calculation
    :initarg :mark-calculation :type function
    :initform
    '(lambda(&key group peer assessments)
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

(defmethod students((coursework group-coursework))
  (reduce #'append (groups coursework) :key #'students))

(defmethod shared-initialize :before
    ((coursework group-coursework) slot-names
     &key groups &allow-other-keys)
  (setf (slot-value coursework 'groups)
        (mapcar
         #'(lambda(g)
             (make-instance 'group
                            :id (first g)
                            :students (second g)
                            :tutors (third g)))
         groups)))

(defmethod component-completed-p((component group-component)
                                 (coursework coursework))
  (let ((key (id component)))
    (every #'(lambda(group)
               (component-completed-p component
                                      (state key (id group) coursework)))
           (groups coursework))))

;;  accessors
(defun group(id coursework)
  "Given a group return the group object"
  (find id (groups coursework) :test #'equal :key #'id))

(defun students-group(studentname coursework)
  "Given a students name return the appropriate group object"
  (find studentname (groups coursework) :key #'students
   :test #'(lambda(a b) (member a b :test #'equal))))

(defun peer-review(assessor assesse coursework)
  (cdr (assoc assesse (state :peer-reviews assessor coursework) :test #'equal)))

(defun (setf peer-review)(review assessor assesse coursework)
  (let ((old (assoc assesse (state :peer-reviews assessor coursework)
                    :test #'equal)))
    (if old
        (setf (cdr old) review)
        (push (cons assesse review)
              (state :peer-reviews assesse coursework)))))

(defun group-criteria-confirmed-p(groupid coursework)
  (let ((confirmed-by (state :weights-confirmed groupid coursework)))
   (not (set-difference (students (group groupid coursework))
                        confirmed-by :test #'equal))))

(defun peer-reviews-completed-p(group coursework &optional assessor)
  "Return whether the group (or optionally a student in the group)
have completed all their peer reviews"
  (let ((group (if (typep group 'group) group (group group coursework))))
    (if assessor
        (let ((reviews (state :reviews assessor coursework)))
          (every #'(lambda(s) (member s reviews :test #'equal :key #'car))
                 (students group)))
        (every #'(lambda(s) (peer-reviews-completed-p group coursework s))
               (students group)))))

(defgeneric can-edit-component-data(coursework entityid component
                                               &optional user)
  (:method(coursework entityid component &optional (user *current-user*))
    (declare (ignore entityid))
    (has-permission :tutor coursework user))
  (:method(coursework entityid (component group-component)
           &optional (user *current-user*))
    (or (call-next-method)
        (let ((group (or (group entityid coursework)
                         (students-group entityid coursework))))
          (and group (has-permission :tutor group user))))))

(defgeneric can-view-component-data(coursework entityid component
                                               &optional user)
  (:method(coursework studentid component &optional (user *current-user*))
    (or (equal studentid (username user))
        (can-edit-component-data coursework studentid component user)))
  (:method(coursework entityid (component group-component)
           &optional (user *current-user*))
      (or (call-next-method)
          (let ((group (or (group entityid coursework)
                           (students-group entityid coursework))))
            (and group
                 (member (username user) (students group) :test #'equal))))))

;; external coursework commands - to be called using execute-command

(defcommand set-component-data
    (coursework &key (tutor (username *current-user*))
                componentid entityid data)
  (:documentation "Set the coursework form data for a group")
  (:check
   (declare (ignore data tutor))
   (let ((component (component componentid coursework)))
     (when component
       (can-edit-component-data coursework entityid component))))
  (:action
   (setf (state componentid entityid coursework) `(:tutor ,tutor ,@data)))
  (:undo
   (declare (ignore data tutor))
   (let* ((olddata (state componentid entityid coursework)))
     #'(lambda(coursework)
         (setf (state componentid entityid coursework) olddata)))))

(defcommand set-group-criteria-weights
    (coursework
     &key data
     (groupid (let ((g (students-group (username *current-user*)
                                         coursework)))
                (when g (id g)))))
  (:documentation "Set the group criteria weights - remove confirmations")
  (:check
   (declare (ignore data))
   (let ((group (group groupid coursework)))
     (and
      group
      (or (has-permission :tutor coursework)
          (has-permission '(:tutor :student) group))
      (not (group-criteria-confirmed-p group coursework)))))
  (:action
   (setf (state :weights groupid coursework) data
         (state :weights-confirmed groupid coursework) nil)
   (let ((group (group groupid coursework)))
     (when (has-permission :student group)
       (confirm-group-criteria-weights groupid
                                       :student (username *current-user*)))
     data))
  (:undo
   (declare (ignore data))
   (let* ((weights (state :weights groupid coursework))
          (weights-confirmed (state :weights-confirmed groupid coursework)))
     #'(lambda(coursework)
         (setf (state :weights groupid coursework) weights
               (state :weights-confirmed groupid coursework)
               weights-confirmed)))))

(defcommand confirm-group-criteria-weights
    (coursework &key (student (username *current-user*)))
  (:documentation "Confirm the groups criteria selection")
  (:check
   (let* ((group (students-group student coursework))
          (groupid (when group (id group))))
     (and group
          (state :weights groupid coursework)
          (or (has-permission :tutor coursework)
              (has-permission :tutor group)
              (and (has-permission :student group)
                   (equal (username *current-user*) student))))))
  (:action
   (let* ((group (students-group student coursework))
          (groupid (id group)))
     (pushnew student
              (state :weights-confirmed groupid coursework) :test #'equal))))

(defcommand set-peer-review(coursework &key assessor assessee data)
  (:documentation "Set review of assessee by assessor")
  (:check
   (declare (ignore data))
   (let ((group (students-group assessor coursework)))
     (and group
          (or (has-permission :tutor coursework)
              (has-permission :tutor group)
              (and (has-permission :student group)
                   (not (peer-reviews-completed-p group coursework))
                   (member assessee (students group) :test #'equal))))))
  (:action
   (setf (peer-review assessor assessee coursework) data))
  (:undo
   (declare (ignore data))
   (let* ((olddata (peer-review assessor assessee coursework)))
     #'(lambda(coursework)
         (setf (peer-review assessor assessee coursework) olddata)))))

(defun criteria-weights-form(coursework &optional (submit "Submit"))
  `((form :name :criteria-weights :method :post)
    (table
     (tr (th "Weight (1-20)") (th "Criteria"))
     ,@(mapcar
        #'(lambda(c)
            `(tr (td ((input :name ,(car c) :value "10"
                             :format (integer :min 1 :max 2)
                             :size 5)))
                 (td ,(cdr c))))
        (group-criteria coursework))
     (tr ((td :colspan 2 :align :center)
          ((input :type :submit :value ,submit)))))))

(defun peer-review-form(coursework assesse)
  (let* ((group (students-group assesse coursework))
         (weights (state :weights (id group) coursework)))
  `((form :name :peer-review :method :post)
    (table
     (tr (th "Mark (1-20)") (th "Weighting") (th "Criteria"))
     ,@(mapcar
        #'(lambda(c)
            `(tr (td ((input :name ,(car c) :value "10"
                             :format (integer :min 1 :max 2)
                             :size 5)))
                 ((td :align :right) ,(or (getf weights (car c)) 0))
                 (td ,(cdr c))))
        (group-criteria coursework))
     (tr ((td :colspan 2 :align :center)
          ((input :type :submit :value "Submit Peer Review marks"))))))))

(defun peer-form-mark(group coursework form-data)
  "Return the weighted mark for peer review form data for a group in coursework"
  (let ((weights (state :weights (id group) coursework)))
    (/ (reduce #'+
               (mapcar
                #'(lambda(c)
                    (let ((k (car c)))
                      (* (getf k form-data) (getf k weights))))
                (group-criteria coursework)))
       (reduce #'+ (mapcar #'(lambda(c) (getf (car c) weights))
                           (group-criteria coursework))))))

(defun peer-marks(student coursework)
  "Return the list of a groups peer marks with the students one first"
  (let* ((group (students-group student coursework))
         (students
          (cons student (remove student (students group) :test #'equal))))
    (mapcar
     #'(lambda(assessee)
         (mean
          (mapcar
           #'(lambda(assessor)
               (peer-form-mark group coursework
                               (peer-review assessor assessee coursework)))
           (rest students))))
         students)))

(defun self-mark(student coursework)
  (let ((group (students-group student coursework)))
    (peer-form-mark group coursework (peer-review student student coursework))))

(defun link-to-coursework(coursework componentid entityid
                          &optional (relative "../"))
  (if (can-execute-command-p
       'set-component-data coursework
       :componentid componentid :entityid entityid)
      `((a :href ,(format nil "~A?component=~A&entity=~A"
                          relative componentid entityid))
        ,(let ((m (component-mark coursework  componentid  entityid)))
              (if  (format nil "~,2D" m) "YES")))
      "NO"))

(defun components-table(coursework componentids entityids
                        &optional (relative "../"))
  `(table
    (tr (th )
        ,@(mapcar #'(lambda(c) `(th ,(string c))) componentids))
    ,@(mapcar
       #'(lambda(entityid)
           `(tr (th ,entityid)
                ,@(mapcar
                   #'(lambda(componentid)
                       `(td ,(link-to-coursework
                              coursework componentid entityid relative)))
                   componentids)))
       entityids)))

;;; views on this coursework
(defview enter-coursework(group-coursework)
  (:title "Coursework")
  (:description "Enter or review an coursework")
  (:check(coursework &key componentid entityid)
         (some
          #'(lambda(entityid)
              (some
               #'(lambda(componentid)
                   (can-execute-command-p
                    'set-component-data coursework
                    :componentid componentid :entityid entityid))
               (if componentid
                   (list componentid)
                   (mapcar #'id (components coursework)))))
          (if entityid
              (list entityid)
              (append (mapcar #'id (groups coursework))
                      (students coursework)))))
  (:handler(coursework request)
      (let ((componentid
             (first (query-values :component request)))
            (entityid (first (query-values :entity request)))
            (groupids (mapcar #'id (groups coursework)))
            (studentids (students coursework))
            group-components individual-components)
        (dolist(c (components coursework))
          (if (typep c 'group-component)
              (push (id c) group-components)
              (push (id c) individual-components)))
        (nconc
         (when (and componentid entityid)
           (let ((componentid (intern componentid :keyword)))
             (unless
                 (can-execute-command-p
                  'set-component-data coursework
                  :componentid componentid :entityid entityid)
               (error 'permission-denied-error
                      :action "Assess"
                      :service componentid))
             (let* ((component (component componentid coursework))
                    (group-p (typep component 'group-component)))
               `((section
                  :title ,(format nil "~S coursework for ~:[student~;group~] ~S"
                                  componentid group-p entityid))
                 ,(do-form-with-confirmation
                   :request request
                   :data (state componentid entityid coursework)
                   :form (component-form component)
                   :handler #'(lambda(data)
                                (set-component-data
                                 coursework
                                 :componentid componentid
                                 :entityid entityid
                                 :data data)))
                 (hr)))))
        `(((section :title "Group Components")
           ,(components-table coursework group-components groupids))
          ((section :title "Individual Components")
           ,(components-table coursework individual-components studentids)))))))

(defview set-group-criteria-weights(group-coursework)
  (:title "Set Peer review criteria weights")
  (:description "Set the weighting for the group for the various
  criteria you will be reviewing each other on.")
  (:check(coursework)
         (can-execute-command-p 'set-group-criteria-weights coursework))
  (:handler(coursework request)
    (let ((group (students-group (username *current-user*) coursework)))
      (list
       (do-form-with-confirmation
           :data (state :weights (id group) coursework)
           :form (criteria-weights-form coursework)
           :handler
           #'(lambda(data)
               (set-group-criteria-weights
                coursework :groupid (id group) :data data))
           :request request)))))

(defview confirm-group-criteria-weights(group-coursework)
  (:title "Confirm Peer Review criteria weights")
  (:description "Set the weighting for the group for the various
  criteria you will be reviewing each other on.")
  (:check(coursework)
         (can-execute-command-p 'confirm-group-criteria-weights coursework))
  (:handler(coursework request)
    (let ((group (students-group (username *current-user*) coursework)))
      `((p "Comfirm the group review criteria weightings below")
        ,(do-form-with-confirmation
          :data (state :weights (id group) coursework)
            :form `((form :method :post)
                    ((input :name :submit :type :submit :value "Confirm")))
            :handler
            #'(lambda(data)
                (confirm-group-criteria-weights coursework)
                data)
            :on-success "Group Criteria Review Weights Confirmed"
            :request request)
        (hr)
        ,(markup-form
          (criteria-weights-form coursework)
          (state :weights (id group) coursework) t)
        (hr)
        '(p "If you disagree with these criteria you may reset
        them" ((a :href "../set-group-criteria-weights") "here") "- note however all of your team will than have to
        reconfirm the new weights.")))))



