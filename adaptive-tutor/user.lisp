;;;; Application specific user handling
;;;; Copyright (C) 2002-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; Application specific user handling In all of these functions
;;;; user-data is the specific property subset associated with this
;;;; application currently this is an alist of properties enabling
;;;; simple extensions but sacrificing efficiency and compiler
;;;; checking
;;;; $Id: user.lisp,v 1.1 2006/07/30 17:38:52 willijar Exp $

(in-package :clews.adaptive-tutor)

(defvar *user-default-properties* nil)
;;; user information is stored in an alist property instance with the
;;; following fields
;;; :last-concept-id  - id of the last concept visited
;;; :goals            - list of the ids of the goals for this user
;;; :knowledge        - the knowledge property subset (see below)
;;;          a hash table property instance keyed by the concept id
;;;          with values containing fields specific to a particular user
;;;          and concept. The currently used fields are
;;; :last-visit-time   - when the user last visited this concept
;;; :time-spend        - total time spend on this concept

(defun make-knowledge-subset(defaults)
  (let ((hash (make-hash-table :test #'equal)))
    (if defaults
        (make-instance 'property-holder
                       :default-properties defaults
                       :properties hash)
        hash)))

(defmethod knowledge(user-data (concept-id string))
  (property-subset
   (property-subset user-data :knowledge
                    :maker #'make-knowledge-subset
                    :defaults *user-default-properties*)
   concept-id))

(defmethod (setf knowledge)((value list) user-data (concept-id string))
  (setf (property
         (property-subset user-data :knowledge
                          :maker #'make-knowledge-subset
                          :defaults *user-default-properties*)
         concept-id)
        (or value (list nil))))

(defmethod knowledge(user-data (concept concept))
  (knowledge user-data (concept-id concept)))

(defun leave-concept(user-data tutor)
  "Update user-data properties (user) when a user-data leaves a concept"
  (let ((now (get-universal-time)))
    ;; update time spent on the last concept
    (when-bind (last-concept-id	(property user-data :last-concept-id))
      (let* ((knowledge (knowledge user-data last-concept-id))
             (time-interval (- now (property knowledge :last-visit-time now)))
             (time-spent (property knowledge :time-spent 0))
             (concept (concept tutor last-concept-id :if-not-exist :ignore)))
        (when (and concept knowledge
                   (< time-interval (property concept :timeout)))
          (setf (property knowledge :time-spent)
                (+ (or time-spent 0) time-interval))))))
  ;;remove last-concept-id record
  (rem-property user-data :last-concept-id)
  ;;remove completed goals from
  ;;users goal list
  (setf (property user-data :goals)
        (mapcan
         #'(lambda(concept-id)
             (when-bind (concept
                         (concept tutor concept-id :if-not-exist :ignore))
               (unless (completed-p user-data tutor concept)
                 (list concept-id))))
         (property user-data :goals))))

(defun visit-concept(user-data tutor concept)
  "Record when a user-data visits a concept, updating user-data goals as necessary"
  (let ((concept-id (concept-id concept)))
    (setf (property user-data :last-concept-id) concept-id)
    (setf (property (knowledge user-data concept-id) :last-visit-time)
          (get-universal-time))
    ;; if concept is not in a path of a current goal add it as a goal
    ;; if item is a goal put to start of list
    (let ((goal
           (cond ((some
                   #'(lambda (concept-id)
                       (when (ancestors (concept tutor concept-id) concept)
                         concept-id))
                   (property user-data :goals)))
                 (t concept-id))))
      (setf (property user-data :goals)
            (remove goal (property user-data :goals) :test #'equal))
      (push goal (property user-data :goals)))))

(defun record-visit(user-data tutor concept)
  "Record a visit by user-data to concept. Updates user-data properties and knowledge"
  (leave-concept user-data tutor)
  (visit-concept user-data tutor concept))

(defun direct-completion(user-data concept)
  "Percentage completion level achieved by user
   of this concept (ignoring its children)"
  (let ((f (property concept :completion))
        (knowledge (knowledge user-data concept)))
    (cond ((functionp f) (funcall f knowledge concept))
          ((assessment concept)
           (if (completed knowledge) 1.0 0.0))
          ((property knowledge :time-spent)
           (let ((min-time (property concept :min-time-required)))
             (if (and min-time (> min-time 0))
                 (min 1.0 (/ (property knowledge :time-spent) min-time))
                 t)))
          (0))))

(defun accumulate-weighting(&optional a b)
  "Essential the equivalent of addition for probabilities"
  (if a (- 1 (* (- 1 a) (- 1 b))) 0))

#|
(defun outcomes-for-weighting(outcomes-for &optional filter)
  "Total outcomes product from an outcomes dataset - if supplied filtered by filter function"
  (reduce #'accumulate-weighting
          (mapcan #'(lambda(item)
                      (when (or (not filter) (apply filter (car item)))
                        (list (cdr item))))
                  outcomes-for)))

(defun accumulate-concept-scores(tutor concept &optional func filter)
  (reduce #'accumulate-weighting
          (mapcar #'(lambda(o)
                      (* (funcall func (car o)) (cdr o)))
                  (outcomes-for tutor concept))))
|#

(defun accumulate-concept-scores(tutor concept &key score test outcomes)
  "Accumulate a score function across all outcomes towards concept
which satisfy the test. If score is nil then the weighting is
returned. If test is nil then all outcomes are examined. if outcomes
is provided it will be used as the list of outcome records otherwise
they will be determined from the tutor"
  (reduce #'accumulate-weighting
          (mapcan #'(lambda(o)
                      (when (or (not test) (funcall test (car o)))
                        (list
                         (if score
                             (* (funcall score (car o)) (cdr o))
                             (cdr o)))))
                  (or outcomes (outcomes-for tutor concept)))))

(defun concept-completion(user-data tutor concept)
  "Percentage completion level achieved by user
   of this concept (including outcomes towards it)"
  (accumulate-concept-scores
   tutor concept
   :score #'(lambda(concept) (direct-completion user-data concept))))

(defun concept-assessment-mark(user-data concept)
  "Mark from assessment on this concept only"
  (let ((assessment (assessment concept))
        (knowledge (knowledge user-data concept)))
    (if (and assessment (assessment-count-p knowledge assessment))
        (assessment-mark knowledge assessment)
        0)))

(defun concept-understanding(user-data tutor concept &key outcomes)
  "Percentage mark achieved by user of this concept
 (including outcomes towards it)"
  (let((outcomes (or outcomes (outcomes-for tutor concept)))
       (weighting (accumulate-concept-scores
                   tutor concept
                   :test #'assessment
                   :outcomes outcomes)))
    (if (> weighting 0)
        (/ (accumulate-concept-scores
            tutor concept
            :score #'(lambda(concept)
                       (concept-assessment-mark user-data concept))
            :test #'assessment
            :outcomes outcomes)
           weighting)
        0)))

(defun directly-completed-p(user-data tutor concept)
  "True if user-data has actually directly completed this concept, ignoring its
   children"
  (declare (ignore tutor))
  (>= (direct-completion user-data concept)
      (property concept :completion-threshold)))

(defun completed-p(user-data tutor concept)
  "Returns true if the user-data is considered to have completed this concept
   (and its children)"
  (and (directly-completed-p user-data tutor concept)
       (every #'(lambda(c) (completed-p user-data tutor c))
              (direct-children concept))))

(defun incomplete-children(user-data tutor concept)
  "returns list of children of given concept not directly completed by
the user. Will be in correct sequential ordering"
  (mapcan #'(lambda(c) (when (not (directly-completed-p
                                   user-data tutor c))
                         (list c)))
          (children concept)))

(defun incomplete-prerequisites(user-data tutor concept)
  "returns all incomplete prerequsits of given concept for given user
   in order of importance * how much student has left to do"
  (mapcar #'cdr
          (sort
           (mapcan
            #'(lambda(p)
                                        ;list weight w * (1-completion level) and prerequisit concept
                (let ((prerequisite (car p)) (w (cdr p)))
                  (unless
                      (if (is-child prerequisite concept)
                          (directly-completed-p user-data tutor prerequisite)
                          (completed-p user-data tutor prerequisite))
                    (list
                     (cons (* w (- 1 (concept-completion user-data
                                                         tutor prerequisite)))
                           prerequisite)))))
            (prerequisites concept))
           #'> :key #'car)))

(defun user-tasks(user-data tutor)
  "return a list of the tasks (incomplete concepts)
   to acheive the goals for given user-data in an appropriate order. "
  (delete-duplicates
   (mapcan #'(lambda(concept-id)
               (let ((c (concept tutor concept-id)))
                 (if (directly-completed-p user-data tutor c)
                     (incomplete-children user-data tutor c)
                     (cons c (incomplete-children user-data tutor c)))))
           (property user-data :goals)) :from-end t))

(defun next-concept(user-data tutor concept)
  "Return the next recommended concept for given user. This will be
   the first user-task , or, if its first prerequisites if they have
   not yet been met"
  (when-bind (task (some #'(lambda(c)
                             (unless (and concept
                                          (or (eq c concept)
                                              (is-child c concept)))
                               c))
                         (user-tasks user-data tutor)))
    (let ((p (incomplete-prerequisites user-data tutor task)))
      (if p (first p) task))))

(defun required-concept-ids(tutor)
  (mapcan #'(lambda(c) (property c :required)) (concepts tutor)))

(defun next-required-concept(user-data tutor)
  (dolist (concept-id (append (required-concept-ids tutor)
                              (property user-data :required)))
    (let ((concept (concept tutor concept-id)))
      (unless (property (knowledge user-data concept-id) :last-visit-time)
        (return-from next-required-concept concept)))))

(defmethod goals((tutor adaptive-tutor) (user user))
  "returns list of concepts which are the goals of this user"
  (delete-duplicates
   (append
    (mapcan
     #'(lambda(id)
         (when-bind (concept (concept tutor id :if-not-exist :ignore))
           (list concept)))
     (property (user-component-properties tutor user) :goals))
    (mapcan
     #'(lambda(c)
         (let ((roles (property c :topnode)))
           (when (or (eq roles t)
                     (has-role user roles))
             (list c))))
     (concepts tutor)))))

(defmethod assessment-goals((tutor adaptive-tutor) (user user))
  "return a list of concepts with assessments which are underneath the
goals of the user"
  (delete-duplicates
   (mapcan
    #'(lambda(goal)
        (mapcan
         #'(lambda(concept) (when (assessment concept) (list concept)))
         (cons goal (children goal))))
    (goals tutor user))))

(defun knowledge-set(tutorial concept students)
  "Given a list of students, return the corresponding list of
knowledge sub-records for a given concept in the tutorial"
  (mapcar
   #'(lambda(student)
       (knowledge (user-component-properties tutorial student) concept))
   students))

(defun list+(a b)
  "Add corresponding elements in two lists together"
  (mapcar #'(lambda(a b) (+ (or a 0) (or b 0))) a b))

(defun normalised-concept-marks(tutorial concept students
                                normalisation-students)
  "Return for the group of students their marks for the particular
concept in the tutorial, normalised against the
normalisation-students. This takes account of the assessments in the
concept and all its descendants"
  (let* ((contributers (mapcan #'(lambda(c) (when (assessment c) (list c)))
                               (cons concept (children concept))))
         (weights (mapcar
                   #'(lambda(c)
                       (assessment-normalisation-weighting
                        (knowledge-set tutorial c normalisation-students)
                        (assessment c)))
                   contributers))
         (weight-total (reduce #'+ weights))
         (marks (mapcar #'(lambda(c w)
                            (mapcar #'(lambda(m) (* (or m 0) w))
                                    (assessment-normalised-marks
                                     (knowledge-set tutorial c students)
                                     (assessment c)
                                     normalisation-students)))
                        contributers weights)))
    (when marks
      (if (= weight-total 0) (setf weight-total 1))
      (mapcar #'(lambda(m) (/ m weight-total))
              (reduce #'list+ marks)))))

(defun concept-marks(tutor concept students &key outcomes)
  "Return for the group of students their marks for the particular
concept in the tutorial taking account of the assessments in the
concept and all its descendants only"
  (let* ((contributers (mapcan #'(lambda(c) (when (assessment c) (list c)))
                               (cons concept (children concept))))
         (outcomes (or outcomes (outcomes-for tutor concept)))
         (weights (mapcar #'(lambda(c)
                              (let ((outcome (assoc c outcomes)))
                                (if outcome (cdr outcome) 0)))
                          contributers))
         (weight-total (reduce #'+ weights))
         (marks (mapcar
                 #'(lambda(c w)
                     (mapcar #'(lambda(student)
                                 (* (concept-assessment-mark
                                     (user-component-properties tutor student)
                                     c) w))
                             students))
                 contributers weights)))
    (when marks
      (if (= weight-total 0) (setf weight-total 1))
      (mapcar  #'(lambda(m) (/ m weight-total))
               (reduce #'list+ marks)))))