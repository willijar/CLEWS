;; $Id: assessment.lisp,v 1.3 2006/08/09 10:00:55 willijar Exp willijar $
;; General API for assessment management
;; Copyright (C) 2003-2006 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of CLEWS: Assessments

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;; There are three views of assessments, do, feedback and manage
;; corresponding to a student doing an assessment, a student getting
;; feedback on their attempt and management for marking/analysing etc
;; the assessment does not control who can access what - it only operates
;; on a knowledge record.

;; These access definitions are a workaround as CMUCL does not allow
;; call-next-method with macro method combinations. Once this is
;; sorted there the ??-p-reason methods can be changed to access-p
;; and an aroung method can be used.

;;; Code:

(in-package :clews.assessment)

(defgeneric knowledge(user assessment)
  (:documentation "Return the knowledge for given user and assessment"))

(defgeneric (setf knowledge)(knowledge user assessment)
  (:documentation "Store  the knowledge for given user and assessment"))

(defclass assessment(access-controlled-entity)
  ((description :initform nil :initarg :description :reader description
                :documentation "String descripting this assessment")
   (knowledge :initform (make-hash-table :test #'equal) :initarg :knowledge
              :documentation "Store of users answers")))

(defgeneric validate-assessment(assessment)
  (:documentation "Signal an invalid-assessment-definition condition
if the assessment is invalid.")
  (:method-combination progn)
  (:method progn (assessment) (declare (ignore assessment))))

(defmethod initialize-instance :after((assessment assessment) &key answers)
  (dolist(a answers) (setf (knowledge (car a) assessment) (cdr a))))

(defmethod initargs nconc ((obj assessment))
  `(,@(opt :acl (acl obj))
    ,@(opt :description (description obj))
    ,@(opt :answers
           (let ((entries nil))
             (maphash #'(lambda(k v) (push (cons k v) entries))
                      (slot-value obj 'knowledge))
             entries))))

(defmethod knowledge((username string) assessment)
  (or (get-dictionary username (slot-value assessment 'knowledge))
      (setf (get-dictionary username (slot-value assessment 'knowledge))
            (list nil))))

(defmethod (setf knowledge)(knowledge (username string) assessment)
  (setf (get-dictionary username (slot-value assessment 'knowledge))
        knowledge))

(defgeneric assessment-access-p-reason(knowledge assessment)
  (:documentation "Return the reason if access is not allowed,
otherwise nil if access is allowed i.e. they operate in the opposite
sense")
  (:method-combination or :most-specific-last)
  (:method or (knowledge assessment)
           (declare (ignore knowledge assessment))
           nil))

(defun assessment-attempt-p(knowledge assessment)
  "True if assessment can be accessed, otherwise values false and a
reason why the assessment cannot be accessed."
  (let ((v (assessment-attempt-p-reason knowledge assessment)))
    (if v (values nil v) t)))

(defgeneric assessment-attempt-p-reason(knowledge assessment)
  (:documentation "Return the reason if access is not allowed,
otherwise nil if access is allowed i.e. they operate in the opposite
sense")
  (:method-combination or :most-specific-last)
  (:method or (knowledge assessment)
           (assessment-access-p-reason knowledge assessment)))

(defgeneric assessment-should-not-attempt-reason(knowledge assessment)
  (:documentation "If it is recommended that an assessment should not
be attempted then this will return a string reason, otherwise returns nil")
  (:method-combination or :most-specific-first)
  (:method or (knowledge assessment)
           nil))

(defun assessment-feedback-p(knowledge assessment)
  "True if assessment feedback can be accessed, otherwise values false and a
reason why the assessment cannot be accessed."
  (let ((v (assessment-feedback-p-reason knowledge assessment)))
    (if v (values nil v) t)))

(defgeneric assessment-marked-p(knowledge assessment)
  (:documentation "Return true if an assessment has been marked"))

(defun assessment-reset-p(knowledge assessment)
  (and
   (eql (clews.assessment::multiple-attempt-p assessment) :resetable)
   (assessment-feedback-p knowledge assessment)))

(defgeneric assessment-feedback-p-reason(knowledge assessment)
  (:documentation "Return the reason if access is not allowed,
otherwise nil if access is allowed i.e. they operate in the opposite
sense")
  (:method-combination or :most-specific-last)
  (:method or (knowledge assessment)
           (or (assessment-access-p-reason knowledge assessment)
               (unless (completed knowledge)
                 "You have not completed this assessment and so
feedback is not available")
               (unless (assessment-marked-p knowledge assessment)
                 "This assessment has not been marked yet."))))

(defun assessment-count-p(knowledge assessment)
  "True if assessment can be accessed, otherwise values false and a
reason why the assessment cannot be accessed."
  (let ((v (assessment-count-p-reason knowledge assessment)))
    (if v (values nil v) t)))

(defgeneric assessment-count-p-reason(knowledge assessment)
  (:documentation
   "False if questionnaire mark counts (as a formal mark) otherwise
return reason why assessment is not counted.  Marks are only counted
when feedback is available.")
  (:method-combination or :most-specific-last)
  (:method or (knowledge assessment)
           (assessment-feedback-p-reason knowledge assessment)))

(defgeneric time-remaining(knowledge assessment)
  (:documentation "Time remaining to complete this assessment")
  (:method-combination list)
  (:method :around (knowledge assessment)
           (declare (ignore knowledge assessment))
           (let ((m (mapcan #'(lambda(v) (if v (list v)))
                            (call-next-method))))
             (if m (max (apply #'min m) 0) nil)))
  (:method list (knowledge assessment)
           (declare (ignore knowledge assessment))
           nil))

(defgeneric assessment-status-long(knowledge assessment)
  (:documentation "Return a list of status items. car of
each item is a string label cdr is the value.")
  (:method-combination append :most-specific-last)
  (:method append (knowledge assessment)
           (let ((started (started knowledge))
                 (completed (completed knowledge))
                 (remaining (time-remaining knowledge assessment)))
             (nconc
              (when (description assessment)
                (list (cons "Description" (description assessment))))
              (when started
                (list (cons "Started" (datestring started))
                      (cons "Time taken"
                            (format nil "~,1F minutes"
                                    (/ (timetaken knowledge) 60.0) ))))
              (when completed
                (list (cons "Completed" (datestring completed))))
              (when remaining
                (list (cons "Time Remaining"
                            (if (> remaining 0)
                                (format nil "~,1F minutes (until ~A)"
                                        (/ remaining 60.0) (datestring (+ remaining (get-universal-time))))
                                "None") )))
              (list
               (cons "Marked"
                     (if (assessment-marked-p knowledge assessment)
                         "Yes" "No")))
              (when (assessment-feedback-p knowledge assessment)
                (list
                 (cons
                  "Mark"
                  (format nil "~2,1F%~:[ ~;. Not Counted: ~:*~A~]"
                          (* 100 (assessment-mark knowledge assessment))
                          (assessment-count-p-reason
                           knowledge assessment)))))) )))

(defgeneric assessment-status-short(knowledge assessment)
  (:documentation "A short single one line string description of
status for using in lists. Primary nconc methods should return a list
of string items - these will all be appended together in a comma
seperated list to form the final answer")
  (:method-combination append :most-specific-last)
  (:method :around (knowledge assessment)
           (declare (ignore knowledge assessment))
           (join-strings (call-next-method) ", "))
  (:method append (knowledge assessment)
           (let ((started (started knowledge))
                 (completed (completed knowledge))
                 (remaining (time-remaining knowledge assessment)))
             (nconc
              (if completed (list "Completed")
                  (if started (list "Started")))
              (when (and remaining
                         (assessment-attempt-p knowledge assessment))
                (list (format nil "~D minutes remaining"
                              (/ remaining 60))))
              (when (assessment-feedback-p knowledge assessment)
                (list (format nil "~2,1F%~:[ (Not counted)~;~]"
                              (* 100 (assessment-mark knowledge assessment))
                              (assessment-count-p knowledge assessment))))))))

(defgeneric assessment-metadata(assessment)
  (:documentation
   "Return an alist of descriptive metadata associated with the assessment")
  (:method-combination append :most-specific-last)
  (:method append (obj)
           (list (cons :type (string (type-of obj))))))

(defgeneric assessment-attempt-markup(knowledge assessment request)
  (:documentation "Markup for this assessment to be returned in place. knowledge modified")
  (:method :before (knowledge assessment request)
    (declare (ignore request))
    (access-assert #'assessment-attempt-p knowledge assessment)
    (initialize-knowledge knowledge assessment))
  (:method :after(knowledge assessment request)
     (declare (ignore request assessment))
     (unless (property knowledge :started)
       (setf (property knowledge :started) (get-universal-time)))))

(defgeneric assessment-feedback-markup(knowledge assessment request)
  (:documentation "Markup for this assessment to be returned in place")
  (:method :before (knowledge assessment request)
    (declare (ignore request))
    (access-assert #'assessment-feedback-p knowledge assessment)))

(defgeneric assessment-mark(knowledge assessment)
  (:documentation "return raw users mark for this assessment")
  (:method :around (knowledge assessment)
    (declare (ignore assessment))
    (when (completed knowledge) (call-next-method))))

(defgeneric assessment-mark-markup(knowledge assessment request)
  (:documentation "Return the markup to mark this assessment")
  (:method(knowledge assessment request)
    (handler-case
        (assessment-feedback-markup knowledge assessment request)
      (invalid-assessment-access(c)
        (declare (ignore c))
        (invoke-restart 'continue)))))

(declaim (inline initialize-knowledge-value))
(defun initialize-knowledge-value(knowledge field value)
  (if value (setf (property knowledge field) value)
      (rem-property knowledge field)))

(defgeneric initialize-knowledge (knowledge assessment)
  (:documentation "initialize a knowledge record for this
assessment if not already initialized. Knowledge record is
 modified and returned.")
  (:method-combination progn :most-specific-last)
  (:method :around (knowledge assessment)
           (declare (ignore assessment))
           (unless (property knowledge :initialized)
             (call-next-method)
             (setf (property knowledge :initialized) t))
           knowledge))

(defgeneric reinitialize-knowledge (knowledge assessment)
  (:method(knowledge assessment)
    (rem-property knowledge :initialized)
    (rem-property knowledge :started)
    (rem-property knowledge :completed)
    (initialize-knowledge knowledge assessment)))

(defgeneric assessment-normalisation-weighting(normalisation-set
                                               assessment)
  (:documentation "returns the weighting for this
assessment on the basis of a normalisation set of submissions"))

(defgeneric assessment-normalised-marks
    (knowledge-set assessment &key normalisation-set counted-only)
  (:documentation
   "returns the normalised marks for knowledge-set for this assessment
normalisation-set is the set of knowledge to be used in detemrining
normalisation.
If keyword counted-only is true (the default) then marks which are not
counted will be returned as nil, otherwise the mark will be returned
regardless. Non-completions are always nil"))

(defmethod assessment-normalisation-weighting(normalisation-knowledge-set
                                              assessment)
  "The relative weighting for this assessment calculated on basis of entire
normalisation set (even if their mark would't be calculated)"
  (let ((knowledge-set (mapcan #'(lambda(k) (when (completed k) (list k)))
                               normalisation-knowledge-set)))
    (if knowledge-set
        (*
         (- 1 (mean (mapcar #'(lambda(m) (or m 0))
                            (assessment-normalised-marks
                             knowledge-set assessment
                             :normalisation-set knowledge-set
                             :counted-only nil))))
         (/ (mean (mapcar #'timetaken knowledge-set)) 60))
        1)))

(defmethod assessment-normalised-marks
    (knowledge-set assessment &key normalisation-set (counted-only t) )
  "Default- just return the raw marks"
  (declare (ignore normalisation-set))
  (mapcar #'(lambda(k)
              (when (or (not counted-only) (assessment-count-p k assessment))
                (assessment-mark k assessment)))
          knowledge-set))

(defgeneric assessment-detail-statistics
    (knowledge-set assessment &optional normalisation-set )
  (:documentation "Return markup giving some detailed information on assessment
statistics e.g. component averages etc")
  (:method(knowledge-set assessment &optional normalisation-set )
    (declare (ignore knowledge-set assessment normalisation-set))
    "Default - nothing" nil))

(defgeneric assessment-distance-metric(knowledge1 knowledge2 assessment)
  (:documentation "Returns the (Hamming) distance metric for this assessment between knowledge1 and knowledge2, or nil if not applicable")
  (:method(knowledge1 knowledge2 assessment)
    (declare (ignore knowledge1 knowledge2 assessment))))

;;; some default mixin implementations for standard API

(defclass ephemeral(assessment)
  ((start-date :initform nil :type datespec :reader default-start-date
    :documentation "When assessment is available from or nil for anytime")
   (end-date :initform nil :type datespec :reader default-end-date
    :documentation "When assessment is available to or nil for anytime"))
  (:documentation "As assessment is only accessable ephemerably
between a start and end date"))

(defmethod initialize-instance :after ((assessment ephemeral)
				       &key start-date end-date)
  ;;; allow init args to specified as strings or UT but store as UT
  (setf (slot-value assessment 'start-date) (datespec start-date))
  (setf (slot-value assessment 'end-date) (datespec end-date)))

(defmethod validate-assessment progn ((assessment ephemeral))
  (with-accessors((start-date default-start-date)
                  (end-date default-end-date)) assessment
    (when (and start-date end-date (< end-date start-date))
      (error 'invalid-assessment-definition
             :reason "Assessment start date is after end date."))))

(defmethod initargs nconc ((obj ephemeral))
           `(,@(opt :start-date (datestring (default-start-date obj)))
             ,@(opt :end-date (datestring (default-end-date obj)))))

(defmethod initialize-knowledge progn (knowledge (assessment ephemeral))
           (initialize-knowledge-value knowledge :start-date
                                       (default-start-date assessment))
           (initialize-knowledge-value knowledge :end-date
                                       (default-end-date assessment)))

(defmethod assessment-metadata append ((obj ephemeral))
           (list (cons :start-date (datestring (default-start-date obj)))
                 (cons :end-date (datestring (default-end-date obj)))))

(defgeneric start-date(knowledge assessment)
  (:documentation "When assessment is available from or nil for anytime")
  (:method (knowledge (assessment ephemeral))
    (or (property knowledge :start-date)
        (default-start-date assessment))))

(defgeneric end-date(knowledge assessment)
  (:documentation"When assessment is available to or nil for anytime")
  (:method (knowledge (assessment ephemeral))
    (or (property knowledge :end-date)
	(default-end-date assessment))))

(defmethod assessment-access-p-reason or (knowledge
                                          (assessment ephemeral))
           (let ((now (get-universal-time))
                 (start-date (start-date knowledge assessment))
                 (end-date (end-date knowledge assessment)))
             (cond ((and start-date (> start-date now))
                    (format nil "This assessment not available until ~A"
                            (datestring start-date)))
                   ((and end-date (< end-date now))
                    (format nil "This assessment not available after ~A"
                            (datestring end-date))))))

(defclass scheduled(assessment)
  ((deadline-date
    :initform nil :type datespec :reader default-deadline-date
    :documentation "The deadline for submissions to an assessment
or nil if anytime")
   (feedback-date
    :initform nil :type datespec :reader default-feedback-date
    :documentation "When feedback will be available from or nil if anytime")
   (strict-deadline :initarg :strict-deadline
    :type boolean :initform nil :reader strict-deadline-p
    :initarg :strict-deadline
    :documentation "If true attempts not allowed after deadline,
otherwise submissions will be allowed after the deadline but they will
not be counted in mark")
   (multiple-attempt :initarg :multiple-attempt
    :type symbol :initform nil
    :reader multiple-attempt-p
    :initarg :multiple-attempt
    :documentation "If true multiple attempts are allowed, even after the feedback is available. If = :resetable then the option to reset the questionnaire for a student is available."))
  (:documentation "As assessment which has a deadline and feedback
schedule"))

(defmethod initialize-instance :after ((assessment scheduled)
                                       &key deadline-date feedback-date)
  ;;; allow init args to specified as strings or UT but store as UT
  (setf (slot-value assessment 'deadline-date) (datespec deadline-date))
  (setf (slot-value assessment 'feedback-date) (datespec feedback-date)))

(defmethod validate-assessment progn ((assessment scheduled))
  (with-accessors((deadline-date default-deadline-date)
                  (strict-deadline strict-deadline-p)) assessment
    (when (and strict-deadline (not deadline-date))
      (error 'invalid-assessment-definition
             :reason "A strict deadline is set with no actual deadline date."))))

(defmethod initargs nconc ((obj scheduled))
  `(,@(opt :deadline-date (datestring (default-deadline-date obj)))
    ,@(opt :feedback-date (datestring (default-feedback-date obj)))
    ,@(opt :strict-deadline (strict-deadline-p obj))
    ,@(opt :multiple-attempt (multiple-attempt-p obj))))

(defmethod initialize-knowledge progn (knowledge (assessment scheduled))
  (initialize-knowledge-value knowledge :deadline-date
                              (default-deadline-date assessment))
  (initialize-knowledge-value knowledge :feedback-date
                              (default-feedback-date assessment)))

(defmethod assessment-metadata append ((obj scheduled))
  (list (cons :deadline-date (datestring (default-deadline-date obj)))
        (cons :feedback-date (datestring (default-feedback-date obj)))
        (cons :strict-deadline (strict-deadline-p obj))
        (cons :multiple-attempt (multiple-attempt-p obj))))

(defgeneric deadline-date(knowledge assessment)
  (:documentation "Deadline for attempts at this assessment or nil if anytime")
  (:method (knowledge assessment)
    (declare (ignore assessment))
    (property knowledge :deadline-date))
  (:method (knowledge (assessment scheduled))
    (or (property knowledge :deadline-date)
        (default-deadline-date assessment))))

(defgeneric feedback-date(knowledge assessment)
  (:documentation "Date from which feedback is available or nil if anytime")
  (:method (knowledge (assessment scheduled))
    (or (property knowledge :feedback-date)
        (default-feedback-date assessment)
        (deadline-date knowledge assessment))))

(defmethod assessment-attempt-p-reason or (knowledge (assessment scheduled))
           (let ((now (get-universal-time))
                 (deadline-date (deadline-date knowledge assessment)))
             (cond ((and deadline-date
                         (strict-deadline-p assessment)
                         (< deadline-date now))
                    (format nil "The strictly enforced deadline of ~A for
submissions to this assessment has now passed and further attempts are
not allowed"
                            (datestring deadline-date)))
                   ((and (not (multiple-attempt-p assessment))
                         (assessment-feedback-p knowledge assessment))
                    "Feedback is now available for this assessment and further
attempts are therefore not allowed"))))

(defmethod assessment-should-not-attempt-reason or (knowledge
                                                    (assessment scheduled))
           (let ((now (get-universal-time))
                 (deadline-date (deadline-date knowledge assessment)))
             (when (and deadline-date (< deadline-date now))
               (format nil "The deadline of ~A for submissions to this
assessment has now passed and further attempts are not recommended."
                       (datestring deadline-date)))))

(defmethod assessment-feedback-p-reason or (knowledge (assessment scheduled))
  (let ((feedback-date (feedback-date knowledge assessment)))
    (when (and feedback-date (>= feedback-date (get-universal-time)))
      (format nil "Feedback will not be available until ~A"
              (datestring feedback-date)))))

(defmethod assessment-count-p-reason or (knowledge (assessment scheduled))
  (let ((deadline (deadline-date knowledge assessment))
        (completed (completed knowledge)))
    (when (and deadline (< deadline completed))
      (format
       nil
       "The submission time of ~A is after the assessment deadline of ~A"
       (datestring completed) (datestring deadline)))))

(defmethod assessment-status-long append (knowledge (assessment scheduled))
  (let ((deadline-date (deadline-date knowledge assessment))
        (feedback-date (feedback-date knowledge assessment)))
    (nconc
     (when deadline-date
       (list (cons "Deadline" (datestring deadline-date))))
     (when (completed knowledge)
       (list (cons "Feedback Available"
                   (cond
                     ((assessment-feedback-p knowledge assessment)  "Now")
                     ((not (assessment-marked-p knowledge assessment))
                      "When Marked")
                     (feedback-date (datestring feedback-date))
                     ("No")))))
     (when (multiple-attempt-p assessment)
       (list '("Multiple Attempts" . "Allowed"))))))

(defmethod assessment-status-short append (knowledge (assessment scheduled))
  (let ((deadline-date (deadline-date knowledge assessment)))
    (when deadline-date
      (list (format nil "Deadline ~A" (datestring deadline-date))))))

(defmethod time-remaining list (knowledge (assessment scheduled))
  "Time remaining to complete this assessment"
  (let ((deadline (deadline-date knowledge assessment)))
    (when deadline (- deadline (get-universal-time)))))

(defclass timelimited(assessment)
  ((timelimit
    :initform nil :type integer :reader default-timelimit
    :initarg :timelimit
    :documentation "Time limit in seconds to complete this assessment")
   (strict-timelimit
    :type boolean :initform nil :reader strict-timelimit-p
    :initarg :strict-timelimit
    :documentation "If true submissions taking longer than timelimit
will not be counted."))
  (:documentation "Support for time limited assessments"))

(defmethod initialize-knowledge progn (knowledge (assessment timelimited))
  (initialize-knowledge-value knowledge :timelimit
                              (default-timelimit assessment)))

(defmethod validate-assessment progn ((assessment timelimited))
  (when (and (strict-timelimit-p assessment)
             (not (default-timelimit assessment)))
      (error 'invalid-assessment-definition
             :reason
             "A strict timelimit is set with no actual timelimit given.")))

(defmethod initargs nconc ((obj timelimited))
  `(,@(opt :timelimit (default-timelimit obj))
    ,@(opt :strict-timelimit (strict-timelimit-p obj))))

(defmethod assessment-metadata append ((obj timelimited))
  (list (cons :timelimit (default-timelimit obj))
        (cons :strict-timelimit (strict-timelimit-p obj))))

(defgeneric timelimit(knowledge assessment)
  (:documentation "Max time allowed for the assessment or nil if none")
  (:method (knowledge assessment)
    (declare (ignore knowledge assessment)) nil)
  (:method (knowledge (assessment timelimited))
    (or (property knowledge :timelimit)
        (default-timelimit assessment))))

(defmethod assessment-attempt-p-reason or (knowledge (assessment timelimited))
  (when (strict-timelimit-p assessment)
    (let ((timelimit (timelimit knowledge assessment)))
      (when timelimit
        (let ((timetaken (timetaken knowledge)))
          (when  (> timetaken timelimit)
            (format nil "The time taken of ~D minutes exceeds the
strictly enforced timelimit of ~D minutes for this assessment."
                    timetaken timelimit)))))))

(defmethod assessment-should-not-attempt-reason
    or (knowledge (assessment timelimited))
    (let ((time-remaining (time-remaining knowledge assessment)))
      (when (and time-remaining (< time-remaining 0))
        (format nil "There are ~D minutes of time remaining. If
you submit now you will therefore have exceeded the timelimit. Further
attempts are not therefore recommended." (/ time-remaining 60)))))

(defmethod assessment-count-p-reason or (knowledge (assessment timelimited))
  (when (strict-timelimit-p assessment)
    (let ((timelimit (timelimit knowledge assessment)))
      (when timelimit
        (let ((timetaken (timetaken knowledge)))
          (when  (> timetaken timelimit)
            (format nil "The time taken of ~D minutes exceeds the
strictly enforced timelimit of ~D minutes for this assessment."
                    timetaken timelimit)))))))

(defmethod assessment-status-long append (knowledge (assessment timelimited))
  (let ((timelimit (timelimit knowledge assessment)))
    (when timelimit
      (list (cons "Time limit"
                  (format nil "~,1F minutes ~:[~; (Strictly enforced)~]"
                          (/ timelimit 60.0)
                          (strict-timelimit-p assessment)))))))

(defmethod assessment-status-short append (knowledge (assessment timelimited))
  (let ((timelimit (timelimit knowledge assessment)))
    (when timelimit
      (list (format nil "Time limit ~,1F minutes~:[~; (Strictly enforced)~]"
                    (/ timelimit 60.0)
                    (strict-timelimit-p assessment))))))

(defmethod assessment-mark :around (knowledge (assessment timelimited))
  (let ((mark (call-next-method))
        (timetaken (timetaken knowledge))
        (timelimit (timelimit knowledge assessment)))
    (if (and mark timelimit timetaken (> timetaken timelimit))
        (* mark (/ timelimit timetaken))
        mark)))

(defmethod time-remaining list (knowledge (assessment timelimited))
  (let ((timelimit (timelimit knowledge assessment))
        (started (started knowledge)))
    (when timelimit
      (if started
          (- timelimit (- (get-universal-time) (started knowledge)))
          timelimit))))