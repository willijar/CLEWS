;; $Id: questionnaire.lisp,v 1.4 2007/02/11 13:21:58 willijar Exp willijar $
;; Questionnaire type assessments
;; Copyright (C) 2003-2006 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of CLEWS: Assessments

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;; In most of this interface we use knowledge to represent the users
;; knowledge and assessment to represent the assessment object.
;; In the default methods both of these are objects with properies
;; knowledge is a property subset, usually for a user,
;; containing their state information - it is modified by many methods here
;; Assessments have properties and a set of question specs
;; The properties control how the questions are allocated/controlled etc
;;
;; The assessments have the following properties
;; :questions          - an a- list of question specifications
;; :randomise-questions - if true questions are randomly allocated
;; :set-no-questions    - the number of questions a user has to complete
;; :deadline            - universal time for deadline
;; :feedback-date       - universal time for feeback
;; :no-questions-counted - may be less than set number
;;
;; Users knowledge had the following properties
;; :answers             - a a-list of the users answers and other state info
;;                        relating to the users assessment
;; :deadline            - universal time for deadline - overides assessment
;; :feedback-date       - universal time for feeback - overides assessment

;;; Code:

(in-package :clews.assessment)

(defclass questionnaire-base()
  ((questions-init :initarg :questions :initform nil
                   :documentation "List of the questional
initialisation data for saving later.")
   (questions :reader question-specifications
              :documentation "List of the processed question
specifications in this questionnaire")
   (randomise-questions
    :initform t :initarg :randomise-questions
    :reader randomise-questions-p
    :documentation "If true question selection and order will be randomised")
   (set-no-questions
    :initform nil :initarg :set-no-questions
    :documentation "No of questions to be set")
   (no-questions-counted
    :initform nil :initarg :no-questions-counted
    :documentation "Number of questions to be counted out of the set") )
  (:documentation "base class for questionnaires"))

(defclass questionnaire(scheduled timelimited questionnaire-base)
  ()
  (:documentation "Actual class for questionnaire assessments"))

(defmethod initargs nconc ((obj questionnaire-base))
  `(:questions ,(slot-value obj 'questions-init)
    ,@(opt :randomise-questions (randomise-questions-p obj))
    ,@(opt :set-no-questions (slot-value obj 'set-no-questions))
    ,@(opt :no-questions-counted (slot-value obj 'no-questions-counted))))

(defmethod assessment-marked-p(knowledge (questionnaire questionnaire-base))
  "Check to ensure it has been marked - if appropriate"
  (let ((questions (questions knowledge questionnaire)))
    (when questions (every #'question-marked-p questions))))

(defmethod initialize-instance :after ((questionnaire questionnaire-base)
                                       &key questions)
  (init-question-specifications questionnaire questions))

(defmethod validate-assessment progn ((questionnaire questionnaire))
  (let* ((questions  (question-specifications questionnaire))
         (len (length questions)))
    (when (<  len 1)
      (error 'invalid-assessment-definition
             :reason "There must be at least 1 question in a questionnaire"))
    (with-slots(set-no-questions no-questions-counted) questionnaire
      (when (and set-no-questions (> set-no-questions len))
        (error 'invalid-assessment-definition
               :reason `("The ~D questions set is more than the ~D available."
                         ,set-no-questions ,len)))
      (when (and no-questions-counted
                 (> no-questions-counted set-no-questions))
        (error 'invalid-assessment-definition
               :reason `("The ~D questions counted is more than the ~D set."
                         ,no-questions-counted ,set-no-questions))))))

(defgeneric init-question-specifications(questionnaire definitions)
  (:documentation "Initialise question specifications from their definitions")
  (:method((questionnaire questionnaire-base) questions)
    "Compile question functions"
    (let ((*compile-print* nil))
      (setf
       (slot-value questionnaire 'questions)
       (mapcar
        #'(lambda(qspec)
            (cond
              ((functionp (cdr qspec))
               (cons (car qspec) (compile nil (cdr qspec) )))
              ((listp (second qspec))
               (let ((args (second qspec))
                     (body (cddr qspec))
                     (datavar (gensym))
                     (initdata (gensym)))
                 (cons
                  (car qspec)
                  (compile
                   nil
                   (eval
                    `(lambda(,initdata)
                      (let* ((,datavar (init-data ,initdata))
                             ,@(mapcar
                                #'(lambda(arg)
                                    (let* ((name (car arg))
                                           (key (intern (string-upcase name) :keyword))
                                           (form (cdr arg)))
                                      `(,name (or (getf ,datavar ,key)
                                               (setf (getf ,datavar ,key)
                                                ,@form)))))
                                args))
                        (setf (init-data ,initdata) ,datavar)
                        ,@body)))))))
              (t  qspec)))
        questions)))))

(defmethod assessment-metadata append ((obj questionnaire-base))
   (list (cons :set-no-questions (slot-value obj 'set-no-questions))
         (cons :no-questions (length (question-specifications obj)))
         (cons :randomise-questions (randomise-questions-p obj))
         (cons :no-questions-counted (slot-value obj 'no-questions-counted))))

(defun default-set-no-questions(questionnaire)
  (or (slot-value questionnaire 'set-no-questions)
      (length (question-specifications questionnaire))))

(defun default-no-questions-counted(questionnaire)
  (or (slot-value questionnaire 'no-questions-counted)
      (default-set-no-questions questionnaire)))

(defun set-no-questions(knowledge questionnaire)
  (let ((a (property knowledge :answers)))
    (cond ((first a) (length a))
          ((default-set-no-questions questionnaire)))))

(defun no-questions-counted(knowledge questionnaire)
  (or (property knowledge :no-questions-counted)
      (set-no-questions knowledge questionnaire)))

(defun allocate-question-specifications(knowledge questionnaire)
  "allocates new question specifications
from an questionnaire, possibly on the basis of knowledge"
  (subseq (if (randomise-questions-p questionnaire)
              (randomise-set (copy-list
                              (question-specifications questionnaire)))
              (question-specifications questionnaire))
          0 (set-no-questions knowledge questionnaire)))

(defmethod initialize-knowledge
    progn (knowledge (questionnaire questionnaire-base))
  "the knowledge record for this questionnaire"
  (let ((answers (property-subset knowledge :answers)))
    (dolist (spec (allocate-question-specifications knowledge questionnaire))
      (let ((id (first spec)))
        (setf (property answers id)
              (user-record (make-question id (rest spec)))))))
  (initialize-knowledge-value knowledge :no-questions-counted
                              (default-no-questions-counted questionnaire)))

(defun questions(knowledge questionnaire)
  "return the list of questions objects for this user and
questionnaire, allocating them if need be"
  (mapcan
   #'(lambda(answer)
       (let* ((id (first answer))
              (spec (assoc id (question-specifications questionnaire))))
         (when spec
           (list (make-question id (rest spec) (rest answer))))))
   (property knowledge :answers)))

#+nil(defun answers (knowledge questions)
  (mapcan #'(lambda(question)
	      (list (name question) (submitted-value question)))
	  questions))

(defun (setf answers) (form-data knowledge questions)
  (let ((answers (property-subset knowledge :answers)))
    (dolist (question questions)
      (setf (property answers (name question))
            (make-user-record (init-data (property answers (name question)))
                              (getf form-data (name question))))))
  (setf (completed knowledge) (get-universal-time)) )

(defun (setf tutor-marks) (form-data questions)
  (dolist (question questions)
    (let* ((prefix (string-upcase (name question)))
           (end (length prefix)))
      (loop
       :for a :on form-data :by #'cddr
       :do (let ((name (string (car a))))
             (when (and (> (length name) end)
                        (string-equal prefix name :start2 0 :end2 end))
               (setf (getf (init-data (user-record question)) (car a))
                     (cadr a))))))))

(defmethod assessment-status-long append (knowledge
					 (questionnaire questionnaire-base))
  (list (cons "No Questions" (set-no-questions knowledge questionnaire))))

(defmethod assessment-status-short append (knowledge
                                           (questionnaire questionnaire-base))
  (list (format nil
                "~D questions" (set-no-questions knowledge questionnaire))))

(defmethod assessment-mark(knowledge (questionnaire questionnaire-base))
  "return users mark for this questionnaire and as a second value whether it
   is to be counted or not (i.e. withing deadline etc)"
  (let* ((questions (questions knowledge questionnaire))
         (w (reduce #'+ (mapcar #'weighting questions))))
    (if w
        (/ (reduce #'+
                   (mapcar #'(lambda(q)
                               (* (weighting q) (question-mark q)))
                           questions))
           w)
        0) ))

(defmethod assessment-distance-metric(knowledge1 knowledge2
                                      (questionnaire questionnaire-base))
  (when (and knowledge1 knowledge2)
    (let ((questions1 (questions knowledge1 questionnaire))
          (questions2 (questions knowledge1 questionnaire)))
      (reduce #'+
              (mapcan #'(lambda(q1)
                          (let ((q2 (find (name q1) questions2 :key #'name)))
                            (when q2 (list (distance-metric q1 q2)))))
                      questions1)))))

(defgeneric make-assessment-form(knowledge assessment)
  (:documentation "Create the markup form for an assessment")
  (:method (knowledge (questionnaire questionnaire-base))
    (make-instance 'form
                   :elements (questions knowledge questionnaire)
                   :form-attrs (list :method "POST"))))

(defmethod assessment-attempt-markup(knowledge
                                     (questionnaire questionnaire-base)
                                     request)
  (let* ((questions (questions knowledge questionnaire))
         (form (make-assessment-form knowledge questionnaire)))
    (if (and request (form-values "Submit" request))
        (progn                          ; submitted
          (multiple-value-bind(form-data condition) (form-data form request)
            (if condition
                `((P :class "error")    ; unsuccessful
                  "Your submission is NOT accepted.
Please correct the errors below."
                  ,(markup-form form form-data))
                (progn                  ; successful
                  (setf (answers knowledge questions) form-data)
                  (setf (completed knowledge) (get-universal-time))
                  `(div
                    (P "Your submission as follows has been successfully
received.")
                    ,(markup-form form form-data t)) ))))
        (markup-form form))))

(defmethod assessment-feedback-markup(knowledge
				      (questionnaire questionnaire-base)
				      request)
  (declare (ignore request))
  `(OL
    ,@(mapcar #'(lambda(q) `(li ,@(question-feedback-markup q)))
	      (questions knowledge questionnaire))))

(defmethod assessment-mark-markup
    (knowledge (questionnaire questionnaire-base) request)
  "If this assessment has manually marked questions return the mark form"
  (let* ((questions (questions knowledge questionnaire))
         (form
          `((form :method :post)
            (OL
             ,@(mapcar
                #'(lambda(question)
                    `(li
                      (div ,@(question-feedback-markup question))
                      (div ,@(question-mark-markup question))))
                questions))
            ((input :type :submit :name :submit :value "Submit Marks")))))
    (if (form-values :submit request)
        (multiple-value-bind(form-data condition) (form-data form request)
          (if condition
              `(div
                ((P :class "error")     ; unsuccessful
                 "Your submission is NOT accepted.
Please correct the errors below.")
                ,(markup-form form form-data))
              (progn                    ; successful
                (setf (tutor-marks questions) form-data)
                `(div
                  (P "Your submission as follows has been successfully
received.")
                  ,(markup-form form form-data :text)) )))
        (markup-form form
                     (mapcan #'(lambda(q)
                                 (copy-list (init-data (user-record q))))
                             questions)))))

(defun collate-question-marks(knowledge-set questionnaire)
  "Given a knowledge set return the marks by question for all and for
counted-only sets"
  (let ((all '()) (counted-only '()))
    ;; collate information by question id
    (dolist (k knowledge-set)
      (let ((counted-p (assessment-count-p k questionnaire)))
        (dolist (question  (questions k questionnaire))
          (let ((id (name question))
                (m (question-mark question)))
            (when m
              (push m (getf all id))
              (when counted-p (push m (getf counted-only id))))))))
    (values all counted-only)))

(defun normalised-weighting(marks)
  "Return the normalised numeric weighting for a question given a
set of marks for it"
  (- 1.0 (mean marks)))

(defmethod assessment-detail-statistics
    (knowledge-set (questionnaire questionnaire-base)
     &optional (normalisation-set knowledge-set))
  "Return stats by question"
  (labels ((%fmt(num) (format nil "~,1F%" (* 100 num)))
           (stats(marks)
             (when marks
               `(((td :align :right) ,(%fmt (mean marks)))
                 ((td :align :right) ,(%fmt (stddev marks)))))))
    (multiple-value-bind(all counted-only)
        (collate-question-marks knowledge-set questionnaire)
      (let ((normalisation (collate-question-marks normalisation-set
                                                   questionnaire)))
        `((table :border 1)
          (caption "Table of detailed statistics for questionnaire")
          ((tr :class :header) (th "Question id") (th "Type")
           ((th :colspan 2) "Submissions")
           ((th :colspan 2) "Counted Mark") ((th :colspan 2) "Total Mark")
           ((th :colspan 2) "Weighting"))
          ((tr :class :header)
           (th) (th) (th "Total") (th "Counted") (th "Avg ")
           (th "Std") (th "Avg ") (th "Std") (th "Preset") (th "Normalised"))
          ,@(mapcar
             #'(lambda(spec)
                 (let* ((id (first spec))
                        (q (make-question id (rest spec))))
                   `(tr (td ,(string id))
                     (td ,(string (type-of q)))
                     ((td :align :right) ,(length (getf all id)))
                     ((td :align :right) ,(length (getf counted-only id)))
                     ,@(stats  (getf all id))
                     ,@(stats (getf counted-only id))
                     ((td :align :right),(weighting q))
                     ((td :align :right)
                      ,(format nil "~,2F"
                               (normalised-weighting
                                (getf normalisation id)))))))
             (question-specifications questionnaire)))))))

(defmethod assessment-normalised-marks
    (knowledge-set (questionnaire questionnaire-base)
     &key normalisation-set (counted-only t) )
  "return users mark for this questionnaire and as a second value whether it
   is to be counted or not (i.e. withing deadline etc)"
  (let ((question-weights
         (mapcar #'(lambda(item)
                     (if (listp item) (normalised-weighting item) item))
                 (collate-question-marks normalisation-set questionnaire))))
    (flet ((weighting(q) (getf question-weights (name q) 1.0)))
      (mapcar
       #'(lambda(k)
           (when (or (not counted-only) (assessment-count-p k questionnaire))
             (let* ((questions (questions k questionnaire)))
               (when questions
                 (let ((wsum (reduce #'+ (mapcar #'weighting questions))))
                   (if (= wsum 0)
                       0
                       (/ (reduce
                           #'+ (mapcar
                                #'(lambda(q)
                                    (* (weighting q) (question-mark q)))
                                questions))
                          wsum)))))))
       knowledge-set))))

(defclass questionnaire-stub(questionnaire)
  ()
  (:documentation "A questionnaire without the actual questions"))

(defmethod initialize-instance :after ((q questionnaire-stub)
                                       &key &allow-other-keys)
  (slot-makunbound q 'questions))

(defmethod init-question-specifications((q questionnaire-stub) defs)
  (declare (ignore defs))
  (values))

(defgeneric assessment-stub(assessment)
  (:documentation "Return an assessment metadata stub sufficient for
presenting status information for users.")
  (:method ((q questionnaire))
    (let ((args (initargs q)))
      (remf args :questions)
      (setf (getf args :set-no-questions) (default-set-no-questions q))
      (apply #'make-instance (cons 'questionnaire-stub args)))))

(defmethod weighting((questionnaire questionnaire))
  (reduce #'+ (mapcar #'weighting (question-specifications questionnaire))))