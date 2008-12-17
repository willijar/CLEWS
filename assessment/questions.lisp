;; $Id: questions.lisp,v 1.1 2006/07/31 07:13:20 willijar Exp willijar $
;; Question types for assessments
;; Copyright (C) 2003-2006 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of CLEWS: Assessments

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;; user-record is used to store state information relating to a
;; question in the user knowledge - it is a list of the user
;; submitted value followed by other values which may be needed by
;; the question such as random parameters.

;;; Code:


(in-package :clews.assessment)


(deftype user-record () 'cons)
(declaim (inline submitted-value init-data (setf init-data)
		 (setf submitted-value) make-user-record))
(defun submitted-value(record) (first record))
(defun init-data(record) (rest record))
(defun (setf init-data)  (value record) (setf (rest record) value))
(defun (setf submitted-value)(new-value record)
  (setf (first record) new-value))
(defun make-user-record(&optional init-data init-value)
  (cons init-value init-data))

;;; A question object can be used as a form element

(defgeneric make-question (id spec &optional user-record)
  (:documentation
   "Created and return a question object given a spec user-record"))

(defgeneric text(question)
  (:documentation "generates the question spec for this question"))

(defgeneric question-mark(question)
  (:documentation
   "returns the mark - a value between 0 and 1.0 for this question"))

(defgeneric markup(question)
  (:documentation "return the form element markup for this question in
a form suitable for the form handler"))

(defgeneric suffix(question)
  (:documentation "return the suffix text for this question"))

(defgeneric default(question)
  (:documentation "THe default value to be used by form
if there is no user value"))

(defgeneric weighting(item)
  (:documentation "The relative weighting to be applied to the mark from
 this item")
  (:method(anything) 1))

(defgeneric user-record(question)
  (:documentation "The record to be stored for this user to be used
later in make-question"))

(defgeneric question-feedback-markup(question)
  (:documentation "Provide the feedback relating to this question"))

(defgeneric question-mark-markup(question)
  (:documentation "Return the mark markup for questions which are
manually marked, nil otherwise")
  (:method(question) nil))

(defgeneric question-marked-p(question)
  (:documentation "Return true if qeustion has been marked")
  (:method(question)
    (declare (ignore question))
    t))

(defgeneric distance-metric(question1 question2)
  (:documentation "Return a distance between the user submissions for these
two questions"))

(defgeneric feedback(question)
  (:documentation "Provide the feedback markup component for a question"))

(defmethod make-question (name (spec list)
                          &optional (user-record (make-user-record)))
  (if (listp (first spec))
      (make-question name (eval `(lambda ,@spec)) user-record)
      (apply #'make-instance
             (cons (first spec)
                   (nconc (list :name name :user-record user-record)
                          (rest spec))))))

(defmethod make-question (id (spec function) &optional
                          (user-record (make-user-record)))
  (make-question id (funcall spec user-record) user-record))

;;; some obvious default behaviours
(defmethod question-mark (question) (declare (ignore question)) 0)
(defmethod suffix(question) (declare (ignore question)) nil)
(defmethod weighting(question) (declare (ignore question)) 1.0)
(defmethod distance-metric(q1 q2)
  (let ((c1 (submitted-value (user-record q1)))
        (c2 (submitted-value (user-record q2))))
    (if (or (null c1) (null c2) (not (equalp c1 c2))) 1 0)))

(defmethod question-feedback-markup(question)
  (let ((text (text question))
        (feedback (feedback question) )
        (mark (question-mark question)))
    `( ,(if (listp text) text (list 'P text))
      (P "Your answer: "
       ,@(element-markup (markup question)
                         (submitted-value (user-record question)) t)
       ,(suffix question))
      (p (b ,(if (= mark 0) "Wrong: " (if (= mark 1) "Correct: ")))
       " Mark " ,mark)
      ,@(if (listp feedback) feedback (list (list 'P feedback)) ))))

(defclass question()
  ((name :initarg :name :type symbol :reader name
         :documentation "Uniquely identifies the question in the assessment")
   (weighting :initarg :weighting
              :initform 1.0 :type number
              :documentation "relative weighting of this question")
   (user-record :reader user-record
                :initarg :user-record
                :initform (make-user-record)
                :documentation "User data") )
  (:documentation "base class for basic question handling"))

(defmethod is-form-element-p((element question)) t)

(defmethod default-value(question) (submitted-value (user-record question)))

(defmethod weighting((question question))
  (or (getf (init-data (user-record question)) :weighting)
      (slot-value question 'weighting)))

;;; provide the interface for the ewas-form handling

(defun randomise-set(set)
  "Returns a randomised ordering of set which is destructively modified."
  (let ((rand-set nil))
    (do ()((not set))
      (let ((q (elt set (random (length set)))))
        (push q rand-set)
        (setq set (delete q set))))
    rand-set))

(declaim (inline integer-sequence))
(defun integer-sequence(end)
  "Returns order sequence of integers 0 to n-1"
  (let ((a (make-list end )))
    (dotimes (i end a) (setf (elt a i) i))))

(defclass simple-question(question)
  ((text :initarg :question :reader text
         :documentation "Question markup to be used")
   (feedback :initform "" :initarg :feedback :reader feedback-markup
             :documentation "Text or markup to be used in feedback")
   (suffix :initform "" :initarg :suffix :reader suffix
           :documentation "Suffix e.g. units")
   (default :initarg :default :initform nil :reader default
            :documentation "The default value for this question")
   (type :initarg :type :initform nil :reader datatype
         :documentation "The data type for this question - used
to dispatch a validation /conversion function") )
  (:documentation "Simplest implementation - create and store these
attributes upon creation of the question"))

(defmethod element-markup((element simple-question)
                          &optional (value (default-value element))
                          disabled error-msg)
  (let ((text (text element)))
    `((P ,@(if (listp text) text (list text)) " "
       ,@(element-markup (markup element) value disabled) " "
       ,(suffix element)
       ,@(when error-msg `(((P :class "error") ,error-msg))) ))))

(defmethod default-value((question simple-question))
  (or (call-next-method question) (default question)))

(defclass multiple-choice-q (simple-question)
  ((answer :initarg :answer
           :documentation "The value of the correct choice")
   (choices :initarg :choices :reader choices
            :documentation "The list of choices for this question")
   (style :initarg :style :type symbol :initform nil
          :documentation "Presentation style for MCQ")
   (randomise :initarg :randomise :initform t
              :documentation "If true randomise order of answers")
   (allow-no-answer :initarg :allow-no-answer :initform t
                    :documentation "If true don't know allowed as an answer"))
  (:documentation "Multiple choice Question"))

(declaim (inline choices choice-value choice-values choice-markup
                 number-choices))

(defun choice-value(item) (if (listp item) (car item) item))
(defun choice-values(q) (mapcar #'choice-value (choices q)))
(defun choice-markup(item) (if (listp item) (rest item) item))
(defun number-choices(q) (length (choices q)))

(defmethod datatype((q multiple-choice-q))
  (let ((answer (slot-value q 'answer)))
    (list 'member :set (cons nil (choice-values q)) :type
          (list
           (cond ((integerp answer) 'integer)
                 ((numberp answer) 'number)
                 ((symbolp answer) 'symbol)
                 ((stringp answer) 'string))
           :nil-allowed (slot-value q 'allow-no-answer)))))

(defmethod markup((question multiple-choice-q))
  (with-slots(style randomise allow-no-answer) question
    (let ((choices (choices question)))
      `((mcq ,@(when style (list :style style)) :name ,(name question))
        ,@(when allow-no-answer '((NIL . "Don't know")))
        ,@(if randomise (randomise-set (copy-list choices)) choices)))))

(defmethod initialize-instance :after ((question multiple-choice-q)
                                       &key
                                       &allow-other-keys)
  (with-slots(default allow-no-answer) question
   (unless allow-no-answer
     (setf default (choice-value (first (choices question)))))))

(defmethod weighting((question multiple-choice-q))
  (* (call-next-method question) (/ (number-choices question) 5)))

(defmethod question-mark((question multiple-choice-q))
  "negative marking with no negative marks!! 0, (1+1/n)*1/n, 1"
  (let ((value (submitted-value (user-record question))))
    (cond ((equal value (slot-value question 'answer))
           1)
          ((equal value nil)
           (let ((n (/ 1 (number-choices question))))
             (* n (1+ n))))
          (t 0))))

(defmethod feedback ((question multiple-choice-q))
  (let ((feedback (slot-value question 'feedback)))
    `((p "The correct answer was \""
       ,(choice-markup (find (slot-value question 'answer) (choices question)
                             :test #'equal
                             :key #'choice-value))
       "\"")
      ,@(if (listp feedback) feedback (list (list 'P feedback))))))

(defclass multiple-answer-q (multiple-choice-q)
  ()
  (:documentation "Multiple Answer Question"))

(defmethod datatype((q multiple-answer-q))
  (let ((answer (or (first (slot-value q 'answer))
                    (first (choice-values q)) )))
    (jarw.debug:debug-log
     "datatype=~S~%"
     (list 'list
           :max-length (length (choice-values q))
           :type (list 'member
                       :set (choice-values q)
                       :type
                       (list
                        (cond ((integerp answer) 'integer)
                              ((numberp answer) 'number)
                              ((symbolp answer) 'symbol)
                              ((stringp answer) 'string))
                        :nil-allowed t))))
    (list 'list
          :max-length (length (choice-values q))
          :type (list 'member
                      :set (choice-values q)
                      :type
                      (list
                       (cond ((integerp answer) 'integer)
                             ((numberp answer) 'number)
                             ((symbolp answer) 'symbol)
                             ((stringp answer) 'string))
                       :nil-allowed t)))))

(defmethod markup((question multiple-answer-q))
  (with-slots(style randomise) question
    (let ((choices (choices question)))
    `((maq ,@(when style (list :style style)) :name ,(name question))
      ,@(if randomise (randomise-set (copy-list choices)) choices)))))

(defmethod weighting((question multiple-answer-q))
  (* (call-next-method question) (/ (number-choices question) 5)))

(defmethod question-mark((question multiple-answer-q))
  (let* ((submitted (submitted-value (user-record question)))
         (answers (slot-value question 'answer))
         (n (number-choices question))
         (nc 0))
    (dolist (choice (choice-values question) (/ nc n))
      (let ((is-answer (null (member choice answers :test #'equal)))
            (is-chosen (null (member choice submitted :test #'equal))))
        (when (eq is-answer is-chosen) (incf nc))))
    #+nil(if submitted (/ (- n (+ incorrect-chosen incorrect-not-chosen)) n) 0)
    ))

(defmethod feedback ((question multiple-answer-q))
  (let ((feedback (slot-value question 'feedback))
        (answers (slot-value question 'answer)))
    `((p ,(format nil "The correct selections were - ~S."
                  (or (mapcan #'(lambda(choice)
                                  (when (find (choice-value choice)
                                              answers :test #'equal)
                                    (list (choice-markup choice))))
                              (choices question)) "None of them")))
      ,@(if (listp feedback) feedback (list (list 'P feedback))))))

(defclass numeric-q(simple-question)
  ((answer :initarg :answer)
   (tol :initarg :tol :type number :initform 0.01)
   (fmt :initarg :format :type string :initform "~,3G"))
  (:default-initargs :type 'number :default 0)
  (:documentation "A numeric question"))


(defmethod markup((question numeric-q))
  `((input
     :name ,(name question)
     :align :right
     :size
     ,(+ 8 (round (abs (log (slot-value question 'tol) 10)))))))

(defmethod  question-mark((question numeric-q))
  (let* ((a  (submitted-value (user-record question)))
         (c  (slot-value question 'answer)))
    (if (and a
             (or (= a c)
                 (and (/= 0 c)
                      (< (abs (/ (- a c) c)) (slot-value question 'tol)))))
        1 0 )))

(defmethod feedback ((question numeric-q))
  `((p ,(format nil "The correct answer is ~@? "
                (slot-value question 'fmt)
                (slot-value question 'answer))
     ,(suffix question))
    ,@(let ((f (slot-value question 'feedback)))
           (if (listp f) f (list (list 'P f))))))

(defmacro def-dynamic-question((&rest args) &body body)
  "Macro creates an environment for a dynamic question. arguments
  is a list of variable definitions and initialisation values. It returns a
  function definition containing the body. "
  (labels ((varname(arg) (car arg))
           (fieldname(arg) (intern (string (varname arg)) :keyword))
           (value(arg) (second arg)))
    (let ((g-param (gensym))
          (g-user-data (gensym)))
      `(lambda(,g-user-data)
        (let ((,g-param (rest ,g-user-data)))
          (let* ,(mapcar
                  #'(lambda(arg) `(,(varname arg)
                                   (or (getf ,g-param ,(fieldname arg))
                                    (setf (getf ,g-param ,(fieldname arg))
                                     ,(value arg))) ))
                  args)
            (setf (rest ,g-user-data) ,g-param)
            ,@body))))))

(defclass written-q(question)
  ((text :initarg :question :reader text
         :documentation "Question markup to be used")
   (answer :initarg :answer :reader answer :initform nil
           :documentation "A model answer")
   (min-word-count :initarg :min-word-count
                   :reader min-word-count :initform nil
                   :documentation "Minimum word count required in answer")
   (max-word-count :initarg :max-word-count
                   :reader max-word-count :initform nil
                   :documentation "Maximum word count required in answer")
   (question-mark-markup
    :initarg :question-mark-markup
    :initform
    `((p (b "Mark: ")
       ((input :name :mark :value 0 :size 3 :datatype (integer :min 0 :max 20))) "/20")
      (p (b "Comment: ") ((textarea :rows 5 :cols 50 :name :comment)))))
   (rows :initarg :rows :initform 5 :reader rows)
   (cols :initarg :cols :initform 80 :reader cols))
  (:documentation "A written answer type question, manually marked
using the mark-form"))

(defmethod datatype((q written-q))
  `(string
    :word-count ,(min-word-count q)
    :max-word-count ,(max-word-count q)))

(defmethod question-mark-markup ((question written-q))
  "Markup put in markers form"
  (let ((prefix (string-upcase (name question))))
    (rest
     (substitute-markup-when
      #'(lambda(e) (and (is-form-element-p e)
                        (and (listp e) (listp (car e))
                             (not (eql :text (getf (cdar e) :disabled))))))
      (cons 'div (slot-value question 'question-mark-markup))
      #'(lambda(element)
          (multiple-value-bind(tag attr content)
              (split-markup element)
            (let ((new-attr (copy-list attr)))
              (setf (getf new-attr :name)
                    (intern (concatenate 'string prefix "."
                                         (string-upcase (getf attr :name)))
                            :keyword))
              (join-markup tag new-attr content))))
      :as-list nil))))

(defmethod element-markup((element written-q)
                          &optional (value (default-value element))
                          disabled error-msg)
  (let ((text (text element)))
    `(,@(if (listp text) text (list (list 'p text)))
      ,@(element-markup
         `((textarea
            :name ,(name element)
            :rows ,(rows element)
            :cols ,(cols element))) value disabled)
      ,@(when error-msg `(((P :class "error") ,error-msg))) )))

(defmethod question-feedback-markup ((question written-q))
  (let ((answer (answer question) )
        (feedback (init-data (user-record question))))
    `((div
       ,@(element-markup question
                         (submitted-value (user-record question)) :text))
      (div
       ,@(when answer
               `((p (b "Model Answer"))
                 ,(if (listp answer) answer (list 'P answer)))))
      (div
       (p (b "Tutors Feedback"))
       ,(markup-form
         `(div ,@(question-mark-markup question)) feedback :text)))))

(defmethod question-mark((question written-q))
  (or (form-mark (init-data (user-record question))
                 `(form ,@(question-mark-markup question)))
      0))

(defmethod question-marked-p((question written-q))
  (when (form-mark (init-data (user-record question))
                   `(form ,@(question-mark-markup question))) t))