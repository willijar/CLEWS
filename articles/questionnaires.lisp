;; $Id: questionnaires.lisp,v 1.2 2007/07/26 08:54:58 willijar Exp willijar $
;; Questionnaire directives for Restructured Text
;; Copyright (C) 2006 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of CLEWS Articles Library

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;; Provide directives to enable embedding of questionnaires in articles
;; using restructured text.

;;; Code:

(defpackage :docutils.assessment
  (:documentation "Assessments written in restructured text")
  (:use :cl :docutils :docutils.parser.rst)
  (:import-from :clews.assessment
                #:question-specifications #:make-assessment-form
                #:make-question #:markup #:default-value #:text #:suffix
                #:feedback #:question-mark #:submitted-value #:user-record
                #:element-markup #:question-feedback-markup
                #:init-question-specifications #:name
                #:assessment-feedback-p #:assessment-attempt-p
                #:assessment-should-not-attempt-reason
                #:assessment-status-table
                #:started #:timelimit)
  (:import-from :jarw.parse #:date)
  (:import-from :docutils #:with-children #:child #:add-child #:number-children
                #:node #:compound #:copy-of-node #:move-children #:as-text
                #:strip)
  (:import-from :docutils.parser.rst #:def-directive #:*directives*
                #:&allow-spaces #:&option #:&content-parser)
  (:import-from :markup #:div #:p #:rst #:b)
  (:export #:questionnaire #:question #:multiple-choice-question
           #:multiple-answer-question #:assessments
           #:numeric-question #:written-question #:*questionnaire-directives*))

(in-package :docutils.assessment)

(defclass questionnaire(compound clews.assessment:questionnaire)
  ((name :type symbol :initarg :name :reader name))
  (:documentation "A questionnaire node - can be used to create an
assessment questionnaire later"))

(defmethod clews.assessment::assessment-stub((q questionnaire))
    (change-class (call-next-method) 'questionnaire :name (name q)))

;; question specifications are the children of the questionnaire node

(defclass question-specification(compound)
  ((name :type symbol :initarg :name :reader name)
   (question-type :type symbol :initarg :type
                  :documentation "Question type to be made")
   (initargs :type list :initform nil :initarg :args
             :documentation "Initialisation arguments for the question")
   (vars :type list :initarg :params :initform nil
         :documentation "List of user specific variables")
   (factory :type function
            :documentation "Function which when called with user data
returns newly created question"))
  (:documentation "A question specification node with a name and a function to
construct the question"))

(defmethod question-specifications((questionnaire questionnaire))
  (let ((specs nil))
    (with-children(child questionnaire)
      (when (typep  child 'question-specification)
        (push (cons (name child) child)
              specs)))
    (nreverse specs)))

(defun copy-eval(node)
  (let ((copy (copy-of-node node)))
    (setf (slot-value copy 'docutils:parent)
          (slot-value node 'docutils:parent))
    (docutils.transform:evaluate copy)))

(defmethod factory((spec question-specification))
  (unless (slot-boundp spec 'factory)
    (with-slots(question-type initargs vars factory name) spec
      (setf
       factory
       (labels ((varname(arg) (car arg))
                (fieldname(arg) (intern (string (varname arg)) :keyword))
                (value(arg) (second arg)))
         (let ((g-param (gensym))
               (g-user-data (gensym)))
           `(progn
              ,@(mapcar #'(lambda(name) `(defvar ,(varname name))) vars)
              (lambda(,g-user-data)
                (let ((,g-param (rest ,g-user-data)))
                  (let* ,(mapcar
                          #'(lambda(arg)
                              `(,(varname arg)
                                 ;; varnames starting with _ are not stored in
                                 ;; user data
                                 ,(if (eql (char (string (varname arg)) 0) #\_)
                                      (value arg)
                                      `(or
                                        (getf ,g-param ,(fieldname arg))
                                        (setf (getf ,g-param ,(fieldname arg))
                                              ,(value arg))) )))
                          vars)
                    (declare (special ,@(mapcar #'varname vars)))
                    (setf (rest ,g-user-data) ,g-param)
                    (list
                     ',question-type
                     :name ,name
                     :user-record ,g-user-data
                     ,@(loop :for a :on initargs :by #'cddr
                          :nconc (list (car a)
                                       (if (typep (cadr a) 'node)
                                           `(copy-eval ,(cadr a))
                                           `(eval ',(cadr a)))))))))))))))
    (slot-value spec 'factory))

(defmethod allowed-child-p((q questionnaire) child &optional index)
  ;; questionnaire can have nodes as per itsd base class then a set of
  ;; question specifications.
  (unless index (setf index (number-children q)))
  (or (typep child 'question-specification)
      (typep child 'docutils.nodes::system-message)
      (and (or (= 0 index)
               (not (typep (child q (1- index))
                           'question-specification)))
           (call-next-method))))

(defmethod make-question(id (spec question-specification)
                         &optional (user-record
                                    (clews.assessment::make-user-record)))
    (make-question id (eval (factory spec))  user-record))

(defvar *questionnaire-directives*
  (make-instance 'dictionary:shadow-dictionary
                 :shadowing *directives*
                 :dictionary (make-hash-table :test #'equalp)))

(def-directive questionnaire
    (parent name
            &allow-spaces
            &option
            description
            (start-date (date :nil-allowed t))
            (end-date (date :nil-allowed t))
            (deadline-date (date :nil-allowed t))
            (feedback-date(date :nil-allowed t))
            (strict-deadline boolean nil)
            (multiple-attempt symbol nil)
            (timelimit (integer :nil-allowed t) nil)
            (strict-timelimit boolean nil)
            (randomise-questions boolean t)
            (set-no-questions (integer :nil-allowed t) nil)
            (no-questions-counted (integer :nil-allowed t) nil)
            &content-parser parser)
  (let ((*directives* *questionnaire-directives*)
        (questionnaire
         (make-instance 'questionnaire
                        :name (intern name :keyword)
                        :description description
                        :start-date start-date
                        :end-date end-date
                        :deadline-date deadline-date
                        :feedback-date feedback-date
                        :strict-deadline strict-deadline
                        :multiple-attempt multiple-attempt
                        :timelimit timelimit
                        :strict-timelimit strict-timelimit
                        :randomise-questions randomise-questions
                        :set-no-questions set-no-questions
                        :no-questions-counted no-questions-counted)))
    (add-child parent questionnaire)
    (funcall parser questionnaire)))

(defclass rst-question(clews.assessment:simple-question)
  ()
  (:documentation "Version of question using rst for text and feedback"))

(defmethod element-markup((element rst-question)
                          &optional (value (default-value element))
                          disabled error-msg)
  (let ((text (text element)))
    `(((div :class "question")
       ,text
       ,@(element-markup (markup element) value disabled) " "
       ,(suffix element)
       ,@(when error-msg `(((p :class "error") ,error-msg))) ))))

(defmethod question-feedback-markup((question rst-question))
  (let ((text (text question))
        (feedback (feedback question))
        (mark (question-mark question)))
   `(((div :class "feedback")
       ,text
       (div
       "Your answer: "
        ,@(element-markup (markup question)
                          (submitted-value
                           (user-record question))
                          t)
       ,(suffix question))
      (p (b ,(if (= mark 0) "Wrong: " (if (= mark 1) "Correct: ")))
         " Mark " ,mark)
       (div ,@feedback)))))

(defun make-value-compound(argname field parent)
  ;; note the field is modified by this method
  (let ((node (make-instance
               'compound
               :attributes (list :class (string argname))
               :line (docutils:line (child field 1)))))
    (when parent (add-child parent node))
    (docutils::move-children (child field 1) node)
    node))

(defun decode-question-args(parent parser
                            &key id type params formatted unformatted
                            argsfilter)
  "Return containers with field-body nodes as values corresponding to argspec"
  (let ((results nil)
        (fieldlist (make-instance 'docutils.nodes:field-list))
        (spec (make-instance
                   'question-specification
                   :name
                   (intern (docutils.utilities:normalise-name id) :keyword)
                   :type type
                   :params params)))
    (add-child parent spec)
    (add-child spec fieldlist)
    (funcall parser fieldlist
             :states '(docutils.parser.rst::field-list)
             :initial-state 'docutils.parser.rst::field-list)
    (with-children(field fieldlist)
      (let ((argname (intern (string-upcase (strip (as-text (child field 0)))) :keyword)))
        (cond
          ((member argname formatted)
           (setf (getf results argname)
                 (make-value-compound argname field spec)))
          ((member argname unformatted)
           (let ((value  (child field 1)))
             (if (and (= 1 (number-children value))
                      (typep (child value 0) 'docutils.nodes:paragraph))
                 (setf (getf results argname)
                       (let ((*package* (find-package :markup)))
                         (read-from-string (as-text value))))
                 (docutils:report :error `("Question option ~A
 may contain a single paragraph only." ,argname)))))
          (t (report
              :error
              `("~A is not a recognised option for this question type: Allowed options are ~S"
                ,argname ,(append formatted unformatted)))))))
    (when argsfilter (funcall argsfilter results))
    (setf (slot-value spec 'initargs) results)
    (values fieldlist results)))

(defclass numeric-q(rst-question clews.assessment:numeric-q)
  ())

(defmethod feedback ((question numeric-q))
  `((p ,(format nil "The correct answer is ~@? "
                (slot-value question 'clews.assessment::fmt)
                (slot-value question 'clews.assessment::answer))
       ,(suffix question))
    (div
    ,(slot-value question 'clews.assessment::feedback))))

(defclass multiple-choice-q(rst-question clews.assessment:multiple-choice-q)
  ()
  (:documentation "An RST multiple choice question"))

(defmethod clews.assessment::choices((question multiple-choice-q))
  (with-slots(clews.assessment::choices) question
    (if (listp clews.assessment::choices)
        clews.assessment::choices
        (setf clews.assessment::choices
              (let ((results nil))
                (with-children(field (slot-value question 'clews.assessment::choices))
                  (push (cons (read-from-string (as-text (child field 0)))
                              (make-value-compound "choice" field nil))
                        results))
                (nreverse results))))))

(defmethod feedback ((question multiple-choice-q))
  `((p "The correct answer was: ")
    ,(cdr (find (slot-value question 'clews.assessment::answer)
                (clews.assessment::choices question)
                :test #'equal
                :key #'car))
    ,(slot-value question 'clews.assessment::feedback)))

(defclass multiple-answer-q(multiple-choice-q clews.assessment:multiple-answer-q)
  ()
  (:documentation "An RST multiple answer question"))

(defmethod feedback ((question multiple-answer-q))
  `((p "The correct selections are: ")
    (blockquote
    ,@(or (mapcan
           #'(lambda(choice)
               (when (find (car choice)
                           (slot-value question 'clews.assessment::answer)
                           :test #'equal)
                 (list (cdr choice))))
           (clews.assessment::choices question))
          (list "None of them")))
    ,(slot-value question 'clews.assessment::feedback)))

(defclass compound-q(clews.assessment:compound-q rst-question)
  ()
  (:documentation "An RST multiple choice question"))

(defclass question-input(docutils.nodes::evaluate)
  ()
  (:documentation "An node representing user input"))

(docutils.parser.rst::def-role input(text)
  (handler-case
      (make-instance
       'question-input
       :expr (let ((res nil)
                   (p 0))
               (loop
                  (multiple-value-bind(s np)
                      (read-from-string text nil '_end :start p)
                    (when(eql s '_end) (return (nreverse res)))
                    (setf p np)
                    (push s res)))))
    (error(e)
      (make-node 'docutils.nodes::problematic
                 (write-to-string e :escape nil :readably nil )))))

(defmethod docutils.transform:evaluate((node question-input))
  (unless (slot-boundp node 'docutils::result)
    (setf (slot-value node 'docutils::result) nil)
    (handler-case
        (loop
           :for a :on (slot-value node 'docutils::expr) :by #'cddr
           :do
           (setf (getf (slot-value node 'docutils::result) (first a))
                 (eval (second a))))
      (error(e)
        (docutils:report
         :warn (write-to-string e :escape nil :readably nil ) ))))
  (slot-value node 'docutils::result))


(defmethod clews.assessment::parts((q compound-q))
  (mapcar
   #'docutils.transform:evaluate
   (docutils:collate-nodes(node (text q)) (typep node 'question-input))))

(defmethod visit-node((writer docutils.writer.html:html-writer)
                      (node question-input))
  (let ((result (docutils.transform::evaluate node)))
    (part-append
     (markup:html
      nil
      `((markup:input :name ,(getf result :name)
                      :datatype ,(getf result :type)
                      ,@(when (getf result :disabled) `(:disabled t))
                      :value ,(getf result :value)))))))

(defmethod visit-node ((writer docutils.writer.latex::latex-writer)
                       (node question-input))
  (let ((result (docutils.transform::evaluate node)))
    (part-append
     (markup:latex
      nil
      `((markup:input :name ,(getf result :name)
                      :datatype ,(getf result :type)
                      ,@(when (getf result :disabled) `(:disabled t))
                      :value ,(getf result :value)))))))

(defmethod element-markup((element compound-q)
                          &optional (value (default-value element))
                          disabled error-msg)
  (setq *tmp* element)
  (let ((text  (copy-of-node (text element))))
    (setf (slot-value text 'docutils:parent)
          (slot-value (text element) 'docutils:parent))
  (map 'nil
       #'(lambda(part value)
           (with-slots((result docutils::result)) part
               (setf (getf result :name) (name element))
               (setf (getf result :value) value)
               (when disabled (setf (getf result :disabled) t))))
       (docutils:collate-nodes(node text)
                              (typep node 'question-input))
       value)
  `(div
    ,@(when error-msg `(((p :class :error) ,error-msg)))
    ,text)))

(defmethod question-feedback-markup((question compound-q))
  `((p (b "Your Answer"))
    ,(element-markup question
                     (submitted-value (user-record question)) t)
    (p (b "Your Mark: " ,(question-mark question)))
    (p (b "Correct Answer"))
    ,(element-markup question
                     (mapcar #'(lambda(p) (getf p :answer))
                             (clews.assessment::parts question))
                     t)
    (p (b "Feedback"))
    ,(feedback question)))


(let ((*directives* docutils.assessment::*questionnaire-directives*))
  (def-directive numeric (parent id
                                   &option (params (read :multiplep t :package :markup))
                                   &content-parser parser)
    (decode-question-args
     parent parser
     :id id
     :type 'numeric-q
     :params params
     :formatted '(:question :feedback :suffix)
     :unformatted '(:radix :weighting :default :answer :tol :format)))

  (def-directive multiple-choice
      (parent id
              &option (params (read :multiplep t :package :markup))
              &content-parser parser)
    (decode-question-args
     parent parser
     :id id
     :type 'multiple-choice-q
     :params params
     :formatted '(:question :feedback :suffix :choices)
     :unformatted '(:weighting :default :answer :style
                    :randomise :allow-no-answer)
     :argsfilter #'(lambda(args)
                     (setf (getf args :choices)
                           (child (getf args :choices) 0)))))

  (def-directive multiple-answer
      (parent id
              &option (params (read :multiplep t :package :markup))
              &content-parser parser)
    (decode-question-args
     parent parser
     :id id
     :type 'multiple-answer-q
     :params params
     :formatted '(:question :feedback :suffix :choices)
     :unformatted '(:weighting :default :answer :style
                    :randomise :allow-no-answer)
     :argsfilter #'(lambda(args)
                      (setf (getf args :choices)
                            (child (getf args :choices) 0)))))

  (def-directive compound-q
      (parent id
              &option (params (read :multiplep t :package :markup))
              &content-parser parser)
    (decode-question-args
     parent parser
     :id id
     :type 'compound-q
     :params params
     :formatted '(:question :feedback))))

(defvar *user-data* nil
  "Dictionary which with a questionnaire name will return user data")

(defvar *articles* nil)

(defvar *assessment-base-url*  "../assessment/"
  "Base url for questionnaires")

(defvar *knowledge* nil)
(defvar *assessment* nil)

(defmethod visit-node((writer docutils.writer.html:html-writer)
                      (assessment questionnaire))
  (let ((knowledge
         (when *user-data*
           (dictionary:get-dictionary (name assessment) *user-data*))))
    (setq *knowledge* knowledge *assessment* assessment)
    (part-append
     (markup:html
      nil
      `((markup:section :class "assessment-status"
         :title ,(format nil "Assessment ~S"  (string (name assessment))))
        ,(assessment-status-table knowledge assessment)
        ,(multiple-value-bind(attempt-p reason)
            (assessment-attempt-p knowledge assessment)
            (if attempt-p
               `(p
                 ,@(jarw.lib:when-bind(reason
                                       (assessment-should-not-attempt-reason
                                        knowledge assessment))
                     (list reason " Only click the link to start
or continue this assessment if you are sure you know what you are doing. "))
                 ,@(when (timelimit knowledge assessment)
                        `("The assessment has a time limit. "
                          ,(when (started knowledge)
                                 (let ((remaining (clews.assessment:time-remaining
                                                   knowledge assessment)))
                                   (if (<= remaining 0)
                                       "If you continue this assessment you will be exceeding the time limit."
                                       (format nil "You have only ~D seconds left to complete this assessment." remaining))))))
                 ((a :href  ,(format nil "~A?name=~A&action=attempt"
                                     *assessment-base-url*
                                     (inet.uri:uri-escape (string (name assessment)))))
                  ,(if (started knowledge) "Continue" "Start")
                  " this assessment."))
               `(p ,reason)))
        ,(when (assessment-feedback-p knowledge assessment)
               `(p ((a :href ,(format nil "~A?name=~A&action=feedback"
                                      *assessment-base-url*
                                      (inet.uri:uri-escape (string (name assessment)))))
                    "Feedback on your answers"))))))))

(register-settings-spec
 `((:latex-assessment-render
    (member :type symbol :set (nil :status :questions :feedback))
    :status
    "How assessments are to be rendered in latex documents")))

(defun source(node)
  (or (attribute node :source)
      (let ((parent (parent node)))
        (when parent (source parent)))))

(defmethod visit-node
    ((writer docutils.writer.latex::latex-writer)
     (assessment questionnaire))
  (let ((render (setting :latex-assessment-render (document assessment))))
    (when render
      (ccase render
        (:status
         (part-append
          (format nil "\\begin{assessment}
~@[\\item ~A~%~]~@[\\item[\\em Deadline:] ~A~%~]\\item[\\em No Questions:] ~A
~@[\\item[\\em Time Allowed:] ~A~~min~%~]\\end{assessment}~%"
                  (clews.assessment::description assessment)
                  (let ((deadline
                         (clews.assessment::deadline-date nil assessment)))
                    (when deadline
                      (jarw.parse::format-output 'jarw.parse:date deadline)))
                  (clews.assessment::set-no-questions nil assessment)
                  (let ((tt (clews.assessment::timelimit nil assessment)))
                    (when tt (/ tt 60))))))))))
