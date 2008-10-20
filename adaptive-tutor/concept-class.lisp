;;;; Concept class and related types
;;;; Copyright (C) 2002-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; $Id: concept-class.lisp,v 1.1 2006/07/30 17:40:10 willijar Exp $

(in-package :clews.adaptive-tutor)

(deftype concept-content () `(or list function string))

(defvar *default-concept-properties*
  '((:min-time-required . 30)
    (:timeout . 300)
    (:completion-threshold . 0.7)))

;; concept represents a single concept.
;; they have prerequisites and outcomes in terms of other concepts
(defclass concept ( property-holder)
  ((id :reader concept-id :documentation "Unique id for this concept"
       :initarg :concept-id)
   (prerequisites :reader direct-prerequisites :type list :initform '()
                  :documentation
                  "an alist of direct prerequisite concepts and their
weightings")
   (outcomes :reader direct-outcomes :type list :initform '()
             :documentation "alist of outcome concepts and their weightings")
   (children :reader direct-children :type list :initform '()
             :documentation "list of direct children concepts of this concept")
   (content :accessor content :type concept-content :initform ()
            :documentation "list of content tags or a function returning such")
   (assessment :reader assessment :initform nil
               :documentation "Assessment associated with this concept"))
  (:default-initargs :default-properties *default-concept-properties*)
  (:documentation
   "class representing a concept in the adaptive tutor concept map."))

(defmethod concept-title((concept concept))
  (or (property concept :title) (concept-id concept)))

(defmethod keywords((concept concept) &key (ignore-word-p #'ignore-word-p) &allow-other-keys)
  (mapcan #'(lambda(item)
              (typecase item
                (string (keywords item :ignore-word-p ignore-word-p))
                (cons (markup-keywords item :ignore-word-p ignore-word-p))))
          (list (property concept :title)
                (property concept :summary)
                (let ((c (content concept)))
                  (unless (functionp c) (cons 'div c))))))

(defmethod initialize-instance :after ((c concept) &rest rest)
  (declare (ignore rest))
  (setf (property c :creation-time) (get-universal-time)))

(defun probability-relationship-p(p)
  (and (typep (car p) 'concept)
       (let ((w (cdr p)))
         (and (numberp w)
              (<= w 1)
              (>= w 0)))))

(deftype probability-relationship()
  '(and cons (satisfies probability-relationship-p)))


(defvar *concept-property-descriptions*
  '((:prerequisites
     "Contains an association list of the concept names that are DIRECT
prerequsites of this concept and a weigthing factor w (0.0<=w<=1.0)
determining the relative importance of this prerequisite. In determine
the overall prerequisites of a concept the prerequsities are cascaded
i.e. if A is a direct prerequesite of B with w=0.5 and B is a direct
prerequisite of C with w=0.3 then the system will infer that A is a
prerequisite of C with w=0.3x0.5. Circular references are not allowed
and an error will be generated if a definition would lead to a
circular reference such that a concept could be infered to be a
prerequisite of itself."
     "(:prerequisites ((concept-a . 0.5) (concept-b . 0.3)))")
    (:children
     "A list of the DIRECT child concepts for this concept. All children
of a concept must be completed if the concept itself is to be
considered completed. This enables the tutor to impose a traditional
heirarchical structure on top of the concept map. Children are cascaded so
if A is a direct task of B and B is a direct task of C then both A and
B must be completed if C is to be considered completed. Circular
references are not allowed and an error will be generated if a
definition would lead to a circular reference such that a concept
could be infered to be a child of itself. Note a concept may be a child
of many other concepts and may indeed be a descendant by several roots
in a heirarchical arrangement."
     "(:children (subconcept-a subconcept-b))")

    (:content
     "The concept definition. It may be a string or a list of (tag
content) where tag may be (tagname attrs).  It may alternative be a
lambda expression taking of three arguments (REQUEST CONCEPT USER) if
the concept is interactive or includes an assessment."
     "((a :href \"/\") \"home\")")

    (:outcomes
     "Contains an association list of the concept names for which this
concept has as DIRECT outcomes and a weigthing factor w (0.0<=w<=1.0)
determining hope much of the other concept is included in this one. It
is effectively a measure of overlap - we are saying that if a student
has acheived a certain completion level of this concept then we can
infer that he/she has also undrstood w times the outcome concept.
When detemining how much of a concept has been understood by a student
all of the concepts which have outcomes towards this concept are
considered and their weighting is multiplied by the students
completion level.  Outcome definitions are chained so if A has an
outcome of B with w=0.5 and B has an outcome of C with w=0.3 then the
system will infer that A contributes w=0.3x0.5 towards C. The outcomes
can have circular references, indeed this is to expected as overlap
may be two way (if we explicitely state it) i.e. we may have A as an
outcome of B with a certain weighting and B as an outcome of B with
another. During the inference stage the system will ensure that a
concept does not contribute towards its itself more than once (it is
implicit than an a concept has an outcome of 1 towards itself). It is
however quite possible, and sensible that a concept may contribute
along more than one route towards another one."
     "(:prerequisites ((concept-a . 0.5) (concept-b . 0.3)))")

    (:min-time-required  "An integer number of seconds which is the minimum amount of time a student should be required to spend on a concept"
     "(:min-time-required 600)")

    (:timelimit "An integer number of seconds which is the maxmimum amount of time a student should be required to spend on a concept - for example the maximum time allowed
to complete a test. This may also be set on an individual basis per student using set-knowledge function"
     "(:max-time-required 1800)")

    (:completion-threshold "An value between 0 and 1 which determines the level at which a concept
  can be considered completed."
     "(:completion-threshold 0.7)")

    ;;The follwoing is probably unnecessary as user can simply acheive this by
    ;;not having other concepts having outcomes towards a particular required one
    ;;(or at least insufficient to pass completion level)
    ;;(:must-visit nil
                                        ; "The system can infer completion of a concept in terms of outcomes
                                        ;from other concepts. Thus a student may achieve the
                                        ;completion-threshold of a module without actually visiting it. If this
                                        ;is set to true then the student must actually visit and complete this
                                        ;concept. This may be used, for example, to ensure students visit an
                                        ;assessment."
                                        ;"(:must-visit t)")

    (:completion  "A function which when given a concept and a user record returns the
   completion level in range 0-1. Example given below is the preset default
   calculation which is simply percentage of min time that a user has spent
   viewing a concept. This property is usually set implicitely when
   assessments are defined in a concept."
     "(lambda (user concept)
      (min 1.0 (/ (cond ((knowledge user concept :time-spent)) (0))
	      (property concept :min-time-required))))")

    (:mark  "A function which when given a concept and a user record returns the
   mark achieved by the student on this concept.
   This property is usually set implicitely when assessments are defined
    in a concept."
     "#'calculate-mark")

    (:deadline  "The deadline for the assessment submission as a universal time. If
not specified there is no deadline and submissions can be submitted
and viewed at any time between the start-date and end-date. Typically
initialised using (encode-universal-time sec min hour day month year
&optional time-zone). DEPRECATED - this should be set on an individual basis using set-knowledge"
     "(encode-universal-time 0 0 0 1 1 2000)")

    (:feedback-date"The date on which feedback will be avalable for those who have
completed an assessment. A universal time typically initialised using
(encode-universal-time sec min hour day month year &optional
 time-zone).  If left as nil it will be immediately after the deadline.
DEPRECATED - this should be set on an individual basis using
set-knowledge"
  "(encode-universal-time 0 0 0 1 1 2000)")

    (:start-date"The first date for which the assessment can be viewed as a
universal time. Typically initialised using (encode-universal-time sec
min hour day month year &optional time-zone). DEPRECATED as it breaks the asynchronous learning aspect of using the web."
	     "(encode-universal-time 0 0 0 1 1 2000)")

    (:end-date  "The last date for which the assessment can be viewed as a universal
time. Typically initialised using (encode-universal-time sec min hour
day month year &optional time-zone). DEPRECATED - as it breaks the asynchronous learning aspect of using the web."
  "(encode-universal-time 0 0 0 1 1 2000)")

    (:set-no-questions  "The number of questions given to the student. If less than the number listed in the assessment then a random selection of the questions will be presented to the student."
  5)

                                        ;(:set-questions-weight nil
                                        ;  "May be used to set question by some weighting factor rather than by a cound - will allocate questions up to this weighting" 10)

    (:strict-timelimit  "If true submissions outside the timelimit will not be accepted. If
false (the default) they will be accepted and marked with a penality
system proportional to the amount of extra time taken e.g. if a
student takes twice the timelimit then their mark will be halved."
t)

    (:randomise-questions  "When true the order of the questions will be randomised. When used together with a set number of questions which is less than the total number provided allows the setting of a random subset of questions"
  nil)))

