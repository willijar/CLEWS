;;;; CLEWS: Scoratic Assessment handling
;;;; Copyright (C) 2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;; $Id: socratic.lisp,v 1.1 2006/08/08 08:18:49 willijar Exp willijar $

(in-package :clews.assessment)

(defpackage :clews.assessment.socratic
   (:documentation "Generic online assessment handling")
   (:use :cl :markup :jarw.properties :clews.assessment)
   (:shadowing-import-from :inet.body #:body)
   (:shadowing-import-from :inet.mbox #:head)
   (:import-from :inet.mbox
		 #:first-article-number #:last-article-number #:write-mbox
		 #:in-reply-to #:field-search #:post-article)
   (:import-from :dictionary #:initargs)
   (:import-from :inet.message #:header-field #:content)
   (:import-from :inet.acl #:*current-user* #:username #:has-permission)
   (:import-from :clews.form #:markup-form #:is-form-element-p)
   (:import-from :inet.http #:query-values #:response)
   (:import-from :clews.assessment
		 #:countdown-html
		 #:datespec #:datestring #:knowledge #:default-timelimit
		 #:assessment-access-p-reason #:opt #:question-specifications
		 #:strict-timelimit-p #:questionnaire-base
		 #:initialize-knowledge #:ephemeral #:scheduled #:timelimited
		 #:assessment-marked-p #:assessment-status-table)
   (:import-from :clews.assessment.discussion
		 #:threaded-message-sort #:message-post-markup
		 #:short-date-string
		 #:message-summaries-markup #:message-display-markup)
   (:export #:socratic))

(defun (setf submit-name)(new-name form)
  (substitute-markup-when
   #'(lambda(e) (and (consp e) (consp (car e))
		     (eql (caar e) 'input)
		     (eql (getf (cdar e) :type) :submit)
		     (getf (cdar e) :name)))
   form
   #'(lambda(e) (setf (getf (cdar e) :name) new-name) e)
   :as-list t))

(in-package :clews.assessment.socratic)

(import 'socratic (find-package :clews.assessment))

(defclass socratic-questionnaire(timelimited questionnaire-base)
  ())

(defclass socratic(ephemeral scheduled)
  ((questionnaire :type questionnaire-base :reader questionnaire
		  :documentation "Questionnaire to be discussed")
   (min-no-posts :type integer
		 :initform 1 :initarg :min-no-posts :reader min-no-posts
		 :documentation
		 "Minimum number of posts required per student")
   (groups :type list :initform nil :reader groups :initarg :groups
	   :documentation "Groups as a List of lists of usernames")
   (weighting :type list :initform '(:attempt1 0.6 :attempt2 0.4)
	      :reader weighting)
   (schedule
    :initform '(:attempt1 7 :discussion 7 :attempt2 3)
    :documentation "Intervals in days for each component, or nil if freeform")
   (stages :allocation :class :initform '(:attempt1 :deadline :attempt2))
   (deadlines :reader deadlines :initform nil)
   (mbox :reader mbox
	 :documentation "Discussion associated with this assessment"))
  (:documentation "Class for socratic learning"))

(defun schedule-ok-p(instance)
  "Checks schedule and sets deadlines"
  (with-slots(start-date end-date deadline-date
			 stages schedule deadlines) instance
    (if (not schedule) ;; free form, just check start<deadline
	(or (not (and start-date deadline-date))
	    (< start-date deadline-date))
	(let  ((intervals
		(mapcar #'(lambda(s)
			    (let ((v (getf schedule s)))
			      (or (and (integerp v) (< v 52))
				  (datespec v))))
			stages)))
	  (and
	   (or start-date end-date)
	   (= (length intervals) (length stages))
	   (every #'numberp intervals)
	   (let ((date
		  (or start-date
		      (let ((date deadline-date))  ; work backwards
			(dolist(v (reverse intervals)); from deadline
			  (setf date               ; to get start date
				(if (< v 52) (- date (* 7 24 60 60 v)) v)))))))
	     (and
	      (every
	       #'(lambda(s v)
		   (let ((new-date
			  (if (< v 52) (+ date (* 7 24 60 60 v)) v)))
		     (when (> new-date date)
		       (setf (getf deadlines s) new-date
			     date new-date))))
	       stages intervals)
	      (or (not deadline-date) (>= deadline-date date)))))))))

(defmethod initialize-instance :after
    ((instance socratic) &key
     questions discussion timelimit strict-timelimit
     start-date schedule deadline-date
     &allow-other-keys)
  ;; check and initialise schedule and deadlines.
  (assert
   (progn
     (setf (slot-value instance 'deadline-date) (datespec deadline-date)
	   (slot-value instance 'start-date) (datespec start-date)
	   (slot-value instance 'schedule) schedule)
     (schedule-ok-p instance))
   (start-date schedule deadline-date)
   "Schedule ~S with start date ~S and deadline ~S is invalid for a
socratic assessment"
   schedule start-date deadline-date)
  (setf (slot-value instance 'mbox) (inet.mbox:make-mbox discussion))
  (setf (slot-value instance 'questionnaire)
	(make-instance 'socratic-questionnaire
		       :questions questions
		       :timelimit timelimit
		       :strict-timelimit strict-timelimit
		       :randomise-questions nil)))

(defgeneric stage-deadline(stage knowledge assessment)
  (:method deadline(stage knowledge (instance socratic))
           (or (property (property-subset knowledge stage) :deadline-date)
               (getf (slot-value instance 'deadlines) stage))))

(defmethod initargs nconc ((instance socratic))
  `(,@(opt :min-no-posts (min-no-posts instance))
    :questions ,(question-specifications (questionnaire instance))
    :weighting ,(weighting instance)
    ,@(opt :timelimit (default-timelimit (questionnaire instance)))
    ,@(opt :strict-timelimit (strict-timelimit-p (questionnaire instance)))
    :schedule ,(slot-value instance 'schedule)
    :groups ,(groups instance)
    :discussion ,(with-output-to-string(os)
		   (write-mbox (mbox instance) os))))

(defmethod knowledge(username (assessment socratic))
  ;; need username in knowledge for socratic
  (or (gethash username (slot-value assessment 'knowledge))
      (setf (gethash username (slot-value assessment 'knowledge))
	    (list (list :discussion (cons :username  username))))))

(defmethod has-permission ((role (eql :student)) (instance socratic)
			   &optional (user *current-user*))
  (let ((username (username user)))
    (some #'(lambda(group) (member username group :test #'string-equal))
	  (groups instance))))

(defmethod initialize-knowledge progn (knowledge (instance socratic))
  (dolist(stage (slot-value instance 'stages))
    (setf (property (property-subset knowledge stage) :deadline-date)
	  (getf (slot-value instance 'deadlines) stage))))

(defun group(username socratic)
  (find-if #'(lambda(group) (member username group :test #'string-equal))
	  (groups socratic)))

;; states

(defun socratic-state(knowledge instance)
  "return state keyword. :attempt1, :discussion, :attempt2, :completed
reflecting state for this knowledge in the assessment. Also return
reason why cannot move to next state (if appropriate)"
  (let ((now (get-universal-time))
	(group (group (property
				(property-subset knowledge :discussion)
				:username)
		      instance))
	(deadlines (deadlines instance))
	(min-no-posts (min-no-posts instance)))
    (flet((group-not-completed(attempt)
	    (mapcan #'(lambda(username)
			(let ((k (knowledge username instance)))
			  (when (not (completed (property k attempt)))
			    (list username))))
		    group)))
      (cond ((let ((access-p (assessment-access-p-reason knowledge instance)))
	       (when access-p (values nil access-p))))
	    ((not (started knowledge))
	     (values nil "Assessment has not been started yet."))
	    ((completed knowledge) :completed)
	    ((not (completed (property knowledge :attempt1)))
	     (values :attempt1 "First attempt at assessment has been
started but not completed"))
	    ((if (getf deadlines :attempt1)
		 (when (> (getf deadlines :attempt1) now)
		   (values :attempt1 (format nil "You cannot proceed
to the discussion stage until ~A" (datestring (getf deadlines :attempt1)))))
		 (let ((not-completed (group-not-completed :attempt1)))
		   (when not-completed
		     (values :attempt1
			     (format nil "You cannot proceed to the
discussion until the following members of your group complete the
assessment: ~{~S~}"
				     not-completed))))))
	    ((if (getf deadlines :discussion)
		 (when (> (getf deadlines :discussion) now)
		   (values :discussion "You cannot proceed to the
second assessment attempt until ~A" (getf deadlines :discussion)))
		 (when min-no-posts
		   (let ((not-completed
			  (mapcan
			   #'(lambda(username)
			       (when (< (length (field-search :from username
							      (mbox instance)
							      :key #'body))
					min-no-posts)

				 (list username)))
			   group)))
		     (when not-completed
		       (values
			:discussion
			(format nil "You cannot proceed to the second
assessment attempt untile the following members of your group complete
at least ~S posts in the discussion: ~{~S~}"
				min-no-posts not-completed)))))))
	    ((not (completed (property knowledge :attempt2)))
	     (values :attempt2
		     "Second attempt at assessment has been started
but not completed"))
	    ((if (getf deadlines :attempt2)
		 (when (> (getf deadlines :attempt2) now)
		   (values :attempt2 (format nil "Deadline for attempt
2 of assessment is ~A" (getf deadlines :attempt2)))
		 (let ((not-completed (group-not-completed :attempt2)))
		   (when not-completed
		     (values :attempt2
			     (format nil "You get feedback until the
following members of your group complete the second attempt of
theassessment: ~{~S~}"
				     not-completed)))))))
	    (t (error "Unknown Socratic State"))))))

;; to complete
(defmethod assessment-status-long append (knowledge (instance socratic))
  (nconc
   (when (min-no-posts instance)
     `(("Min No Posts" . ,(min-no-posts instance))))
   (multiple-value-bind(state msg) (socratic-state knowledge instance)
     `(("Current Stage"
	. ,(format nil "~A. ~@[~A~]"
		   (ecase state
		     ((nil) "")
		     (:attempt1 "Initial Assessment Attempt")
		     (:discussion "Socratic Discussion")
		     (:attempt2 "Second Assessment Attempt")
		     (:completed "Assessment Completed"))
		  msg))))))

(defun questionnaire-markup(knowledge socratic request attempt state)
  (let ((k (property knowledge attempt))
	(q (questionnaire socratic)))
    (cons
     (assessment-status-table k q)
     (ecase state
       (:attempt
	(list
	 (let ((reason (assessment-should-not-attempt-reason k q)))
	   (when reason `((p :class :error) ,reason)))
	 (let ((time-remaining (time-remaining k q)))
	   (when time-remaining
	   `((div :escape nil :align "center")
	     ,(countdown-html (/ time-remaining 60)))))
	 (assessment-attempt-markup k q request)))
       (:view
	 (list (markup-form (assessment-attempt-markup k q nil) nil :text)))
       (:mark
	(list (assessment-mark-markup k q request)))
       (:feedback
	(list (assessment-feedback-markup k q request)))))))

(defmethod assessment-attempt-markup(knowledge (instance socratic) request)
  (when (query-values "n" request)
    (return-from assessment-attempt-markup
      (message-view-markup knowledge instance request)))
  (multiple-value-bind(state msg) (socratic-state knowledge instance)
    `((table :border 1)
      (tr
       ((td :width "50%" :valign :top)
	((section :title "First Attempt")
	 ,@(questionnaire-markup
	     knowledge instance request :attempt1
	     (case state ((:attempt1 nil) :attempt) (t :view)))))
	((td :width "50%" :valign :top)
	 ((section :title "Second Attempt")
	 ,@(case state
	     (:attempt2
	      (prog1
		  (questionnaire-markup
		   knowledge instance request :attempt2 :attempt)
		(setf (property knowledge :completed)
		      (property (property knowledge :attempt2) :completed))))
	     ((nil :attempt1))
	     (:discussion `((p ,msg)))
	     (t (questionnaire-markup
		 knowledge instance request :attempt2 :view))))))
      (tr
       ((td :colspan 2)
	((section :title "Discussion")
	 ,(case state
	    ((:attempt1 nil) `(p ,msg))
	    (t
	     (discussion-markup
		(property knowledge :discussion)
		instance
		(when (eql state :discussion) request))))))))))

(defmethod assessment-feedback-markup(knowledge (instance socratic) request)
  (when (query-values "n" request)
    (return-from assessment-feedback-markup
      (message-view-markup knowledge instance request)))
  `((table :border 1)
    (tr
     ,@(mapcar
	#'(lambda(attempt label)
	    `((td :width "50%" :valign :top)
	      ((section :title ,(format nil "~A Attempt" label))
	      ,@(questionnaire-markup
		knowledge instance request attempt :feedback))))
	'(:attempt1 :attempt2)
	'("First" "Second")))
    (tr
     ((td :colspan 2)
      ,(discussion-markup
	(property knowledge :discussion) instance nil)))))

(defun can-view-message(instance n group)
  (let ((from (body (header-field :from (head n (mbox instance))))))
    (or (member from group :test #'string-equal)
	(and
	 (not (in-reply-to n (mbox instance)))
	 (has-permission :tutor instance from)))))

(defgeneric discussion-markup(knowledge assessment request)
  (:method (knowledge (instance socratic) request)
  (let* ((mbox (mbox instance))
	 (post-markup
	  (multiple-value-bind(markup message) (message-post-markup request)
	    (if message
		(progn
		  (setf (header-field :date message) (get-universal-time))
		  (post-article message mbox)
		  `(p (em "Article successfully posted")))
		markup)))
	 (username (or (property knowledge :username)
		       (error "No name in knowledge")))
	 (group (group username instance))
	 (threads
	  (threaded-message-sort
	   mbox
	   (loop ;; collect messages in group or top level tutors messages
	      :for n
	      :from (first-article-number mbox)
	      :to (last-article-number mbox)
	      :when (can-view-message instance n group)
	      :collect n))))
    `(div
      ,(if threads
	   (message-summaries-markup
	    threads mbox
	    :columns
	    `((:subject 50 :href "?n=~A" :target "~A")
	      (:from 20)
	      ,#'(lambda(m)
		   (short-date-string
		    (content (header-field :date m))))))
	   '(p "No posts have been made yet"))
      ,(when request
	 `((section :title "Start a new thread")
	   ,post-markup))))))

(defgeneric message-view-markup(knowledge assessment request)
  (:method (knowledge (instance socratic) request)
  (let* ((n (parse-integer (car (query-values "n" request)) :junk-allowed t))
	 (username (property (property knowledge :discussion) :username))
	 (title (format nil "Message ~A" n)))
    (unless (and n (can-view-message instance n (group username instance)))
      (throw 'response :forbidden))
    (let* ((message (inet.mbox:article n (mbox instance)))
	   (reply
	   (make-instance
	    'message
	    :body ""
	    :header
	    (mapcar
	     #'(lambda(c)
		 (make-instance 'inet.header::field
				:name (car c) :body (cdr c)))
	     `((:subject . ,(concatenate
			     'string "Re: "
			     (body (header-field :subject message)))))))))
      (multiple-value-bind(post-form reply)
	(message-post-markup
	 request
	 :message reply
	 :headers `((:from . ((nil . ,(username *current-user*))))
		    (:in-reply-to
		     .
		     ,(list (content (header-field :message-id message))))
		    :subject))
      (throw 'response
	`(html
	  (markup:head (markup:title ,title))
	  (markup:body
	   ((section :title ,title)
	    ,(message-display-markup n (mbox instance))
	    (hr))
	   ((section :title "Reply")
	    ,(if reply
	      (progn (setf (header-field :date reply) (get-universal-time))
		     (post-article reply (mbox instance))
		     `(p (em "Message successfully posted")))
	      post-form))))))))))

(defmethod assessment-mark-markup(knowledge (instance socratic) request)
  (when (query-values "n" request)
    (return-from assessment-mark-markup
      (message-view-markup knowledge instance request)))
  (let ((state (let ((str (first (query-values "mark" request))))
		 (when str (intern (string-upcase str) :keyword)))))
    `((table :border 1)
      (tr
     ,@(mapcar
	#'(lambda(attempt label)
	    `((td :width "50%" :valign :top)
	      ((section :title ,(format nil "~A Attempt" label))
	       (p ((a :href ,(format nil "?mark=~A" attempt))
		   ,(format nil "Mark ~A attempt" label)))
	      ,@(questionnaire-markup
		knowledge instance request attempt
		(if (eql attempt state) :mark :feedback)))))
	'(:attempt1 :attempt2)
	'("First" "Second")))
      (tr
       ((td :colspan 2)
	((section :title "Discussion")
	 ,(discussion-markup
	   (property knowledge :discussion)instance request)))))))

(defmethod assessment-mark(knowledge (instance socratic))
  (let ((max 0)
	(count 0))
    (dolist(attempt '(:attempt1 :attempt2))
      (let ((w (getf (weighting instance) attempt 1)))
	(incf max w)
	(incf count (* w (assessment-mark (property knowledge attempt)
					  (questionnaire instance))))))
    (/ count max)))

(defmethod assessment-distance-metric(knowledge1 knowledge2 (instance socratic))
  (+ (assessment-distance-metric (property knowledge1 :attempt1)
				 (property knowledge2 :attempt1)
				 (questionnaire instance))
     (assessment-distance-metric (property knowledge1 :attempt2)
				 (property knowledge2 :attempt2)
				 (questionnaire instance))))

(defmethod assessment-marked-p(knowledge (instance socratic))
  (every
   #'(lambda(a)
       (assessment-marked-p (property knowledge a) (questionnaire instance)))
   '(:attempt1 :attempt2)))






