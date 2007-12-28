;;;; CLEWS: Discussion subunit handling
;;;; Copyright (C) 2003-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details

(defpackage :clews.assessment.discussion
   (:documentation "Generic online assessment handling")
   (:use :cl :markup :jarw.properties :clews.assessment)
   (:shadowing-import-from :inet.body #:body)
   (:shadowing-import-from :inet.mbox #:head)
   (:import-from :inet.header #:field)
   (:import-from :inet.mbox
		 #:article #:first-article-number #:last-article-number
		  #:replies #:stat)
   (:import-from :dictionary #:initargs)
   (:import-from :inet.message #:header-field #:content #:message
		 #:*message-header-specifications*)
   (:import-from :inet.acl #:*current-user* #:username)
   (:import-from :clews.form #:markup-form #:form-data #:name #:form-mark
		#:is-form-element-p #:output-value #:input-validation
		#:invalid-input
		#:default-value #:element-markup #:datatype #:form-values
		#:submitted-action))

(in-package :clews.assessment.discussion)

(defun short-date-string(utime)
  "Decode the universal time UTIME and return an RFC-822-format string
  using UT rather than the local timezone"
  (when utime
    (multiple-value-bind (se mi ho da mo)
	(decode-universal-time utime 0)
      (declare (fixnum mi ho da mo) (ignore se))
      (format nil "~2,'0d ~a ~2,'0d:~2,'0d"
	      da (aref port:+month-names+ (1- mo)) ho mi))))

(defun truncate-field(field head max-length)
  (let ((string (body (header-field field head))))
    (if (and max-length (< max-length (length string)))
	(subseq string 0 max-length)
	string)))

(defparameter *message-summary-columns*
  `((:subject 50)
    (:from 20)
    ,#'(lambda(m)
	 (short-date-string
	  (content (header-field :date m))))))

(defun message-summary-line-markup
    (n mbox &key
     (columns *message-summary-columns*)
     (indent 0)
     class)
  (let ((message (article n mbox))
	(indent-field (if (consp indent) (car indent) (caar columns)))
	(indent (if (consp indent) (cdr indent) indent)))
    `((tr ,@(when class (list :class class)))
      ,@(mapcar
	 #'(lambda(colspec)
	     (multiple-value-bind(colname content indent)
		 (etypecase colspec
		   (symbol (values colspec
				   (content (header-field colspec message))))
		   (function (values nil (funcall colspec message)))
		   (cons
		    (let* ((colname (first colspec))
			   (indent (when (eql colname indent-field) indent))
			   (args (cddr colspec))
			   (text (truncate-field
				  colname message
				  (when (second colspec)
				    (- (second colspec) (or indent 0))))))
		      (values colname
			      (if args
				  `((a ,@(loop
					    :for a :on args :by #'cddr
					    :nconc (let ((arg (cadr a)))
						     (list
						      (car a)
						      (etypecase arg
							(string
							 (format nil arg n))
							(function
							 (funcall arg n)))))))
				    ,text)
				  text)
			      indent))))
	       `((td :valign :top
		     ,@(when colname (list :headers (string colname)))
		     ,@(when indent
			 (list :style (format nil "{padding-left: ~Dem;}"
					       indent))))
		 ,content)))
	 columns))))

(defun message-display-markup(n mbox
			      &key
			      (headers '(:from :date :subject))
			      format-body)
  (let ((article (article n mbox)))
    `(table
     ,@(mapcar
	#'(lambda(h)
	    (let ((name (string h))
		  (field (header-field h article)))
	    `(tr ((th :id ,name :valign :top :align :right) ,name)
		 ((td :valign :top :align :left :headers ,name)
		  ,(when field (body field))))))
	headers)
     (tr ((th :id "body" :valign :top :align :right) "Body")
	 ((td :valign :top :align :left :headers "body")
	  ,(typecase format-body
	     (function (funcall format-body (body article)))
	     (null `(pre ,(body article)))
	     (symbol `((div :class ,format-body) ,(body article)))))))))

(defun message-post-markup
    (request
     &key (headers `((:from . ((nil . ,(username *current-user*))))
		     :subject))
     body message (cols 68) (rows 10))
  "Construct a form for posting a message. Constructs and returns an
article if the request contains a posted article. Headers is a list
specifying which headers are to appear in the form. They may be a
symbol, in which case a form entry field is given or a cons of a
fieldname and a value in which case the value will be displayed and
used as a header field but not editable. If an article is given its
values it will be modified, otherwise an new article will be returned"
  (let ((form
	 `((form :method :post)
	   (table
	    ,@(mapcar
	       #'(lambda(h)
		   (multiple-value-bind(h v)
		       (if (consp h) (values (car h) (cdr h)) h)
		     (let ((name (string h)))
		       `(tr ((th :id ,name :valign :top :align :right) ,name)
			    ((td :valign :top :align :left :headers ,name)
			     ,(let ((type `(header-field :name ,h)))
				(if v
				    (output-value type v)
				    `((input :name ,h :size ,cols
					     :datatype ,type)))))))))
	       headers)
	    (tr ((th :id "body" :valign :top :align :right) "Body")
		((td :valign :top :align :left :headers "body")
		 ((textarea :name :body
			    :rows ,rows :cols ,cols :value ,body)))))
	   ((input
	     :type :submit :name :submit
	     :value
	     ,(let ((in-reply-to
		     (cdr (find :in-reply-to headers
				:key #'(lambda(a) (if (consp a) (car a) a))))))
		(if in-reply-to
		    (format nil "Reply to ~A" in-reply-to)
		    "Post New Topic")))))))
    (if (not (submitted-action form request))
	(markup-form
	 form
	 (when message
	   (nconc (list :body (body message))
		  (mapcan
		   #'(lambda(h)
		       (when (symbolp h)
			 (list h (body (header-field h message)))))
		   headers) )))
	(multiple-value-bind (form-data condition) (form-data form request)
	    (if condition
		`(div ((p :class :error) ,condition)
		      (markup-form form request))
		(values
		 (markup-form form request :text)
		 (let ((message (or message (make-instance 'message))))
		   (setf (body message) (getf form-data :body))
		   (dolist(h headers)
		     (multiple-value-bind(h v)
			 (if (consp h)
			     (values (car h) (cdr h))
			     (values h (getf form-data h)))
		       (setf (header-field h message) v)))
		   message)))))))

;;;; map header field specifications onto form validation.
;;;; header-field form datatype returns a header fielkd instance
;;;; if data is valid. User edits body form of header field
(defmethod jarw.parse::parse-input
    ((spec (eql 'header-field)) input
     &key name specification (specifications *message-header-specifications*)
     &allow-other-keys)
  (if name
      (handler-case
          (content (make-instance
                    'field :name name :body input
                    :specification (or specification
                                       (gethash name specifications)) ))
        (non-conforming-header(c) (invalid-input input "~A" c)))
      input))

(defmethod jarw.parse::format-output
    ((spec (eql 'header-field)) (field list)
			&key name specification	(specifications *message-header-specifications*)
			&allow-other-keys)
  (when field
    (body (make-instance
	      'field :name name :content field
	      :specification (or specification
				 (gethash name specifications) )))))


#|
(defmethod print-object((mbox discussion-base) stream)
  (print-unreadable-object (mbox stream :type t :identity t)
    (format stream "~D articles (~D-~D)"
	    (number-articles (mbox mbox))
	    (first-article-number (mbox mbox))
	    (last-article-number (mbox mbox)))))

(defclass discussion(discussion-base ephemeral)
  ((number-assessed-articles
    :initform nil :reader number-assessed-articles
    :initarg :number-assessed-articles
    :documentation "Number of articles to be assessed, or, if nil
user-homedir-pathnameq is assessed individually")
   (plagiarism-penalty
    :initform 2 :reader plagiarism-penalty
    :initarg :plagiarism-penalty
    :documentation "Number of articles to be discounted per plagiarsed
article"))
  (:documentation "Discussion type assessment"))

(defgeneric can-view-article(key discussion &optional (user *current-user*))
  (:method-combination and)
  (:documentation "Return true if user can view this article")
  (:method and (key (mbox array) &optional (user *current-user*))
	    (let ((head (head key mbox)))
	      (when head
		(not (header-field :control head)))))
  (:method and (key (mbox discussion-base) &optional (user *current-user*))
	   (can-view-article key (mbox mbox)))
  (:method and (key (discussion access-controlled-entity)
		    &optional (user *current-user*))
	   (has-permission '(:tutor :admin :student) discussion user)))

(defun parse-x-mark(mark)
  (let ((s (string-trim '(#\Space) mark)))
    (if (equalp s "plagiarised")
	:plagiarised
	(parse-integer s))))

(defun marks-score(marks no-counted plagiarism-penalty)
  (if (and marks (> no-counted 0))
      (let ((no-plagiarised (count :plagiarised marks)))
	(/ (reduce #'+
		   (subseq
		    (sort (mapcan #'(lambda(m) (when (numberp m) (list m)))
				  marks)
			  #'>)
		    0
		    (max 0
			 (min (length marks)
			      (- no-counted
				 (* plagiarism-penalty no-plagiarised))))))
	   no-counted))
      0))


(defmethod filtered-articles(mbox &key (test #'identity))
  "Return a sequence of article numbers for which test (a function
which takes an the article number) returns true and which are
viewable"
  (let* ((first (first-article-number mbox))
	 (last (last-article-number mbox)))
    (loop :for n :from first :to last
	  :when (and (can-view-article n mbox) (funcall test n))
	  :collect n)))
|#




(defun threaded-message-sort(mbox &optional subset)
  "Return a tree reflecting the thread topology of messages in
mbox. Only messaages whose numbers appear in subset will be
included. Each node on the tree may be a message number or a list
whose first element is a message number (or nil if the message number
is not int the subset), the rest of which are further nodes."
  (let ((subset
	 (if subset
	     (copy-list subset)
	     (loop
		:for n
		:from (first-article-number mbox)
		:to (last-article-number mbox)
		:when (stat n mbox)
		:collect n))))
    (labels((get-thread(n)
	    (let ((subthreads (mapcan #'get-thread (replies n mbox))))
	      (cond
		((member n subset)
		 (setf subset (delete n subset))
		 (if subthreads
		     (list (cons n subthreads))
		     (list n)))
		(t (when subthreads
		     (list (cons nil subthreads))))))))
      (let ((threads nil))
	(do ((n (car subset) (car subset)))
	    ((not n) threads)
	  (setf threads (nconc threads (get-thread n))))))))

(defun message-summaries-markup(tree mbox
				&key (columns *message-summary-columns*)
				(indent 1)
				row-class
				table-class)
  (let ((rows nil)
	(width
	 (reduce #'+
		 (mapcan #'(lambda(spec) (when (and (consp spec) (second spec))
						     (second spec)))
			 columns))))
    (labels ((do-thread(thread depth)
	     (etypecase thread
	       (cons
		(when (car thread)
		  (push (message-summary-line-markup
			 (car thread) mbox
			 :columns columns
			 :indent (* depth indent)
			 :class row-class)
			rows))
		(dolist(subthread (rest thread))
		  (do-thread subthread (1+ depth))))
	       (integer
		(push (message-summary-line-markup
			 thread mbox
			 :columns columns
			 :indent (* depth indent)
			 :class row-class)
		      rows)))))
      (dolist(thread tree) (do-thread thread 0))
      `((table :class ,table-class)
	(tr
	 ,@(mapcar
	    #'(lambda(spec)
		(typecase spec
		  (symbol `(th ,spec))
		  (cons `((th
			   ,@(when (second spec)
			       (list :width (format
					     nil "~A%"
					     (* 100
						(/ (second spec)
						(if (> width 0) width 100)))))))
			  ,(string-capitalize (first spec))))
		  (t '(th))))
	    columns))
	,@(nreverse rows)))))


;; put discussion into assessment package
;;(import 'discussion (find-package :clews.assessment))