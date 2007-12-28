;;;; CLEWS Discussion System
;;;; Copyright (C) 2004-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; assessed group is an accessed controlled mbox.
;;;; The following extra headers are used
;;;; x-authenticated-author: the authenticated author (username) of a message
;;;;                         - maintained by server
;;;; x-mark: an assessment message applying to the message it is in reply to
;;;;         - entered by the client but overridden if marking not allowed
;;;;         Only tutors or the author of the parent message can view this
;;;;         message

;;;; The following access roles apply to the group apply
;;;; :view to read messages (default all)
;;;; :post to post top level topics and delete your own
;;;; :reply to reply to messages and delete your own
;;;; :tutor can add messages with marks, or delete any messages
;;;; $Id: group.lisp,v 1.1 2006/08/21 07:12:14 willijar Exp willijar $

(in-package :clews.discussion)

(defun parse-x-mark(mark)
  (let ((s (string-trim '(#\Space) mark)))
    (if (equalp s "plagiarised")
	:plagiarised
	(parse-integer s))))

(defparameter *header-specifications*
  (inet.header:merge-field-specifications
   inet.nntp:*rfc1036-header-specifications*
   (inet.header:make-field-specifications
    `((:x-mark 0 1 ,#'parse-x-mark ,#'princ-to-string)
      (:x-authenticated-author 0 1))))
  "Header specifications for the Assessed news group")

(defclass assessed-group(access-controlled-entity mbox)
  ((description :initform nil :initarg :description :reader description
		:documentation "A description for this news group")
   (number-assessed-articles :initform 20 :initarg :number-assessed-articles
			     :reader number-assessed-articles))
  (:default-initargs :specifications *header-specifications*)
  (:documentation "Mixin class for assessed groups"))

(defmethod mark-total((mbox assessed-group) marks no-plagiarised)
  (if marks
      (/ (reduce #'+
		 (subseq (sort (copy-list marks) #'>)
			 0
			 (max 0
			      (min (length marks)
				   (- (number-assessed-articles mbox)
				      (* 2 no-plagiarised))))))
	 (number-assessed-articles mbox))
      0))

(defmethod print-object((mbox assessed-group) stream)
  (print-unreadable-object (mbox stream :type t :identity t)
    (format stream "~D articles (~D-~D)"
	    (- (last-article-number mbox) (first-article-number mbox) -1)
	    (first-article-number mbox) (last-article-number mbox))))

(defmethod initialize-instance :after ((mbox assessed-group) &key)
  (pushnew :x-authenticated-author (slot-value mbox 'indexed-fields))
  (pushnew :control (slot-value mbox 'indexed-fields))
  (pushnew :x-mark (slot-value mbox 'indexed-fields))
  (pushnew :references (slot-value mbox 'indexed-fields))
  (inet.mbox:reindex-mbox mbox))

(defmethod can-view-article ((n integer) (mbox assessed-group)
			     &optional (user *current-user*))
  "Return true if article with given id can be viewed by user"
  (let ((idx (index n mbox)))
    (when idx
      (or (has-permission :tutor mbox user)
	  (and (has-permission :view mbox user)
	       (not (getf idx :control))
	       (or (not (getf idx :x-mark))
		   (equal (username user)
			  (getf (index (in-reply-to n mbox) mbox)
				:x-authenticated-author)	)))))))

(defmethod filtered-articles((mbox assessed-group) &key (test #'identity))
  "Return a sequence of article numbers for which test (a function which takes an the article number) returns true and which are viewable"
  (let* ((first (first-article-number mbox))
	 (last (last-article-number mbox)))
    (loop for n from first to last
	  when (and (can-view-article n mbox) (funcall test n))
	  collect n)))

(defmethod number-articles((mbox assessed-group))
  (loop for i from (first-article-number mbox) to (last-article-number mbox)
	count (can-view-article i mbox)))

(defmethod article :before ((n integer) (mbox assessed-group))
  (unless (can-view-article n mbox)
    (error 'permission-denied-error :action "article" :service "mbox")))

(defmethod head :before ((n integer) (mbox assessed-group))
  (unless (can-view-article n mbox)
    (error 'permission-denied-error :action "head" :service "mbox")))

(defmethod stat :before ((n integer) (mbox assessed-group))
  (unless (can-view-article n mbox)
    (error 'permission-denied-error :action "stat" :service "mbox")))

(defmethod last-article(n (mbox mbox))
  (let ((start  (first-article-number mbox)))
    (loop for i from (1- n) downto start
	  when (can-view-article i mbox) do (return (stat i mbox)))))

(defmethod next-article(n (mbox mbox))
  (let ((end  (last-article-number mbox)))
    (loop for i from (1+ n) to end
	  when (can-view-article i mbox) do (return (stat i mbox)))))

(defmethod can-post-article (article (mbox assessed-group)
                             &optional (user *current-user*))
  "Return true if article  can be posted by user"
  (let ((ctl (inet.message::message-control article)))
    (cond
      ((has-permission :tutor mbox))
      ((header-field :x-mark article) nil)
      (ctl (let ((target (getf ctl :cancel)))
             (and target (can-cancel-article target mbox user))))
      ((header-field :in-reply-to article) (has-permission :reply mbox))
      (t (has-permission :post mbox)))))

(defmethod post-article :before (article (mbox assessed-group))
  (setf (header-field :x-authenticated-author article)
	(username *current-user*))
  (unless (can-post-article article mbox)
    (error 'permission-denied-error :action "post" :service "mbox")))

(defmethod can-cancel-article (id (mbox assessed-group)
			       &optional (user *current-user*))
  (and (not (getf (index id mbox) :control))
       (not (mark-reply id mbox))
       (or (has-permission :tutor mbox)
           (equal (username user)
                   (getf (index id mbox) :x-authenticated-author)))))

(defmethod cancel-article :before ((n integer) (mbox assessed-group))
  (unless (can-cancel-article n mbox)
    (error 'permission-denied-error :action "cancel-article"
           :service "mbox")))

(defun mark-reply(n mbox)
  "Returns the article number of the message which is the mark reply to n"
  (some #'(lambda(r)
	    (when (and (getf (index r mbox) :x-mark)
		       (not (getf (index r mbox) :control)))
	      r))
        (replies n mbox)))

(defun user-articles(username mbox)
  "Return the list of articles authored by user with username"
  (filtered-articles
   mbox
   :test #'(lambda(n)
	     (let ((idx (index n mbox)))
	       (and (equal username (getf idx :x-authenticated-author))
		    (not (getf idx :control)))))))

(defun user-stats(username mbox)
  "Return analysis of users marks"
  (let ((stats nil))
    (dolist(n (user-articles username mbox))
      (incf (getf stats :count 0))
      (let ((r (mark-reply n mbox)))
	(if r
	    (progn
	      (push n (getf stats :marked))
	      (let ((m (getf (index r mbox) :x-mark)))
		(if (numberp m)
		    (push m (getf stats :marks))
		    (push n (getf stats :plagiarised)))))
	    (push n (getf stats :unmarked)))))
    (setf (getf stats :mark) ( mark-total mbox (getf stats :marks)
					  (length (getf stats :plagiarised))))
    stats))


(defclass assessed-mbox-group(mbox-threading unix-mbox assessed-group)
  ())

(defclass directory-news-source(news-source)
  ((directory :initarg :directory :initform nil :reader news-path
	      :documentation "Directory in which to find news groups")))

(defmethod reload((news directory-news-source))
  (let ((specs
         (with-open-file(is (merge-pathnames (news-path news) "index"))
           (read is))))
    (clrhash (slot-value news 'inet.nntp::groups))
    (dolist(spec specs)
      (let ((name (first spec))
            (args (rest spec)))
        (setf (getf args :file)
              (merge-pathnames (news-path news)
                               (getf args :file name)))
        (setf (gethash name (slot-value news 'inet.nntp::groups))
             (apply #'make-instance
                    (cons 'assessed-mbox-group
                          (append args (list :specifications
                                             *header-specifications*)))))))))

(defmethod initialize-instance :after ((news directory-news-source) &key
                                       &allow-other-keys)
  (reload news))

