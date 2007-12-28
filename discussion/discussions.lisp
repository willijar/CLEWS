;; $Id: discussions.lisp,v 1.2 2006/08/21 08:26:42 willijar Exp willijar $
;; Web based discussion implementation
;; Copyright (C) 2006 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords: CLEWS, discussion

;; This file is part of CLEWS discussion

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;;

;;; Code:

(in-package :clews.discussion)

(defclass discussion-groups (application)
  ((news-source :type news-source	:reader news-source
                :initarg :news-source
                :documentation "Source of discussion groups"))
  (:default-initargs
      :id :discussion-groups
    :acl '((:view . (:all))
           (:admin . (:admin))))
  (:documentation "Class discussion groups system"))

(defmethod response-handler((app discussion-groups) request rest)
  (let* ((args (split-string rest 4 '(#\/)))
         (group-name (first args))
         (group (when group-name (group group-name (news-source app))))
         (msg (when (second args)
                (let ((n (parse-integer (second args) :junk-allowed t)))
                  (or n (second args)))))
         (cmd (third args)))
    (cond
      (group
       (cond
         ((not (has-permission :view group)) :forbidden)
         ((numberp msg)
          (cond
            ((not (stat msg group)) :not-found)
            ((not (can-view-article msg group)) :forbidden)
            ((not cmd) (view-message app request group-name group msg))
            ((string= cmd "cancel")
             (cancel-message app request group-name group msg))
            (:not-found)))
         ((string= msg "marks")
          (group-marks app request group-name group))
         ((string= msg "search")
          (group-search app request group-name group))
         (t (group-summary app request group-name group))))
      (t (group-directory app)))))

(defun group-directory(app)
  `(html
    (markup:head
     (markup:title "Directory of Discussion Groups"))
    (markup:body
     (navbar ((("search" "Search")
               ("preferences/" "Preferences") )))
     ((section :title
               ,(format nil "Directory of Discussion Groups for ~S"
                        (username *current-user*)))
      (dl
       ,@(mapcan
          #'(lambda(entry)
              (let ((name (car entry))
                    (group (cdr entry)))
                (when (has-permission :view group)
                  `((dt ((a :href ,(concatenate 'string name "/")) ,name)
                     ,(unless (or (has-permission :post group)
                                  (has-permission :reply group))
                              " (read only)"))
                    (dd ,(description group)))	)))
          (sort (inet.nntp:groups (news-source app))
                #'string< :key #'car)))))))

(defun article-summary-list(app group-name group
                            &key (sort :threaded) (view-all nil))
  "Returns sequence of cons of article number and indent level for current
user according to keyword arguments"
  (let ((test
         (if view-all
             #'(lambda(n) (not (header-field :x-mark (head n group))))
             #'(lambda(n) (not (or (header-field :x-mark (head n group))
                                   (article-mark :read n app group-name)))))))
    (case sort
      ((or :threaded (and :threaded-with-parents view-all))
       (labels ((get-thread(n &optional (depth 0))
                  (if (and (can-view-article n group) (funcall test n))
                      (cons (cons n depth)
                            (mapcan #'(lambda(n) (get-thread n (1+ depth)))
                                    (replies n group)))
                      (mapcan #'(lambda(n) (get-thread n depth))
                              (replies n group)))))
         (mapcan
          #'get-thread
          (filtered-articles group
                             :test #'(lambda(n)
                                       (not (in-reply-to n group)))))))
      (:threaded-with-parents
       (labels ((get-thread( n &optional (depth 0))
                  (let ((result nil))
                    (dolist(r (replies n group))
                      (setf result (nconc result (get-thread r (1+ depth)))))
                    (when (and (can-view-article n group)
                               (or result (funcall test n)))
                      (push (cons n depth) result))
                    result)))
         (mapcan
          #'get-thread
          (filtered-articles group
                             :test #'(lambda(n)
                                       (not (in-reply-to n group)))))))
      (:reversed (nreverse (filtered-articles group :test test)))
      (t (filtered-articles group :test test)))))

(defun group-summary(app request group-name group)
  (let* ((start (or (let ((s (car (query-values "start" request))))
                      (when s (parse-integer s :junk-allowed t)))
                    0))
         (count (user-preference :articles-per-page app	*current-user* 50))
         (view-all-p
          (let ((view (car (query-values "view" request))))
            (cond ((string= view "all") t)
                  ((string= view "unread") nil)
                  ((user-preference :view app *current-user*)))))
         (articles (article-summary-list
                    app group-name group
                    :sort (user-preference :sort app *current-user* :threaded)
                    :view-all view-all-p))
         (end (min (+ start count) (length articles)))
         (post-form (post-article-form app request group-name group)))
    `(html
      (markup:head
       (markup:title "Summary of group " ,group-name))
      (markup:body
       (navbar ((("../"  "All Groups")
                 ("marks" "Marks")
                 ("search" "Search")
                 ("../preferences/" "Preferences")  )))
       ((section
         :title
         ,(format nil "Summary of ~:[unread~;all~] articles in ~S for ~S"
                  view-all-p group-name  (username *current-user*)))
        (p ,(if view-all-p
                '((a :href "./?view=unread")
                  "View UNREAD articles only")
                '((a :href "./?view=all") "View ALL articles")))
        ,(if (= start end)
             '(p "There are no new posts for you to view in this topic")
             `(div
               ,@(when (> start 0)
                       `(p ((a :href
                             ,(format nil "?start=~D&view=~:[unread~;all~]"
                                      (max 0 (- start count))
                                      view-all-p))
                            "... More Articles")))
               ((table :style "{table-layout: fixed;}")
                ,(message-summary-header app)
                ,@(map
                   'list
                   #'(lambda(n) (if (consp n)
                                    (message-summary-line
                                     (car n) app group-name group (cdr n))
                                    (message-summary-line
                                     n app group-name group 0)))
                   (subseq articles start end)))
               ,@(when (< end (length articles))
                       `(((p :align :right)
                          ((a :href
                              ,(format nil "?start=~D&view=~:[unread~;all~]"
                                       (+ end 1) view-all-p))
                           "More Articles ...")))))))
       ,@(when (has-permission :post group)
               `((hr)
                 ((section :title "Post New Topic")
                  ,post-form)))))))

(defun group-search(app request group-name group)
  (let ((search-term (car (form-values "search-term" request))))
    `(html
      (markup:head
       (markup:title "Group Search for " ,group-name))
      (markup:body
       (navbar ((("../"  ,group-name)
                 ("marks" "Marks")
                 ("search" "Search")
                 ("../preferences/" "Preferences")  )))
       ((section :title ,(format nil "Search ~S Discussion Group" group-name))
        ((form :method "POST")
         (p "Enter Search Pattern: "
            ((input :type "text" :size 20 :name "search-term"
                    :value ,(or search-term "")))
            ((input :type "submit" :name "submit" :value "Search"))))
        ,(when search-term
               (handler-case
                   (let ((matcher (regex:compile-str search-term)))
                     `((table :style "{table-layout: fixed;}")
                       ,(message-summary-header app)
                       ,(mapcan
                         #'(lambda(n)
                             (message-summary-line
                              n app group-name group 0))
                         (nreverse
                          (filtered-articles
                           group
                           :test #'(lambda(n)
                                     (let ((article (article n group)))
                                       (or (regex:scan-str
                                            matcher
                                            (body (header-field :subject article)))
                                           (regex:scan-str
                                            matcher
                                            (body article))))))))))
                 (error (condition)
                   `(((p :class :error)
                      ,(format nil "Error : ~S"
                               condition)))))))))))

(defun message-summary-header(app)
  (declare (ignore app))
  '((tr :bgcolor "#EEEEEE") (th)
    ((th :id "subject") "Subject")
    ((th :id "author") "Author")
    ((th :id "date")  "Date")
    ((th :id "mark")  "Assess")))

(defun message-summary-line(n app group-name group
                            &optional (indent 0))
  (flet ((make-field(string max-length)
           (subseq string 0 (min max-length (length string)))))
    (when (can-view-article n group)
      (let* ((head (head n group))
             (is-tutor (has-permission :tutor app *current-user*))
             (read (article-mark :read n app group-name))
             (other-window
              (user-preference :article-window app *current-user* t))
             (subject-line
              (make-field (body (header-field :subject head))
                          (user-preference :subject-width app
                                           *current-user* 50)))
             (author (or (body (header-field :x-authenticated-author head))
                         (body (header-field :from head)))))
        `((tr :class ,(if read :read-message :unread-message))
          (td ,(if (header-field :x-mark head)
                   "M"
                   `((img
                      ,@(if read
                            '(:src "/static/small_open_message.gif"
                              :alt "Read")
                            '(:src "/static/small_closed_message.gif"
                              :alt "Unread"))))))
          ((td :valign :top :escape t :headers "subject"
            :style ,(format nil "{padding-left: ~Dem;}" indent))
           ,(if (can-view-article n group)
                `((a :href ,n
                   :title ,(format nil "Article ~D in Group ~A" n group-name)
                   ,@(when other-window '(:target "article")))
                  ,subject-line)
                subject-line))
          ((td :valign :top :headers "author")
           ,(make-field  author
                         (user-preference :author-width app
                                          *current-user* 20)))
          ((td :headers "date" :align :right)
           ,(short-date-string
             (content (header-field :date head))))
          ,(let ((r (mark-reply n group)))
                (when (and r (can-view-article r group))
                  (let* ((article (article r group))
                         (m (content (header-field :x-mark article))))
                    `((td :headers "mark" :align :right)
                      ((a :href ,r :title ,(body article)
                        ,@(when (eql m :plagiarised) '(:class :warning))
                        ,@(when other-window '(:target "article")))
                       ,(let ((m (content (header-field :x-mark article))))
                             (if (eql m :plagiarised) "P"
                                 (if is-tutor m "Yes"))))))))	)))))

(defun short-date-string(utime)
  "Decode the universal time UTIME and return an RFC-822-format string
  using UT rather than the local timezone"
  (when utime
    (multiple-value-bind (se mi ho da mo)
        (decode-universal-time utime 0)
      (declare (fixnum mi ho da mo) (ignore se))
      (format nil "~2,'0d ~a ~2,'0d:~2,'0d"
              da (aref port:+month-names+ (1- mo)) ho mi))))

(defun view-message-content(msg &optional is-tutor mark)
  `((table
    (tr ((th :align :left) "From")
     (td ,(body (header-field :from msg))))
    ,@(when is-tutor
            `((tr ((th :align :left) "Author")
               (td ,(let ((h (header-field
                              :x-authenticated-author msg)))
                         (when h (body h)))))))
    ,@(when (and mark (or is-tutor (eql mark :plagiarised)))
            `((tr ((th :align :left) "Mark")
               (td ,(body mark)))))
    (tr ((th :align :left) "Subject")
     (td ,(body (header-field :subject msg)))))
    ((div :escape nil :class "rst")
    ,(parsed-body msg))))

(defun view-message(app request group-name group n)
  (labels ((get-parents(n)
             (let ((parent (in-reply-to n group)))
               (if parent
                   (cons n (get-parents parent))
                   (list n)))))
    (let* ((msg (article n group))
           (is-tutor (has-permission :tutor app))
           (post-form (post-article-form app request group-name group n))
           (depth -1)
           (mark (header-field :x-mark msg)))
      (setf (article-mark :read n app group-name) t)
      `(html
        (markup:head
         (markup:title ,group-name " - " ,(body (header-field :subject msg))))
        (markup:body
         (navbar
          ((("../"  "All Groups")
            (,(format nil "../~A/" group-name) "Group Summary")
            ("search" "Search")
            ("../preferences/" "Preferences")  )) :on-url ,group-name)
         ((section :title ,(format nil "Message ~D in Group ~A" n group-name))
          (table
           ,(message-summary-header app)
           ,@(mapcar #'(lambda(n)
                         (message-summary-line n app group-name
                                               group (incf depth)))
                     (nreverse (get-parents n)))
           (tr
            (td)
            ((td :colspan 3
                 :style  ,(format nil "{padding-left: ~Dem;}" (incf depth)))
             ,@(view-message-content msg is-tutor mark)
             ,(when (can-cancel-article n group)
                    `((p :align :right)
                      ((a :href ,(format nil "~D/cancel" n)
                        :title ,(format nil "Cancel message ~D in ~A"
                                        n group-name))
                       "Cancel This Message")))))
           ,@(mapcar #'(lambda(n)
                         (message-summary-line n app group-name
                                               group (1+ depth)))
                     (replies n group))))
         ,@(unless mark
                   `((hr)
                     ((section :title "Reply")
                      ,post-form))))))))

(defun submission-form(in-reply-to &optional is-tutor)
  `((form :method :post)
    (table
     (tr
      ((th :align :left) "Author")
      (td ,(username *current-user*)))
     (tr
      ((th :align :left) "Subject")
      (td ((input :size 68 :name :subject
                  :value ,(when in-reply-to
                                (body (header-field :subject in-reply-to)))
                  :datatype (string :word-count 1)))))
     ,@(when (and is-tutor in-reply-to)
             `((tr
                (th "Mark (0-100)")
                ((th :align :left)
                 ((input :size 3 :name :mark :value 0
                         :datatype (integer :min 0 :max 100)))
                 " Plagiarised"	((boolean :name :plagiarised)))))))
    ((textarea :rows 8 :cols 82 :name :body
      :datatype (string :strip-return t :word-count 5))) (br)
    ((input :type :submit :name :preview :value "Preview Only")) (br)
    ((input :type :submit :name :submit
      :value ,(if in-reply-to	"Post Reply"	"Post New Topic")))))

(defun post-article-form(app request group-name group &optional in-reply-to)
  "Construct form for posting articles/marking etc"
  (let* ((in-reply-to (when in-reply-to (article in-reply-to group)))
         (form (submission-form in-reply-to (has-permission :tutor group) )))
    (multiple-value-bind (form-data condition) (form-data form request)
      (if (and (or (getf form-data :submit) (getf form-data :preview))
               (not condition) form-data)
          (let ((message
                 (make-instance 'message
                                :specifications *header-specifications*
                                :body (getf form-data :body) )))
            (when in-reply-to
              (let ((reply-id
                     (content (header-field :message-id in-reply-to))))
                (setf (header-field :in-reply-to message)
                      (list reply-id))
                (setf (header-field :references message)
                      (nconc (content (header-field
                                       :references in-reply-to))
                             (list reply-id)))))
            (setf	(header-field :date message) (get-universal-time)
                  (header-field :subject message)
                  (getf form-data :subject)
                  (header-field :from message)
                  (list (cons (display-name *current-user*)
                              (username *current-user*)))
                  (header-field :path message) (list
                                                (machine-instance)))
            (setf (header-field :newsgroups message)
                  (list group-name))
            (when (has-permission :tutor group)
              (cond ((getf form-data :plagiarised)
                     (setf (header-field :x-mark message) :plagiarised))
                    ((let ((mark (getf form-data :mark )))
                       (when (and mark (> mark 0))
                         (setf (header-field :x-mark message) mark))))))
            (handler-case
                (cond
                  ((getf form-data :preview)
                   (remf form-data :submit)
                   (remf form-data :preview)
                   `(div ,(markup-form form form-data)
                     ((section :title "Article Preview - article not saved")
                      ,@(view-message-content
                         message (has-permission :tutor group)))))
                  (t
                   (let ((n (post-article message group)))
                     (unless n (error "Post unsuccessful"))
                     (setf (article-mark :read n app group-name) t)
                     `(div
                       (p "New Article "
                        ,(body (header-field :message-id message))
                        " posted successfully - see below.")
                       ,@(view-message-content message (has-permission :tutor group))))))
              (error(c)
                `((p :class :error) "Post unsuccessful" (br)
                  ,(format nil "~A" c))
                (remf form-data :submit)
                (remf form-data :preview)
                (markup-form form form-data)
                )))
          (markup-form form nil)))))

(defun author(head)
	(or (content (header-field :x-authenticated-author head))
	    (cdr (first (content (header-field :from head))))))

(defun tutors-summary(group)
  (let((records (make-hash-table :test #'equal)))
    (dolist(n (filtered-articles group))
      (let* ((head (head n group)))
        (unless (or (header-field :x-mark head) (header-field :control head))
          (let* ((author (author head))
                 (record (gethash author records))
                 (date (content (header-field :date head)))
                 (week-ago (- (get-universal-time) (* 7 24 60 60))))
            (push n (getf record :articles))
            (when (integerp date)
              (when (> date week-ago) (incf (getf record :last-week 0)))
              (when (> date (getf record :last-post-date 0))
                (setf (getf record :last-post-date) date))
              (let ((r (mark-reply n group)))
                (if r
                    (let* ((mark (content (header-field :x-mark
                                                        (head r group)))))
                      (cond
                        ((numberp mark) (push mark (getf record :marks)))
                        ((equal mark :PLAGIARISED)
                         (incf (getf record :plagiarised 0)))
                        (t (incf (getf record :unmarked 0)))))
                    (incf (getf record :unmarked 0))))
              (setf (gethash author records) record))))))
    (let ((result nil))
      (maphash #'(lambda(k record)
                   (setf (getf record :mark)
                         (mark-total group (getf record :marks)
                                     (getf record :plagiarised 0)))
                   (push (cons k record) result))
               records)
      (sort result #'string< :key #'car))))

(defun group-marks(app request group-name group)
  (declare (ignore request))
  `(html
    (markup:head
     (markup:title "Statistics for " ,group-name))
    (markup:body
     (navbar
      ((("../"  "All Groups")
        (,(format nil "../~A/" group-name) "Group Summary")
        ("search" "Search")
        ("../preferences/" "Preferences")  )) :on-url ,group-name)
     ,(let* ((username (username *current-user*))
             marked unmarked plagiarised marks)
            (flet ((list-messages(title messages)
                     `((tr ((th :colspan 5 :bgcolor "#EEEEFF")
                            ,(length messages) " " ,title))
                       ,(message-summary-header app)
                       ,@(mapcar #'(lambda(n)
                                     (message-summary-line
                                      n app group-name group 0))
                                 messages))))
              (dolist(n (user-articles username group))
                (let* ((r (mark-reply n group))
                       (mark (when r
                               (content (header-field :x-mark
                                                      (head r group))))))
                  (cond
                    ((numberp mark) (push n marked) (push mark marks))
                    ((equal mark :PLAGIARISED) (push n plagiarised))
                    ((not (header-field :x-mark (head n group)))
                     (push n unmarked)))))
              `((section :title ,(format nil "Marks for ~S in group ~S"
                                         username group-name))
                (p (strong "Overall mark: ")
                 ,(format nil "~3,1F%"
                          (mark-total group marks (length plagiarised))))
                (table
                 ,@(list-messages "Marked Messages" marked)
                 ,@(list-messages "Messages Counted as Plagiarised" plagiarised)
                 ,@(list-messages "Unmarked Messages" unmarked)))))
     ,(when (has-permission :tutor group)
            (let ((records (tutors-summary group)))
              `((section :title "Tutors Summary")
                ((table :border 1 :cellspacing 0 :cellpadding 0)
                 ((tr :bgcolor "#EEEEFF")
                  ((th :id "author" :rowspan 2) "Author")
                  ((th :id "mark" :rowspan 2) "Mark (%)")
                  ((th :colspan 5) "Number Posts")
                  ((th :id "last-post" :rowspan 2)  "Last Post"))
                 ((tr :bgcolor "#EEEEFF")
                  ((th :id "marked")  "Marked")
                  ((th :id "unmarked")  "Unmarked")
                  ((th :id "plagiarised")  "Plagiarised")
                  ((th :id "total")  "Total")
                  ((th :id "week")  "Last Week")	)
                 ,(mapcan
                   #'(lambda(rec)
                       (let ((p (rest rec)))
                         `(tr
                           (td ,(car rec))
                           ((td :align :right) ,(format nil "~3,1f"
                                                        (getf p :mark)))
                           ((td :align :right) ,(length (getf p :marks)))
                           ((td :align :right) ,(getf p :unmarked 0))
                           ((td :align :right) ,(getf p :plagiarised 0))
                           ((td :align :right) ,(+ (length (getf p :marks))
                                                   (getf p :unmarked 0)
                                                   (getf p :plagiarised 0)))
                           ((td :align :right) ,(getf p :last-week 0))
                           ((td :align :right)
                            ,(short-date-string
                              (getf p :last-post-date 0))))))
                   records)))))
     )))

(defun wrap-paragraphs(text &optional (wrap 72))
  (with-output-to-string(os)
    (with-input-from-string(is text)
      (labels ((read-a-char()
                 (let ((c (read-char is nil nil)))
                   (if (eq c #\Newline)
                       (read-char is nil nil)
                       c))))
        (with-output-to-string(buf)
          (do ((c (read-a-char) (read-a-char)))
              ((null c))
            (case c
              (#\Return
               (let ((next-c (read-a-char)))
                 (case next-c
                   ((nil) (return))
                   (#\Return
                    (write-folded (get-output-stream-string buf)	os
                                           :length wrap :leading-sequence "")
                    (write-char #\Linefeed os)
                    (write-char #\Linefeed os))
                   (otherwise
                    (unread-char next-c is)
                    (write-char #\space buf)))))
              (otherwise (write-char c buf))))
          (write-folded (get-output-stream-string buf)	os
                                 :length wrap :leading-sequence ""))))))

(defun cancel-message(app request group-name group n)
  (declare (ignore app))
  (let* ((form
          '((form :method :post)
            (p "Reason For Cancelling Article: "
             ((input :name :body :value "Unspecified Reason"))
             ((input :type :submit :name :submit :value "Cancel Message")))))
         (article (article n group))
         (message-id (content (header-field :message-id article))))
    (multiple-value-bind (form-data condition) (form-data form request)
      (if (and (submitted-action form request) (not condition) form-data)
          (progn
            (setf (body article) (getf form-data :body ""))
            (setf (header-field :message-id article)
                  (concatenate 'string "cancel-" message-id)
                  (header-field :control article) (list :cancel message-id)
                  (header-field :date article) (get-universal-time)
                  (header-field :subject article)
                  (format nil "Cancel <~A>" message-id))
            (post-article article group)
            (redirect request (format nil "../~D" n)))
          `(html
            (markup:head
             (markup:title ,group-name " - delete message " ,n))
            (markup:body
             ((section :title ,(format nil "Cancel message ~D in ~S"
                                       n group-name))
              ,(markup-form form))))))))

(defmethod clews::user-component-preferences((self discussion-groups) user)
  (declare (ignore user))
  (append (call-next-method)
	  '((:sort
	     :text "How do you want messages listed in the group summary?"
	     :markup (mcq
		      (:threaded . "Threaded")
		      (:threaded-with-parents
		       . "Threaded with all parent messages")
		      (:reversed . "Latest First")
		      (nil . "Earliest First"))
	     :type (symbol :nil-allowed t)
	     :default :threaded)
	    (:view
	     :text "Which view do you want by default?"
	      :markup (mcq
		       (:view-all . "All Messages")
		       (nil . "Unread messages only"))
	      :type (symbol :nil-allowed t)
	      :default nil)
	    (:article-window
	     :text "Display articles in a seperate window"
	     :markup (boolean)
	     :type (boolean)
	     :default t)
	    (:articles-per-page
	     :text "How many articles do you want listed per page?"
	     :markup (input :size 5)
	     :type (integer :min 5)
	     :default 50)
	    (:subject-width
	     :text "Width of subject line in columns?"
	     :markup (input :size 5)
	     :type (integer :min 25 :max 999)
	     :default 50)
	    (:author-width
	     :text "Width of author line in columns?"
	     :markup (input :size 5)
	     :type (integer :min 8 :max 999)
	     :default 25))))
