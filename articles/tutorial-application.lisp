;; $Id: tutorial-application.lisp,v 1.1 2007/07/26 08:55:06 willijar Exp willijar $
;; Tutorial Articles
;; Copyright (C) 2006 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of CLEWS Articles

;; This is free software releasedunder the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;;

;;; Code:

(in-package :clews.articles)

(defclass clews-tutorial(clews-articles)
  ((collection :type tutorial-collection))
  (:documentation "A collection of tutorial articles"))

(defgeneric goals-section(collection request)
  (:method ((collection tutorial-collection) request)
    `((ol
       ,@(mapcar #'(lambda(a) (article-list-item a :relative "../"))
                 (updated-goals collection *current-user*))))))

(defmethod article-view((article tutorial-article) request)
  (let ((docutils.assessment::*user-data*
         (user-state article *current-user*))
        (docutils.assessment::*assessment-base-url*  "./assessment"))
    (call-next-method)))

(defun deadlines-section(collection request)
  (declare (ignore request))
  (let* ((nodays (user-preference :deadline collection *current-user* 14)))
    (multiple-value-bind(missed imminent feedback)
        (assessment-deadlines collection *current-user* nodays)
      (multiple-value-bind(amissed aimminent)
          (article-deadlines collection *current-user* nodays)
      (flet ((li(entry)
               (article-list-item
                (if (typep (cdr entry) 'article)
                    (cdr entry)
                    (article (cdr entry)))
                :abstract nil
                :relative "../"
                :status-line
                `(b "Due " ,(format-output '(date :fmt :short) (car entry))))))
        (nconc
         (when missed
           `(((markup:section :id "missed-deadlines"
               :title "Missed Assessments")
              ((markup:ol :class "condensed")
               ,@(mapcar #'li missed)))))
         (when imminent
           `(((markup:section :id "imminent-deadlines"
               :title
               ,(format nil "Assessments due in the next ~D days" nodays))
              ((markup:ol :class "condensed")
               ,@(mapcar #'li imminent)))))
         (when feedback
           `(((markup:section :id "feedback available"
               :title "New Feedback Available")
              ((markup:ol :class "condensed")
               ,@(mapcar #'li feedback)))))
         (when amissed
           `(((markup:section :id "missed-deadlines"
               :title "Articles Behind Schedule")
              ((markup:ol :class "condensed")
               ,@(mapcar #'li amissed)))))
         (when aimminent
           `(((markup:section :id "imminent-deadlines"
               :title
               ,(format nil "Articles due in the next ~D days" nodays))
              ((markup:ol :class "condensed")
               ,@(mapcar #'li aimminent)))))))))))

(defmethod sections((articles tutorial-collection))
  (nconc
   (call-next-method)
   (list
    (make-action
     :name "goals" :permission :view :handler #'goals-section
     :label "Tutorial Goals")
    (make-action
     :name "deadlines" :permission :view :handler #'deadlines-section
     :label "Deadlines"))))

(defmethod body-section((article tutorial-article) request)
  (unless (and (not (query-values "view-anyway" request))
               (incomplete-prerequisites *current-user* article))
    (return-from body-section (call-next-method)))
  `(,@(prerequisites-section article request)
    (p
     "If you wish to view this page before completing the prerequisites then "
     ((a :href "?view-anyway=t") "click here."))))

(defgeneric child-heirarchy-section(article request)
  (:method((article article) request)
    (declare (ignore request))
    (labels((list-item(other)
              (article-list-item
               other
               :current-article article
               :relative "../" :status-line nil :abstract nil))
            (heirarchy(tree)
              (let ((li (list-item (car tree)))
                    (rest (mapcar #'heirarchy (rest tree))))
                (if rest (nconc li `(((ul :class "condensed")  ,@rest))) li))))

      (let ((next-recommended  (next-article *current-user* article)))
        `(((ul :class "condensed")
           ,(heirarchy
             (let ((goals (goals (collection article) *current-user*)))
               (if (member article goals)
                   (nconc (list article)
                          (mapcar #'list (direct-children article)))
                   (child-heirarchy
                    (find-if #'(lambda(goal) (is-child goal article)) goals)
                    article)))))
          ,@(when next-recommended
              `((p (b "Next Recommended"))
                (ul
                 ,(list-item next-recommended)))))))))

(defmethod title-section((article tutorial-article) request)
  (declare (ignore request))
  (append (call-next-method)
           (when (field-value "abstract" article)
             `(((markup::p :class "abstract")
                ,(field-value "abstract" article))))))

(defgeneric prerequisites-section(article request)
  (:method((article article) request)
    (declare (ignore request))
    (let ((a (incomplete-prerequisites *current-user* article)))
      (when a
      `((p
         "You should complete the following prerequsites before this article")
        ((ul :class "condensed")
         ,@(mapcar
            #'(lambda(a)
                (article-list-item
                 a
                 :current-article article
                 :abstract nil
                 :status-line nil
                 :relative "../"))
               a)))))))

(defgeneric next-article-section(article request)
  (:method((article article) request)
    (declare (ignore request))
    (let ((next (next-article *current-user* article)))
      (when next
        `(((ul :class "condensed")
           ,(article-list-item
             next
             :abstract nil
             :current-article article
             :status-line nil
             :relative "../")))))))

(defun countdown-html(remaining)
  "Returns javascript to display countdown for remaining seconds"
  (format nil "
<form name=\"countdown\"><b>Time remaining</b>
<input type=\"text\" name=\"clock\" align=\"right\" size=5 readonly style=\"color:red;text-align:right;border-style:none;\"> minutes
</form>
<script language=javascript>
<!--
endTime=new Date()
function clockTick()
{
currentTime = new Date();
value=Math.round((endTime-currentTime)/60000+~D);
if (value>0) {
document.countdown.clock.value = value
setTimeout(\"clockTick()\", 10000);
}
else
{
document.countdown.clock.value=\"None\";
}}
clockTick();

-->
</script>" remaining))

(defgeneric article-assessment(article request)
  (:documentation "Handle assessment submissions for an article")
  (:method((article tutorial-article) request)
    (let* ((name (intern (first (query-values "name" request)) :keyword))
           (action-q (first (query-values "action" request)))
           (assessment (find name (assessments article)
                             :test #'equal :key #'docutils.assessment::name)))
      (unless (and assessment action-q) (throw 'inet.http:response :not-found))
      (let* ((knowledge
              (or (state-value name (user-state article *current-user*))
                  (list nil)))
             (time-remaining (clews.assessment:time-remaining
                              knowledge assessment))
             (not-attempt-reason
              (clews.assessment:assessment-should-not-attempt-reason
               knowledge assessment))
             (feedback-p
              (clews.assessment:assessment-feedback-p knowledge assessment))
             (reset-p
              (clews.assessment:assessment-reset-p knowledge assessment))
             (action (intern (string-upcase action-q) :keyword)))
        (setf (char action-q 0) (char-upcase (char action-q 0)))
      `(((markup:section :class "assessment"
          :title ,(format nil "~A: Assessment ~S ~A for ~S"
                          (title article) (string name) action-q
                          (clews:display-name *current-user* )))
         ((markup:div :class "assessment-status")
           ,(clews.assessment::assessment-status-table knowledge assessment)
         ,(when (and time-remaining (eql action :attempt))
                `((div :class "countdown" :escape nil)
                  ,(countdown-html (/ time-remaining 60)))))
         ,@(flet ((action-link(action text)
                  `(markup:p ((markup::a
                     :href , (format nil "?name=~A&action=~A"
                                     (inet.uri:uri-escape (string name))
                                     action))
                       ,text))))
                (case action
                  (:ATTEMPT
                   (prog1
                       (list
                        (when not-attempt-reason
                          `((markup:p :class :error) ,not-attempt-reason
                            "Only submit if you are sure you know what you are doing."))
                        (clews.assessment:assessment-attempt-markup
                         knowledge assessment request)
                        (when feedback-p
                          (action-link
                           "feedback"
                           "View Feedback on your assessment submission"))
                        `((a :href "./view" :taget "view")
                          ,(format nil "Back to main page ~S" (title article)))
)
                     (setf (state-value
                            name
                            (user-state article *current-user*))
                           knowledge)))
                  (:RESET
                   (unless reset-p (throw 'inet.http:response :forbidden))
                   (clews.assessment::reinitialize-knowledge knowledge assessment)
                   (setf (state-value name (user-state article *current-user*))
                         knowledge)
                   `((p "Your submission to this assessment has now
                     been reset and you may now attempt a fresh
                     instance. Your previous submission cannot now be
                     retreived.")
                     ,(when (clews.assessment:assessment-attempt-p
                     knowledge assessment)
                            (action-link "attempt" "Continue attempt"))
                     ((a :href "./view")
                       ,(format nil "Back to ~S" (title article)))))
                  (:FEEDBACK
                   (list
                    (clews.assessment:assessment-feedback-markup
                     knowledge assessment request)
                    (unless not-attempt-reason
                      (action-link "attempt" "Continue attempt"))
                    `((a :href "./view")
                      ,(format nil "Back to ~S" (title article)))
                    (when reset-p
                      (action-link "reset" "Reset your submission for
                      a new attempt"))))
                  ((throw 'inet.http:response :not-found))))))))))

(defmethod sections((article tutorial-article))
  (let ((sections (call-next-method)))
    (nconc
     (list (make-action :name "map" :permission :view
                        :handler #'child-heirarchy-section))
       sections)))

(defmethod format-output((spec (eql '%)) value &key (places 1)
                         &allow-other-keys)
  (if value
      (format nil (format nil "~~,~DF%" places) (* 100 value))
      "%"))

(defmethod visits-report((article tutorial-article) request)
  (let ((sort (car (query-values "sort" request))))
    (multiple-value-bind(users form)
        (group-select (collection article) request)
      `(,form
        (markup::hr)
        ((markup:section :title "For This Article")
         ,(users-report
          (list
           (list "Time Spent" 'jarw.parse:time-period
                 #'mean-with-nil #'<-with-nil)
           (list "Last Visited" '(jarw.parse:date :fmt :short :if-nil "Never")
                 #'mean-with-nil #'<-with-nil)
           (list "Completion" '% #'mean-with-nil #'<-with-nil)
           (list "Mark" '% #'mean-with-nil #'<-with-nil))
          users
          #'(lambda(user)
              (let ((state (user-state article user))
                    (*current-user* user))
                (list
                 (state-value :time-spent state)
                 (state-value :last-visit-time state)
                 (article-completion (user-state (collection article) user)
                                     article)
                 (mark (user-state article user) article))))
          sort))
        (markup::hr)
        ,@(when (assessments article)
        `(((markup:section :title "By Assessment")
         ,@(mapcar
            #'(lambda(assessment)
                `((markup:section
                   :title ,(docutils.assessment::name assessment))
                  ,(users-report
                    (list
                     (list "Time Taken" 'jarw.parse:time-period
                           #'mean-with-nil #'<-with-nil)
                     (list "Submitted" '(jarw.parse:date :fmt :short :if-nil "Never")
                           #'mean-with-nil #'<-with-nil)
                     (list "Deadline" '(jarw.parse:date :fmt :short)
                           nil #'<-with-nil)
                     (list "Mark" '% #'mean-with-nil #'<-with-nil)
                     (list "Note"))
                    users
                    #'(lambda(user)
                        (let* ((state (user-state article user))
                               (knowledge
                                (state-value
                                 (docutils.assessment::name assessment)
                                 state))
                               (*current-user* user)) ;; needed so correctly do deadlines
                        (list
                         (clews.assessment:timetaken knowledge)
                         (clews.assessment:completed knowledge)
                         (clews.assessment:deadline-date knowledge assessment)
                         (clews.assessment:assessment-mark
                          knowledge assessment)
                         (clews.assessment::assessment-count-p-reason
                          knowledge assessment))))
                    sort)
                  ,(clews.assessment:assessment-detail-statistics
                    (mapcar
                     #'(lambda(user)
                         (state-value (docutils.assessment::name assessment)
                                      (user-state article user)))
                     users)
                    assessment)))
            (assessments article)))))
        (markup::hr)
        ,@(when (children article)
            `(((markup:section :title "For Complete Section")
                ,(users-report
                  (list (list "Mark" '% #'mean-with-nil #'<-with-nil))
                  users
                  #'(lambda(user)
                      (let ((*current-user* user))
                        (list (mark user (cons article (children article))))))
                  sort))))))))

(defmethod actions((article tutorial-article))
  (nconc
    (call-next-method)
    (list
     (make-action :name "assessment" :permission :view
                  :handler #'article-assessment))))


#|
(in-package :clews.articles)
(defvar *u* (setf inet.acl::*current-user*
                  (get-dictionary "willijar" aston::*user-source*)))
(defvar *c* aston::*tutorials*)

(defvar *a* (get-article "modulation-basics" *c*))
(goals *c* *u*)

(defvar *d* (document *a*))
(defvar *q* (first (assessments *a*)))
(defvar *us* (state-value :|error-rate| (user-state *a* *u*)))

(defvar *d* (document *a*))

(defun testit(&optional (id (id *a*) ))
  (setf *a* (get-dictionary id aston::*tutorials*))
  (slot-makunbound *a* 'document)
  (update-instance-from-record *a*)
  (setf *d* (document *a*))
  (with-open-file(os
                  (merge-pathnames #p"/home/willijar/tmp/"
                                   (make-pathname :name id :type "html"))
                  :direction :output
                  :if-exists :overwrite
                  :if-does-not-exist :create)
    (markup::html os *d*))
  (values (setf *d* (document *a*))
          (prog1
              (errors *a*)
            (princ (first (errors *a*))))))

|#

#|

% dump routines to dump page fragments as html files and to "recreate" images in another directory

(defvar *media-server*
  (make-instance
   'jarw.media:media-server
   :path "/home/willijar/tmp/images/"))

(defvar *tutorials*
  (make-instance
   'clews.articles:tutorial-collection
   :id :tutorials
   :acl '((:view . (:all))
          (:edit . ("willijar"))
          (:admin . ("willijar")))
   :class 'clews.articles:tutorial-article
   :path (translate-logical-pathname #p"clews:tutorials;")
   :file-type "rst"))

(defun do-one(name &optional (article (get-dictionary name *tutorials*)))
  (let ((jarw.media:*media-server* *media-server*)
        (jarw.media:*media-base-url* "/x-media-base/")
        (inet.acl::*current-user*
         (get-dictionary "willijar" aston::*user-source*)))
    (multiple-value-bind(article-head article-body) (html-parts article)
      article-body)))


(defun do-all()
(let ((jarw.media:*media-server* *media-server*)
      (jarw.media:*media-base-url* "/x-media-base/")
      (inet.acl::*current-user*
       (get-dictionary "willijar" aston::*user-source*)))
  (map-dictionary
   #'(lambda(name article)
       (princ name)
       (multiple-value-bind(article-head article-body) (html-parts article)
         (with-open-file(os
                         (merge-pathnames name
                                          (make-pathname :directory "/home/willijar/tmp/html" :type "html"))
                         :direction :output
                         :if-does-not-exist :create
                         :if-exists :overwrite)
           (write-string article-body os)))
       (terpri))
   *tutorials*)))





|#