;;;; Adaptive Tutor application class for CLEWS - deals with web interface
;;;; Copyright (C) 2002-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; $Id: adaptive-tutor-handlers.lisp,v 1.1 2006/07/30 17:41:07 willijar Exp $

(in-package :clews.adaptive-tutor)

(defun concept-list-item(tutor id user &optional this
                         &key with-summary (relative "") prefix suffix)
  "returns html list item entry for given concept"
  (let* ((concept (concept tutor id :if-not-exist :ignore))
         (assessment (assessment concept)))
    (if  concept
         `((li
            :class
            ,(format
              nil "~A-~A"
              (if (direct-children concept) "folder" "document")
              (cond
                ((eq concept this) "this")
                ((completed-p user tutor concept)
                 (if assessment "completed-q" "completed"))
                ((incomplete-prerequisites user tutor concept)
                 (if assessment "notok-q" "notok"))
                (t (if assessment "ok-q" "ok")))))
           ,prefix
           ((a :href ,(format nil "~A~A" relative id)
             :title ,(concept-title concept))
            ,(concept-title concept))
           ,(let ((u (concept-understanding user tutor concept)))
                 (when u
                   (format nil " (~4,1F%~:[~; | ~:[N/A~;~4,1F%~]~])"
                           (* 100 u)
                           assessment
                           (assessment-feedback-p (knowledge user concept)
                                                  assessment)
                           (* 100 (concept-assessment-mark user concept)))))
           ,@(when (and with-summary
                        (or (stringp with-summary)
                            (property concept :summary)))
                   `((br) ,(if (stringp with-summary) with-summary
                               (property concept :summary))))
           ,@suffix)
         '((li :class :error)
           (format nil "Concept ~A not found - please notify the web master"
            id) ))))

(defun concept-recommendation(tutor concept user-data request)
  (declare (ignore request))
  (let ((next-concept (next-concept user-data tutor concept)))
    `(((section :title "Recommendation")
       ,@(if next-concept
             `((p "The system recommends that you should next
proceed to:")
               ((ul :escape nil)
                ,(concept-list-item tutor (concept-id next-concept) user-data
                                    nil :with-summary t)))
             `(p "The system has no further recommendations for you to
study at the moment. Why not choose a new goal from the
directory."))))))

(defun assessment-status(tutor user request)
  "Provide a report on assessed concepts"
  (declare (ignore request))
  (let* ((user-data (user-component-properties tutor user))
         (now (get-universal-time))
         (interval (property user-data :deadlines 1209600))
         (future (+ now interval))
         (past (- now interval))
         (todo ())
         (done ())
         (missed ()))
    (dolist (concept (assessment-goals tutor user))
      (let* ((knowledge (knowledge user-data concept))
             (deadline (deadline-date knowledge (assessment concept)))
             (completed (completed knowledge))
             (item (concept-list-item
                    tutor (concept-id concept) user-data nil
                    :with-summary
                    (when deadline
                      (format-time nil deadline :fmt :short )))))
        (cond
          (completed
           (when (> completed past) (push (cons completed item) done)))
          ((and deadline (<= deadline future))
           (if (< deadline now)
               (push (cons deadline item) missed)
               (push (cons deadline item) todo))))))
    (when (or todo done missed)
      `(((section :title "Assessment Status" :level 3 :escape nil)
         (p "Assessments with deadlines between "
          ,(format-time nil past :fmt :short )
          " and "
          ,(format-time nil future :fmt :short )
          " are listed. You can change this in your preferences.")
         (table
          (tr ,(when todo
                     `((td :valign "top")
                       ((section :title ,(format nil "Todo (~D)" (length todo)))
                        (ul ,@(mapcar #'cdr (sort (reverse todo)
                                                  #'< :key #'car))))))
              ,(when missed
                     `((td :valign "top")
                       ((section  :title ,(format nil "Missed (~D)"
                                                  (length missed)))
                        (ul ,@(mapcar #'cdr (sort missed #'< :key #'car))))))
              ,(when done
                     `((td :valign "top")
                       ((section  :title ,(format nil "Completed (~D)"
                                                  (length done)))
                        (ul ,@(mapcar #'cdr (sort done #'> :key #'car)))))))))))))

(defmethod home-handler((tutor adaptive-tutor) request rest)
  (declare (ignore rest))
  (let* ((user (remote-user request))
         (user-data (user-component-properties tutor user))
         (completed-goals '())
         (incomplete-goals '()))
    (leave-concept user-data tutor)
    (let ((c (next-required-concept user-data tutor)))
      (when c (return-from home-handler (redirect request (concept-id c)))))
    (dolist (concept (goals tutor user))
      (let ((concept-item (concept-list-item
                           tutor (concept-id concept) user-data nil
                           :with-summary t)))
        (if (completed-p user-data tutor concept)
            (push concept-item completed-goals)
            (push concept-item incomplete-goals))))
    `(html (head (title "Adaptive Tutor Home"))
      (body
       (navbar ,(menus tutor))
       ((section :title "Adaptive Tutor Home")
        ,@(concept-recommendation tutor nil user-data request)
        (table
         (tr
          ,(when incomplete-goals
                 `((td :valign "top")
                   ((section :title "Goals")
                    (p "Your tutorial goals are as follows:")
                    ((ul :escape nil)
                     ,@incomplete-goals))))
          ,(when completed-goals
                 `((td :valign "top")
                   ((section :title "Achievements")
                    (p "Your completed tutorial goals are as follows:")
                    ((ul :escape nil)
                     ,@completed-goals))))))
        ,@(assessment-status tutor user request))))))

(defmethod bibliography-handler((tutor adaptive-tutor) request rest)
  (declare (ignore request))
  (let ((letters "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
        (matches '())
        (bibliography (bibliography tutor))
        (len (length rest)))
    (when (> len 0)
      (maphash
       (if (string-equal rest "*")
           #'(lambda(name value)
               (declare (ignore value))
               (when (not (find (elt name 0) letters :test #'char-equal))
                 (push name matches)))
           #'(lambda(name value)
               (declare (ignore value))
               (when (string-equal (string name) rest
                                   :end1 (min len (length (string name))))
                 (push name matches))))
       bibliography))
    `(html (head (title "Bibliography"))
      (body
       (navbar ,(menus tutor) :on-url "bibliography/" :relative "../")
       (h1 "Bibliography Entries for \"" ,rest "\"")
       (p "Select the prefix letter for the bibliography entries you want listed.")
       (p ,@(map 'list #'(lambda(c)
                           `((a :href ,(string c)) ,(string c) " "))
                 letters)
        ((a :href "*" :title "Others") "Others"))
       ,(if matches
            `((dl :class "bibliography")
              ,@(mapcan #'(lambda(r)
                            `((dt ,r)
                              (dd ,@(gethash r bibliography))))
                        (sort matches #'string-lessp)))
            `(p "There are no bibliography entries prefixed with \""
              ,rest "\""))))))

(defmethod directory-handler((tutor adaptive-tutor) request letter)
  (when (= 0 (length letter)) (setq letter "A"))
  (let* ((letters "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
         (len (length letter))
         (user (remote-user request))
         (matches '())
         (user-data (user-component-properties tutor user)))
    (map-dictionary
     (if (string-equal letter "*")
         #'(lambda(name value)
             (declare (ignore name))
             (when (not (find (elt (concept-title value) 0) letters
                              :test #'char-equal))
               (push value matches)))
         #'(lambda(name value)
             (declare (ignore name))
             (when (string-equal (concept-title value) letter
                                 :end1 (min len (length (concept-title value))))
               (push value matches))))
     (concept-map tutor))
    `(html
      (head (title "Concept Directory"))
      (body
       (navbar ,(menus tutor) :on-url "directory" :relative "../")
       ((section :title "Directory of Concepts")
        (p "Welcome " (b ,(username user)))
        (p "There are " ,(dictionary-count (concept-map tutor))
           " articles in total.")
        (p ,@(map 'list #'(lambda(c)
                            `((a :href ,(string c)) ,(string c) " "))
                  letters)
           ((a :href "*" :title "Others") "Others") )
        ((ul :escape nil)
         ,@(mapcar
            #'(lambda(c) (concept-list-item tutor (concept-id c) user-data
                                            nil :relative "../"))
            (sort matches #'string-lessp :key #'concept-title))))))))

(defmethod search-handler((tutor adaptive-tutor) request rest)
  (declare (ignore rest))
  (let ((search-term (car (form-values  "search-term" request)))
        (user (remote-user request)))
    `(html
      (head (title "Tutorial Search"))
      (body
       (navbar ,(menus tutor) :on-url "search")
       (h1 "Search")
       ((form :method "POST")
        (p "Enter Search Pattern: "
           ((input :type "text" :size 20 :name "search-term"
                   :value ,(or search-term "")))
           ((input :type "submit" :name "keyword" :value "Keyword Search" ))
           ((input :type "submit" :name "pattern" :value "Pattern Search" )))
        ,(if search-term
             `((ol :escape nil)
               ,@(or
                  (if (form-values "pattern" request )
                      (handler-case
                          (let ((matcher (regex:compile-str search-term)))
                            (mapcan
                             #'(lambda(concept)
                                 (let ((content
                                        (concept-content tutor concept
                                                         user request)))
                                   (when (and
                                          content
                                          (regex:scan-str
                                           matcher
                                           (strip-markup
                                            nil
                                            `((section :title
                                               ,(concept-title concept))
                                              ,@content))))
                                     (list (concept-list-item tutor
                                                              (concept-id concept)
                                                              user nil
                                                              :with-summary t)))))
                             (concepts tutor)))
                        (error (condition)
                          (list 'p (format nil
                                           "Error in regular expression: ~S"
                                           condition))))
                      (let ((threshold
                             (user-preference :search-threshold tutor user 0.2)))
                        (mapcan
                         #'(lambda(entry)
                             (let ((concept (car entry)))
                               (when (> (cdr entry) threshold)
                                 (list
                                  (concept-list-item
                                   tutor
                                   (concept-id concept)
                                   user
                                   nil
                                   :prefix
                                   (format nil "~5,1F " (* 100 (cdr entry)))
                                   :with-summary t)) )))
                         (sort (search-index (keyword-index tutor) search-term)
                               #'> :key #'cdr))))
                  '((li "No matches Found"))))))))))

(defmethod application-handler(request
                               rest
                               (tutor adaptive-tutor)
                               (method (eql #'response-handler))
                               display-plugins-p role)
  "We set up html document context based on concept-id for response handler"
  (declare (ignore request display-plugins-p role rest))
  (let ((*path-to-root* "."))
    (declare (special *path-to-root*))
    (call-next-method)))

(defmethod response-handler((tutor adaptive-tutor) request path)
  (when (or (= (length path) 0) (string-equal path "."))
    (return-from response-handler (home-handler tutor request path)))
  (let* ((user (remote-user request))
         (path (subseq path 0 (position #\? path)))
         (parts (split-string  path 2 #(#\/ #\.)))
         (concept (concept tutor (first parts) :if-not-exist :ignore))
         (action (second parts)))
    (declare (special *path-to-root*))
    (setf *path-to-root* (if (find #\/ path) ".." "."))
    (if concept
        (with-new-document (first parts)
          (with-tutor (tutor)
            (setf (property (user-component-properties tutor user) :concept)
                  (first parts))
            (cond ((not action)
                   (concept-page tutor concept user request))
                  ((string= action "assessment")
                   (assessment-handler tutor concept user request))
                  ((string= action "feedback")
                   (feedback-handler tutor concept user request))
                  #+nil((string= action "reset")
                        (reset-handler tutor concept user request))
                  ((string= action "expanded")
                   (expanded-handler tutor concept user request))
                  ((string= action "pdf")
                   (pdf-handler tutor concept user request))
                  ((string= action "tex")
                   (latex-handler tutor concept user request))
                  ((string= action "outcomes")
                   (outcomes-handler tutor concept user request))
                  ((string= action "marks")
                   (marks-handler tutor concept user request))
                  (t (cons :not-found (format nil "Unknown action ~S" action))))))
        (cons :not-found
              (format nil "Concept ~S not found. If you have arrived
here from a link inside the tutorial system please inform the web
administrator of the url of both the referrer page and this page."
                      (first parts))))))

(defmethod render-page((tutor adaptive-tutor) stream markup)
  (declare (ignore stream markup))
  (declare (special *path-to-root*))
  (with-tutor(tutor) (call-next-method)))

(defmethod concept-page((tutor adaptive-tutor) (concept concept)
                        user request)
  "Construct page on the tutor for the given user, concept and request"
  (let* ((user-data (user-component-properties tutor user)))
    (record-visit user-data tutor concept)
    (if (and (property concept :display-self) (functionp (content concept)))
        (funcall (content concept) tutor concept user request)
        `(html (head (title ,(concept-title concept))
                ,@(let ((refresh (user-preference :refresh tutor user nil)))
                       (when refresh
                         `(((meta :HTTP-EQUIV "refresh" :CONTENT ,refresh))))))
          (body
           (navbar ,(menus tutor)
            :on-url ,(concept-id concept))
           ((section :title ,(concept-title concept))
            ,@(when (property concept :summary)
                    `((p (em ,(property concept :summary)))))
            ,@(concept-prerequisites tutor concept user-data request)
            ,@(concept-navigation tutor concept user-data request)
            (section
             ,@(concept-content tutor concept user-data request)
             (footnotes))
            ,@(concept-assessment tutor concept user-data request)
            ,@(concept-recommendation tutor concept user-data request)))))))

(defun concept-prerequisites(tutor concept user request)
  (declare (ignore request))
  (let ((prerequisites (incomplete-prerequisites user tutor concept)))
    (when prerequisites
      `((hr)((p :class "warning") "The system assumes you have
satisfied some prerequisites to successfully work at this page. If you
do not possess sufficient knowledge of the following topics you are
strongly advised to work at them before continuing here.")
        ((ul :class "warning" :escape nil)
         ,@(mapcar #'(lambda (c)
                       (concept-list-item tutor (concept-id c)
                                          user concept))
                   prerequisites)) (hr)))))

(defun concept-navigation(tutor concept user request)
  (declare (ignore request))
  (let ((ancestor (concept tutor (first (property user :goals))
                           :if-not-exist :ignore)))
    (labels ((tree-view (list)
               (when list
                 `((ul :escape nil)
                   ,(concept-list-item
                     tutor (concept-id (first list))
                     user concept :suffix (mapcar #'tree-view (rest list)))
                   ))))
      (list (tree-view (tree-heirarchy ancestor concept))) )))

(defun assessment-status-table(knowledge assessment)
  `((table :class "assessment-status")
    ,@(mapcar
       #'(lambda(item)
           (when (cdr item)
             `(tr ((th :align "right") ,(car item))
               (td ,(cdr item)))))
       (assessment-status-long knowledge assessment))))

(defun concept-assessment(tutor concept user-data request)
  "Template for presenting and http interaction with assessments"
  (declare (ignore tutor request))
  (let ((knowledge (knowledge user-data concept))
        (assessment (assessment concept)))
    (when assessment
      `(((section :title "Assessment")
         ,(when (timelimit knowledge assessment)
                " The assessment has a timelimit so do not start it until your are sure you have understood this concept.")
         ,(assessment-status-table knowledge assessment)
         ,(when (assessment-attempt-p knowledge assessment)
                `(p
                  ,@(when-bind (reason (assessment-should-not-attempt-reason
                                        knowledge assessment))
                               (list reason "Only click the link to start
this assessment if you are sure you know what you are doing. "))
                  ((a :href ,(concatenate 'string
                                          (concept-id concept) "/assessment"))
                   "Start the assessment.")))
         ,(when (assessment-feedback-p knowledge assessment)
                `(p ((a :href ,(concatenate 'string
                                            (concept-id concept) "/feedback" ))
                     "Feedback on your answers")))
                                        #|,(when (assessment-reset-p knowledge assessment)
         `(p ((a :href ,(concatenate 'string
         (concept-id concept) "/reset"))
         "Reset this assessment")))|# )))))

(defun countdown-html(remaining)
  "Returns javascript to display countdown for remaining seconds"
  (format nil "
<form name=\"countdown\">Time remaining
<input type=\"text\" name=\"clock\" align=\"right\" size=5 readonly style=\"color:red;text-align:right;border-style:none;\"> minutes
</form>
<script language=javascript>
<!--
endTime=new Date()
function clockTick()
{
currentTime = new Date();
document.countdown.clock.value = Math.round((endTime-currentTime)/60000+~D);
setTimeout(\"clockTick()\", 10000);
}
clockTick();
-->
</script>" remaining))

(defmethod assessment-handler((tutor adaptive-tutor) concept user request)
  (let* ((user-data (user-component-properties tutor user))
         (knowledge (knowledge user-data concept))
         (assessment (assessment concept)))
    (multiple-value-bind (access-p reason)
        (assessment-attempt-p knowledge assessment)
      (unless access-p (throw 'response (cons :forbidden reason))))
    (values
     `(html
       (head (title "Assessment for " ,(concept-title concept)))
       (body
        ((section :title
                  ,(concatenate 'string
                                "Assessment of " (concept-title concept)
                                " for " (username user)))
         ,(assessment-status-table knowledge assessment)
         ,(when (time-remaining knowledge assessment)
                `((div :escape nil :align "center")
                  ,(countdown-html (/ (time-remaining knowledge assessment) 60))))
         ,(when-bind (reason (assessment-should-not-attempt-reason
                              knowledge assessment))
                     (list '(p :class :error) reason
                           "Only submit if you are sure you know what
you are doing."))
         ,(assessment-attempt-markup knowledge assessment request)
         ,(when (assessment-feedback-p knowledge assessment)
                `(p ((a :href "feedback")
                     "View Feedback on your assessment submission ")))
         (p "Back to " ((a :href ,(concatenate 'string "../" (concept-id concept)))
                        ,(concept-title concept)) " topic." ))))
     nil t)))

(defmethod feedback-handler((tutor adaptive-tutor) concept user request)
  (let* ((user-data (user-component-properties tutor user))
         (knowledge (knowledge user-data concept))
         (assessment (assessment concept)))
    (multiple-value-bind (access-p reason)
        (assessment-feedback-p knowledge assessment)
      (unless access-p (throw 'response (cons :forbidden reason))))
    (values
     `(html
       (head (title "Assessment feedback for " ,(concept-title concept)))
       (body ((section :title ,(concept-title concept))
              ,(assessment-status-table knowledge assessment)
              ,(assessment-feedback-markup knowledge assessment request)
                                        #|,(when (assessment-reset-p knowledge assessment)
              '(p ((a :href "reset") "Reset this assessment")))|#
	       (p "Back to " ((a :href ,(concatenate 'string "../" (concept-id concept)))
			      ,(concept-title concept)) " topic" ))))
      nil t)))

#+nil(defmethod reset-handler((tutor adaptive-tutor) concept user request)
  (let* ((user-data (user-component-properties tutor user))
	 (knowledge (knowledge user-data concept))
	 (assessment (assessment concept)))
    (multiple-value-bind (reset-p reason)
	(assessment-reset-p knowledge assessment)
      (unless reset-p (throw 'response (cons  401 reason))))
    (reset-assessment knowledge assessment)
    `(html
      (head (title "Clear Assessment"))
      (body ((section :title "Clear Assessment")
	     (p "Assessment for \"" ,(concept-title concept)
		"\" has been cleared.")
	     (p ((a :href "assessment") "Redo this assessment"))
	     (p "Back to "
		((a :href ,(concatenate 'string "../" (concept-id concept)))
		 ,(concept-title concept)) " topic" ))))))


(defun concept-expanded-markup(tutor concept user request
                               &key with-assessments)
  `((section :title ,(concept-title concept) :id ,(concept-id concept))
    ,@(when (property concept :summary)
            `((p (em ,(property concept :summary)))))
    ,@(concept-content tutor concept user request)
    ,@(mapcar #'(lambda(c)
                  (concept-expanded-markup
                   tutor c user request :with-assessments with-assessments))
              (direct-children concept))))

(defmethod expanded-handler((tutor adaptive-tutor) concept user request)
  (values
   `(html (head (title ,(concept-title concept)))
     (body ,(concept-expanded-markup tutor concept user request)))
   nil t))

(defun expanded-for-latex(tutor concept user request)
  `(html (head
          (title ,(concept-title concept))
          (markup::author ,(property concept :author))
          ,@(when-bind(date (property concept :created))
                      `((markup::date ,date)) ))
    (body
     ,@(when-bind(abstract (property concept :summary))
                 `((markup::abstract ,abstract)))
     (markup::tableofcontents)
     ,@(concept-content tutor concept user request)
     ,@(mapcar #'(lambda(c)
                   (concept-expanded-markup
                    tutor c user request :with-assessments nil))
               (direct-children concept)))))

(defmethod pdf-handler((tutor adaptive-tutor) concept user request)
  (with-tutor (tutor)
    (make-instance
     'response
     :content-type "application/pdf"
     :content (with-output-to-string(os)
                (pdf os (expanded-for-latex tutor concept user request))))))

(defmethod latex-handler((tutor adaptive-tutor) concept user request)
  (with-tutor (tutor)
    (make-instance
     'response
     :content-type "text/x-tex"
     :content (with-output-to-string(os)
                (latex os (expanded-for-latex tutor concept user request))))))

(defmethod user-component-preferences((self adaptive-tutor) user)
  (declare (ignore user))
  (append (call-next-method)
          '((:refresh
             :text
             "How often do you want the tutorial pages to automatically refresh?
Every"
             :markup ((mcq :style :dropdown)
                      (30 . "30 seconds")
                      (60 . "1 minute")
                      (120 . "2 minutes")
                      (600 . "5 minutes")
                      (nil . "Never"))
             :type (integer :min 10 :max 86400 :nil-allowed t)
             :default nil)
            (:deadlines
             :text "How far ahead do you want to see deadlines?"
             :markup ((mcq :style :dropdown)
                      (604800 . "1 Week")
                      (1209600 . "2 Weeks")
                      (2419200 . "4 Weeks")
                      (7257600 . "12 Weeks"))
             :type (integer :min 604800)
             :default 1209600)
            (:search-threshold
             :text "Set the search term threshold. A larger number means articles must be more similar to the search term to be listed and reduces the number of entries listed when searching."
             :markup ((mcq :style :dropdown)
                      (0.1 . "10%")
                      (0.2 . "20%")
                      (0.4 . "40%")
                      (0.8 . "80%")
                      (0.9 . "90%"))
             :type (number :min 0.0 :max 1.0)
             :default 0.1))))
