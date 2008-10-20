;;;; Adaptive Tutor application class for EWAS - deals with web interface
;;;; Copyright (C) 2002-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; $Id: analysis-handlers.lisp,v 1.1 2006/07/30 17:40:40 willijar Exp $

(in-package :clews.adaptive-tutor)

(defun groups-form(app &optional extra-fields)
  (let* ((roles (sort (roles (users app)) #'string<)))
    (flet ((roles-maq(name)
             `((maq
                :name ,name
                :value ,(first (last roles))
                :style :dropdown
                :datatype symbol
                :size ,(min (length roles) 5))
               ,@roles)))
      `((form :method :post)
        ((table :cellspacing 5)
         (tr (th "Student Set") (th "Normalisation Set"))
         (tr ((td :align :center) ,(roles-maq :students))
          ((td :align :center) ,(roles-maq :normalisation))))
        ,@extra-fields
        ((input :type "submit" :name "Submit" :value "Analyse"))))))

(defun student-detail-cells(student)
  "markup of table cells describing a student"
  `((td ,(username student))
    (td ,(property student :studentid))
    (td ,(property student :lastname)
     ,@(when-bind (f (property student :firstname))
                  (list ", " f)))))

(declaim (inline %fmt))
(defun %fmt(v) "Output format for %" (when v (format nil "~,1F%" v)))

(defmethod outcomes-handler((tutor adaptive-tutor) concept user request)
  (declare (ignore user))
  (let ((outcomes-for (outcomes-for tutor concept)))
    (labels ((link-markup(entry)
               (let ((concept (car entry)) (weighting (cdr entry)))
                 `((li :class
                    ,(format nil "~:[document~;folder~]-~:[ok~;ok-q~]"
                             (direct-children concept)
                             (assessment concept) ))
                   ,@(let ((outcome-for (find concept outcomes-for :key #'car)))
                          (when outcome-for
                            `((b ,(format nil "~5,3F " (cdr outcome-for))))))
                   ,(format nil "~4,2F " weighting)
                   ((a :href
                     ,(concatenate 'string "../../"
                                   (concept-id concept)
                                   "/outcomes/"))
                    ,(concept-title concept)))))
             (list-markup(tree)
               (let ((parent (first tree)) (children (rest tree)))
                 `(,@(link-markup parent)
                   ,@(when children
                           `((ol ,@(mapcar
                                    #'list-markup children))))))))
      (values
       `(html
         (head (title "Outcomes analysis for " ,(concept-title concept)))
         (body ((section :title ,(concept-title concept))
                ((section :title "Properties of this node")
                 (table
                  ,@(mapcar #'(lambda(key) `(tr ((th :align "right")
                                                 ,(string key))
                                             (td ,(property concept key))))
                            (sort (property-keys concept) #'string<))))
                ,@(when (assessment concept)
                        `(((section :title "Assessment Metadata")
                           (table
                            ,@(mapcar
                               #'(lambda(item)
                                   `(tr ((th :align "right")
                                         ,(string (car item)))
                                     (td ,(cdr item))))
                               (assessment-metadata (assessment concept)))))))
                ((section :title "Prerequisites for this node")
                 (ol ,@(mapcar #'link-markup (prerequisites concept) )))
                ((section :title "Outcomes from this node")
                 (ol ,(list-markup
                       (remove-circularity
                        (cons concept (direct-outcome concept concept))
                        #'(lambda(entry) (direct-outcomes (car entry)))))))
                ((section :title "Outcomes to this node")
                 (ol ,(list-markup
                       (remove-circularity
                        (cons concept (direct-outcome concept concept))
                        #'(lambda(entry)
                            (direct-outcomes-for tutor (car entry))))))
                 (table (tr (td "Accumulated Weighting: ")
                            (td ,(accumulate-concept-scores
                                  tutor concept :outcomes outcomes-for)))
                        (tr (td "Accumulated Assessments Weighting:")
                            (td ,(accumulate-concept-scores
                                  tutor concept :test #'assessment
                                  :outcomes outcomes-for)))))
                (p "Back to "
                   ((a :href ,(concatenate 'string "../../"
                                           (concept-id concept)))
                    ,(concept-title concept)) " topic" ) ))) nil t))))

(defmethod marks-handler((app adaptive-tutor) concept user request)
  (unless (has-permission :tutor app user)
    (throw 'response
      (cons :forbidden "You do not have the authority to view the marks.")))
  (flet ((userrecords(usernames)
           (mapcar #'(lambda(id) (get-dictionary id (users app)))
                   usernames)))
    (let* ((form (groups-form
                  app
                  `((p "Plagiarism - Distance Metric:"
                     ((input :name :distance :value 2 :size 2
                             :datatype (integer :min 0)))
                     " Time Interval (minutes):"
                     ((input :name :delay :value 30 :size 3
                             :datatype (number :min 0))))) ) )
           (form-data
            (if (submitted-action form request)
                (setf (property (user-component-properties app user)
                                :groups-default) (form-data form request))
                (property (user-component-properties app user)
                          :groups-default)) )
           (students  (userrecords
                       (sort (get-users (getf form-data :students)
                                        (users app))
                             #'string<) ))
           (normalisation (or (userrecords
                               (get-users (getf form-data :normalisation)
                                          (users app)))
                              students))
           (weighting (reduce #'accumulate-weighting
                              (mapcan #'(lambda(item)
                                          (when (assessment (car item))
                                            (list (cdr item))))
                                      (outcomes-for app concept)))))
      (declare (ignore weighting))
      (values
       `(html
         (head (title "Cohort analysis for " ,(concept-title concept)))
         (body
          ((section :title ,(format nil "Marks analysis for ~A"
                                    (concept-title concept)))
           ,(markup-form form form-data)
           ,@(when students
                   `(((section :title "Concept")
                      ,(cohort-table-concept app concept students normalisation))
                     ,@(when (assessment concept)
                             `(((section :title "Assessment")
                                ,(cohort-table-assessment app concept
                                                          students normalisation))
                               ((section :title "Assessment Detail")
                                ,(cohort-table-assessment-detail
                                  app concept students normalisation))
                               ((section :title "Potential Plagiarism")
                                ,(cohort-table-plagiarism
                                  app concept students
                                  (getf form-data :distance 2)
                                  (getf form-data :delay 30) )))))))
          (p ((a :href "../") "Display page") (br)
           ((a :href "../outcomes/") "Display Metadata")) ))
       nil t))))

(defun report-time(tm)
  (when tm (format-time nil tm :fmt :short)))

(defun cohort-table-concept(app concept students normalisation-students)
  "Present table of information on concept - its completion and understanding"
  (flet ((exclude-0(marks)
           (mapcan #'(lambda(mark)
                       (when (and mark (not (= mark 0))) (list mark)))
                   marks)))
    (let ((row 0)
          (total-time 0)
          (total-completion 0)
          (total-understanding 0)
          (normalised-marks (normalised-concept-marks app concept
                                                      students normalisation-students))
          (concept-marks (concept-marks app concept students))
          (n (length students)))
      `((table :cellspacing 0 :cellpadding 2)
        (caption "Table of concept related information")
        ((tr :class :header)
         (th "Username") (th "SUN") (th "Name")
         (th "Last Visit") (th "Time") (th "Completion")
         (th "Understanding") (th "Normalised Mark") (th "Static Mark"))
        ,@(mapcar
           #'(lambda(student mark cmark)
               (let* ((student-data (user-component-properties app student))
                      (knowledge (knowledge student-data concept))
                      (rowclass (if (evenp (incf row)) :even :odd))
                      (time (or (property knowledge :time-spent) 0))
                      (completion
                       (* 100 (concept-completion student-data app concept)))
                      (understanding
                       (* 100 (concept-understanding student-data app concept))))
                 (incf total-time time)
                 (incf total-completion completion)
                 (incf total-understanding understanding)
                 `((tr :class ,rowclass)
                   ,@(student-detail-cells student)
                   (td ,(report-time (property knowledge :last-visit-time)))
                   ((td :align "right") ,time)
                   ((td :align "right")
                    ,(when (completed-p student-data app concept) "C")
                    ,(%fmt completion))
                   ((td :align :right) ,(%fmt understanding))
                   ((td :align :right) ,(%fmt (* 100 mark)))
                   ((td :align :right) ,(%fmt (* 100 cmark))))))
           students normalised-marks concept-marks)
        ((tr :class :header) ((td :colspan 4) "Mean (excluding 0)")
         ((td  :align "right") ,(floor (/ total-time n)))
         ((td :align "right") ,(%fmt (/ total-completion n)))
         ((td :align "right") ,(%fmt (/ total-understanding n)))
         ((td :align "right")
          ,(%fmt (* 100 (mean (exclude-0 normalised-marks)))))
         ((td :align "right")
          ,(%fmt (* 100 (mean (exclude-0 concept-marks))))))
        ((tr :class :header) ((td :colspan 7) "Stddev (excluding 0)")
         ((td :align "right")
          ,(%fmt (* 100 (stddev (exclude-0 normalised-marks)))))
         ((td :align "right")
          ,(%fmt (* 100 (stddev (exclude-0 concept-marks))))))))))

(defun cohort-table-assessment(app concept students normalisation-students)
  "Present information relating to assessment - assessment mark"
  (let ((row 0)
        (marks '())
        (assessment (assessment concept)))
    (unless assessment (return-from cohort-table-assessment))
    `((table :cellspacing 0 :cellpadding 2)
      (caption "Table of assessment related information")
      ((tr :class :header)
       (th "Username") (th "Studentid") (th "Name")
       (th "Mark") (th "Time taken") (th "started") (th "Completed"))
      ,@(mapcan
         #'(lambda(student)
             (let* ((student-data (user-component-properties app student))
                    (knowledge (knowledge student-data concept))

                    (rowclass (if (evenp (incf row)) :even :odd)))
               `(((tr :class ,rowclass)
                  ,@(student-detail-cells student)
                  ((td :align "right")
                   ,(when assessment
                          (let ((m (assessment-mark knowledge assessment)))
                            (when m (push m marks)
                                  (%fmt (* 100 m))))))
                  ((td :align "right") ,(timetaken knowledge))
                  ((td :align "right")
                   ,(report-time (started knowledge)))
                  ((td :align "right")
                   ,(report-time (completed knowledge)))
                  ,(when-bind (status (assessment-count-p-reason
                                       knowledge assessment))
                              `((tr :class ,rowclass)
                                (td)
                                ((td :align "left" :colspan 9)
                                 ,status)))))))
         students)
      ((tr :class :header)
       (th "Averages")
       ((td :colspan 2))
       ((td  :align "right")  ,(%fmt (* 100 (mean marks))) )
       ((td :colspan 2)) )
      ((tr :class :header) (th "Weighting")
       (td ,(assessment-normalisation-weighting
             (mapcar #'(lambda(student)
                         (knowledge (user-component-properties app student)
                                    concept))
                     normalisation-students)
             assessment) )))))

(defun cohort-table-assessment-detail(app concept students normalisation)
  (when (assessment concept)
    (flet ((knowledge-set(students)
             (mapcar #'(lambda(student)
                         (knowledge (user-component-properties app student)
                                    concept))
                     students)))
      (let* ((knowledge-set (knowledge-set students))
             (normalisation-set (or (knowledge-set normalisation)
                                    knowledge-set)))
        (assessment-detail-statistics knowledge-set (assessment concept)
                                      normalisation-set)))))

(defun cohort-table-plagiarism(app concept students
                               &optional (distance 2) (delay 60))
  (let ((completed (make-hash-table)))
    `(table
      (tr (th "Student1") (th "Student 2") (th "Distance Metric")
       (th "Interval (mins)"))
      ,@(mapcan
         #'(lambda(student1)
             (setf (gethash student1 completed) t)
             (let ((k1 (knowledge (user-component-properties app student1)
                                  concept)))
               (when (clews.assessment::completed k1)
                 (let ((candidates
                        (mapcan
                         #'(lambda(student2)
                             (unless (gethash student2 completed)
                               (let ((k2
                                      (knowledge (user-component-properties
                                                  app student2)
                                                 concept)))
                                 (when (clews.assessment::completed k2)
                                   (let ((d (assessment-distance-metric
                                             k1 k2 (assessment concept)))
                                         (interval
                                          (min
                                           (abs
                                            (- (clews.assessment::started k1)
                                               (clews.assessment::started k2)))
                                           (abs
                                            (- (clews.assessment::completed k2)
                                               (clews.assessment::completed
                                                k1))))))
                                     (when (and (<= interval (* 60 delay))
                                                (<= d distance))
                                       (list (list student2 d
                                                   (/ interval 60.0)))))))))
                         students)))
                   (when candidates
                     (mapcar
                      #'(lambda(candidate)
                          `(tr
                            (th ,(username student1))
                            (td ,(username (first candidate)))
                            ((td :align "right") ,(second candidate))
                            ((td :align "right")
                             ,(format nil "~,2f" (third candidate)) )))
                      candidates))))))
         students))))

(defun extract-assessments(app concept user)
  (let* ((user (if (stringp user) (get-dictionary user (users app)) user))
         (concept (if (stringp concept) (concept app concept) concept))
         (assessment (assessment concept)))
    (cons
     (when assessment
       `((section :title ,(concept-title concept))
         ,@(let* ((user-data (user-component-properties app user))
                  (knowledge (knowledge user-data concept)))
                 (list
                  (assessment-status-table knowledge assessment)
                  (if (assessment-feedback-p knowledge assessment)
                      (assessment-feedback-markup knowledge assessment nil)
                      (list 'p
                            (assessment-feedback-p-reason knowledge
                                                          assessment)))))))
     (mapcan #'(lambda(concept)
                 (extract-assessments app concept user))
             (direct-children concept)))))

(defun make-nav(app concept)
  "Creates a hashtable containing the naviagation information
for a heirarchicy of concepts"
  (let ((nav (make-hash-table)))
    (labels ((really-make-navigation(parent)
               (do* ((children (direct-children parent) (rest children)))
                    ((not children))
                 (let* ((child (first children))
                        (next (second children)))
                   (setf (getf (gethash child nav) :up) parent)
                   (when next
                     (setf (getf (gethash child nav) :next) next)
                     (setf (getf (gethash next nav) :prev) child))
                   (really-make-navigation child)))))
      (really-make-navigation
       (if (stringp concept) (concept app concept) concept)))
    nav))

(defun concept-anchor(concept &key rel key)
	`((a :href ,(format nil "~A.html" (concept-id concept))
		 ,@(when rel (list :rel rel))
		 ,@(when key (list :accesskey key)))
		,(concept-title concept)))

(defun concept-navbar(concept nav)
  (let* ((rec (gethash concept nav)))
    `((div :class "node")
      ((p :escape nil)
       "Node:&nbsp;" ,(concept-anchor concept)
       ,@(mapcan
          #'(lambda(k)
              (let((d (getf rec (first k))))
                (when d
                  (list "," #\Newline
                        (second k) "&nbsp;"
                        (concept-anchor d :rel (third k)
                                        :key (fourth k))))))
          '((:next "Next" "next" "n")
            (:prev "Previous" "previous" "p")
            (:up "Up" "up" "u"))))
      (hr) (br))))

(defun concept-menu(concept)
  (let ((children (direct-children concept)))
    (when children
      (let ((key 0))
        `((ul :class "menu")
          ,@(mapcar
             #'(lambda(child)
                 (list 'li
                       (concept-anchor
                        child
                        :key (when (< key 10) (format nil "~D" (incf key))))))
             children))))))

(defun extract-concepts-html(app concept directory
                             &key (add-navigation t) (add-menus t))
  "Extracts a concept and its children as a static web of one
directory per concept"
  (ensure-directories-exist directory)
  (with-markup-environment
      (:run-external-programs t
                              :rerendered-math nil
                              :references (references app)
                              :bibliography (bibliography app)
                              :destination-path directory
                              :file-prefix ""
                              :ref-suffix ".html"
                              :math-cache (make-hash-table :test #'equal))
    (let* ((concept (if (stringp concept) (concept app concept) concept))
           (nav (make-nav app concept)))
      (labels ((extract-concept(concept)
                 (let ((concept-id (concept-id concept)))
                   (with-open-file
                       (os
                        (merge-pathnames
                         directory
                         (format nil "~A.html" (string-downcase concept-id)))
                        :direction :output
                        :if-does-not-exist :create
                        :if-exists :supersede)
                     (with-markup-environment
                         (:document-name concept-id
                                         :source-path (property concept :source-path))
                       (html
                        os
                        (preprocess-html
                         `(html
                           (head
                            (title ,(concept-title concept)))
                           (body
                            ,(when add-navigation
                                   (concept-navbar concept nav))
                            ((section :title ,(concept-title concept))
                             ,@(when (property concept :summary)
                                     `((p (em ,(property concept :summary)))))
                             ,@(let ((c (content concept)))
                                    (if (functionp c)
                                        '(((p :class :error)
                                           "Unable to convert content"))
                                        c))
                             ,(when add-menus (concept-menu concept))
                             (footnotes)))))))))
                 (dolist (child (direct-children concept))
                   (extract-concept child))))
        (extract-concept concept)))))

(defun concept-topology(app concept)
  "Return a topology tree of concept ids"
  (labels ((get-topology(concept)
             (cons (concept-id concept)
                   (mapcar #'get-topology (direct-children concept)))))
    (get-topology (if (stringp concept) (concept app concept) concept))))

(defun extract-index(app concept directory)
  (labels ((get-menu(concept)
             (let ((children (direct-children concept)))
               `(li ,(concept-anchor concept)
                 ,@(when children
                         `((ul ,@(mapcar #'get-menu children))))))))
    (let* ((concept (if (stringp concept) (concept app concept) concept)))
      (with-open-file (os (merge-pathnames directory "index.html")
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :overwrite)
        (html
         os
         `(html
           (head
            (title ,(concept-title concept)))
           (body
            ((section :title ,(concept-title concept))
             ((section :title "Table of Contents")
              (ul
               ,@(mapcar #'get-menu (direct-children concept))))))))))))