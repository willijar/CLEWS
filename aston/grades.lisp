;;;; $Id: projects.lisp,v 1.1 2005/03/10 19:54:25 willijar Exp $
;;;; Copyright (C) 2003 Dr. John A.R. Williams, J.A.R.Williams@blueyonder.co.uk

;;;; This file is part of the Aston EWAS application configuration

;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2 of the License, or
;;;; (at your option) any later version.

;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.

;;;; You should have received a copy of the GNU General Public License
;;;; along with this program; if not, write to the Free Software
;;;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA
;;;; or retreive a copy from http://www.gnu.org/licenses/gpl.txt

(in-package :aston)

(defun db-connect(db)
    "open a local db connection"
    (clsql:connect
     (list
      (if (member (machine-instance) '("eas-nw709pc01" "spinoza") :test #'equal)
          "cld045026"
          "localhost")
      db
      "willijar"
      (progn
        (format t "Enter Password for ~S database: " db)
        (finish-output)
        (read-line)))
 		 :database-type :postgresql-socket :if-exists :new))

(setq *db* (db-connect "MSc"))

(defvar *grades*
  (make-instance
   'clews.grades:grades-manager
   :pwd-source *pwd-source*
   :user-dictionary *user-source*
   :db *db*
   :id :msc-grades
   :acl '((:view . (:all))
          (:student . (:tt2008 :dn2008 :ttd2007 :dn2007 :tt2007 :it2007 :it2006 :tt2006 :pn2006))
          (:tutor . (:staff :industry))
          (:supervisor . (:staff :industry))
          (:admin . ("willijar" "chuyk")))))

;; project forms used in the past

(register-form
 :legacy-project-description
 '((form :name :legacy-project-description :method "POST")
   ((section :title "Title")
    ((input :name :title :size 60)))
   ((section :title "Location")
    ((input :name :location :size 60 :value "Aston University")))
   ((section :title "Context")
    (p (em "Give the background/context of this project"))
    ((textarea :name :context :structured-text t :cols 80 :rows 20
	       :datatype (string :word-count 75))))
    ((section :title "Outline")
    (p (em "Give an outline of this project of at least 100 words
(formatted using structured text))"))
    ((textarea :name :outline :structured-text t :cols 80 :rows 20
	       :datatype (string :word-count 75))))
   ((section :title "Skills")
    (p (em "List the skills required for this project"))
    ((input :name :skills :size 60)))
   ((input :type :submit :name "Update" :value "Update Description"))))


(REGISTER-FORM
 :LEGACY-DISSERTATION-2002
 '((FORM :NAME :LEGACY-DISSERTATION-2002 :METHOD "POST")
   (TABLE
    (TR (TH "Structure (a) Summary")
     (TD
      ((INPUT :NAME :SUMMARY :VALUE 0 :DATATYPE
	      (INTEGER :MIN 0 :MAX 5)))))
    (TR (TH "Structure (b) References/Bibliography")
     (TD
      ((INPUT :NAME :REFERENCES :VALUE 0 :DATATYPE
	      (INTEGER :MIN 0 :MAX 5)))))
    (TR (TH "Structure (c) Layout and Formatting")
     (TD
      ((INPUT :NAME :LAYOUT :VALUE 0 :DATATYPE
	      (INTEGER :MIN 0 :MAX 5)))))
    (TR (TH "Background (a) Context of Work")
     (TD
      ((INPUT :NAME :CONTEXT :VALUE 0 :DATATYPE
	      (INTEGER :MIN 0 :MAX 5)))))
    (TR
     (TH
      "Background (b) Clarity of review of introductory material")
     (TD
      ((INPUT :NAME :REVIEW :VALUE 0 :DATATYPE
	      (INTEGER :MIN 0 :MAX 15)))))
    (TR (TH "Background (c) Critical Assessment")
     (TD
      ((INPUT :NAME :CRITICAL-ASSESSMENT :VALUE 0 :DATATYPE
	      (INTEGER :MIN 0 :MAX 5)))))
    (TR (TH "Contribution (a) Personal Contribution")
     (TD
      ((INPUT :NAME :PERSONAL-CONTRIBUTION :VALUE 0 :DATATYPE
	      (INTEGER :MIN 0 :MAX 30)))))
    (TR (TH "Contribution (b) Conclusions")
     (TD
      ((INPUT :NAME :CONCLUSIONS :VALUE 0 :DATATYPE
	      (INTEGER :MIN 0 :MAX 10)))))
    (TR (TH "Contribution (c) Suggestions for Further work")
     (TD
      ((INPUT :NAME :FURTHER-WORK :VALUE 0 :DATATYPE
	      (INTEGER :MIN 0 :MAX 5)))))
    (TR (TH "Analysis (a) Technical evaluation of results")
     (TD
      ((INPUT :NAME :ANALYSIS :VALUE 0 :DATATYPE
	      (INTEGER :MIN 0 :MAX 15)))))
    (TR (TH "Comments (which may be read by the student)")
     (TD
      ((TEXTAREA :NAME NIL :ROWS 5 :COLS 60 :STRUCTURED-TEXT
		 T))))
    (TR (TH "Comments")
     (TD
      ((TEXTAREA :ROWS 5 :COLS 60 :STRUCTURED-TEXT T :NAME
		 :COMMENTS :DATATYPE (STRING :WORD-COUNT 25)))))
    (TR (TD)
     (TD
      ((INPUT :TYPE :SUBMIT :NAME "Update" :VALUE
	      "Update Report")))))))

(REGISTER-FORM
 :LEGACY-PROJECT-ORAL-2002
 '((FORM :NAME :LEGACY-PROJECT-ORAL-2002 (publish *tutorial-app* "/tutorials/"):METHOD "POST")
   (TABLE
    (TR (TH "Confidence")
     (TD
      ((INPUT :NAME :CONFIDENCE :VALUE 0 :DATATYPE
	      (INTEGER :MIN 0 :MAX 10)))))
    (TR (TH "Articulation")
     (TD
      ((INPUT :NAME :ARTICULATION :VALUE 0 :DATATYPE
	      (INTEGER :MIN 0 :MAX 10)))))
    (TR (TH "Engaged Audience")
     (TD
      ((INPUT :NAME :AUDIENCE :VALUE 0 :DATATYPE
	      (INTEGER :MIN 0 :MAX 10)))))
    (TR (TH "Knowledge of Material")
     (TD
      ((INPUT :NAME :KNOWLEDGE :VALUE 0 :DATATYPE
	      (INTEGER :MIN 0 :MAX 10)))))
    (TR (TH "Structure of Talk")
     (TD
      ((INPUT :NAME :STRUCTURE :VALUE 0 :DATATYPE
	      (INTEGER :MIN 0 :MAX 10)))))
    (TR (TH "Clarity of Slides")
     (TD
      ((INPUT :NAME :CLARITY :VALUE 0 :DATATYPE
	      (INTEGER :MIN 0 :MAX 10)))))
    (TR (TH "Use of Visual Aids")
     (TD
      ((INPUT :NAME :VISUAL-AIDS :VALUE 0 :DATATYPE
	      (INTEGER :MIN 0 :MAX 10)))))
    (TR (TH "Handling of Questions")
     (TD
      ((INPUT :NAME :QUESTIONS :VALUE 0 :DATATYPE
	      (INTEGER :MIN 0 :MAX 10)))))
    (TR (TH "Clarity of Extent of Personal Contribution")
     (TD
      ((INPUT :NAME :PERSONAL-CONTRIBUTION :VALUE 0 :DATATYPE
	      (INTEGER :MIN 0 :MAX 10)))))
    (TR (TH "Clarity of Presentation of Results Obtained")
     (TD
      ((INPUT :NAME :CLARITY :VALUE 0 :DATATYPE
	      (INTEGER :MIN 0 :MAX 10)))))
    (TR (TH "Comments (which may be read by student)")
     (TD
      ((TEXTAREA :NAME NIL :ROWS 5 :COLS 60 :STRUCTURED-TEXT
		 T))))
    (TR (TH "Comments")
     (TD
      ((TEXTAREA :ROWS 5 :COLS 60 :STRUCTURED-TEXT T :NAME
		 :COMMENTS :DATATYPE (STRING :WORD-COUNT 25)))))
    (TR (TD)
     (TD
      ((INPUT :TYPE :SUBMIT :NAME "Update" :VALUE
	      "Update Report")))))))

(REGISTER-FORM
 :DISSERTATION-2000
 '((FORM :NAME :DISSERTATION-2000 :METHOD "POST")
   (TABLE
    (TR (TH "Review")
     (TD
      ((MCQ :NAME :REVIEW :STYLE :DROPDOWN :DATATYPE
	    (INTEGER :MIN 0 :MAX 100))
       (20 . "F") (42 . "E") (48 . "D") (55 . "C") (65 . "B")
       (75 . "A") (90 . "A*"))))
    (TR (TH "Comments")
     (TD
      ((TEXTAREA :ROWS 5 :COLS 60 :STRUCTURED-TEXT T :NAME
		 :COMMENTS :DATATYPE (STRING :WORD-COUNT 25)))))
    (TR (TD)
     (TD
      ((INPUT :TYPE :SUBMIT :NAME "Update" :VALUE
	      "Update Report")))))))

(REGISTER-FORM
 :LEGACY-ORAL-2000
 '((FORM :NAME :LEGACY-ORAL-2000 :METHOD "POST")
   (TABLE
    (TR (TH "Examiners")
     (TD
      ((TEXTAREA :NAME NIL :ROWS 5 :COLS 60 :STRUCTURED-TEXT
		 T))))
    (TR (TH "Review")
     (TD
      ((MCQ :NAME :REVIEW :STYLE :DROPDOWN :DATATYPE
	    (INTEGER :MIN 0 :MAX 100))
       (20 . "F") (42 . "E") (48 . "D") (55 . "C") (65 . "B")
       (75 . "A") (90 . "A*"))))
    (TR (TH "Venue")
     (TD
      ((TEXTAREA :NAME :VENUE :ROWS 5 :COLS 60 :STRUCTURED-TEXT
		 T))))
    (TR (TH "Comments")
     (TD
      ((TEXTAREA :ROWS 5 :COLS 60 :STRUCTURED-TEXT T :NAME
		 :COMMENTS :DATATYPE (STRING :WORD-COUNT 25)))))
    (TR (TD)
     (TD
      ((INPUT :TYPE :SUBMIT :NAME "Update" :VALUE
	      "Update Report")))))))

(REGISTER-FORM
 :LEGACY-ORAL
 '((FORM :NAME :LEGACY-ORAL :METHOD "POST")
   (TABLE
    (TR
     (TH
      "Knowledge of content was thorough and well articulated")
     (TD
      ((INPUT :NAME :KNOWLEDGE :VALUE 0 :DATATYPE
	      (INTEGER :MIN 0 :MAX 30)))))
    (TR
     (TH "Presentation structured in a clear and logical manner")
     (TD
      ((INPUT :NAME :STRUCTURE :VALUE 0 :DATATYPE
	      (INTEGER :MIN 0 :MAX 20)))))
    (TR
     (TH
      "Delivery Style was effective with appropriate use of visual (and other) aids.")
     (TD
      ((INPUT :NAME :STYLE :VALUE 0 :DATATYPE
	      (INTEGER :MIN 0 :MAX 20)))))
    (TR
     (TH
      "Ability to answer questions demonstrated understanding of material and key implications")
     (TD
      ((INPUT :NAME :QUESTIONS :VALUE 0 :DATATYPE
	      (INTEGER :MIN 0 :MAX 20)))))
    (TR (TH "Ideas and Findings presented confidently")
     (TD
      ((INPUT :NAME :CONFIDENCE :VALUE 0 :DATATYPE
	      (INTEGER :MIN 0 :MAX 10)))))
    (TR (TH "Examiners")
     (TD
      ((TEXTAREA :NAME NIL :ROWS 5 :COLS 60 :STRUCTURED-TEXT
		 T))))
    (TR (TH "Comments")
     (TD
      ((TEXTAREA :ROWS 5 :COLS 60 :STRUCTURED-TEXT T :NAME
		 :COMMENTS :DATATYPE (STRING :WORD-COUNT 25)))))
    (TR (TD)
     (TD
      ((INPUT :TYPE :SUBMIT :NAME "Update" :VALUE
	      "Update Report")))))))

(REGISTER-FORM
 :LEGACY-DISSERTATION-1
 '((FORM :NAME :LEGACY-DISSERTATION :METHOD "POST")
   (TABLE
    (TR
     (TH (strong "Content:") "Technical and intellectual demands and
achievement. Presentation of findings and/or description of practical
work are concise and accurate. ")
     (TD
      ((INPUT :NAME :CONTENT :VALUE 0 :DATATYPE
	      (INTEGER :MIN 0 :MAX 50)))))
    (TR
     (TH (strong "Structure:") "Structure as outlined in project brief
adhered to including word and presentation requirements (Guidance
notes). Evaluative and relevant use of readings and other research
material (including use of bibliography/citing). Summary page clearly
and precisely states the objectives, the work described in the body of
the report and the conclusions that are drawn.")
     (TD
      ((INPUT :NAME :STRUCTURE :VALUE 0 :DATATYPE
	      (INTEGER :MIN 0 :MAX 20)))))
    (TR
     (TH (strong "Context:") "Project topic proposal, and the context
of the project, clearly defined.  Project Conclusions are properly
founded and expressed in an analytical/evaluative way.
Recommendations and suggestions for further work are included.")
     (TD
      ((INPUT :NAME :CONTEXT :VALUE 0 :DATATYPE
	      (INTEGER :MIN 0 :MAX 20)))))
    (TR
     (TH (strong "Style:") "Ideas and supporting work presented with
clarity and in a logical and scholarly manner. Reasoned argument
presented which demonstrates a clear understanding of the issues.
Presentation of findings and/or description of practical work are
concise and accurate.")
     (TD
      ((INPUT :NAME :STYLE :VALUE 0 :DATATYPE
	      (INTEGER :MIN 0 :MAX 20)))))
    (TR (TH "Comments")
     (TD
      ((TEXTAREA :ROWS 5 :COLS 60 :STRUCTURED-TEXT T :NAME
		 :COMMENTS :DATATYPE (STRING :WORD-COUNT 25)))))
    (TR (TD)
     (TD
      ((INPUT :TYPE :SUBMIT :NAME "Update" :VALUE
	      "Update Report")))))))

(REGISTER-FORM :LEGACY-DISSERTATION-2 (find-form :LEGACY-DISSERTATION-1))

;;;; transition stuff

#|



(clsql:def-view-class project()
  ((projectid :accessor projectid :type integer  :db-kind :key)
   (studentid :accessor studentid :type (string 9)
	      :void-value nil)
   (title :accessor title :type (string 255))
   (manager :accessor manager :type (string 50)
	    :column "manager_name")
   (manager_email :accessor manager-email :type (string 50))
   (supervisor :accessor supervisor :type (string 50)
	       :column "supervisor_name")
   (supervisor_email :accessor supervisor-email :type (string 50))
   (company :accessor company :type (string 50))
   (site-address :accessor site-address :type string)
   (context :accessor context :type string)
   (outline :accessor outline :type string)
   (skills_reqd :accessor skills-reqd :type string)
   (proposal_date :accessor proposal-date :type string)
   (start_date  :accessor start-date :type string)
   (submission_date :accessor submission-date :type string)
   (deadline_date :accessor deadline-date :type string)
   (tutor :accessor tutor :type string)
   (description :accessor description :type list)
   (supervisors :accessor supervisors :type list)
   (assessments :accessor assessments :type list))
  (:base-table projects))

(clsql:locally-enable-sql-reader-syntax)
(defmacro simple-query(db table  &rest keys)
  `(clsql:select ,table
	   :database ,db
	     ,@(when keys
		 `(:where
		   (sql-and
		    ,(loop for a on keys by #'cddr
			collect [= [slot-value table (car a)]
				  (cadr a) ]))))
	     :flatp t
	     :refresh t))

(setq *p* (simple-query *msc* 'project))

(dolist(p *p*)
  (setf (description p)
	(list :form :legacy-project-description
	      :location
	      (if (> (length (site-address p)) 0)
		  (concatenate 'string  (company p) "," (site-address p))
		  (company p))
	      :context (context p)
	      :outline (outline p)
	      :skills (skills-reqd p)))
  (flet ((notnull(s) (when (> (length s) 0) (list s))))
    (setf (supervisors p) `(,@(notnull (supervisor-email p))
			      ,@(notnull (manager-email p)))))
  (let ((allowed (mapcar #'first *field-mappings*))
	(assessments nil))
    (dolist(submission
	     (clsql:query
	      (format nil
		      "select questionnaireid,submissionid from submissions where contextid=~D" (projectid p))))
      (when (member (first submission) allowed)
	(push (make-submission (second submission)) assessments)))
    (format t "~D: ~D~%" (projectid p) (length assessments))
    (setf (assessments p) assessments)))

(map 'nil #'clsql:update-records-from-instance *p*)

select distinct questionnaireid from submissions where contextid=167;

select distinct students.year,questionnaireid,questionnaires.title from students left join projects using (studentid) left join submissions on (projectid=contextid) left join questionnaires using (questionnaireid) where not projectid isnull;
 year | questionnaireid |                 title
------+-----------------+----------------------------------------
 1998 |               6 | MSc Oral Assessment (pre 2000)
 1998 |               7 | MSc Discussion Group Feedback
 1998 |               8 | MSc Dissertation Assessment (pre 2000)
 1999 |               2 | MSc Project Supervisors Report
 1999 |               3 | MSc Placement Site Visit Report
 1999 |               4 | MSc Dissertation Assessment
 1999 |               5 | MSc Oral Assessment
 1999 |               7 | MSc Discussion Group Feedback
 1999 |              11 | MSc Dissertation Assessment (2002)
 2000 |               2 | MSc Project Supervisors Report
 2000 |               3 | MSc Placement Site Visit Report
 2000 |               4 | MSc Dissertation Assessment
 2000 |               5 | MSc Oral Assessment
 2000 |               7 | MSc Discussion Group Feedback
 2000 |                 |
 2001 |               2 | MSc Project Supervisors Report
 2001 |               3 | MSc Placement Site Visit Report
 2001 |               7 | MSc Discussion Group Feedback
 2001 |              10 | MSc Oral Assessment (2002)
 2001 |              11 | MSc Dissertation Assessment (2002)
 2002 |               2 | MSc Project Supervisors Report
 2002 |               7 | MSc Discussion Group Feedback
 2002 |              10 | MSc Oral Assessment (2002)
 2002 |              11 | MSc Dissertation Assessment (2002)
 2002 |                 |
 2003 |               2 | MSc Project Supervisors Report
 2003 |               3 | MSc Placement Site Visit Report
 2003 |               7 | MSc Discussion Group Feedback
 2003 |              10 | MSc Oral Assessment (2002)
 2003 |              11 | MSc Dissertation Assessment (2002)
 2004 |               7 | MSc Discussion Group Feedback
 2004 |                 |

select assessmentid,moduleid,title,type,year from assessments where moduleid='EE4006' order by year ;

 assessmentid | moduleid |           title            | type | year
--------------+----------+----------------------------+------+------
           59 | EE4006   | Project Oral               | ORAL | 2001
           62 | EE4006   | Dissertation 2             | DIS  | 2001
           61 | EE4006   | Dissertation 1             | DIS  | 2001
           60 | EE4006   | Project Supervisors Report | SUP  | 2001
           84 | EE4006   | Project Oral               | ORAL | 2002
           85 | EE4006   | Dissertation 2             | DIS  | 2002
          113 | EE4006   | Dissertation 1             | DIS  | 2002
          114 | EE4006   | Project Supervisors Report | SUP  | 2002
          127 | EE4006   | Project Oral               | ORAL | 2003
          128 | EE4006   | Dissertation 2             | DIS  | 2003
          149 | EE4006   | Dissertation 1             | DIS  | 2003
          150 | EE4006   | Project Supervisors Report | SUP  | 2003
          166 | EE4006   | Project Oral               | ORAL | 2004
          167 | EE4006   | Dissertation 2             | DIS  | 2004
          183 | EE4006   | Dissertation 1             | DIS  | 2004
          184 | EE4006   | Project Supervisors Report | SUP  | 2004

(defun get-form(assessmentid)
  (feedback-form (car (clews.grades::assessment-records *db* 'assessmentid assessmentid))))

(defun set-form (assessmentid form)
  (let ((rec (car (clews.grades::assessment-records *db* 'assessmentid assessmentid))))
    (setf (slot-value rec 'feedback-form) form)
    (clews.grades::update-records-from-instance rec)))


(let ((form (make-form-form 11)))
  (dolist(assessmentid '(167 183 128 149 85 113 62 61))
    (let ((assessment (car (clews.grades::assessment-records *db* 'clews.grades::assessmentid assessmentid))))
      (setf (clews.grades::feedback-form assessment) form)
      (clsql:update-records-from-instance assessment))))

(let ((form (make-form-form 10)))
  (dolist(assessmentid '(166 127 84 59))
    (let ((assessment (car (clews.grades::assessment-records *db* 'clews.grades::assessmentid assessmentid))))
      (setf (clews.grades::feedback-form assessment) form)
      (clsql:update-records-from-instance assessment))))

(let ((form (make-form-form 2)))
  (dolist(assessmentid '(184 150 114 60))
    (let ((assessment (car (clews.grades::assessment-records *db* 'clews.grades::assessmentid assessmentid))))
      (setf (clews.grades::feedback-form assessment) form)
      (clsql:update-records-from-instance assessment))))

(let ((assessment (car (clews.grades::assessment-records *db* 'clews.grades::assessmentid 166))))
  (setf (clews.grades::feedback-form assessment)
	 '((form :method :post :name :project-oral :title "Project Oral Report")
    ((section :title "Content")
     (p "An abundance of material clearly related to thesis; points
are clearly made and all evidence supports thesis; varied use of
materials")
     ((mcq :name :content :value 3  :style :dropdown
	   :datatype (integer :max 4))
     (4 . "Exceptional") (3 . "Admirable") (2 . "Acceptable")
      (1 . "Amateur")))
    ((section :title "Coherence and Organization")
     (p "Thesis is clearly stated and developed; specific examples
are appropriate and clearly develop thesis; conclusion is clear; shows
control; flows together well; good transitions; succinct but not
choppy; well organized")
     ((mcq :name  :coherence :value 3
	   :style :dropdown :datatype (integer :max 4))
      (4 . "Exceptional") (3 . "Admirable") (2 . "Acceptable")
      (1 . "Amateur")))
    ((section :title "Creativity")
     (p "Very original presentation of material; uses the unexpected
to full advantage; captures audience's attention")
     ((mcq :name :creativity :value 3
	   :style :dropdown :datatype (integer :max 4))
     (4 . "Exceptional") (3 . "Admirable") (2 . "Acceptable")
      (1 . "Amateur") ))
    ((section :title "Material")
     (p "Balanced use of multimedia materials; properly used to
develop thesis; use of media is varied and appropriate")
     ((mcq :name :material :value 3
	   :style :dropdown :datatype (integer :max 4))
      (4 . "Exceptional") (3 . "Admirable") (2 . "Acceptable")
      (1 . "Amateur")))
    ((section :title "Speaking Skills")
     (p "Poised, clear articulation; proper volume; steady rate;
good posture and eye contact; enthusiasm;")
     ((mcq :name :speaking :value 3
	   :style :dropdown :datatype (integer :max 4))
     (4 . "Exceptional") (3 . "Admirable") (2 . "Acceptable")
      (1 . "Amateur")))
    ((section :title "Audience Response")
     (p "Involved the audience in the presentation; points made in
creative way; held the audience's attention throughout")
     ((mcq :name :response  :value 3
	   :style :dropdown :datatype (integer :max 4))
      (4 . "Exceptional") (3 . "Admirable") (2 . "Acceptable")
      (1 . "Amateur")))
    ((section :title "Length of Presentation")
     ((mcq  :name :length :value 3 :style :dropdown :datatype (integer :max 3))
      (4 . "Within 30 seconds of allotted time")
      (3 . "Within 1 minute of allotted time")
      (2 . "Within 3 minutes of allotted time")
      (1 . "More than 3 minutes above or below the allotted time")))
    ((section :title "Comments")
     ((textarea :rows 5 :cols 60  :structured-text t
		:name :comments :datatype (string :word-count 25))))
    ((input :type :submit :name "Update" :value "Update Report"))))
  (clsql:update-records-from-instance assessment))

(let ((form  '((form :method :post :title "Project Dissertation Report")
	       ((section :title "Introduction")
     (p "Clearly describes extent of the study to be undertaken.
Outlines a methodology appropriate to the study.")
     ((mcq :style :dropdown
	   :name :introduction  :value 3 :datatype (integer :max 3))
      (4 . "Exceptional") (3 . "Admirable") (2 . "Acceptable")
      (1 . "Amateur")))
    ((section :title "Body of Dissertation")
     (p "Provides evidence of extensive range of reading.  Includes a
review of the relevant literature.  States explicitly the links
between the review and area of study proposed.")
     ((mcq :style :dropdown
	   :name :review  :value 3 :datatype (integer :max 3))
      (4 . "Exceptional") (3 . "Admirable") (2 . "Acceptable")
      (1 . "Amateur"))
     (p "Objective and critical analysis of supporting/opposing
evidence.  Draws together the different points to emerge from the
study.  Discusses points at length and develops a clear line of
argument.")
     ((mcq :style :dropdown
	   :name :analysis  :value 3 :datatype (integer :max 3))
      (4 . "Exceptional") (3 . "Admirable") (2 . "Acceptable")
      (1 . "Amateur"))
     (p "Provides own interpretation which is put in the context of
other interpretations.  Shows awareness of understanding of
assumptions underlying own interpretation.")
     ((mcq :style :dropdown
	   :name :interpretation  :value 3 :datatype (integer :max 3))
      (4 . "Exceptional") (3 . "Admirable") (2 . "Acceptable")
      (1 . "Amateur")))
    ((section :title "Conclusion")
     (p "Solutions/conclusion relate to the questions posed in the
introduction.  Solutions/conclusions are well founded on the evidence.
Links conclusion to the relevant literature.")
     ((mcq :style :dropdown
	   :name :conclusion  :value 3 :datatype (integer :max 3))
      (4 . "Exceptional") (3 . "Admirable") (2 . "Acceptable")
      (1 . "Amateur")))
    ((section :title "Structure/ Organisation")
     (p "Makes explicit the sequence of chapters and sections within
chapters.  Links conclusion to the relevant literature.  Uses
different chapters to develop different themes.  Inter-linkage of
chapters.  Makes use of headings and summaries as appropriate.
Accurate use of references.")
     ((mcq :style :dropdown
	   :name :organisation  :value 3 :datatype (integer :max 3))
      (4 . "Exceptional") (3 . "Admirable") (2 . "Acceptable")
      (1 . "Amateur")))
    ((section :title "Presentation")
     (p "Accurate use of references.  References properly noted and
listed.  Clear and easy to read.  Correct spelling.  Meets University
Requirements.  Few errors of grammar and syntax.  Fluent use of
English.  Of the appropriate length.")
     ((mcq :style :dropdown
	   :name :presentation  :value 3 :datatype (integer :max 3))
      (4 . "Exceptional") (3 . "Admirable") (2 . "Acceptable")
      (1 . "Amateur")))
    ((section :title "Comments")
     (p "Comments justifying the above assessment")
     ((textarea :rows 5 :structured-text t :cols 60
		:name :comments :datatype (string :word-count 50))))
    ((input :type :submit :name "Update" :value "Update Report")))))
  (dolist(assessmentid '(167 183))
    (let ((assessment (car (clews.grades::assessment-records *db* 'clews.grades::assessmentid assessmentid))))
      (setf (clews.grades::feedback-form assessment) form)
      (clsql:update-records-from-instance assessment))))

;; fill in feedback for projects into marks (from submissions)


(defparameter *field-mappings*
  '((2 (60 114 150 184)
     1 :challenge     2 :support     3 :confidentiality     4 :understanding
     5 :attainment     6 :initiative     7 :independance     8 :organisation
     9 :diligence     10 :rigour     11 :teamwork     12 :mark)
    (3 :site-visit     1 :work-progress     2 :project-progress
     3 :issues     4 :challenge     5 :support     6 :confidentiality)
    (4 :legacy-dissertation     1 :content     2 :structure     3 :context
     4 :style)
    (5 :legacy-oral     1 :knowledge     2 :structure     4 :style
     5 :questions  6 :confidence 7 nil)
    (6 :legacy-oral-2000     1 nil     2 :review     3 :venue)
    (8 :dissertation-2000     1 :review)
    (10 (166 127 84 59)
     1 :confidence     2 :articulation     3 :audience     4 :knowledge
     5 :structure     6 :clarity     7 :visual-aids     8 :questions
     9 :personal-contribution     10 :clarity     11 nil)
    (11 (183 167 128 149 85 113 62 61)
     1 :summary     2 :references     3 :layout     4 :context
     5 :review     6 :critical-assessment     7 :personal-contribution
     8 :conclusions     9 :further-work     10 :analysis     11 nil))
  "mapping between old submissions questionnaires and lisp fields")

(defun make-submission(submissionid)
  (let ((result nil)
	(mappings)
	(total-comment ""))
    (clsql:do-query((questionnaireid created created-by)
		    (format nil "select questionnaireid,created,created_by from submissions where submissionid=~D" submissionid))
      (let ((m (assoc questionnaireid *field-mappings*)))
	(unless m (error "Questionnaire ~D not defined as submission"))
	(setf result
	      (list :submitted  (ext:parse-time created)
		    :submitted-by created-by))
	(setf mappings (cddr m))))
    (dolist(submission (clsql:query
			(format nil "select questions.questionid,mark,comment from submission_marks left join submissions using (submissionid) left join questions using (questionnaireid,questionid) where submissionid=~D" submissionid)))
      (multiple-value-bind(questionid mark comment) (values-list submission)
	(when (> (length comment) 0)
	      (setf total-comment (concatenate 'string total-comment " " comment)))
	(let ((name (getf mappings questionid)))
	  (when name (setf (getf result name) mark)))))
    (setf (getf result :comment) total-comment)
    result))

(dolist(project
	 (clsql:query "select projectid,studentid from projects order by projectid"
		      :database *db*))
  (dolist(questionnaireid '(2 10 11))
      (let ((assessmentids
	     (let ((possible (second (assoc questionnaireid *field-mappings*))))

	     (clsql:query
	      (format nil "select assessmentid from marks where studentid='~A' and (assessmentid=~D~{ or assessmentid=~D~})"
		      (second project)
		      (first possible)
		      (rest possible))
	      :database *db*)))
	    (submissionids (mapcar #'car
		   (clsql:query
		    (format nil "select submissionid from submissions where questionnaireid=~D and contextid=~D"
		    questionnaireid
		    (first project))
		    :database *db*))))
	(when (and assessmentids submissionids)
	  (when  (< (length assessmentids) (length submissionids))
	    (error "Wrong number submissions for questionnaire ~D project ~D"
		   questionnaireid (first project)))
	  (map 'nil
	       #'(lambda(assessmentid submissionid)
		   (let ((mark (car (current-marks
				     (clews.grades::mark-records
				      *db*
				'clews.grades::studentid (second project)
				'clews.grades::assessmentid assessmentid))))
			 (submission (make-submission submissionid)))
		     (format t "~A:~A~%" mark submission)
		     (setf (clews.grades::modified mark)
			   (getf submission :submitted)
			   (clews.grades::modified-by mark)
			   (let ((s (getf submission :submitted-by)))
			     (if (> (length s) 8) (subseq s 0 8) s)))
		     (remf submission :submitted)
		     (remf submission :submitted-by)
		     (setf (clews.grades::feedback mark) submission)
		     (clsql:update-records-from-instance mark)))
	       assessmentids submissionids)))))

(defun make-form-form(questionnaireid)
  (let* ((m (assoc questionnaireid *field-mappings*))
	 (mappings (cddr m)))
    `((form :method "POST")
	(table
       ,@(mapcar
	  #'(lambda(question)
	      (multiple-value-bind(questionid text value no-mark no-comment)
		  (values-list question)
		(let ((choices
		       (clsql:query
			(format nil "select value,text from question_choices where questionnaireid=~D and questionid=~D order by value"
				questionnaireid questionid)))
		      (name (getf mappings questionid :none))
		      (mark-p (equalp no-mark "f")))
		  (when (eql name :none)
		    (error "No definition for question ~S in questionnaire ~D"
			   questionid questionnaireid))
		  `(tr (th ,text)
		       (td
			,(cond
			   (choices
			    `((mcq :name ,name :style :dropdown
				   ,@(when mark-p
				       `(:datatype (integer :min 0
							    :max ,value))))
			      ,@(mapcar
				 #'(lambda(choice)
				       (cons (first choice) (second choice)))
				 choices)))
			   ((not mark-p)
			    `((textarea :name ,name
					:rows 5 :cols 60 :structured-text t)))
			   (t
			    `((input :name ,name :value 0
				     :datatype (integer :min 0 :max ,value))))))))))
	  (clsql:query (format nil "select questionid,text,value,no_mark,no_comment from questions where questionnaireid=~D order by questionid" questionnaireid)))
       (tr (th "Comments")
	   (td
	    ((textarea :rows 5 :cols 60  :structured-text t
		       :name :comments :datatype (string :word-count 25)))))
       (tr (td)
	   (td ((input :type :submit :name "Update" :value "Update Report"))))))))

(map-dictionary
 #'(lambda(k p)
     (let* ((project (make-instance 'project))
	    (d (clews.projects::description p))
	    (studentid
	     (first (first (clsql:query (format nil "select studentid from students where username='~A'" (clews.projects::student p)))))))
       (format t "~A:~A:~A~%"  (clews.projects::project-id p)
	       (clews.projects::student p) studentid)
       (setf (projectid project) (parse-integer (clews.projects::project-id p))
	     (title project) (getf d :title)
	     (description project) (progn (remf d :title) d)
	     (studentid project) studentid
	     (supervisors project) (clews.projects::supervisors p)
	     (start-date project) (clews.projects::start-date p)
	     (deadline-date project)  (clews.projects::deadline-date p))
       (clsql:update-records-from-instance project)))
 (clews.projects::projects aston::*msc-projects-old*))




(clsql:select [questionnaireid] :from [submissions] :where [= [contextid] 167] :distinct t :flatp t)
(clsql:do-query((questionnaireid submissionid created-by created)
	  "select questionnaireid,submissionid,created_by,created from submissions where contextid=167")
  (list questionnaireid submissionid created-by (decode-timestamp created)))
|#


(defun reset-db()
  (setq *db* (db-connect "MSc"))
  (setf (slot-value *grades* 'clews.grades::db) *db*)
  (setf (slot-value *msc-projects* 'clews.grades::db) *db*)
)
