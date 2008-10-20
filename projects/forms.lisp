;;;; Standard assessment and project description forms
;;;; Copyright (C) 2004-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL) 
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; $Id: forms.lisp,v 1.2 2005/03/10 20:14:01 willijar Exp $

(in-package :clews.projects)

(register-form
 "project-description"
 '((form :name "project-description" :method "POST")
   ((section :title "Title")
    ((input :name :title :size 60)))
   ((section :title "Location")
    ((input :name :location :size 60 :value "Aston University")))
   ((section :title "Outline")
    (p (em "Give an outline of this project of at least 100 words (formatted using
structured text))"))
    ((textarea :name :outline :structured-text t :cols 80 :rows 20
	       :datatype (string :word-count 75))))
   ((section :title "Skills")
    (p (em "List the skills required for this project"))
    ((input :name :skills :size 60)))
   ((input :type :submit :name "Update" :value "Update Description"))))

;;questionnaireid=2
(REGISTER-FORM :PROJECT-SUPERVISOR
               '((FORM :NAME :PROJECT-SUPERVISOR :METHOD "POST")
                 (TABLE
                  (TR (TH "Challenge")
                   (TD
                    ((MCQ :NAME :CHALLENGE :STYLE :DROPDOWN) (0 . "Review")
                     (1 . "Contributory Consolidation")
                     (2 . "Progressive Development") (3 . "New Research"))))
                  (TR (TH "Support")
                   (TD
                    ((MCQ :NAME :SUPPORT :STYLE :DROPDOWN) (0 . "Solo")
                     (1 . "Focus Group (<5)") (2 . "Small Team (5-10)")
                     (3 . "Large team (>10)"))))
                  (TR (TH "Confidentiality")
                   (TD
                    ((MCQ :NAME :CONFIDENTIALITY :STYLE :DROPDOWN) (0 . "None")
                     (1 . "Low") (2 . "Medium") (3 . "High"))))
                  (TR (TH "Understanding")
                   (TD
                    ((MCQ :NAME :UNDERSTANDING :STYLE :DROPDOWN) (0 . "Weak")
                     (1 . "Adequate") (2 . "Strong") (3 . "Comprehensive"))))
                  (TR (TH "Attainment")
                   (TD
                    ((MCQ :NAME :ATTAINMENT :STYLE :DROPDOWN)
                     (0 . "No Progress")
                     (1 . "Progressed towards pre-set goals")
                     (2 . "Met all objectives")
                     (3 . "Exceeded Initial Objectives"))))
                  (TR (TH "Initiative")
                   (TD
                    ((MCQ :NAME :INITIATIVE :STYLE :DROPDOWN) (0 . "None")
                     (1 . "Low") (2 . "Defined own goals")
                     (3 . "Defined Own Project"))))
                  (TR (TH "Independence")
                   (TD
                    ((MCQ :NAME :INDEPENDANCE :STYLE :DROPDOWN)
                     (0 . "Hand held") (1 . "Adequately Independent")
                     (2 . "Largely Independent")
                     (3 . "Entirely Independent"))))
                  (TR (TH "Organisation")
                   (TD
                    ((MCQ :NAME :ORGANISATION :STYLE :DROPDOWN)
                     (0 . "Disorganised") (1 . "Mostly Organised")
                     (2 . "Organised") (3 . "Totally Organised"))))
                  (TR (TH "Diligence")
                   (TD
                    ((MCQ :NAME :DILIGENCE :STYLE :DROPDOWN)
                     (0 . "Took it easy") (1 . "Adequate") (2 . "Hard working")
                     (3 . "Beyond the call of duty"))))
                  (TR (TH "Rigour")
                   (TD
                    ((MCQ :NAME :RIGOUR :STYLE :DROPDOWN) (0 . "Sloppy")
                     (1 . "Adequately Rigorous") (2 . "Thorough")
                     (3 . "Pedantic"))))
                  (TR (TH "Teamwork")
                   (TD
                    ((MCQ :NAME :TEAMWORK :STYLE :DROPDOWN)
                     (0 . "Strongly individualistic")
                     (1 . "Able to work in a team") (2 . "Good team member")
                     (3 . "Team builder"))))
                  (TR
                   (TH
                    "Overall Mark")
                   (TD
                    ((INPUT :NAME :MARK :VALUE 0 :DATATYPE
			    (INTEGER :MIN 0 :MAX 15))) "(/15)"))
                  (TR (TH "Comments")
                   (TD
                    ((TEXTAREA :ROWS 5 :COLS 60 :STRUCTURED-TEXT T :NAME
                      :COMMENTS :DATATYPE (STRING :WORD-COUNT 25)))))
                  (TR (TD)
                   (TD
                    ((INPUT :TYPE :SUBMIT :NAME "Update" :VALUE
                      "Update Report")))))))


(register-form
 :project-oral
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

(register-form
 :project-dissertation
 '((form :method :post :name :project-dissertation-1
      :title "Project Dissertation Report")
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
    ((input :type :submit :name "Update" :value "Update Report"))))

;; questionnaireid=2
(register-form
 :site-visit
 '((form :name :site-visit :method "post")
   (table
    (tr (th "Progress within the working environment")
     (td
      ((textarea :name :work-progress :rows 5 :cols 60
		 :structured-text t))))
    (tr
     (th "Project Progress (include estimate completion date)")
     (td
      ((textarea :name :project-progress :rows 5 :cols 60
		 :structured-text t))))
    (tr (th "Issues")
     (td
      ((textarea :name :issues :rows 5 :cols 60 :structured-text
		 t))))
    (tr (th "Challenge")
     (td
      ((mcq :name :challenge :style :dropdown :datatype
	    (integer :min 0 :max 3))
       (0 . "Review") (1 . "Contributory Consolidation")
       (2 . "Progressive Development") (3 . "New Research"))))
    (tr (th "Support")
     (td
      ((mcq :name :support :style :dropdown :datatype
	    (integer :min 0 :max 3))
       (0 . "Solo") (1 . "Focus Group (<5)")
       (2 . "Small Team (5-10)") (3 . "Large team (>10)"))))
    (tr (th "Confidentiality")
     (td
      ((mcq :name :confidentiality :style :dropdown :datatype
	    (integer :min 0 :max 3))
       (0 . "None") (1 . "Low") (2 . "Medium") (3 . "High"))))
    (tr (th "Comments")
     (td
      ((textarea :rows 5 :cols 60 :structured-text t :name
		 :comments :datatype (string :word-count 25)))))
    (tr (td)
     (td
      ((input :type :submit :name "Update" :value "Update Report")))))))