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

;; review forms used in the past

(defvar *msc-projects*
  (make-instance
   'clews.grades:projects-manager
   :db *db*
   :student-url #'(lambda(user)
                    (format nil "http://heisenberg.aston.ac.uk:8080/cv/~A"
                            (username user)))
   :acl '((:view . (:all))
          (:student . (:it2006 :tt2006 :pn2006
                       :pns2007 :it2007 :tt2007 :dn2007 "madkouws"))
          (:tutor . (:staff :industry))
          (:supervisor . (:staff :industry))
          (:admin . ("willijar" "chuyk")))
   :pwd-source *pwd-source*
   :user-dictionary *user-source*))

;; (defvar *cvs*
;;   (make-instance
;;    'clews.cv:cv-manager
;;    :acl '((:view . (:all))
;; 	  (:student . ("willijar" "webbdj" "test"
;;                  :it2005 :tt2005 :pn2005
;;                  :it2006 :tt2006 :pn2006))
;; 	  (:tutor . ("willijar" :industry :staff))
;; 	  (:admin . ("willijar" "webbdj")))
;;    :pwd-source *pwd-source*
;;    :user-dictionary *user-source*))

#|

(defvar *beng-projects*
  (make-instance
   'clews.projects:project-manager
   :id :beng-project
   :project-source (make-filesystem-dictionary
		    (translate-logical-pathname
		     "www:BEng-projects;"))
;   :description-form :ug-project-description
   :assessments `((:beng-supervisor-personal 1/3 ,#'clews.projects::supervisors)
		  (:ug-oral 2/15 :tutor)
		  (:beng-dissertation-supervisor 1/3 ,#'clews.projects::supervisors)
		  (:beng-dissertation-moderator 1/5 :tutor))
   :acl '((:view . (:all))
	  (:student . ("test"))
	  (:tutor . (:staff))
	  (:supervisor . (:staff))
	  (:admin . ("willijar" "eberhama")))
   :pwd-source *pwd-source*
   :user-dictionary *user-source*))

(defvar *meng-projects*
  (make-instance
   'clews.projects:project-manager
   :id :meng-project
   :project-source (make-filesystem-dictionary
		    (translate-logical-pathname
		     "www:MEng-projects;"))
;   :description-form :ug-project-description
   :assessments `((:meng-supervisor-personal 1/3 ,#'clews.projects::supervisors)
		  (:ug-oral 2/15 :tutor)
		  (:meng-dissertation-supervisor 1/3 ,#'clews.projects::supervisors)
		  (:meng-dissertation-moderator 1/5 :tutor))
   :acl '((:view . (:all))
	  (:student . ("test"))
	  (:tutor . (:staff))
	  (:supervisor . (:staff))
	  (:admin . ("willijar" "eberhama")))
   :pwd-source *pwd-source*
   :user-dictionary *user-source*))

(register-form
 :ug-oral
  '((form :name :ug-oral :method :post :title "Oral Report")
    (table
     (tr
      (th "Overall Mark")
      (td ((input :size 3 :name :mark :datatype (integer :min 0 :max 20))) "/10"))
      (tr (th "Comments")
	  (td ((textarea :rows 5 :cols 50 :structured-text t
			:name :comments
			:datatype (string :word-count 25))))))
    ((input :type :submit :name "Update" :value "Update Report"))
    (table (caption "Marking Guide")
     (tr (td "I") (td "2i") (td "2ii") (td "3") (td "P") (td "F"))
     (tr (td "20-16") (td "15-13") (td "12-11") (td "10-9") (td "8") (td "7-0")))))

(register-form
 :beng-supervisor-personal
 '((form :name :beng-supervisor-personal :method :post
    :title "Supervisor Personal")
   (table
    (tr
     (td "Comprehension of problem and its solution")
     (td ((input :size 3 :name :comprehension
		 :datatype (integer :min 0 :max 10))) "/10"))
    (tr
     (td "Originality")
     (td ((input :size 3 :name :originality
		 :datatype (integer :min 0 :max 10))) "/10"))
        (tr
     (td "Thoroughness in implementation and validation")
     (td ((input :size 3 :name :thoroughness
		 :datatype (integer :min 0 :max 10))) "/10"))
    (tr
     (td "Achievements beyond level of taught course")
     (td ((input :size 3 :name :achievements
		 :datatype (integer :min 0 :max 10))) "/10"))
    (tr
     (td "Effort")
     (td ((input :size 3 :name :effort
		 :datatype (integer :min 0 :max 10))) "/10"))
    (tr (th "Comments")
	  (td ((textarea :rows 5 :cols 50 :structured-text t
			:name :comments
			:datatype (string :word-count 25))))))
   ((input :type :submit :name "Update" :value "Update Report"))))

(register-form
 :beng-dissertation-supervisor
 '((form :name :beng-dissertation-supervisor :method :post
    :title "Supervisors Report")
   (table
    (tr ((td :colspan 2)) "To what extent is the report:")
    (tr
     (td "In good English")
     (td ((input :size 3 :name :grammar
		 :datatype (integer :min 0 :max 3))) "/3"))
    (tr
     (td "Well structured and presented (format/structure/bibliography/references)")
     (td ((input :size 3 :name :presentation
		 :datatype (integer :min 0 :max 5))) "/5"))
    (tr ((td :colspan 2) "From the report, how do you rate the"))
    (tr
     (td "Description to the background of the project")
     (td ((input :size 3 :name :background :datatype (integer :min 0 :max 5))) "/5"))
    (tr
     (td "Quality of design and implementation")
     (td ((input :size 3 :name :design
		 :datatype (integer :min 0 :max 12))) "/12"))
    (tr
     (td "Evaluation/testing of the end product")
     (td ((input :size 3 :name :evaluation
		 :datatype (integer :min 0 :max 5))) "/5"))
    (tr
     (td "Scope/depth of the project as actually carried out")
     (td ((input :size 3 :name :scope
		 :datatype (integer :min 0 :max 5))) "/5"))
    (tr
     (td "Quality of the end product")
     (td ((input :size 3 :name :end-product
		 :datatype (integer :min 0 :max 10))) "/10"))
    (tr
     (td "Analysis/conclusions/suggestions")
     (td ((input :size 3 :name :analysis
		 :datatype (integer :min 0 :max 5))) "/5"))
    (tr (th "Comments")
	  (td ((textarea :rows 5 :cols 50 :structured-text t
			:name :comments
			:datatype (string :word-count 25))))))
    ((input :type :submit :name "Update" :value "Update Report"))))

(register-form
 :beng-dissertation-moderator
 '((form :name :beng-dissertation-moderator :method :post
    :title "Moderators Report")
   (table
    (tr ((td :colspan 2)) "To what extent is the report:")
    (tr
     (td "In good English")
     (td ((input :size 3 :name :grammar
		 :datatype (integer :min 0 :max 3))) "/3"))
    (tr
     (td "Well structured and presented (format/structure/bibliography/references)")
     (td ((input :size 3 :name :presentation
		 :datatype (integer :min 0 :max 5))) "/5"))
    (tr ((td :colspan 2) "From the report, how do you rate the"))
    (tr
     (td "Description to the background of the project")
     (td ((input :size 3 :name :background :datatype (integer :min 0 :max 5))) "/5"))
    (tr
     (td "Quality of design and implementation")
     (td ((input :size 3 :name :design
		 :datatype (integer :min 0 :max 12))) "/12"))
    (tr
     (td "Evaluation/testing of the end product")
     (td ((input :size 3 :name :evaluation
		 :datatype (integer :min 0 :max 5))) "/5"))
    (tr
     (td "Scope/depth of the project as actually carried out")
     (td ((input :size 3 :name :scope
		 :datatype (integer :min 0 :max 5))) "/5"))
    (tr
     (td "Quality of the end product")
     (td ((input :size 3 :name :end-product
		 :datatype (integer :min 0 :max 10))) "/10"))
    (tr
     (td "Analysis/conclusions/suggestions")
     (td ((input :size 3 :name :analysis
		 :datatype (integer :min 0 :max 5))) "/5"))
    (tr (th "Comments")
	  (td ((textarea :rows 5 :cols 50 :structured-text t
			:name :comments
			:datatype (string :word-count 25))))))
    ((input :type :submit :name "Update" :value "Update Report"))))


(register-form
 :meng-supervisor-personal
 '((form :name :meng-supervisor-personal :method :post
    :title "Supervisor Personal")
   (table
    (tr
     (td "Comprehension of problem and its solution")
     (td ((input :size 3 :name :comprehension
		 :datatype (integer :min 0 :max 15))) "/15"))
    (tr
     (td "Originality")
     (td ((input :size 3 :name :originality
		 :datatype (integer :min 0 :max 12))) "/12"))
        (tr
     (td "Thoroughness in implementation and validation")
     (td ((input :size 3 :name :thoroughness
		 :datatype (integer :min 0 :max 8))) "/8"))
    (tr
     (td "Achievements beyond level of taught course")
     (td ((input :size 3 :name :achievements
		 :datatype (integer :min 0 :max 10))) "/10"))
    (tr
     (td "Effort")
     (td ((input :size 3 :name :effort
		 :datatype (integer :min 0 :max 5))) "/5"))
    (tr (th "Comments")
	  (td ((textarea :rows 5 :cols 50 :structured-text t
			:name :comments
			:datatype (string :word-count 25))))))
   ((input :type :submit :name "Update" :value "Update Report"))))

(register-form
 :meng-dissertation-supervisor
 '((form :name :meng-dissertation-supervisor :method :post
    :title "Supervisors Report")
   (table
    (tr ((td :colspan 2)) "To what extent is the report:")
    (tr
     (td "Well structured and presented (format/structure/bibliography/references)")
     (td ((input :size 3 :name :presentation
		 :datatype (integer :min 0 :max 4))) "/4"))
    (tr ((td :colspan 2) "From the report, how do you rate the"))
    (tr
     (td "Quality of the background investigation")
     (td ((input :size 3 :name :background
		 :datatype (integer :min 0 :max 8))) "/8"))
    (tr
     (td "Quality of design and implementation")
     (td ((input :size 3 :name :design
		 :datatype (integer :min 0 :max 12))) "/12"))
    (tr
     (td "Evaluation/testing of the end product")
     (td ((input :size 3 :name :evaluation
		 :datatype (integer :min 0 :max 5))) "/5"))
    (tr
     (td "Scope/depth of the project as actually carried out")
     (td ((input :size 3 :name :scope
		 :datatype (integer :min 0 :max 8))) "/8"))
    (tr
     (td "Quality of the end product")
     (td ((input :size 3 :name :end-product
		 :datatype (integer :min 0 :max 5))) "/5"))
    (tr
     (td "Analysis/conclusions/suggestions")
     (td ((input :size 3 :name :analysis
		 :datatype (integer :min 0 :max 8))) "/8"))
    (tr (th "Comments")
	  (td ((textarea :rows 5 :cols 50 :structured-text t
			:name :comments
			:datatype (string :word-count 25))))))
    ((input :type :submit :name "Update" :value "Update Report"))))

(register-form
 :meng-dissertation-moderator
 '((form :name :meng-dissertation-moderator :method :post
    :title "Moderators Report")
   (table
    (tr ((td :colspan 2)) "To what extent is the report:")
    (tr
     (td "Well structured and presented (format/structure/bibliography/references)")
     (td ((input :size 3 :name :presentation
		 :datatype (integer :min 0 :max 4))) "/4"))
    (tr ((td :colspan 2) "From the report, how do you rate the"))
    (tr
     (td "Quality of the background investigation")
     (td ((input :size 3 :name :background
		 :datatype (integer :min 0 :max 8))) "/8"))
    (tr
     (td "Quality of design and implementation")
     (td ((input :size 3 :name :design
		 :datatype (integer :min 0 :max 12))) "/12"))
    (tr
     (td "Evaluation/testing of the end product")
     (td ((input :size 3 :name :evaluation
		 :datatype (integer :min 0 :max 5))) "/5"))
    (tr
     (td "Scope/depth of the project as actually carried out")
     (td ((input :size 3 :name :scope
		 :datatype (integer :min 0 :max 8))) "/8"))
    (tr
     (td "Quality of the end product")
     (td ((input :size 3 :name :end-product
		 :datatype (integer :min 0 :max 5))) "/5"))
    (tr
     (td "Analysis/conclusions/suggestions")
     (td ((input :size 3 :name :analysis
		 :datatype (integer :min 0 :max 8))) "/8"))
    (tr (th "Comments")
	  (td ((textarea :rows 5 :cols 50 :structured-text t
			:name :comments
			:datatype (string :word-count 25))))))
    ((input :type :submit :name "Update" :value "Update Report"))))
|#

#| new forms
((MARKUP:FORM :METHOD :POST :TITLE "Literature Survey Assessment")
 (p "Review the literature review part of the dissertation only.")
  (MARKUP:OL

   (MARKUP:LI
    (MARKUP:B " The student has submitted the literature survey to
Turnitin, has discussed the results with me and has where necessary
made appropriate changes (multiplicative - required).")
    ((MARKUP:MCQ :NAME :TURNITIN :WEIGHTING * :VALUE 100
                                              :DATATYPE  (INTEGER :MAX 100))
     (100 . "Yes (x100%)")
     (0 . "No (x0%)")))
   (MARKUP:LI
    (markup:p (MARKUP:B "References to literature (50%):")
    ((MARKUP:INPUT :NAME :REFERENCES :WEIGHTING 50 :VALUE 60 :SIZE 4
                   :DATATYPE (INTEGER :MIN 0 :MAX 100)))"/100")
     (table
      (tr ((td :align :right) "75-100")
          (td "Correct referencing with a strong, comprehensive reference list."))
      (tr ((td  :align :right) "60-75") (td "Good reference list."))
      (tr ((td :align :right) "50-60") (td "Adequate reference list"))
      (tr ((td :align :right) "25-50")
          (td "Weak reference list. Needs additional papers to be complete and support the current research."))
      (tr ((td :align :right)  "0-25")
          (td "Poor reference list. There are insufficient papers to
   support the current research or to place the current research in a
   correct context."))))
   (LI
    (p (B "Quality of writing (English language) (15%):")
       ((INPUT :NAME :READABILITY :WEIGHTING 15 :VALUE 60 :SIZE 4
               :DATATYPE (INTEGER :MIN 0 :MAX 100)))"/100")
    (table
     (tr ((td :align :right) "75-100")
         (td "Very Highly readable. Well written and easy to read."))
     (tr ((td :align :right) "60-75") (td "Highly Readable."))
     (tr ((td :align :right) "50-60") (td "Adequately Readable."))
     (tr ((td :align :right) "25-50")
         (td "Difficult to read. The writing should be improved to
   help the reader understand what the author is describing."))
     (tr ((td :align :right) "0-25")
         (td "Difficult or impossible to read. Needs rewriting to make the dissertation context clear."))))
   (LI
    (p (B "Clarity (including organisation of material) (20%):")
       ((input :NAME :CLARITY :WEIGHTING 20 :VALUE 60 :size 4
               :DATATYPE (INTEGER :MIN 0 :MAX 100)))"/100")
    (table
     (tr ((td :align :right) "75-100")
         (td "A well-structured exposition of the material that is
   very easy to understand."))
     (tr ((td :align :right) "60-75")
         (td "The literature review is organised and clear. No real problems."))
     (tr ((td :align :right) "50-60")
         (td  "Adequately organised and clear - could be improved."))
     (tr ((td :align :right) "25-50")
         (td "The organisation of the literature review and/or its clarity
   are poor and need to be revised."))
     (tr ((td :align :right) "0-25")
         (td "Haphazard organisation and unclear concepts making it difficult to comprehend the work."))))
   (LI
    (p (B "Conciseness (length relative to substance) (15%):")
       ((INPUT :NAME :CONCISENESS :WEIGHTING 15 :VALUE 60 :SIZE 4
               :DATATYPE (INTEGER :MIN 0 :MAX 100)))"/100")
    (table
     (tr ((td :align :right) "75-100")
         (td "The literature review is well balanced, concise but with sufficient
material to adequately explain the context of the work."))
     (tr ((td :align :right) "60-75"
          (td "Adequate but the literature review could be improved either by removing some redundant or unnecessary information or by providing some additional detail.")))
     (tr ((td :align :right) "30-60")
         (td "The review either contains an excess of unnecessary information or has insufficient information to adequately set the context of the work."))
     (tr ((td :align :right) "0-30")
         (td "This review is either much too long and needs to be cut
   drastically or much too short or missing."))))
   (LI
    (P (B " Comment")
     " justifying the above assessment and indicating improvements which could have been made.")
    ((TEXTAREA :ROWS 10 :COLS 100 :STRUCTURED-TEXT T :NAME :COMMENT :DATATYPE
      (STRING :WORD-COUNT 10)))))
  (P ((INPUT :TYPE :SUBMIT :NAME "Preview" :VALUE "Preview Mark"))
   ((INPUT :TYPE :SUBMIT :NAME "Submit" :VALUE
     "Submit Report and Mark"))))



 ((FORM :METHOD :POST :TITLE "Project Dissertation Report")
  (p "Review the dissertation content excluding the literature review section which is reviewed separately.")
  (OL
   (li
    (p (b "Evaluation of how much of this is the students own work and its suitability as a masters level project (multiplicative term)")
       ((input :name :input :weighting * :value 100 :size 4
               :datatype (number :min 0 :max 100))) "%x")
    (table
     (tr ((td :align :right) "x100%")
         (td "This appears to be entirely the students own work, is properly referenced and is appropriate for a Masters project."))
     (tr ((td :align :right) "x80-100%")
         (td "There are signs of inappropriate referencing in this work or the work is only marginally appropriate for a Masters project."))
     (tr ((td :align :right) "x50-80%")
         (td "There is significant plagiarism in this work or it is inappropriate for a Masters project"))
     (tr ((td :align :right) "x0-50%")
         (td "There are signs that a significant proportion of the work reported is not the students own work or that data has been
     falsified or is substantially in error.")))
    (p "If the evaluation is less than 1 provide comments on the
    issues. If the work contains significant plagiarism or the data
    appears to have been falsified you most notify the programme
    director after completing this form as disciplinary procedures
    need to be invoked."))
   (LI
    (p (B "Quality of writing (English language) (10%):")
       ((INPUT :NAME :READABILITY :WEIGHTING 10 :VALUE 60 :SIZE 4
               :DATATYPE (INTEGER :MIN 0 :MAX 100)))"/100")
    (table
     (tr ((td :align :right) "75-100")
         (td "Very Highly readable. Well written and easy to read."))
     (tr ((td :align :right) "60-75") (td "Highly Readable."))
     (tr ((td :align :right) "50-60") (td "Adequately Readable."))
     (tr ((td :align :right) "25-50")
         (td "Difficult to read. The writing should be improved to
   help the reader understand what the author is describing."))
     (tr ((td :align :right) "0-25")
         (td "Difficult or impossible to read. Needs rewriting to make
         the dissertation  clear."))))
   (LI
    (p (B "Clarity (including organisation of material) (10%):")
       ((input :NAME :CLARITY :WEIGHTING 10 :VALUE 60 :size 4
               :DATATYPE (INTEGER :MIN 0 :MAX 100)))"/100")
    (table
     (tr ((td :align :right) "75-100")
         (td "A well-structured exposition of the material that is
   very easy to understand."))
     (tr ((td :align :right) "60-75")
         (td "The dissertation is organised and clear. No real problems."))
     (tr ((td :align :right) "50-60")
         (td  "Adequately organised and clear - could be improved."))
     (tr ((td :align :right) "25-50")
         (td "The organisation of the dissertation and/or its clarity
   are poor and need to be revised."))
     (tr ((td :align :right) "0-25")
         (td "Haphazard organisation and unclear concepts making it
         difficult to comprehend the work."))))
   (LI
    (p (B "Conciseness (length relative to substance) (10%):")
       ((INPUT :NAME :CONCISENESS :WEIGHTING 10 :VALUE 60 :SIZE 4
               :DATATYPE (INTEGER :MIN 0 :MAX 100)))"/100")
    (table
     (tr ((td :align :right) "75-100")
         (td "The dissertation is well balanced, concise but with sufficient
material to adequately explain the work."))
     (tr ((td :align :right) "60-75")
          (td "Adequate but the dissertation could be improved either
          by removing some redundant or unnecessary information or by
          providing some additional detail."))
     (tr ((td :align :right) "30-60")
         (td "The dissertation either contains an excess of unnecessary information or has insufficient information to adequately explain the results."))
     (tr ((td :align :right) "0-30")
         (td "This dissertation is either much too long or much too short."))))
   (LI
    (p (B "Relevance of figures (5%):")
       ((input :NAME :FIGURES :WEIGHTING 5 :VALUE 60 :size 4
               :DATATYPE (INTEGER :MIN 0 :MAX 100))) "/100")
    (TABLE
    (tr ((td :align :right) "75-100")
        (td "Excellent graphics that illuminate the text."))
    (tr ((td :align :right) "60-75") (td "Good graphics supporting the text"))
    (tr ((td :align :right) "50-60") (td "The graphics are adequate at best."))
    (tr ((td :align :right) "25-50")
        (td "The figures require revision to increase comprehension or
        readability or there is an excess of trivial figures."))
    (tr ((td :align :right) "0-25")
        (td "Lacks figures to make the text comprehensible, or the
   figures are so poorly drawn as to be useless or figures copied from
   other source with no referencing or evidence of copyright permission."))))
   (LI
    (p
     (B
      "Originality (if not original, cite prior publications in
     comments section): (5%)")
    ((input :NAME :ORIGINALITY :WEIGHTING 5 :VALUE 60 :size 4
            :DATATYPE (INTEGER :MIN 0 :MAX 100))) "/100")
    (table
     (tr ((td :align :right) "90-100")
         (td  "Novel contribution of fundamental importance."))
     (tr ((td :align :right) "70-90")
         (td  "New work. I know of no comparable effort."))
     (tr ((td :align :right) "50-70")
         (td "Derivative work, but provides new results."))
     (tr ((td :align :right) "25-50")
         (td "This manuscript is very similar to the work of others."))
     (tr ((td :align :right) "0-25")
         (td "This has been done before. The manuscript should be
   rejected."))))
   (LI
    (p
     (B "Significance of results (degree of contribution to field) (5%):")
     ((input :NAME :SIGNIFICANCE :WEIGHTING 5 :VALUE 60 :size 4
             :DATATYPE (INTEGER :MIN 0 :MAX 100))) "/100")
    (table
     (tr ((td :align :right) "90-100")
         (td "This is a major advance in this field."))
     (tr ((td :align :right) "70-90") (td "Advances the field."))
     (tr ((td :align :right) "50-60")
         (td "A modest advance that may lead to additional work."))
     (tr ((td :align :right) "25-50")
         (td "No one will care about the work in this dissertation. (40)"))
     (tr ((td :align :right) "0-25")
         (td "The results are trivial and the dissertation should be rejected."))))
   (LI
    (p (B "Technical accuracy of results (20%):")
       ((input :NAME :ACCURACY :WEIGHTING 20 :VALUE 60 :size 4
               :DATATYPE (INTEGER :min 0 :MAX 100))) "/100")
    (table
     (tr ((td :align :right) "75-100")
         (td "The dissertation is accurate. It cannot be faulted on its
   methods, analysis, or conclusions."))
     (tr ((td :align :right) "60-75")
         (td "The dissertation is accurate, but its methods, analysis, or
   conclusions could be improved."))
     (tr ((td :align :right) "50-60")
         (td "There is minor inaccuracy in this dissertation that should
   be corrected."))
     (tr ((td :align :right) "25-50")
          (td  "There is major inaccuracy in this dissertation that must
   be corrected."))
     (tr ((td :align :right) "0-25")
         (td "There are sufficient inaccuracies in this dissertation that
   it should be rejected."))))
   (LI
    (p  (B "Rigour (analytical or practical, whichever is appropriate) (15%):")
        ((input :NAME :RIGOUR :WEIGHTING 15 :VALUE 60 :size 4
                :DATATYPE (integer :min 0 :MAX 100))) "/100")
    (table
     (tr ((td :align :right) "75-100")
         (td "Well derived or argued dissertation."))
     (tr ((td :align :right) "60-75")
         (td "Provides sufficient rigour in the manuscript that the
   results appear to be reasonable and accurate."))
     (tr ((td :align :right) "50-60")
         (td "Needs to provide a better argument in places."))
     (tr ((td :align :right) "25-50")
         (td "Sloppy methods or analysis."))
     (tr ((td :align :right) "0-25")
         (td "Lacks rigour. The results cannot be
   substantiated based on the evidence provided."))))
   (LI (p (B "Detail level of procedures outlined (10%):")
             ((input :NAME :DETAIL :WEIGHTING 10 :VALUE 60 :size 4
                     :DATATYPE (INTEGER :min 0 :MAX 100))) "/100")
   (table
    (tr ((td :align :right) "75-100")
        (td "The details in this dissertation are numerous so that it is
   easy for me to understand the importance of the results and the
   techniques by which they were arrived at."))
    (tr ((td :align :right) "60-75")
        (td "The details in this manuscript are sufficient to permit me
   to understand the results and the techniques."))
    (tr ((td :align :right) "50-60")
        (td "The manuscript lacks some details so that I cannot be
   certain that the results are correct."))
    (tr ((td :align :right) "25-50")
        (td "There are a number of details missing and they must be
   included to be able to justify the results."))
    (tr ((td :align :right) "0-25")
        (td "The manuscript contains so few details that it is
   impossible to judge its worth. It should be rejected."))))
   (LI
    (p (B "Substantiation of conclusions (10%):")
       ((input :NAME :CONCLUSIONS :WEIGHTING 10 :VALUE 60 :size 4
               :DATATYPE (INTEGER :min 0 :MAX 100))) "/100")
    (table
     (tr ((td :align :right) "75-100")
         (td "If I performed the same work, I believe I would reach the
   same conclusions."))
     (tr ((td :align :right) "60-75")
         (td "If I performed the same work, I am fairly confident I
   would reach the same conclusions."))
     (tr ((td :align :right) "50-60")
         (td "If I performed the same work, I might reach the same
   conclusions, but I have some doubts."))
     (tr ((td :align :right) "25-50")
          (td "If I performed the same work, I doubt I would reach the
   same conclusions."))
     (tr ((td :align :right) "0-25")
         (td "The manuscript does not substantiate the conclusions stated."))))
   (LI
    (P (B "Comment")
       " justifying the above assessment and indicating improvements which could have been made.")
    ((TEXTAREA :ROWS 10 :cols 100 :STRUCTURED-TEXT T :NAME :COMMENT :DATATYPE
      (STRING :WORD-COUNT 10)))))
  (P ((INPUT :TYPE :SUBMIT :NAME "Preview" :VALUE "Preview Mark"))
   ((INPUT :TYPE :SUBMIT :NAME "Submit" :VALUE
     "Submit Report and Mark"))))

("Submit" "Submit Report and Mark" "Preview" "Preview Mark" :COMMENT NIL
 :CONCLUSIONS 60 :DETAIL 60 :RIGOUR 60 :ACCURACY 60 :SIGNIFICANCE 60
 :ORIGINALITY 60 :FIGURES 60 :CONCISENESS 60 :CLARITY 60 :READABILITY 60 :INPUT
 100)

|#
