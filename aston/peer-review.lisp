;;;; Aston Web Applications
;;;; Copyright (C) 2002-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; $Id: peer-review.lisp,v 1.17 2005/03/13 09:33:46 willijar Exp $

(in-package :aston)

;; review forms used in the past

(defform "peer-review-2001"
    '((:accuracy
       :text "How would you rate the accuracy of this topic?"
       :markup ((mcq)
                (0.0 . "<70%")
                (0.5 . "70-80%")
                (0.6 . "80-90%")
                (0.8 . "90-98%")
                (0.95 . "98-99%")
                (1.0 . "100%"))
       :type (number :max 1.0 :min 0.0)
       :default 0.95)
      (:inform
       :text "How informative was this topic?"
       :markup ((mcq)
                (0.0 . "Useless")
                (0.4 . "Some Value")
                (0.6 . "Moderately Informative")
                (0.7 . "Informative")
                (0.9 . "Very Informative")
                (1.0 . "Essential Reading"))
       :type (number :max 1.0 :min 0.0)
       :default 0.6)
      (:clarity
       :text "How clear and readable was this article?"
       :markup ((mcq)
                (0.0 . "Impossible to read")
                (0.4 . "Unclear")
                (0.55 . "Needs Careful Reading")
                (0.7 . "Average Readability")
                (0.8 . "Very easy to read")
                (1.0 . "Couldnt be clearer"))
       :type (number :max 1.0 :min 0.0)
       :default 0.7)
      (:interest
       :text "How interesting was the article?"
       :markup ((mcq)
                (0.0 . "I fell asleep")
                (0.4 . "Dull")
                (0.55 . "Of some interest")
                (0.7 . "Interesting")
                (0.9 . "Very interesting")
                (1.0 . "Enthralling"))
       :type (number :max 1.0 :min 0.0)
       :default 0.55)
      (:originality
       :text "How Original do you think the article was?"
       :markup ((mcq)
                (0.0 . "Straight copy")
                (0.4 . "Entirely Paraphrased")
                (0.8 . "Some paraphrasing")
                (0.95 . "Mostly Original")
                (1.0 . "Entirely Original"))
       :type (number :max 1.0 :min 0.0)
       :default 0.95)
      (:comment
       :text "Please justify your choices."
       :markup ((textarea :cols 30 :rows 10 :structured-text t))
       :default "In not less than 30 words."
       :type (string :word-count 30))
      (:anonymous
       :text "Check here if you wish your review to remain anonymous."
       :markup ((boolean))
       :default nil))
  :actions '(("Review" . "Add Review")))

(defmethod clews.form:form-mark(data (form (eql (find-form "peer-review-2001" ))))
  (* 100 (getf data :accuracy) (getf data :originality)
     (/ (+ (getf data :inform) (getf data :clarity)
           (getf data :interest))
        3)))



(clews.form:register-form
 "peer-review-2003"
 '((form :name "peer-review-2003" :method "POST")
   ((section :title "Organization")
    (ol
     (li "Were the basic sections (Introduction, Conclusion, Literature
   Cited, etc.) adequate? If not, comment on what is missing? "
         ((mcq :name :sections :value 0 :datatype number)
          (0 . "No")
          (3 . "Yes")))
     (li "Did the writer use subheadings well to clarify the sections of the text? Explain if you think this is not the case "
         ((mcq :name :subsections :value 0 :datatype (integer :min 0 :max 2))
          (0 . "No")
          (2 . "Yes")))
     (li "Was the material ordered in a way that was logical, clear, easy to follow? Explain. "
         ((mcq :name :logical :value 0 :datatype (integer :min 0 :max 4))
          (0 . "No")
          (4 . "Yes")))))
   ((section :title "Format")
    ((ol :start 4)
     (li "Was the authors use of formatting in the text correct and good?
Provide an evaluation (0-5) " (br)
((input :size 3 :name :format  :value 0
        :datatype (integer :min 0 :max 5))))))
   ((section :title "Citations")
((ol :start 5)
 (li "Did the writer cite sources adequately and appropriately? "
     ((mcq :name :citations :value 5
           :datatype (integer :min 0 :max 10))
      (10 . "Yes")
      (5 . "Mostly")
      (0 . "No"))) * Fix adaptive tutorial marks page

 (li "Did the author  present any material copied directly from another
source without making it clear that it was not his/her own work?
Do a quick check on Google using a characteristic phrase from the text.
If so give an estimate of what percentage (0-100) of the article was copied " (br)
((input :name :copied :value 0 :size 3
        :datatype (integer :min 0 :max 100))) " %")
 (li "Did the author paraphrase material from another source without
quoting the original source? If so entered approximately what percentage
 (0-100) of the article was paraphrased " (br)
((input :name :paraphrased :value 0 :size 3
        :datatype (integer :min 0 :max 100))) " %")))
                                        ;YES=x0.6 NO = x1
   ((section :title "Grammar and Style (20%)")
((ol :start 8)
 (li "Were there any grammatical or spelling problems? If so
please and suggest corrections in your comments. Check for
paragraphing and punctuation. Provide a mark 0-10 for the spelling and
grammar " (br)
((input :name :grammar :value 0 :size 3
        :datatype (integer :min 0 :max 10))))
#|
 For example, has the author
 used the correct forms of the verbs, do the nouns and adjectives agree
 in gender and number, has he used the correct articles, is there a
 repetition of vocabulary, are the sentences too long or too
 short,etc.? Check for paragraphing and punctuation.
 |#
     (li "Was the writer's writing style clear? Were the paragraphs
   and sentences cohesive? Provide a mark 0-10 for style " (br)
	 ((input :name :style :value 0 :size 3
		 :datatype (integer :min 0 :max 10))))))
   ((section :title "Content (50%)")
    ((ol :start 9)
     (li "Did the writer adequately summarize and discuss the topic? Comment
on how the article could be improved. Provide a mark 0-25 " (br)
	 ((input :name :discuss :value 0 :size 3
		 :datatype (integer :min 0 :max 25))))
     (li "Did the writer comprehensively cover appropriate materials
   available from the standard sources (e.g. web, books, lecture
   notes, libraries)? If not, indicate what's missing in then comments?" (br)
	 ((mcq :name :sources :value 5 :datatype (integer :min 0 :max 5))
	  (5 . "Yes")
	  (0 . "No")))
     (li "Did the writer make some contribution of thought to the paper, or
    merely summarize data or publications? Explain in the comments and give an evaluation of the authors own contribution of thought (0-20). "
	 ((input :name :thought :value 0 :size 3
		 :datatype (integer :min 0 :max 20))))))
   ((section :title "Comments")
    (p  "Please justify your choices and provide constructive feedback below."
	((textarea :name :comment :cols 35 :rows 10 :structured-text t
		   :datatype (string :word-count 50))))
    (p "Check here if you wish your review to remain anonymous."
       ((boolean :name :anonymous :value nil)) )
    ((p :align "right")
     ((input :type "submit" :name "Review" :value "Submit Review")))
    )))

(defmethod clews.form:form-mark(data
                                (form (eql (find-form "peer-review-2003" ))))
  (flet ((value(symbol) (getf data symbol)))
    (* (- 1 (/ (value :copied) 66))
       (- 1 (* 6/1000 (value :paraphrased)))
       (reduce #'+
               (mapcar
                #'value
                '(:sections :subsections :logical :citations
                  :grammar :style :discuss :sources :thought))))))

(clews.form:register-form
 "peer-review-2004"
 '((form :name "peer-review-2004" :method "POST")
   (p "Provide comments relating to each aspects of the review in the comment box at the end.")
   ((section :title "Organization")
    (ol
     (li "Were the basic sections (Introduction, Conclusion, Literature
   Cited, etc.) adequate? If not, comment on what is missing? "
         ((mcq :name :sections :value 0 :datatype number)
          (0 . "No")
          (1 . "Yes")))
     (li "Did the writer use subheadings well to clarify the sections of the text? Explain if you think this is not the case "
         ((mcq :name :subsections :value 0 :datatype number)
          (2 . "Yes")
          (1 . "Partially")
          (0 . "No")))
     (li "Was the material ordered in a way that was logical, clear, easy to follow? Explain. "
         ((mcq :name :logical :value 0 :datatype number)
          (0 . "No")
          (1 . "Yes")))))
   ((section :title "Format")
    ((ol :start 4)
     (li "Describe the authors use of formatting in the text?"
         (br)
         ((mcq :name :format :value 2 :style :horizontal :datatype number)
          (4 . "Exceptional")
          (3 . "Admirable")
          (2 . "Acceptable")
          (1 . "Amateur")))))
   ((section :title "Citations")
    ((ol :start 5)
     (li "Did the writer cite sources adequately and appropriately? "
         ((mcq :name :citations :value 1 :style :horizontal :datatype number)
          (2 . "Yes")
          (1 . "Partially")
          (0 . "No")))
     (li "Did the author  present any material copied directly from another
source without making it clear that it was not his/her own work?
Do a quick check on Google using a characteristic phrase from the text.
If so give an estimate of what percentage (0-100) of the article was copied "
         (br)
         ((input :name :copied :value 0 :size 3
                 :datatype (integer :min 0 :max 100))) " %")
     (li "Did the author paraphrase material from another source without
quoting the original source? If so entered approximately what percentage
 (0-100) of the article was paraphrased " (br)
 ((input :name :paraphrased :value 0 :size 3
         :datatype (integer :min 0 :max 100))) " %"))) ;YES=x0.6 NO = x1
   ((section :title "Grammar and Style")
    ((ol :start 8)
     (li "Were there any grammatical or spelling problems? If so
please and suggest corrections in your comments. Check for
paragraphing and punctuation.  Check for paragraphing and
punctuation. For example, has the author used the correct forms of the
verbs, do the nouns and adjectives agree in gender and number, has he
used the correct articles, is there a repetition of vocabulary, are
the sentences too long or too short,etc.?  Provide an assessment of
the spelling and grammar."
         (br)
         ((mcq :name :grammar :value 2 :style :horizontal :datatype number)
          (4 . "Excellent")
          (3 . "Admirable")
          (2 . "Acceptable")
          (1 . "Amateur")
          (0 . "Unreadable")))
     (li "Was the writer's writing style clear? Were the paragraphs
and sentences cohesive? Provide a qualitative assessment of the
authors writing style."
         (br)
         ((mcq :name :style :value 2 :style :horizontal :datatype number)
          (4 . "Excellent")
          (3 . "Admirable")
          (2 . "Acceptable")
          (1 . "Amateur")
          (0 . "Unreadable")))))
   ((section :title "Content")
    ((ol :start 9)
     (li "Evaluate how adequately the writer summarised and discussed the topic? Comment on how the article could be improved."
         (br)
         ((mcq :name :discuss :value 2 :style :horizontal :datatype number)
          (4 . "Excellent")
          (3 . "Admirable")
          (2 . "Acceptable")
          (1 . "Amateur")
          (0 . "inadequate")))
     (li "Did the writer comprehensively cover appropriate materials
   available from the standard sources (e.g. web, books, lecture
   notes, libraries)? If not, indicate what's missing in the comments?"
         (br)
         ((mcq :name :sources :value 1 :style :horizontal :datatype number)
          (2 . "Yes")
          (1 . "Partially")
          (0 . "No")))
     (li "Did the writer make some contribution of thought to the paper, or
    merely summarize data or publications? Explain in the comments and give an evaluation of the authors own contribution of thought. "
         ((mcq :name :thought :value 2 :style :horizontal :datatype number)
          (4 . "Excellent")
          (3 . "Admirable")
          (2 . "Acceptable")
          (1 . "Amateur")
          (0 . "inadequate")))))
   ((section :title "Comments")
    (p  "Please justify your choices and provide constructive feedback below."
     ((textarea :name :comment :cols 35 :rows 10 :structured-text t
                :datatype (string :word-count 50))))
    (p "Check here if you wish your review to remain anonymous."
     ((boolean :name :anonymous :value nil)) )
    ((p :align "right")
     ((input :type "submit" :name "Review" :value "Submit Review"))) )))

(defmethod clews.form:form-mark(data
                                (form (eql (find-form "peer-review-2004" ))))
  (flet ((value(symbol) (getf data symbol)))
    (* (- 1 (/ (value :copied) 66))
       (- 1 (* 6/1000 (value :paraphrased)))
       (reduce
        #'+
        (mapcar #'(lambda(c) (* (car c) (value (cdr c))))
                '((3 . :sections)
                  (2/2 . :subsections)
                  (5 . :logical)
                  (5/4 . :format)
                  (10/2 . :citations)
                  (10/4 . :grammar)
                  (10/4 . :style)
                  (25/4 . :discuss)
                  (5/2 . :sources)
                  (25/4 . :thought)))))))

(clews.form:register-form
 "peer-review-feedback-2005"
 '((form :name "peer-review-feedback-2005" :method "POST")
   (p (em "Review Feedback"))
   (p "How helpful (constructive) did you find this review?"
    ((mcq :name :helpfulness :value 2 :datatype (integer :max 4) :style :dropdown)
     (0 . "Not at all helpful")
     (1 . "Slightly Helpful")
     (2 . "Quite helpful")
     (3 . "Helpful")
     (4 . "Very Helpful")))
   (p  "What changes to your future article writing will you make as a result of this review." (br)
    ((textarea :name :comment :cols 35 :rows 2)))
   (p ((input :type :submit
              :name :submit :value "Submit Review Feedback")))))

(defvar *peer-review*
  (make-instance
   'clews.peer-review:peer-review
   :article-dictionary
   (make-filesystem-dictionary
    (translate-logical-pathname "clews:peer-review;*.lisp")
    :deleted-directory (translate-logical-pathname
                        "clews:peer-review;deleted;"))
   :image-directory (translate-logical-pathname "clews:peer-review;images;")
   :review-form (find-form "peer-review-2004")
   :review-feedback-form (find-form "peer-review-feedback-2005")
   :user-dictionary *user-source*
   :plugins (list *polls-plugin*
                  *who-plugin*
                  *chatterbox-plugin*
                  *error-reporter-plugin*
                  *quotes-plugin*)
   :acl '((:view . (:all))
          (:student . (:dn2008 "molowam1" "mira2" "mahmooka"
                       "tonadeb" "mohamehg"))
          (:tutor . ("willijar"))
          (:admin . ("willijar")))
   :pwd-source *pwd-source* ))

(defmethod render-page ((app (eql *peer-review*)) stream
                        markup)
  (html stream (apply-aston-style markup)))
