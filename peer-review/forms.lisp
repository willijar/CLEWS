;;;; Peer review default forms
;;;; Copyright (C) 2002-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; $Id: forms.lisp,v 1.1 2006/07/30 17:43:08 willijar Exp willijar $
(in-package :clews.peer-review)

(defform "peer-review-2000"
    '((:m1
       :text "How would you rate the accuracy of this topic?"
       :markup ((mcq)
                (12 . "Completely accurate")
                (8 . "Some Errors by omission")
                (4 . "Some errors in content")
                (2 . "Error Prone")
                (0 . "A tissue of lies"))
       :type (integer :max 12 :min 0)
       :default 8 )
      (:m2
       :text "How informative was this topic?"
       :markup ((mcq)
                (8 . "Essential Reading")
                (6 . "Very Informative")
                (4 . "Informative")
                (2 . "Some value")
                (0 . "Useless"))
       :type (integer :max 8 :min 0)
       :default 6)
      (:m3
       :text "How clear and readable was this article?"
       :markup ((mcq)
                (8 . "Couldnt be clearer")
                (6 . "Readable")
                (4 . "Needs Careful Reading")
                (2 . "Unclear")
                (0 . "Impossible to read"))
       :type (integer :max 8 :min 0)
       :default 6)
      (:m4
       :text "How interesting was the article?"
       :markup ((mcq)
                (0 . "I fell asleep")
                (1 . "Dull")
                (3 . "Of some interest")
                (4 . "Interesting")
                (6 . "Enthralling"))
       :type (integer :max 6 :min 0)
       :default 3)
      (:m5
       :text "How Original do you think the article was?"
       :markup ((mcq)
                (0 . "A copy and paste job")
                (2 . "Entirely Paraphrased")
                (4 . "Some paraphrasing")
                (6 . "Mostly Original")
                (8 . "Original"))
       :type (integer :max 8 :min 0)
       :default 6)
      (:c6
       :text "Please justify your choices."
       :markup ((textarea :cols 80 :rows 10))
       :default "In not less than 50 words."
       :type (string :word-count 50))
      (:anonymous
       :text "Check here if you wish your review to remain anonymous."
       :markup ((boolean))
       :default nil)))

(defmethod clews.form:form-mark (data (form (eql (find-form "peer-review-2000"))))
  (* 100.0 (/ (apply #'+ (mapcar (lambda(s) (getf data s))
                                 '(:m1 :m2 :m3 :m4 :m5))) 40)))

(defform "peer-review-article"
    '((:title
       :text "Article Title"
       :markup ((input :type "text" :width 81))
       :type (string :word-count 1 :min-length 3))
      (:content
       :text "Enter your article using restructured text."
       :markup ((textarea :cols 80 :rows 40 :structured-text t))
       :default "Not less than 250 words."
       :type (string :word-count 1000 :strip-return t))
      (:anonymous
       :text "Check here if you wish your article to remain anonymous to other students."
       :markup ((boolean))
       :default nil))
  :actions '(("Edit" . "Update Article") ("Images" . "Manage
Images")))

