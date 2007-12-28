;; $Id$
;; some utilities for ewas-assements

(in-package :clews.assessment)

(setq
 *test*
 (make-instance
  'questionnaire
  :timelimit 30
  :questions
  '((californians
     multiple-choice-q 
     :question "How many californians does it take to change a light bulb?"
     :choices (1 6 50 472 (0 . "None"))
     :answer 6
     :type (integer :min 0 :max 472 :nil-allowed t)
     :feedback "One to turn the bulb, one for support,
and four to relate to the experience")
    (windoze
     multiple-choice-q
     :question "How many windows programmers does it take to change a lightbulb?"
     :choices (1 4 50 472 (0 . "None"))
     :answer 472
     :type (integer :min 0 :max 472 :nil-allowed t)
     :feedback "One to write WinGetLightBulbHandle, one to write WinQueryStatusLightBulb, one to write WinGetLightSwitchHandle...")
    (sqrt(user-data)
	  (unless (getf (rest user-data) :v)
	    (setf (getf (rest user-data) :v) (random 1.0)))
	  (let ((v (getf (rest user-data) :v)))
	    `(numeric-q
	      :question ,(format nil "What is the square root of ~,4G?" v)
	      :answer ,(sqrt v)
	      :feedback "Get a freaking calculator mate"))))))

(setq *k* (list nil))
(setq *k* (initialise-knowledge *k* *test*))