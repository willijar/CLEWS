; Some randomly useful add-hoc interactive stuff
(in-package :aston)
(defun set-tutorial-deadlines(users assessments deadline)
  (setq deadline (clews-assessment::datespec deadline))
  (dolist (username users)
    (let* ((user (get-dictionary username *user-source*))
	   (know (property (property-subset user :adaptive-tutor) :knowledge)))
      (dolist (assessment assessments)
	(setf (property (get-dictionary assessment know) :deadline-date)
	      deadline)
	(format t "~S:~S -> ~S~%" username assessment
		(get-dictionary assessment know)))
      (setf (get-dictionary username *user-source*) user))))

(defun get-assessment-ids(id
			  &key (recurse t) (tutor *tutorial*))
  (let ((concept (adaptive-tutor::concept tutor id)))
    (mapcan
     #'(lambda(concept)
	 (when (assessment concept)
	   (list (adaptive-tutor::concept-id concept))))
     (cons concept (when recurse (adaptive-tutor::children concept))))))

(defun set-assessment-field(userspec conceptspec field value
			    &key (recurse t) (tutor *tutorial*)
			    (user-source *user-source*) (set-nil t))
  (let ((value
	 (ccase field
	   ((:started :completed :start-date :end-date
		      :deadline-date :feedback-date )
	    (clews-assessment::datespec value))
	   ((:timelimit :no-questions-counted) value))))
    (format t "users=~S~%assessments=~S~%"
	    (get-users userspec user-source)
	    (get-assessment-ids conceptspec
				:recurse recurse :tutor tutor))
    (dolist (username (get-users userspec user-source))
      (let ((user (get-dictionary username (users user-source))))
      (dolist (conceptid (get-assessment-ids conceptspec
					     :recurse recurse :tutor tutor))
	(let ((know (adaptive-tutor::knowledge
		     (user-component-properties tutor user) conceptid)))
	  (format t "~A knows ~A~%"  username know)
	  (if (or value (not set-nil))
	      (setf (property know field) value)
	      (rem-property know field))
	  (format t "~A:~A -> ~S~%"
		  username conceptid
		  know)))
      (setf (get-dictionary username user-source) user)))))

#|

(set-tutorial-deadlines '("culshama") '("drichlets-conditions" "phase-and-group-delay" "graphical-convolution" "fourier-transform-of-a-radio-pulse" "fourier-transform-of-a-pulse" "fourier-transform-properties" "dsb-sc-demodulation"))

(set-assessment-field '(:tt2003 :it2003 "allenp2" "dhillonh"
"karalekv" "hicksdj" "wilkinpm" "lindleyo" "buckinjw" "trothp"
"culshama") "fourier-transforms-of-periodic-signals" :deadline-date
"2003-12-13" :recurse nil)

(defun allocate-assessment(users assessments &optional (count 1))
  "Given a list of user objects and a list of assessments allocate count
   assessments to each user. Returns a list of cons of users and assessments"
  (let ((result '())
	(n (length assessments)))
    (dolist (user users)
      (let ((allocated '()))
	(loop
	 (if (>= (length allocated) count) (return))
	 (pushnew (elt assessments (random n)) allocated :test #'equal))
	(dolist (item allocated)
	  (push (cons user item) result))))
    result))

(setq users '(      
 "alig1"---
"asherk""aslami""bansalvb""barkerl""bickera""booseyma""bridgena""cliffgj""doalps""egbep""falahyn""fazosd""ganatrar""ghataops""gillinnr""gilmorma""guirguia""harrisjt""haywoorj""hirjia""ishaqs""jiny""judgeas""kasondel""keeblesp""kellybs""khanh""leungeyo""lloyddag""lowethca""maanks""malhotry""mistryp2""neoksmip""nfalahy""perdawk1""rashids2""rylandbj""shabira""shahh""ulrahmah""wildedre""wub"))

(setq assessments '("http://www.cnn.com/""http://www.salon.com/""http://www.theregister.co.uk/""http://slashdot.org/""http://www.bbc.co.uk/""http://www.kuro5hin.org/""http://www.theregister.co.uk/""http://www.eu.microsoft.com/default.htm""http://www.easyjet.com/en/""http://www.amazon.co.uk""http://www.extremecomputing.com/""http://www.uk.bol.com""http://www.cbso.co.uk/""http://www.rsc.org.uk/""http://mindstorms.lego.com/""http://www.mp3.com""http://www.sonymusic.com/""http://www.timewarner.com""http://www.scoot.co.uk/""http://www.hyperion-records.co.uk/""http://www.interplay.com/""http://www.epinions.com/""http://www.ee.aston.ac.uk/teaching/pg""http://www.aston.ac.uk/""http://www.useit.com""http://www.ti.com/sc/docs/general/dsmenu.htm""http://hotwired.lycos.com/webmonkey/""http://java.sun.com/""http://www.ee.aston.ac.uk/research/photonics""http:/www.ee.aston.ac.uk/research/photonics/members/willijar""http://www.hawking.org.uk/home/hindex.html""http://www.richarddawkins.com/""http://www.susanblackmore.co.uk/""http://www.iec.org/""http://www.iee.org/""http://www.ieee.org/""tttp://www.gnu.org/""http://www.alistapart.com/"w"http://www.baesystems.com/"))


(dolist (review (allocate-assessment users assessments 4))
  (format t "INSERT INTO web_reviews (studentid,url) values ('~A','~A');~%"
	  (car review) (cdr review)))
|#

#|
  (set-tutorial-deadlines '("trothp") '("phase-and-group-delay"
"decibels"
"graphical-convolution"
"fourier-transforms-of-periodic-signals"
"fourier-transform-of-a-radio-pulse"
"fourier-transform-of-a-pulse"
"fourier-transform-properties"
"drichlets-conditions"
"representations-of-signals-and-systems"
"why-modulate"
"digital-modulation-stages"
"what-is-modulation"
"bandwidth-requirements-of-common-signals") "2003-11-13")
|#

#|
#|
(setq *s* (sort (student-records *db* 'year 2004) #'string< :key #'username))

(mapcar #'(lambda(s) (list (username s) (firstname s) (initials s) (lastname s) (fullname s)))
	*s*)

(mapcar #'(lambda(r)
	    (let ((s (car (student-records *db* 'username (first r)))))
	      (setf (firstname s) (second r)
		    (initials s) (third r)
		    (lastname s) (fourth r))
	      (update-records-from-instance s)
	      (fullname s)))
'(("abdio" "Omar" "O" "Abdi" "Omar O Abdi")
 ("aliam" "Abdirizak" "Musse" "Ali" "Abdirizak Musse Ali")
 ("alsayark" "Khalid" "K" "Alsayari" "Khalid K Alsayari")
 ("amant" "Taliah" nil "Aman" "Taliah T Aman")
 ("arshadma" "Muhammad" "Asim" "Arshad" "Muhammad Asim Arshad")
 ("atuchukk" "Kodilinye" "Chidolue" "Atuchukwu"
  "Kodilinye Chidolue Atuchukwu")
 ("azilomei" "Azilomen" NIL "Ibawoh" "Azilomen Ibawoh")
 ("balfouea" "Esmond" NIL "Balfour" "Esmond Balfour")
 ("barigyem" "Maurice" "F" "Barigye" "Maurice F Barigye")
 ("cheemafa" "Farooq" "Ahmed" "Cheema" "Farooq Ahmed FA Cheema")
 ("darsh" "Saddatt" "Hanif" "Dar" "Saddatt Hanif Dar")
 ("dhandk" "Karan" nil "Dhand" "Karan K Dhand")
 ("haqueh" "Hasanul" nil "Haque" "Hasanul H Haque")
 ("iqbaly" "Yasir" nil "Iqbal" "Yasir Y Iqbal")
 ("jarryg" "Gregory" nil "Jarry" "Gregory G Jarry")
 ("jiangs" "Shaocheng" NIL "Jiang" "Shaocheng Jiang")
 ("johnson1" "Nigel" NIL "Johnson" "Nigel Johnson")
 ("kapoors1" "Suhail" nil "Kapoor" "Suhail S Kapoor")
 ("karalekv" "Vassilios" nil "Karalekas" "Vassilios V Karalekas")
 ("liuy11" "Yang" NIL "Liu" "Yang Liu")
 ("mirzaa" "Asim" nil "Mirza" "Asim A Mirza")
 ("mirzamb" "Mohammed" "Baig" "Mirza" "Mohammed Baig Mirza")
 ("mup" "Pengfei" NIL "Mu" "Pengfei Mu")
 ("naeemmf" "Muhammad Faisal" nil "Naeem" "Muhammad Faisal MF Naeem")
 ("ngotp" "The Phong" nil "Ngo" "The Phong TP Ngo")
 ("nguyentd" "Tuan Duong" nil "Nguyen" "Tuan Duong TD Nguyen")
 ("prietojp" "Juan" "Pablo" "Prieto Baez"
  "Juan Pablo BAEZ JP Prieto Baez")
 ("salehma" "Muhaned" "Abdo" "Saleh" "Muhaned Abdo MA Saleh")
 ("sowtermk" "Mark" NIL "Sowter" "Mark Sowter")
 ("stevensn" "Neil" nil "Stevens" "Neil N Stevens")
 ("tranbn" "Bao" "Ngoc" "Tran" "Bao Ngoc BN Tran")
 ("wangk" "Kang" NIL "Wang" "Kang Wang")
 ("werquinm" "Manuel" "F" "Werquin" "Manuel F Werquin")
 ("yaaqubam" "Abubakar" "Maarof" "Yaaqub" "Abubakar Maarof Yaaqub")
 ("zhangg1" "Guchun" nil "Zhang" "Guchun G Zhang")
 ("zhangj2" "Jing" nil "Zhang" "Jing J Zhang")
 ("zhaor" "Rongrong" nil "Zhao" "Rongrong R Zhao")
 ("zhongzz" "Zhi Zhou" nil "Zhong" "Zhi Zhou ZZ Zhong")))
|#