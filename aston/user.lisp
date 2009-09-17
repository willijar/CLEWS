;;;; Aston Web Applications
;;;; Copyright (C) 2002-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; $Id: user.lisp,v 1.1 2007/07/03 19:05:37 willijar Exp willijar $

(in-package :aston)

(defvar +TT2006+
  '(("portatle" "Portatadino" "Luis")
    ("rizwisab" "Rizwi" "Syed")
    ("bameeb" "Bame" "Emmanuel")
    ("perdawk1" "Perdawood" "Kawa")
    ("mirzahs" "Mirza" "Haseeb")
    ("iros" "Iro" "Stephen")
    ("bugajevk" "Bugajev" "Kiril")
    ("avosehsa" "Avoseh" "Suru")
    ("odierols" "Odiero" "Lawi")
    ("mbihmn" "Mbih" "MacElenus Ntih")
    ("ajiaao" "Ajia" "Abdulkabir")))

(defvar +IT2006+
 '(("hussas17" "Hussain" "Saeed")
   ("hassanf" "Hassan" "Fazli")
   ("bareghem" "Baregheh" "Mandana")
   ("syedh" "Syed" "Hamza")
   ("harperc"  "Harper" "Christopher")))


(defvar +PN2006+
  '(("osmansm"  "Osman"  "Said")))

(defvar +EE3DTR-2005+
  '("amonooef" "bhogaydm" "osmansm" "qureshj1"  "harrisjt" "smithhl"))

(defvar +STAFF+
  '(("webbdj" "Webb" "David J")
    ("wrennms" "Wrenn" "Mike S")
    ("zhangl" "Zhang" "Lin")
    ("harperp" "Harper" "P.")
    "norriswt"
    ("traila" "Trail" "A.")
    ("chuyk" "Chu" "Yuen")
    ("lowed" "Lowe" "D.")
    ("bennioni" "Bennion" "I.")
    ("willijar" "Williams" "John A R")
    ("pengx1" "Peng" "X.")
    ("blowkj" "Blow" "K.J.")
    ("khrushci" "Khruschev" "I.")
    ("yardhm" "Yard" "Helen")
    ("carpengf" "Carpenter" "G.F.")
    ("turitssk" "Turitsyn" "S.K.")
    ("mezentsv" "Mezentsev" "V.")
    ("holdindj" "Holding" "D.J.")
    ("eberhama" "Eberhard" "Marc")
    ("turitseg" "Turitsyna" "E.")))

(defvar +INDUSTRY+
 '("Kakkavas.Kostas@lannet.gr"
 "limit@eurodyn.com"
 "yu.zhou@bt.com"
 "olivier.dubuisson@francetelecom.com"
 "Olivier.Philippe@imaginebroadband.com"
 "Andrew.Lord@bt.com"
 "Andy.catchpole@bt.com"
 "Anthony.P.Walsh@marconi.com"
 "Dan.Beaman@asn.alcatel.co.uk"
 "Dave.Harrold@marconicomms.com"
 "Derek.Nesset@marconi.com"
 "Fernando.Rodriguez@asn.alcatel.co.uk"
 "Graham.Baxter@orange.co.uk"
 "I.Wright@fujitsu.co.uk"
 "Ian.Beeby@wfinet.com"
 "Ian.Simpson@enst-bretagne.fr"
 "James.Conroy@orange.co.uk"
 "Joerg.Schwartz@asn.alcatel.co.uk"
   "xuanye.gu@bt.com"
 "K.P.Jones@ftel.co.uk"
 "Neil.Chapman@orange.co.uk"
 "Nick.Goodall@marconi.com"
 "Nicole.Thompson@marconicomms.com"
 "Paul.W.Reece@bt.com"
 "Peter.Brand@bt.com"
 "Philip.Biles@marconicomms.com"
 "PoustieAJ@corning.com"
 "Richard.Oberland@asn.alcatel.co.uk"
 "Stephen.Ferguson@marconicomms.com"
 "Stephen.Hope@orange.co.uk"
 "Steve.Betteley@marconicomms.com"
 "Wladek.Forysiak@marconi.com"
 "andy.catchpole@bt.com"
 "andy.sutton@orange.co.uk"
 "archepee"
 "armfinet@dircon.co.uk"
"steve.d.bryant@orange.co.uk"
 "attul@m-venue.com"
 "carol.satchwell@orange.co.uk"
 "clive.twist@orange.co.uk"
 "cris@smdata.gr"
 "david.bozward@imcoco.com"
 "dwaters@btinternet.com"
 "fevrier@nortelnetworks.com"
 "industry_guest"
 "jane.tateson@bt.com"
 "jared.price@orange.co.uk"
 "jonathan.a.clark@bt.com"
 "kourtis@iit.demokritos.gr"
 "mak@nortelnetworks.com"
 "martin.d.cookson@bt.com"
 "martin.lomas@marconi.com"
 "mbk@nortelnetworks.com"
"michael.sharratt@marconi.com"
 "mike.foxton@bt.com"
 "mike.johnson@orange.co.uk"
 "nick.doran@marconi.com"
 "ntan@intracom.gr"
 "paul.chambers@wfinet.com"
 "peter.chaloner@bt.com"
 "roust@nortelnetworks.com"
 "russell.davey@bt.com"
 "steve.culverhouse@bt.com"
 "tambouris@archetypon.gr"
 "tariq.mahmood@orange.co.uk"
 "webbdj@aston.ac.uk"
 "cefn.hoile@bt.com"
 "tambouris@archetypon.com"
 "Stephen.Ferguson@marconi.com"
 "david@ardenphotonics.com"
 "k.sugden@indigo-photonics.com"
 "pete.brand@bt.com"
 "ian.lynch2@ntlworld.com"
 "abampakos@nethabilis.gr"
 "keith@daconi.co.uk"
 "Derek.Wetter@ofcom.org.uk"
 "domenico.giannone@multitel.be"
   "Jim.Everett@marconi.com"
   ))

(defvar +EE3DTR-2007+
  '(( "boltha" "Bolt" "Han")
    ( "jonesal" "Jones" "Alex"   )
    ( "manimanb" "Manimani" "Baderha"  )
    ( "nazranv" "Nazran" "Vijay"   )
    ( "saimbij" "Saimbi" "Jasjit")
    ( "senkoroa" "Senkoro" "Adam"   )
    ( "sharafs1" "Sharafbainy" "Shadman" )
    ( "stonecm" "Stone" "Christopher" )))

(defvar +AUTHORS+
 '(("bennioni" "Bennion" "Ian")
   ("blowkj" "Keith J")
   ("harperp" "Harper" "Paul")
   ("turitssk" "Turitsyn" "Sergei K")
   ("webbdj" "Webb" "David J")
   ("willijar" "Williams" "John A R")
   ("eberhama" "Eberhard" "Mark A")
   ("khrushci" "Khrushchev" "Igor")
   ("mezentsv" "Mezentsev" "Vladimir")
   ("zhangl" "Zhang" "Lin")
   ("allsotdp" "Allsop" "Tom D P")
   ("aniacajd" "Ania-Castañón" "J D")
   ("birkidjl" "Birkin" "J L")
   ("boscolsa" "Boscolo" "S")
   ("derevyas" "Derevyanko" "S")
   ("dubovm" "Dubov" "M")
   ("huangz" "Huang" "Z")
   ("ibbotsonr" "Ibbotson" "R")
   ("laiy" "Lai" "Yicheng")
   ("luh" "Lu" "H")
   ("turitsyeg" "Turitsyna" "Elena G")
   ("wongwm" "Wong" "W M")
   ("zhaod" "Zhao" "D")
   ("zhouk" "Zhou" "K")
   ;; students
   ("bhambers"  "Bhamber" "R")
   ("braimioc"  "Braimiotis" "C")
   ("cowiebm"  "Cowie" "B")
   ("chenx2"  "Chen" "X")
   ("dobbhl1"  "Dobb" "H")
   ("ellintj1"  "Ellingham" "T")
   ("floreanf"  "Floreani" "F")
   ("gillooam"  "Gillooly" "A")
   ("graya"  "Gray" "A")
   ("harrija2"  "Harrison" "J")
   ("huy"  "Hu" "Y")
   ;;  "Lloyd" "G"
   ("mainas1"  "Main" "A")
   ("martinea"  "Martinez" "A")
   ("muhyalds"  "Muhyaldin" "S")
   ("nahasm"  "Nahas" "M")
   ("nasievio"  "Nasieva" "I")
   ("normand"  "Norman" "D")
   ("petrovij"  "Petrovich" "J")
   ("simpsag1"  "Simpson" "G")
   ("turitseg"  "Turitsyna" "E")
    ;;  "Wahid" "H"
   ("wonpc"  "Won" "P" "C")
   ("zakariaz"  "Zakaria" "Z"   )
   ))

;;(add-aston-users '(("kf@qred.co.uk" "Mr Khalil Faouaz")) :roles '(:industry))

(defun add-aston-users(userspecs &key (roles '(:ug)) (goals))
  "adds/updates user roals and goals for users according to user specs
A user spec is a username or a list of username, and optionally lastname,
firstname"
  (dolist (userspec userspecs)
    (multiple-value-bind (username lastname firstname)
        (if (stringp userspec) userspec (values-list userspec))
      (let* ((user (or (get-dictionary username *user-source*)
                       (make-instance 'clews:user  :username username))))
        (setf (slot-value user 'inet.acl::roles)
              (union roles (slot-value user 'inet.acl::roles)))
	(format t "~S~:[(no pwd)~;~]~%" user
		(get-dictionary username *pwd-source*) )
	(when goals
	  (setf (property (property-subset user :adaptive-tutor) :goals)
		(union goals
		       (property (property-subset user :adaptive-tutor) :goals)
		       :test #'equal)))
        (when lastname (setf (property user :lastname) lastname))
        (when firstname (setf (property user :firstname) firstname))
        (setf (get-dictionary  username *user-source*) user)
        (aston::reset-password username :if-set :ignore)))))

(defun group-sizes(target-group-size total-number &key (larger nil))
  (let*((no (/ total-number target-group-size)))
    (multiple-value-bind(no-groups remainder)
        (if larger (floor no) (ceiling no))
      (let* ((left-over (* remainder target-group-size))
	    (groups (loop :for a :from 1 :to no-groups
		       :collect target-group-size))
	    (step (if (< left-over 0) -1 +1)))
	(loop
	   (when (= left-over 0) (return))
	   (loop
	      :for a :on groups
	      :do (progn (incf (car a) step)
			 (decf left-over step))
	      :until (= left-over 0)))
	groups))))

(defun organise-groups(group-size items &key (larger nil))
  (let((group-sizes (group-sizes group-size (length items) :larger larger))
       (items (copy-list items)))
    (loop
       :for size :in group-sizes
       :collect
       (loop :for x :from 1 :to size
             :collect (let((item (elt items (random (length items)))))
                        (setf items (delete item items))
                        item)))))


(in-package :clews.grades)

(defun get-input(message &optional default type)
  (format *terminal-io* "~A ~@[[~A]~]: " message default)
  (finish-output *terminal-io*)
  (let ((line
         (string-trim '(#\space #\newline #\return) (read-line *terminal-io*))))
    (if (= 0 (length line)) default (jarw.parse:parse-input type line))))

(defun studentid-p(id)
  (and (= 9 (length id)) (every #'digit-char-p id)))

(defmethod jarw.parse:parse-input((type (eql 'studentid)) value
                                  &key &allow-other-keys)
  (unless (studentid-p value)
    (jarw.parse::invalid-input value "STUDENTID must consist of 9 digits"))
  value)

(defun check-student(id &optional (year 2008) (if-set :ignore))
  (let ((student
         (first (student-records
                 *db* 'year year
                 (if (studentid-p id) 'studentid 'username) id))))
   r (unless student (error "No student ~S found." id))
    (format *terminal-io* "~%Student ~A~%" student)
    (flet ((get-field(slot message &optional type)
             (let ((default (when (slot-boundp student slot)
                              (slot-value student slot))))
               (setf (slot-value student slot)
                     (get-input message default type)))))
      (get-field 'username "Username")
      (get-field 'studentid "SUN" 'studentid)
      (get-field 'firstname "Firstname")
      (get-field 'initials "Initials")
      (get-field 'lastname "Lastname")
      (get-field 'nationality "nationality")
      (get-field 'programmeid "Programme (IT(D), TT(D), DN(D), PNS)")
      (get-field 'year "Year" '(integer :min 2005 :max 2015))
      (setf (slot-value student 'active) t)
      (update-records-from-instance student)
      (let ((username (username student))
            (programmeid (programmeid student)))
        (aston::reset-password username :if-set if-set)
        (let ((user (or (get-dictionary username aston::*user-source*)
                        (make-instance 'clews:user  :username username))))
          (setf (slot-value user 'inet.acl::roles)
                (list
                 :student
                 (intern (format nil "~A~A" programmeid year) :keyword)))
          (setf (get-dictionary username aston::*user-source*) user)))
      student)))


#|

(dolist(student
         (mapcan #'(lambda(student)
                     (let ((username (clews.grades::username student))
                           (programmeid (clews.grades::programmeid student)))
                     (when username
                       (list (list username programmeid)))))
                 (clews.grades::student-records clews.grades::*db* 'clews.grades::year 2007)))
  (let ((username (first student))
        (programmeid (second student)))
    (let ((user (or (get-dictionary username *user-source*)
                    (make-instance 'clews:user  :username username))))
      (setf (slot-value user 'inet.acl::roles)
            (list
             :student
             (intern (concatenate 'string programmeid "2007") :keyword)))
      (setf (get-dictionary username *user-source*) user))))

(dolist(username +IT2004+)
  (let ((user (get-dictionary (first username) *user-source*)))
    (setf (slot-value user 'inet.acl::roles) '(:student :it2004))
    (setf (get-dictionary (first username) *user-source*) user)))
(add-aston-users  '(("barkerl" "Barker" "Lee"))
      :roles '(:TT2005 :student))

(add-aston-users +TT2006+ :roles '(:TT2006 :student))
(add-aston-users +IT2006+ :roles '(:IT2006 :student))
(add-aston-users +PN2006+ :roles '(:PN2006 :student))

		 :goals '("lab-digital-transmission-assessments-2004" "EE401B-assessment"))
(add-aston-users +PN2004+ :roles '(:PN2004 :student)
		 :goals '("lab-digital-transmission-assessments-2004" "EE401B-assessment"))
(add-aston-users +IT2004+ :roles '(:IT2004 :student)
		 :goals '("internet-application-principles"))
(add-aston-users +EE3DTR-2005+ :roles '(:EE3DTR-2005 :student))
		 :goals '("lab-digital-transmission-assessments-2004" "EE3DTR-assessment"))

(add-aston-users +EE3DTR-2007+ :roles '(:EE3DTR-2007 :student))

(add-aston-users +STAFF+
		 :roles '(:staff :author))
(add-aston-users +INDUSTRY+ :roles '(:industry))
(add-aston-users +AUTHORS+  :roles '(:author))

(add-aston-users '(("groberts@rim.com" "Roberts" "Gideon")) :roles '(:industry))

|#

