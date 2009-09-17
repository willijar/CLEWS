;;;; Aston Web Applications
;;;; Copyright (C) 2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; $Id: publications.lisp,v 1.1 2005/03/10 19:54:25 willijar Exp $

(in-package :aston)

(defvar *publications*
  (make-instance
   'clews.publications:publications-manager
   :user-dictionary *user-source*
   :pwd-source *pwd-source*
   :external-format :latin1
   :acl '((:view . (:all))
          (:author . (:author))
          (:admin . ("willijar" "bennioni")))
   :directory (translate-logical-pathname #p"clews:publications;")))

(defun apply-new-aston-style(markup)
  (multiple-value-bind (tag attrs content) (split-markup markup)
    (join-markup
     tag attrs
     (list
      (multiple-value-bind (tag attrs content) (split-markup (first content))
        (unless (eq tag 'head)
          (warn "Applying Aston head style to a ~A tag" tag))
        (join-markup
         tag attrs
         (cons '((link :rel "stylesheet" :href "http://www.ee.aston.ac.uk/share/ee.css" :type "text/css")) content)))
      (multiple-value-bind (tag attrs content) (split-markup (second content))
        (declare (ignore attrs))
        (unless (eq tag 'body)
	  (warn "Applying Aston body style to a ~A tag" tag))
        (let ((nav
               (multiple-value-bind(tag attr menus) (split-markup (first content))
                 (declare (ignore attr))
                 (when (eql tag 'markup:navbar)
                   (setf content (rest content))
                   (apply #'append (car menus))))))
          (join-markup
           tag
           '(:bgcolor "white" :text "black" :link "#0000FF" :vlink "#840084" :alink "#0000FF")
           `(((div :id "page")
             ((div :id "skip-links" :escape nil)
"Skip to <a href=\"#content\">content</a> | <a href=\"#global-navigation\">main University navigation</a> | <a href=\"#local-navigation\">local section navigation</a> | <a href=\"#search\">search</a>")
             ((div :id "header" :escape nil)
"<a href=\"http://www.seas.aston.ac.uk\">
<img id=\"logo\" src=\"http://www.aston.ac.uk/images/logo-small.gif\" width=\"118\" height=\"57\" alt=\"Aston University, in the heart of Birmingham\"></a>
<form id=\"search\" action=\"/cgi-bin/htsearch\" method=\"post\">
<input value=\"all\" name=\"method\" type=\"hidden\"><input value=\"builtin-long\" name=\"format\" type=\"hidden\"><input value=\"score\" name=\"sort\" type=\"hidden\"><input value=\"htdig\" name=\"config\" type=\"hidden\"><input name=\"restrict\" type=\"hidden\"><input name=\"exclude\" type=\"hidden\"><input id=\"words\" name=\"words\" type=\"text\" title=\"Type some words to search for then click Go\" value=\"Search this site\" onfocus=\"if(this.value=='Search this site')this.value='';\" onblur=\"if(this.value=='')this.value='Search this site';\"><input id=\"search-button\" type=\"submit\" value=\"Go\">
</form>
<div id=\"banner\"><p><img src=\"http://www.ee.aston.ac.uk/images/banners/rotate.php\" width=\"570\" height=\"116\" alt=""></p></div>")
	   ((div :id "content") ,@content)
     ((div :id "local-navigation" :escape nil)
      "<p class=\"screen-reader-only\">Electronic Engineering navigation links</p>"
"<p class=\"down\"><a href=\"http://www.ee.aston.ac.uk/\">Electronic Engineering</a></p>
<p class=\"horiz1\"><a href=\"http://www.ee.aston.ac.uk/courses/ug.html\">Undergraduate Programmes</a></p>
<p class=\"horiz1\"><a href=\"http://www.ee.aston.ac.uk/courses/pg.html\">Postgraduate Programmes</a></p>
<p class=\"horiz1\"><a href=\"http://www.ee.aston.ac.uk/modules/index.html\">Taught Modules</a></p>
<p class=\"horiz1\"><a href=\"http://www.ee.aston.ac.uk/research/index.html\">Research</a></p>
<p class=\"horiz2\"><a href=\"http://www.ee.aston.ac.uk/research/prg/\">Photonics Research Group</a></p>
<p class=\"horiz3\"><a href=\"http://www.ee.aston.ac.uk/research/prg/prg-programme.html\">Research Programme</a></p>
<p class=\"horiz3\"><a href=\"http://www.ee.aston.ac.uk/research/prg/prg-facilities.html\">Facilities and Equipment</a></p>
<p class=\"horiz3\"><a href=\"http://www.ee.aston.ac.uk/research/prg/prg-members.html\">Members</a></p>
<p class=\"horiz3\"><a href=\"http://www.ee.aston.ac.uk/research/prg/prg-grants.html\">Research Grants and Contracts</a></p>
<p class=\"horiz3\"><a href=\"http://www.ee.aston.ac.uk/research/prg/prg-student-projects.html\">Student Research Projects</a></p>
<p class=\"horiz3\"><a href=\"http://www.ee.aston.ac.uk/research/prg/prg-projects.html\">Other Current Research Projects</a></p>
<p class=\"horiz3\"><a href=\"htt(defun complete-record(rec)
  (append rec
	  (let ((journal (journal (fieldvalue :journalid rec))))
	    (when journal
	      `((:journal . ,(fieldvalue :title journal))
2		(:journal-abbrev . ,(fieldvalue :abbreviation journal))
		(:issn . ,(fieldvalue :issn journal)))))
	  (let ((xref (fieldvalue :crossref rec)))
	    (when xref (complete-record (publication xref))))))p://www.ee.aston.ac.uk/research/prg/prg-thesis.html\">Doctoral Thesis</a></p>
<p class=\"downselected3\"><a href=\"/publications/\">Publications</a></p>"
	     ,@(mapcar #'(lambda(a)
			   (format nil "<p class=\"horiz4\"><a href=\"~A\">~A</a></p>"
				   (first a) (second a)))
                 nav)
      "<p class=\"horiz3\"><a href=\"http://www.ee.aston.ac.uk/research/prg/prg-studentships.html\">Research (PhD) Studentships</a></p>
<p class=\"horiz3\"><a href=\"http://www.ee.aston.ac.uk/research/prg/prg-employment.html\">Employment</a></p>
<p class=\"horiz2\"><a href=\"http://www.ee.aston.ac.uk/research/ssrg/index.html\">Surface Science Research Group</a></p>
<p class=\"horiz2\"><a href=\"http://www.ee.aston.ac.uk/research/acrg/index.html\">Adaptive Communications Networks Research Group</a></p>
<p class=\"horiz2\"><a href=\"http://www.ee.aston.ac.uk/research/research-study.html\">Research Study</a></p>
<p class=\"horiz1\"><a href=\"http://www.ee.aston.ac.uk/contacts/index.html\">Contact Information</a></p>
<p class=\"horiz1\"><a href=\"http://www.ee.aston.ac.uk/appendices/pr06.html\">Appendices</a></p>")
     ((div :id "global-navigation"  :escape nil)
"<p class=\"screen-reader-only\">Main University navigation links</p>
<ul>
<li><a href=\"http://www.aston.ac.uk//prospective-students/index.jsp\"><img src=\"http://www.seas.aston.ac.uk/images/prospects-nav.gif\" alt=\"Prospective students\" height=\"35\" width=\"162\"></a></li>
<li><a href=\"http://www.aston.ac.uk//research\"><img src=\"http://www.seas.aston.ac.uk/images/research-nav.gif\" alt=\"Research\" height=\"26\" width=\"69\"></a></li>
<li><a href=\"http://www.aston.ac.uk//from-business\"><img src=\"http://www.seas.aston.ac.uk/images/services-nav.gif\" alt=\"Services\" height=\"26\" width=\"62\"></a></li>
<li><a href=\"http://www.aston.ac.uk//about\"><img src=\"http://www.seas.aston.ac.uk/images/about-nav.gif\" alt=\"About Aston\" height=\"27\" width=\"98\"></a></li>
</ul>")
      ((div :id "tools" :escape nil)
       "<a href=\"http://www.aston.ac.uk//index.html\">Home</a><span class=\"hidden-separator\">|</span><a href=\"http://www.aston.ac.uk//atoz.jsp\">Site A-Z</a><span class=\"hidden-separator\">|</span><a href=\"http://www.aston.ac.uk//departments.jsp\">Schools</a><span class=\"hidden-separator\">|</span><a href=\"http://www.aston.ac.uk//about/contact/index.jsp\">Contact Us</a><span class=\"hidden-separator\">|</span><a href=\"http://www.aston.ac.uk//about/directions/index.jsp\">Directions</a>")
      ((div :id  "footer" :style "border-top:1px solid #ddd;padding-top:5px;"
            :escape nil)
"<div id=\"schools\">
<div class=\"column-1\"><ul class=\"link-list\">
<li id=\"abs\"><a href=\"http://www.abs.aston.ac.uk\">Aston Business School</a></li>

<li id=\"combhons\"><a href=\"http://www.aston.ac.uk/combhons\">Combined Honours</a></li>
<li id=\"eas\"><a href=\"http://www.seas.aston.ac.uk\" id=\"current\">Engineering &amp; Applied Science</a></li>
</ul></div>
<div class=\"column-2\"><ul class=\"link-list\">
<li id=\"lss\"><a href=\"http://www.aston.ac.uk/lss\">Languages &amp; Social Sciences</a></li>
<li id=\"lhs\"><a href=\"http://www.aston.ac.uk/lhs\">Life &amp; Health Sciences</a></li>
<li id=\"gradschool\"><a href=\"http://www.aston.ac.uk/graduateschool\">Graduate School</a></li>

</ul></div>
</div>
<div id=\"misc-nav\">
<div class=\"column-1\"><ul class=\"link-list\">
<li><a href=\"http://www.aston.ac.uk/current-students\">Current students</a></li>
<li><a href=\"http://www.aston.ac.uk/alumni\">Alumni</a></li>
<li><a href=\"http://www.aston.ac.uk/lis/index.jsp\">Library</a></li>
</ul></div>
<div class=\"column-2\"><ul class=\"link-list\">
<li><a href=\"http://www.aston.ac.uk/staff\">Staff intranet</a></li>
<li><a href=\"http://www.aston.ac.uk/jobs\">Jobs</a></li>
</ul></div>")
      ((div :id  "crest" :escape nil)
"<img src=\"http://www.seas.aston.ac.uk/images/crest.gif\" height=\"60\" width=\"42\" alt=\"Aston University Crest\">")
      ((address :escape nil)
"<p id=\"postal\">(c) 2006 Aston University, Aston Triangle, Birmingham, B4 7ET.</p>
<p id=\"contacts\">+44 (0)121 204 3400 <span class=\"hidden-separator\">|</span><a href=\"http://www.aston.ac.uk/about/contact/index.jsp\">More contacts</a></p>
<p id=\"policies\"><a href=\"http://www.aston.ac.uk/disclaimer.jsp\">Disclaimer</a><span class=\"hidden-separator\">|</span><a href=\"http://www.aston.ac.uk/survey/feedback/website.htm\">Feedback</a></p>")

      ((div :escape nil)
       "</div></div><script type=\"text/javascript\">var address=location.href;address=replaceCharacters(address,location.host,'eas');if(address.lastIndexOf('.')==-1){var counterName=replaceCharacters(address.substring(7,address.length),'/','.')+\"index\";}else{var counterName=replaceCharacters(address.substring(7,address.lastIndexOf('.')),'/','.');}
function sitestat(ns_l){ns_l+='&ns__t='+(new Date()).getTime();ns_pixelUrl=ns_l;ns_0=document.referrer;ns_0=(ns_0.lastIndexOf('/')==ns_0.length-1)?ns_0.substring(ns_0.lastIndexOf('/'),0):ns_0;if(ns_0.length>0)ns_l+='&ns_referrer='+escape(ns_0);if(document.images){ns_1=new Image();ns_1.src=ns_l;}else
document.write('<img src=\"'+ns_l+'\" width=\"1\" height=\"1\" alt=\"\">');}
sitestat(\"http://uk.sitestat.com/aston/aston/s?\"+counterName);function replaceCharacters(conversionString,inChar,outChar){var convertedString=conversionString.split(inChar);convertedString=convertedString.join(outChar);return convertedString;}
</script>"))))))))))

(defmethod clews::render-page ((app (eql *publications*)) stream markup)
  (call-next-method app stream (apply-new-aston-style markup)))

(declaim (inline fieldvalue))
(defun fieldvalue(key rec) (cdr (assoc key rec)))

(defun fieldtags(tablename database)
  (mapcar #'(lambda(rec) (intern (string-upcase (first rec)) :keyword))
	  (clsql:query  (format nil "SELECT a.attname
FROM pg_class c, pg_attribute a
WHERE c.relname = '~A'
  AND a.attnum > 0 AND a.attrelid = c.oid
ORDER BY a.attnum"  tablename) :database database)))

(defun sql-alist(table database &optional condition &key (filter #'(lambda(a) (declare (ignore a)) t)))
  (let ((tags (fieldtags table database))
	(recs (clsql:query (format nil "select * from ~A ~@[where ~A~]"
				   table condition)
			   :database database
			   :result-types :auto)))
    (mapcan #'(lambda(entry) (when (funcall filter entry) (list entry)))
	    (mapcar #'(lambda(rec)
			(mapcan #'(lambda(a b) (when b (list (cons a b))))
				tags rec))
		    recs))))

(defvar *p* (clews.publications::publications *publications*))
(defvar *j* (clews.publications::journals *p*))


#|(import '(httpd::response-headers httpd::response-body httpd::response-code
	  httpd::response-header))

;;;; Interave code to update school database from ours

;;; database primitives

(defvar *jo* (make-hash-table :test #'equal))
(dolist(j (sql-alist "journals" (db-connect "photonics")))
    (setf (gethash (cdr (assoc :journalid j)) *jo*)
	  (cdr (assoc :abbreviation j)))))

(let ((count 0))
(with-open-file(os "/home/willijar/tmp/publications"
		   :direction :output :if-exists :supersede)
  (dolist(p (sql-alist "publications" (db-connect "photonics")))
    (setf p (jarw.lib:nalist->plist p))
    (setf (getf p :id) (getf p :publicationid))
    (remf p :publicationid)
    (setf (getf p :citetype)
	  (intern (string-upcase (getf p :citetype)) :keyword))
    (when (getf p :year)
      (setf (getf p :year)
	    (parse-integer (getf p :year))))
    (when (getf p :published)
      (setf (getf p :status)
	    (if (and (getf p :year)
		     (< (getf p :year) 2004))
		:public
		:published))
      (remf p :published))
    (when (and (getf p :author)
	       (> (length (getf p :author)) 0))
      (setf (getf p :author)
	    (clews.publications::input-validation
	     'clews.publications::author
	     (getf p :author))))
    (when (and (getf p :chapter)
	       (= (getf p :chapter) 0))
      (remf p :chapter))
    (when (getf p :journalid)
      (unless (gethash (getf p :journalid) *jo*)
	(warn "Missing journal ~S in ~S"
		    (getf p :journalid)
		    (getf p :id)))
      (setf (getf p :journal) (gethash (getf p :journalid) *jo*))
      (remf p :journalid))
    (when (getf p :month)
      (setf (getf p :month)
	    (1+ (position
		 (string-downcase (getf p :month))
		 '("jan" "feb" "mar" "apr" "may" "jun" "jul" "aug" "sep"
		   "oct" "nov" "dec")
		 :test #'equal))))
    (print (list (getf p :id) p) os)
    (incf count)))
count)

(map-dictionary
 #'(lambda(k p)
     (when (member (getf p :citetype) '(:proceedings))
       (unless (or (getf p :booktitle) (getf p :title) (getf p :year))
	 (rem-dictionary k *p*))))
 *p*)


(defvar *publications* (sql-alist "publications" *photonics-db*))
(defvar *journals* (sql-alist "journals" *photonics-db*))

(with-open-file(os "/home/willijar/tmp/journals"
		   :direction :output :if-exists :supersede)
  (print *journals* os))
(with-open-file(os "/home/willijar/tmp/publications"
		   :direction :output :if-exists :supersede)
  (print *publications* os))

(defun publications(condition)
  (mapcan #'(lambda(rec) (when (funcall condition rec)
			   (list (complete-record rec))))
          *publications*))

(defun publication(id)
  (find id *publications* :test #'equal
	:key #'(lambda(rec) (cdr (assoc :publicationid rec)))))

(defun journal(id)
  (find id *journals* :test #'equal
		   :key #'(lambda(rec) (cdr (assoc :journalid rec)))))

(defun complete-record(rec)
  (append rec
	  (let ((journal (journal (fieldvalue :journalid rec))))
	    (when journal
	      `((:journal . ,(fieldvalue :title journal))
		(:journal-abbrev . ,(fieldvalue :abbreviation journal))
		(:issn . ,(fieldvalue :issn journal)))))
	  (let ((xref (fieldvalue :crossref rec)))
	    (when xref (complete-record (publication xref))))))

(defun publication-set(author years)
  (mapcar
   #'complete-record
   (publications #'(lambda(rec)
                     (and (fieldvalue :published rec)
                          (member (fieldvalue :year rec) years
                                  :test #'string=)
                          (search author (fieldvalue :author rec)))))))

(defun pub-no-pages(rec)
  (let* ((pages (httpd::split-string (fieldvalue :pages rec) 3 '(#\-)) ))
    (handler-case
	(- (parse-integer (first (last pages)))
	   (parse-integer (first pages)))
      (error () 1))))

(defun citetype(rec)
  (intern (string-upcase (fieldvalue :citetype rec))
          :keyword))

(defun pub-date(rec)
  (format nil "~A ~A" (fieldvalue :month rec) (fieldvalue :year rec)))


#|
(fieldtags "publications" *db*)
(:PUBLICATIONID :CITETYPE :ADDRESS :AUTHOR :BOOKTITLE :CHAPTER :CROSSREF
 :EDITOR :JOURNALID :CITEKEY :MONTH :NOTE :NUMBER :ORGANISATION :PAGES
 :PUBLISHER :SERIES :TITLE :VOLUME :YEAR :TYPE :EDITION :HOWPUBLISHED :SCHOOL
 :ABSTRACT :PUBLISHED)
(sql-alist "publications" (db-connect "photonics") "author like '%Williams%'")
(with-db-fields ((first *t*)) (print publicationid))
|#


(defvar *authentication* nil)

(defvar *username* nil)

(defvar *default-url* "http://www.cs.aston.ac.uk/~everitca/research/research.php")

(defvar *default-cookies* nil)


(defun make-http-request(&key (method :post) (url *default-url*)
			 headers data (authentication *authentication*)
			 (cookies *default-cookies*))
  (httpd::make-http-request
   method url
   :headers (append headers (mapcar #'(lambda(cookie) (cons :cookie cookie))
				    cookies))
   :data data
   :authentication authentication))


(defun http-login
    (username passwd
     &key (url "http://www.cs.aston.ac.uk/~everitca/research/index.php"))
  (setq *authentication* (cons username passwd))
  (let ((r (make-http-request
	    :method :get :url url :cookies nil )))
    (ecase (response-code r)
      (200 #+nil(setq *default-cookies*
		 (mapcar #'(lambda(set-cookie)
			     (first (httpd::split-string set-cookie 2 '(#\;))))
			 (response-header r :set-cookie))) t))))



(defvar *field-mappings*
    `(((0 :article)  ("year" :year)
       ("data[1]" :author) ("data[2]" :title)  ("data[3]" :journal)
       ("data[5]" :volume "(" :number ")") ("data[6]" :pages)
       ("data[7]" :issn)  ("year" :year))
      ((1 :book)
       ("data[8]" :author) ( "data[9]"  :booktitle)
       ( "data[10)" :publisher) ( "data[12]" . ,#'pub-no-pages)
       ("data[13]" :isbn)  ("year" :year))
      ((2 :inbook)
       ("data[14]" :author) ("data[16]" :booktitle) ("data[15]" :title)
       ("data[17]" :editor) ("data[18]" :publisher) ("data[20]" :isbn)
       ("data[46]" :pages)  ("year" :year))
      ((3 :inproceedings)
       ("data[21]" :author) ("data[22]" :title) ("data[23]" :booktitle)
       ("data[24]" :month " " :year) ("data[25]" :address) ("year" :year)
       ("data[26]" :organisation) ("data[47]" :pages)
       ("data[48]" :publisher))))


(defun make-form-data(rec)
  (let* ((citetype (intern (string-upcase (fieldvalue :citetype rec))
			   :keyword))
	 (fieldmapping  (assoc citetype *field-mappings* :key #'cadr)))
    `(("plans" . "false")
      ("cat" . ,(caar fieldmapping))
      ("action" . "submit")
      ("button" . "Add New >")
      ("id" . 0)
      ("username" . ,(or *username* (car *authentication*)))
      ,@(mapcar
	 #'(lambda(mapping)
	     (let ((dest (first mapping))
		   (src (rest mapping)))
	       (if (functionp src) (cons dest (apply src rec))
		   (cons dest
			 (apply #'concatenate
				(cons 'string
				      (mapcar
				       #'(lambda(a)
					   (if (stringp a) a (fieldvalue a rec)))
				       src)))))))
	 (rest fieldmapping)))))

(defun send-data(data)
  (let ((response (make-http-request :data data :url "http://www.cs.aston.ac.uk/~everitca/research/research.php")))
    (= 200 (response-code response))))

(defun send-records(login password author years &optional (username login))
  (unless (listp years) (setq years (list years)))
  (setq *username* username)
  (http-login login password)
  (dolist(rec (publication-set author years))
    (format t "~A: ~:[Unsuccessful~;Successful~]~%"
	    (fieldvalue :publicationid rec)
	    (let ((data (make-form-data rec)))
	      (and data (send-data data))))))

(defun send-record(login password author record &optional (username login))
  (setq *username* username)
  (http-login login password)
  (let ((rec (complete-record (publication record))))
    (format t "~A: ~:[Unsuccessful~;Successful~]~%"
	    (fieldvalue :publicationid rec)
	    (let ((data (make-form-data rec)))
	      (and data (send-data data))))))

#|
(setq *r* (make-http-request :get "http://www.cs.aston.ac.uk/~everitca/research/index.php" :authentication '("username" . "*******") ))
|#

|#