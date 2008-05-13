;; $Id: specifications.lisp,v 1.4 2007/03/28 08:44:44 willijar Exp willijar $
;; Module Specification handling
;; Copyright (C) 2006 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of CLEWS Grades

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;;

;;; Code:

(in-package :clews.grades)

(clsql:locally-enable-sql-reader-syntax)

(defmacro select*(db table &rest criteria)
  `(clsql:select
    ,table
    :database ,db
    :flatp t
    :caching *caching*
    :refresh *refresh*
   ,@(when criteria
            `(:where
              (clsql:sql-and
                ,@(loop for a on criteria by #'cddr
                      collect
                      `[= [slot-value ,table ,(car a)] ,(cadr a)]))))))

(def-view-class teaching-method()
  ((type :type (string 4) :db-kind :key :db-constraints :primary-key
         :reader teaching-type)
   (description :type string :reader description))
  (:base-table "teaching_methods"))

(def-view-class assessment-method(teaching-method)
  ()
  (:base-table "assessment_methods"))

(defmethod print-object((m teaching-method) stream)
  (print-unreadable-object (m stream :type t :identity t)
    (format stream "~4A ~S"
            (teaching-type m)
            (description m))))

(def-view-class curriculum-item()
  ((curriculum-item-id :accessor curriculum-item-id :type integer :initarg
                       :curriculum-item-id :db-kind :key
                       :db-constraints :primary-key
                       :column "curriculum_item_id")
   (type :accessor curriculum-item-type :type string :initarg :type
         :initform "knowledge"
         :db-constraints :not-null)
   (description :accessor description :type string :db-constraints :not-null))
  (:base-table "curriculum_items"))

(defmethod print-object((item curriculum-item) stream)
  (print-unreadable-object (item stream :type t :identity t)
    (format stream "~4D ~A" (curriculum-item-id item)
            (curriculum-item-type item))))

(def-view-class curriculum-map-item()
  ((curriculum-item-id :type integer :initarg
                       :curriculum-item-id
                       :db-kind :key :db-constraints :not-null)))

(def-view-class teaching-map(curriculum-map-item)
  ((teachingid :type integer :db-kind :key :initarg :teachingid))
  (:base-table "teaching_map"))

(defmethod print-object((item teaching-map) stream)
  (print-unreadable-object (item stream :type t :identity t)
    (format stream "~D -> ~D" (slot-value item 'curriculum-item-id)
            (slot-value item 'teachingid))))

(def-view-class assessment-map(curriculum-map-item)
  ((assessmentid :type integer :db-kind :key :initarg :assessmentid))
  (:base-table "assessment_map"))

(defmethod print-object((item assessment-map) stream)
  (print-unreadable-object (item stream :type t :identity t)
    (format stream "~D -> ~D" (slot-value item 'curriculum-item-id)
            (slot-value item 'assessmentid))))

(defgeneric teaching(item)
  (:documentation "return the teaching record for item")
  (:method(item)
    (select* (clsql-sys::view-database item)
             'teaching
             'teachingid (slot-value item 'teachingid)))
  (:method((module module))
    (select* (clsql-sys::view-database module)
             'teaching
             'year (slot-value module 'year)
             'moduleid (slot-value module 'moduleid))))

(defgeneric curriculum-item(item)
  (:documentation "Return the curriculum item associated with a map item")
  (:method((item curriculum-map-item))
    (first (select* (clsql-sys::view-database item)
             'curriculum-item
             'curriculum-item-id (slot-value item 'curriculum-item-id)))))

(defgeneric assessment(item)
  (:documentation "Return the curriculum item associated with item")
  (:method(item)
    (first (select* (clsql-sys::view-database item)
             'assessment
             'assessmentid (slot-value item 'assessmentid)))))

(def-view-class teaching()
  ((teachingid :type integer :db-kind :key :reader teachingid :initarg :id)
   (moduleid :type (string 6) :reader moduleid :initarg :moduleid)
   (year :type integer :reader year :initarg :year :initform 2007)
   (type :type (string 4) :accessor teaching-type :initarg :type)
   (description :type string :accessor description :initarg :description)
   (contact-time :column "contact_time"
                 :type integer :accessor contact-time)))

(defmethod print-object((teaching teaching) stream)
  (print-unreadable-object (teaching stream :type t :identity t)
    (format stream "~4D ~6A ~4A (~D)"
            (year teaching)
            (moduleid teaching)
            (teaching-type teaching)
            (teachingid teaching))))

(defgeneric teaching-method(teaching)
  (:documentation "Return the teaching method(s) for an entity")
  (:method((teaching teaching))
    (first (select* (clsql-sys::view-database teaching)
             'teaching-method
             'type (slot-value teaching 'type)))))

(defgeneric assessment-method(assessment)
  (:documentation "Return the teaching method for an entity")
  (:method((assessment assessment))
    (first (select* (clsql-sys::view-database assessment)
             'assessment-method
             'type (slot-value assessment 'type)))))

(defgeneric curriculum-items(item)
  (:documentation "Return the curriculum items associated with a map item")
  (:method((teaching teaching))
    (mapcar #'curriculum-item
            (select* (clsql-sys::view-database teaching)
             'teaching-map
             'teachingid (slot-value teaching 'teachingid))))
  (:method((assessment assessment))
    (mapcar #'curriculum-item
            (select* (clsql-sys::view-database assessment)
             'assessment-map
             'assessmentid (slot-value assessment 'assessmentid))))
  (:method((module module))
    (delete-duplicates
     (mapcan #'curriculum-items
             (append (teaching module) (assessments module)))
     :key #'curriculum-item-id))
  (:method((modules list))
    (delete-duplicates (mapcan #'curriculum-items modules))))

(defstruct (curriculum-section (:type list))
  (type "")
  (title "")
  (blurb ""))

(defun curriculum-section-label(section &optional no)
  (format nil "~C~@[~D~]" (char (curriculum-section-title section) 0) no))

(defparameter curriculum-sections
  (list
   (make-curriculum-section
       :type "knowledge"
       :title "A. Knowledge and Understanding"
       :blurb "On successful completion of their programme, students are expected to have knowledge and understanding of:")
    (make-curriculum-section
       :type "intellectual"
       :title "B. Intellectual Skills"
       :blurb "On successful completion of their programme, students are expected to be able to:")
    (make-curriculum-section
       :type  "professional"
       :title "C. Professional Skills"
       :blurb "On successful completion of their programme, students are expected to be able to:")
    (make-curriculum-section
       :type "transferable"
       :title "D. Transferable Skills"
       :blurb "On successful completion of their programme, students are expected to show:")))

(defun curriculum(modules)
  "Return an array of curriculum entries for list of entities - an array
of four buckets for each type of curriculum item, each entry is a a
list of the item label, the item, list of teaching-methods and list of
assessment methods. The section titles are turned as the second
value."
  (let ((items nil))
    (flet ((get-entry(item)
             (or (find-if #'(lambda(i) (= (curriculum-item-id item)
                                          (curriculum-item-id (car i))))
                          items)
                 (first (push (list item nil nil) items)))))
      (dolist(module modules)
        (dolist(teaching (teaching module))
          (dolist (item (curriculum-items teaching))
            (pushnew (teaching-method teaching) (second (get-entry item)))))
        (dolist(assessment (assessments module))
          (dolist (item (curriculum-items assessment))
            (pushnew (assessment-method assessment)
                     (third (get-entry item)))))))
    (let ((sections (make-list (length curriculum-sections))))
      (dolist(item items)
        (push item
              (elt sections
                   (position (curriculum-item-type (first item))
                             curriculum-sections
                             :test #'string-equal
                             :key #'curriculum-section-type))))
      sections)))

(def-view-class module-specification(module)
  ((module-type :accessor module-type :type (string 10)
                :initarg :type :initform "Taught"
                :column "module_type")
   (module-replaces :accessor module-replaces :type (string 10)
                    :initarg :replaces :initform nil
                    :column "module_replaces")
   (minimum-intake :accessor minimum-intake :type integer
                   :initarg :minimum-intake :initform nil
                   :column "minimum_intake")
   (maximum-intake :accessor maximum-intake :type integer
                   :db-type integer
                   :initarg :maximum-intake :initform nil
                   :column "maximum_intake")
   (aims :accessor aims :type list :initarg :aims)
   (summary :accessor summary :type list :initarg :summary)
   (teaching-methods-markup :type list
                     :initarg :teaching-methods-markup
                     :column "teaching_methods")
   (prepared-by :type string :reader prepared-by :initarg :prepared-by
                :initform "willijar"
                :column "prepared_by")
   (learning-resources :accessor learning-resources :type list
                       :initarg :learning-resources :initform nil
                       :column "learning_resources")
   (core-texts :accessor core-texts :type list :initform nil
               :initarg :core-texts :column "core_texts"))
  (:base-table modules
   :documentation "Extension of module with additional specification info"))

(def-view-class module-outcome()
  ((id :accessor id :type integer :db-kind :key)
   (moduleid :accessor moduleid :type (string 6) :initarg :moduleid)
   (year :accessor year :type integer :initarg :year)
   (type :accessor outcome-type :type string :initarg :type)
   (description :accessor description :type list :initarg :description)
   (teaching :accessor teaching :type list :initarg :teaching)
   (assessment :accessor assessment :type list :initarg :assessment))
  (:base-table "module_outcomes"))

(defmethod print-object((outcome module-outcome) stream)
  (print-unreadable-object (outcome stream :type t :identity t)
    (format stream "~D ~S"  (id outcome) (outcome-type outcome))))

(defgeneric teaching-methods-markup(module)
  (:method ((module module-specification))
    (let ((teaching (teaching module)))
      (unless teaching
        (return-from teaching-methods-markup
          (slot-value module 'teaching-methods-markup)))
      `((dl
         ,@(mapcan
            #'(lambda(teaching)
                `((dt ,(description (teaching-method teaching)))
                  (dd ,@(when (contact-time teaching)
                          (list
                           (format nil "~D hours. " (contact-time teaching))))
                   ,(or (description teaching) ""))))
            teaching))))))

(defun curriculum-map(modules &optional (curriculum (curriculum modules)))
  "return the curriculum map and the sectioned list of curriculum items and their labels"
  (let* ((items (mapcar #'car (reduce #'append curriculum)))
        (map (make-array (list (length modules) (length items))
                         :element-type 'bit :initial-element 0)))
    (do((x 0 (1+ x)))
       ((>= x (length items)))
      (let ((item (elt items x)))
        (do ((y 0 (1+ y)))
            ((>= y (length modules)))
          (let ((module (elt modules y)))
            (when (member (curriculum-item-id item)
                          (curriculum-items module)
                          :key #'curriculum-item-id)
              (setf (aref map y x) 1))))))
    (values map (mapcan
                 #'(lambda(section items)
                     (let ((x 0))
                     (mapcar
                      #'(lambda(item)
                          (cons item (curriculum-section-label
                                      section (incf x))))
                       (mapcar #'car items))))
                 curriculum-sections curriculum))))

(defmethod year-of-introduction((module module))
  (first (sort
          (mapcar #'year (module-records (clsql-sys::view-database module)
                                         'moduleid (moduleid module)))
          #'<)))

(defmethod programmes((module module))
  (let ((programmeids (clsql:query
                       (format nil "select programmeid from programme_modules where year=~D and moduleid='~A';" (year module) (moduleid module))
                       :flatp t
                       :database (clsql-sys::view-database module))))
    (when programmeids
      (clsql:select
       'programme
       :database (clsql-sys::view-database module)
       :caching nil
       :flatp t
       :where
       [in [slot-value 'programme 'programmeid] programmeids]))))

(defmethod related-moduleids((module module) relationship)
  (clsql:query
   (format nil "select moduleid from related_modules where year=~D and moduleid='~A' and relationship='~A';" (year module) (moduleid module) relationship)
   :flatp t
   :database (clsql-sys::view-database module)))

(defmethod module-outcomes((module module))
  (clsql:select
   'module-outcome
   :database (clsql-sys::view-database module)
   :caching nil
   :flatp t
   :where [and [= [slot-value 'module-outcome 'moduleid] (moduleid module)]
   [= [slot-value 'module-outcome 'year] (year module)]]))

(defmethod module-specifications(db &rest criteria)
  (simple-query db 'module-specification criteria))

(defgeneric write-module-specification(module format &optional stream)
  (:documentation "Write the module specification in given format to stream"))

(defmethod write-module-specification((module string) format
                                      &optional (stream *standard-output*))
  (write-module-specification
   format
   (first
    (sort
     (clews.grades::module-specifications *db* 'clews.grades::moduleid module)
     #'> :key #'year))
   stream))

(defmethod write-module-specification((module list) format
                                      &optional (stream *standard-output*))
  (write-module-specification
   format
   (first
    (clews.grades::module-specifications
     *db*
     'clews.grades::moduleid (first module)
     'clews.grades::year (second module)))
   stream))

(defmethod fullname((username string))
  (let ((student (first (student-records *db* 'username username))))
    (if student (fullname student)
        (let ((name
                (car (clsql:query
                     (format nil "select title,firstname,initials,lastname
 from staff where username='~A';" username)
                     :flatp t
                     :database *db*))))
          (if name
            (format nil "~{~:[~;~:*~A ~]~}" name)
            username)))))

(defmethod fullname-link((username string))
  (let ((student (first (student-records *db* 'username username))))
    (if student
        (fullname student)
        (format nil  " <xref linkend=~S/> ~A" username (fullname username)))))


(defmethod write-module-specification((format (eql :latex))
                                      (module module-specification)
                                      &optional (stream *standard-output*))
  (flet ((markup-elements(items)
           (with-output-to-string(os)
             (dolist(item items) (latex os item) (terpri os)))))
    (map
     'nil #'(lambda(item) (princ item stream))
     `("\\begin{module-specification}
\\moduleitem{" ,(moduleid module) "}{" ,(title module) "}
\\noindent
\\begin{longtable}{|p{\\colwidth}|p{\\colwidth}|p{\\colwidth}|p{\\colwidth}|p{\\colwidth}|}\\hline
\\entry{School and Subject Group}{School of Engineering and Applied Science,
  Electronic Engineering}
\\entry{Module Code}{" ,(moduleid module) "}
\\entry{Module Title}{" ,(title module) "}
\\entry{Module Type}{",(module-type module)"}
",@(when (module-replaces module)
         `("\\entry{Module Replaces (where appropriate)}{"
           ,(module-replaces module)"}
"))
       "\\entry{Date of introduction of new module}{" ,(year-of-introduction module)"}
\\multicolumn{2}{|p{2\\colwidth}|}{Level} & "
       ,(if (= (level module) 4) "MSc" (level module))
       " & Credit Value & " ,(credits module) "\\\\\\hline
\\entry{Programme(s) in which module is available}{
\\begin{simplelist}
",@(or (mapcar #'(lambda(p) (format nil "\\item ~A~%" (title p)))
               (programmes module))
       '("\\item --"))
       "\\end{simplelist}}
\\entry{Involvement of Other Schools}{None}
\\entry{Resource Split}{}
\\entry{Name of Module Co-ordinator}{" ,(fullname (owner module)) "}
Related Modules
 & Pre-requisites & \\multicolumn{3}{p{3\\colwidth}|}{"
       ,@(or (related-moduleids module "prerequisites") (list "None"))
       "}\\\\\\hline
 & Co-requisites & \\multicolumn{3}{p{3\\colwidth}|}{"
       ,@(or (related-moduleids module "corequisites") (list "None"))
       "}\\\\\\hline
 & Prohibited Combinations & \\multicolumn{3}{p{3\\colwidth}|}{"
       ,@(or (related-moduleids module "forbidden") (list "None"))
       "}\\\\\\hline
\\entry{Minimum and Maximum Intake Sizes}{"
       ,(or (minimum-intake module) "None") " -- "
       ,(or (maximum-intake module) "None") "}
\\mcol{5}{\\begin{minipage}[t]{5\\colwidth}
Aims of the Module
",(markup-elements (aims module)) "
\\end{minipage}
}\\\\\\hline
\\mcol{5}{
\\begin{minipage}[t]{5\\colwidth}
Summary of Content
" ,(markup-elements (summary module)) "
\\end{minipage}
}\\\\\\hline
\\entry{Summary of Methods and Frequency of Teaching}{
",(markup-elements (teaching-methods-markup module)) "
}
\\mcol{5}{
  \\begin{minipage}[t]{5\\colwidth}
Summary of Methods of Assessment\\\\
    \\begin{tabular}{p{\\colwidth}p{0.8\\colwidth}p{0.3\\colwidth}p{2\\colwidth}p{0.7\\colwidth}}
      {\\em Assessment Type} & {\\em Status} & {\\em \\%} & {\\em
        Requirements} & {\\em Due}\\\\\\hline
"
       ,@(mapcar
          #'(lambda(a)
              (format nil "~A & ~A & ~4,1F & ~A. ~A & ~A\\\\~%"
                      (description (assessment-method a)) (status a)
                      (weighting a)
                      (title a)
                      (requirements a)
                      (format-datestamp (deadline-date a))))
          (assessments module))
       "\\end{tabular}
\\end{minipage}
}\\\\\\hline
\\mcol{3}{Module Outcomes - what the student should gain
  from successful completion of the module:} &
\\mcol{2}{
Learning and Teaching and Assessment Strategies to enable outcomes to
be achieved and demonstrated}\\\\\\cline{4-5}
\\mcol{3}{} & Learning and Teaching Methods & Assessment
Methods\\\\\\hline
"
       ,@(let ((outcomes (module-outcomes module)))
              (if outcomes
                  (flet((formatted-outcomes(type)
                          (mapcan
                           #'(lambda(o)
                               (when (equal type (outcome-type o))
                                 (list
                                  "\\mcol{3}{\\begin{minipage}[t]{3\\colwidth}"
                                  (markup-elements (description o)) "\\end{minipage}} & "
                                  (markup-elements (teaching o)) " & "
                                  (markup-elements (assessment o)) "\\\\" #\newline)))
                           outcomes)))
                    `("\\multicolumn{3}{|c|}{Knowledge and Understanding} & & \\\\\\cline{1-3}
" ,@(formatted-outcomes "knowledge") "\\hline
\\multicolumn{3}{|c|}{Intellectual Skills} & & \\\\\\cline{1-3}
" ,@(formatted-outcomes "intellectual") "\\hline
\\multicolumn{3}{|c|}{Professional/Subject Specific Skills} & & \\\\\\cline{1-3}
" ,@(formatted-outcomes "professional") "\\hline
\\multicolumn{3}{|c|}{Transferable Skills} & & \\\\\\cline{1-3}
" ,@(formatted-outcomes "transferable") "\\hline
"))
                  (mapcar
                   #'(lambda(section entries)
                       (let ((teaching nil)
                             (assessment nil))
                       (format nil
                               "\\multicolumn{3}{|c|}{~A} & & \\\\\\cline{1-3}
~:{\\mcol{3}{\\begin{minipage}[t]{3\\colwidth}~A\\end{minipage}} & ~@[~A~] & ~@\[~A~] \\\\

~}\\hline
"
                         (curriculum-section-title section)
                         (mapcar
                          #'(lambda(entry)
                              (list
                               (description (first entry))
                               (let ((n (methods-summary (second entry))))
                                 (when (not (string-equal n teaching))
                                   (setf teaching n)))
                               (let ((n (methods-summary (third entry))))
                                 (when (not (string-equal n assessment))
                                   (setf assessment n)))))
                          entries))))
                   curriculum-sections
                   (curriculum (list module))) ))
       "\\mcol{5}{Please provide either or both of:}\\\\\\hline
\\entry{(i) Introductory Learning Resources}{\\begin{minipage}[t]{3\\colwidth}"
       ,(markup-elements (learning-resources module))"\\end{minipage}}
\\entry{(ii) Core Texts}{\\begin{minipage}[t]{3\\colwidth}"
       ,(markup-elements (core-texts module))"
 \\end{minipage} }"
       ,(format nil "\\entry{Reading Lists}{~:[Not Attached~;Attached~]}"
                (or (learning-resources module) (core-texts module)))
       ,(format nil
                "\\entry{Specification completed by:}{~A}"
                (fullname (prepared-by module)))
       ,(format nil
                "\\entry{Date}{~A}
 \\entry{Date module approved by Teaching Committee(s)}{}
 \\entry{Date module approved by School Board(s)}{}"
                (jarw.parse::format-output 'date (get-universal-time)
                                           :fmt :date-only))
"\\hline\\end{longtable}
 \\end{module-specification}"
     ))))


(defmethod write-module-specification((format (eql :docbook))
                                      (module module-specification)
                                      &optional (stream *standard-output*))
  (flet ((markup-elements(items)
           (with-output-to-string(os)
             (dolist(item items) (docbook os item) (terpri os))))
         (entry(term contents)
           (when (if (stringp contents)
                     (or (= 0 (length contents))
                         (not (eql (char (string-trim
                                          '(#\space #\newline #\return #\tab)
                                          contents) 0)
                                   #\<)))
                     (or (null contents) (not (listp contents))))
             (setf contents `("<para>" ,(or contents "") "</para>")))
           (format nil "<varlistentry>
       <term>~A</term>
  <listitem>~{~A~}</listitem>
</varlistentry>
"
                   term (if (listp contents) contents (list contents)))))
    (map
     'nil #'(lambda(item) (princ item stream))
     `("<!DOCTYPE section PUBLIC \"-//OASIS//DTD DocBook XML V4.3//EN\"
                    \"http://www.oasis-open.org/docbook/xml/4.3/docbookx.dtd\">
<section id=\""
       ,(moduleid module) "\" xreflabel=\""
       ,(moduleid module) ": " ,(title module) "\">
<title>"
       ,(moduleid module) ": " ,(title module) "</title>
<variablelist>
<?dbhtml list-presentation=\"table\"?>
<?dbhtml term-width=\"25%\"  list-width=\"100%\" ?>
<?dbhtml term-presentation=\"bold\" ?>
<?dbhtml table-summary=\"Module Parameters\" ?>
<varlistentry>
  <term>School and  Subject Group</term>
  <listitem>
  <simplelist>
     <member>School of Engineering and Applied Science</member>
     <member>Electronic Engineering</member>
  </simplelist>
  </listitem>
</varlistentry>
"
       ,(entry "Module Code" (moduleid module))
       ,(entry "Module Title" (title module))
       ,(entry "Module Type" (module-type module))
       ,@(when (module-replaces module)
               (list (entry "Module Replaces (where appropriate)"
                            (module-replaces module))))
       ,(entry "Date of introduction of new module"
               (year-of-introduction module))
       ,(entry "Level" (if (= (level module) 4) "MSc" (level module)))
       ,(entry "Credit Value"(credits module))
       ,(entry "Programme(s) in which module is available"
               (when (programmes module)
                 `("<simplelist>"
                   ,@(mapcar
                      #'(lambda(p)
                          (format nil "<member><xref linkend=~S/></member>"
                                  (string-downcase (programmeid p))))
                      (programmes module))
                   "</simplelist>")))
       ,(entry "Involvement of Other Schools" "None")
;;       ,(entry "Resource Split" nil)
       ,(entry "Name of Module Co-ordinator" `("<simplelist><member>"
                                               ,(fullname-link (owner module))
                                               "</member></simplelist>"))
       ,(entry "Related modules"
               `("<variablelist>
<?dbhtml list-presentation=\"table\"?>
<?dbhtml term-presentation=\"italic\" ?>"
                 ,@(flet((lentry(title relationship)
                                (let((related (related-moduleids module relationship)))
                                  (entry title
                                         (when related
                                           (format nil "~A~{, ~A~}"
                                                   (first related)
                                                   (rest related)))))))
                        (list
                         (lentry "Pre-requisites" "prerequisites")
                         (lentry "Co-requisites" "corequisites")
                         (lentry "Prohibited Combinations" "forbidden")))
                 "</variablelist>"))
       ,(entry "Minimum and Maximum Intake Sizes"
               `("<simpara>" ,(or (minimum-intake module) "Any") " - "
                 ,(or (maximum-intake module) "Any") "</simpara>"))
       ,(entry "Aims of the Module" (markup-elements (aims module)))
       ,(entry "Summary of Content" (markup-elements (summary module)))
       ,(entry "Summary of Methods and Frequency of Teaching"
               (markup-elements (teaching-methods-markup module)))
       ,(entry "Summary of Methods of Assessment"
               `("<informaltable frame=\"none\" rowsep=\"0\">
<tgroup cols=\"5\"><colspec colnum=\"3\"  align=\"right\" />
<thead>
  <row>
    <entry>Assessment Type</entry>
    <entry>Status</entry>
    <entry>%</entry>
    <entry>Requirements</entry>
    <entry>Due</entry>
  </row>
</thead>
<tbody>"
                 ,@(mapcar
                    #'(lambda(a)
                        (format nil "~%<row><entry>~A (~A)</entry><entry>~A</entry><entry>~A</entry><entry>~A</entry><entry>~AAssociate Dean for Postgraduate Programmes</entry></row>"
                                (title a) (assessment-type a) (status a)
                                (format-percentage (weighting a))
                                (requirements a)
                                (format-datestamp (deadline-date a))))
                    (assessments module))
                 "</tbody></tgroup></informaltable>" #\newline))
       "<varlistentry>
<term>Module Outcomes</term>
<listitem>
<informaltable frame=\"none\">
<tgroup cols=\"3\">
<colspec colnum=\"1\" colname=\"items\"/>
<colspec colnum=\"2\" colname=\"learning\"/>
<colspec colnum=\"3\" colname=\"assessment\"/>
<thead>
<row>
<entry morerows=\"1\"> - what the student should gain from
successful completion of the module:</entry>
<entry namest=\"learning\" nameend=\"assessment\">
Learning and Teaching and Assessment Strategies to enable outcomes to be achieved and demonstrated</entry>
</row>
<row>
<entry colname=\"learning\">Learning and Teaching Methods</entry>
<entry colname=\"assessment\">Assessment MeAssociate Dean for Postgraduate Programmesthod</entry></row>
</thead>
<tbody>
"
       ,@(let ((outcomes (module-outcomes module)))
              (if outcomes
                  (flet((formatted-outcomes(title type)
                          `("<row><entry namest=\"items\" nameend=\"assessment\" align=\"center\"><emphasis>"
                            ,title
                            "</emphasis></entry></row>"
                            ,@(mapcan
                               #'(lambda(o)
                                   (when (equal type (outcome-type o))
                                     (list
                                      "<row><entry>"
                                      (markup-elements (description o))
                                      "</entry><entry>"
                                      (markup-elements (teaching o))
                                      "</entry><entry>"
                                      (markup-elements (assessment o))
                                      "</entry></row>" #\newline)))
                               outcomes))))
                    (nconc
                     (formatted-outcomes "Knowledge and Understanding" "knowledge")
                     (formatted-outcomes "Intellectual Skills" "intellectual")
                     (formatted-outcomes "Professional/Subject Specific Skills"
                                         "professional")
                     (formatted-outcomes "Transferable Skills" "transferable")))
                  (mapcar
                   #'(lambda(section entries)
                       (let ((teaching nil) (assessment nil))
                       (format nil
                               "<row><entry namest=\"items\" nameend=\"assessment\" align=\"center\"><emphasis>
~A
</emphasis></entry></row>
~:{<row><entry>~A</entry>
     <entry>~@[~A~]</entry>
     <entry>~@[~A~]</entry>
</row>
~}
"
                               (curriculum-section-title section)
                               (mapcar #'(lambda(entry)
                                           (list
                                            (description (first entry))
                                            (let ((n (methods-summary (second entry))))
                                              (when (not (string-equal n teaching))
                                                (setf teaching n)))
                                            (let ((n (methods-summary (third entry))))
                                              (when (not (string-equal n assessment))
                                                (setf assessment n)))))
                                       entries))))
                   curriculum-sections (curriculum (list module)))))
       "</tbody></tgroup></informaltable></listitem></varlistentry>
"
       ,(entry "Learning Resources"
               (markup-elements (learning-resources module)))
       ,(entry  "Core Texts" (markup-elements (core-texts module)))
       "</variablelist></section>"))))

(defvar *m*)

(defun do-spec(moduleid &key aims summary teaching-methods
               learning-resources core-texts)
  (setq *m* (first (clews.grades::module-specifications
                    *db*
                    'clews.grades::moduleid moduleid
                    'clews.grades::year 2005)))
  (when aims (setf (aims *m*) aims))
  (when summary (setf (summary *m*) summary))
  (when teaching-methods (setf (slot-value *m* 'teaching-methods-markup)
                               teaching-methods))
  (when learning-resources
    (setf (learning-resources *m*) learning-resources))
  (when core-texts (setf (core-texts *m*) core-texts))
  (update-records-from-instance *m*))

(defun add-outcome(type &key description teaching assessment)
  (update-records-from-instance
   (make-instance 'module-outcome
                  :moduleid (moduleid *m*)
                  :year (year *m*)
                  :type type
                  :description description
                  :teaching teaching
                  :assessment assessment)))

(defun save-spec(&optional (moduleid (moduleid *m*)) &key (year (year *m*)) (format :latex))
  (with-open-file
      (os (ecase format
            (:latex (format nil "/home/willijar/teaching/MSc/specifications/~A/~A.tex"
                            year moduleid))
            (:docbook (format nil "/home/willijar/www/modules/~A.xml" moduleid)))
          :direction :output :if-exists :supersede :if-does-not-exist :create)
    (write-module-specification format (first
                                        (clews.grades::module-specifications
                                         *db*
                                         'clews.grades::moduleid moduleid
                                         'clews.grades::year year))  os)))

(defmethod programme-modules((programmeid string) year)
  (programme-modules
   (first (programme-records *db* 'programmeid programmeid))
   year))

(defmethod programme-modules((programme programme) year)
  (let ((moduleids
         (or
          (clsql:query
           (format nil "select moduleid from programme_modules where programmeid='~A' and year=~S" (programmeid programme) year)
           :flatp t
           :database (clsql-sys::view-database programme)
           )
          (clsql:query
           (format nil "select distinct moduleid from module_marks left join students using (studentid) where programmeid='~A' and students.year=~S" (programmeid programme) year)
           :flatp t
           :database (clsql-sys::view-database programme)
           ))))
    (clsql:select
	     'module-specification
	     :database (clsql-sys::view-database programme)
	     :flatp t
	     :caching nil
	     :where [and [= [slot-value 'module 'year] year]
			 [in [slot-value 'module-specification 'moduleid] moduleids]])))

(defun outcomes*(m)
  (mapcar
   #'(lambda(o) (cons (intern (string-upcase (outcome-type o)) :keyword) (description o)))
   (module-outcomes m)))

(defgeneric write-curriculum-map(entity &key stream)
  (:documentation "Output the curriculum map in latex format for the given entity"))

(defgeneric write-module-table(entity &key stream)
  (:documentation "Write the module table for entity in latex format to stream"))

(defgeneric write-outcomes-tables(entity &key stream)
  (:documentation "Write out the outcomes table in latex format for entity"))

(defmethod write-curriculum-map((modules list) &key (stream *standard-output*))
  (let ((curriculum (curriculum modules)))
  (multiple-value-bind(map entries)
      (curriculum-map modules curriculum)
    (format stream "\\begin{tabular}{|lp{5cm}l~{~*c~}|}\\hline\\hline~%"
            entries)
    (format stream "Level & Modules & Codes ~{& ~A~}\\\\\\hline\\hline~%"
            (mapcar #'cdr entries))
    (dolist(module modules)
      (format stream "~A & ~A & ~A"
              (level module) (title module) (moduleid module))
      (do((x 0 (1+ x)))
         ((>= x (length entries)))
        (format stream "&~:[~;X~]"
                (= 1 (aref map (position module modules) x))))
      (write-line "\\\\\\hline" stream))
    (format stream "\\hline\\end{tabular}~%~%")
    (mapcar
     #'(lambda(section section-entries)
         (write-line "\\begin{tabular}{|lp{8cm}|}\\hline\\hline" stream)
         (format stream "\\multicolumn{2}{|c|}{~A}\\\\\\hline~%"
                 (curriculum-section-title section))
         (format stream "\\multicolumn{2}{|p{9cm}|}{\\em ~A}\\\\~%"
                 (curriculum-section-blurb section))
         (format stream "~:{~A & ~A\\\\~%~}"
                 (mapcar #'(lambda(entry)
                             (list
                              (cdr (assoc (car entry) entries))
                              (description (car entry))))
                         section-entries))
         (format stream "\\hline\\hline\\end{tabular}~%"))
     curriculum-sections curriculum))))

(defmethod write-module-table((modules list) &key (stream *standard-output*))
  (let ((modules (sort (copy-list modules)
                       #'(lambda(a b)
                           (if (= (stage a) (stage b))
                               (string< (moduleid a) (moduleid b))
                               (< (stage a) (stage b))))))
        (stage nil)
        (total-credits 0))
    (flet((finish-table()
            (when (> total-credits 0)
              (format stream "\\hline\\bf Total & \\bf ~D & & & \\\\\\hline\\hline~%"
                      total-credits))
            (format stream "\\end{structure-table}~%~%")
            (setf total-credits 0)))
    (dolist(module modules)
      (when (not (eql (stage module) stage))
        (when stage (finish-table))
        (setf stage (stage module))
        (format stream "\\begin{structure-table}{Stage ~A}~%"
                (jarw.string::to-roman stage)))
      (incf total-credits (credits module))
      (format stream "~A & ~A & ~A & ~A & Core\\\\~%"
              (title module)
              (credits module)
              (level module)
              (moduleid module)))
    (finish-table))))

(defun methods-summary(list)
  (jarw.string:join-strings
   (sort (delete-duplicates (mapcar #'description list) :test #'string=)
         #'string<)
   ", "))

(defun mapappend(accessor items)
  (mapcan #'(lambda(item) (copy-list (funcall accessor item))) items))

(defmethod write-outcomes-tables((modules list)
                                 &key (stream *standard-output*))
  (mapcar
   #'(lambda(section entries)
       (format stream "\\begin{outcomes-table}~%[~A]~%{Stage ~A}~%"
               (curriculum-section-blurb section)
               (curriculum-section-title section))
       (let ((teaching nil)
             (assessment nil)
             (n 0))
         (format stream "~:{~A & ~A & ~@[~A~] & ~@[~A~]\\\\~%~}"
                 (mapcar
                  #'(lambda(entry)
                      (list
                       (incf n)
                       (description (first entry))
                       (let ((n (methods-summary (second entry))))
                         (if (not (equalp n teaching))
                             (setf teaching n)
                             "\""))
                       (let ((n (methods-summary (third entry))))
                         (if (not (equalp n assessment))
                             (setf assessment n)
                             "\""))))
                  entries)))
       (format stream "\\hline\\end{outcomes-table}~%~%"))
   curriculum-sections (curriculum modules)))

(defun copy-slots(old new slots)
  (dolist(slot slots)
    (if (slot-boundp old slot)
        (setf (slot-value new slot) (slot-value old slot))
        (slot-makunbound new slot))))

(defun nextval(name &key (database *db*))
  (car (clsql:query
           (format nil "select nextval('~A')" name)
           :flatp t
           :database database )))

(defun clone-assessment(old newmoduleid newyear
                        &key (database (clsql-sys::view-database old)))
  (let ((new (make-instance
              'assessment
              :assessmentid (nextval "assessmentid" :database database)
              :moduleid newmoduleid
              :year newyear)))
    (copy-slots old new
                '(title percentage assessor type feedback-form status
                  requirements deadline-date release-date))
    ;; add years to dates
    (dolist(slot '(deadline-date release-date))
      (when (and (slot-boundp old slot) (slot-value old slot))
        (setf (slot-value new slot)
              (+ (slot-value old slot)
                 (* 365 24 60 60 (- newyear (year old)))))))
    (update-records-from-instance new :database database)
    ;; update curriculum map
   (dolist(oldmap
             (select* database
                      'assessment-map
                      'assessmentid (slot-value old 'assessmentid)))
      (update-records-from-instance
       (make-instance 'assessment-map
                      :assessmentid (assessmentid new)
                      :curriculum-item-id
                      (slot-value oldmap 'curriculum-item-id))
       :database database))
    new))

(defun clone-teaching(old newmoduleid newyear
                        &key (database (clsql-sys::view-database old)))
  (let ((new (make-instance 'teaching
                            :id (nextval "teachingid" :database database)
                            :moduleid newmoduleid
                            :year newyear)))
    (copy-slots old new '(type description contact-time))
    (update-records-from-instance new :database database)
    ;; update curriculum-map
    (dolist(oldmap
             (select* database
                      'teaching-map
                      'teachingid (slot-value old 'teachingid)))
      (update-records-from-instance
       (make-instance 'teaching-map
                      :teachingid (teachingid new)
                      :curriculum-item-id
                      (slot-value oldmap 'curriculum-item-id))
       :database database))
    new))

(defun clone-module(old  newyear
                    &key (newmoduleid (moduleid old))
                    (database (clsql-sys::view-database old)))
  (let ((new (make-instance 'module-specification
                            :moduleid newmoduleid
                            :year newyear)))
    (copy-slots old new
                '(title credits level stage teaching_period owner
                  pass-mark module-type module-replaces minimum-intake
                  maximum-intake aims summary teaching-methods-markup
                  learning-resources core-texts))
    (update-records-from-instance new :database database)
    ;; copy teaching
    (dolist(teaching (teaching old))
      (clone-teaching teaching newmoduleid newyear))
    ;; copy assessments
    (dolist(assessment (assessments old))
      (clone-assessment assessment newmoduleid newyear))
    new))





#|
(dolist(m (map 'list #'moduleid (module-records *db* 'year 2006)))
  (save-spec m :year 2007))
;;;; XXXXX

(curriculum-items (car (assessment-records *db* 'year 2007 'type "EXAM")))
|#

(defun write-assessment-audit-trail(assessment
                                    &key (stream *standard-output*))
  (let ((outcomes (curriculum-items assessment))
        (module (module assessment)))
   (format stream "
\\documentclass[a4paper]{article}

\\setlength{\\topmargin}{-1cm}
\\setlength{\\oddsidemargin}{0.5cm}
\\setlength{\\evensidemargin}{-0.5cm}
\\setlength{\\textwidth}{15.5cm}
\\setlength{\\textheight}{24cm}

\\newcommand{\\entry}[3]{
\\item #1's #2

\\noindent #3

\\noindent\\begin{tabular}{lp{4.9cm}lp{4.9cm}}
 #1: & &  Date: & \\vspace{1.5em}
\\end{tabular}
\\hrule
}

\\begin{document}
\\section*{Electronic Engineering Exam Paper Audit Trail}

%% Replace the \\vspace{3cm} in the
%% appropriate sections with the comments

\\noindent\\begin{tabular}{r@{:}p{5cm}r@{:}p{5cm}}
 Module Code & ~A &  Module Title & ~A \\\\
 Year & ~A &  Level & ~A \\\\
 Examiner & ~A&  Moderator & \\\\
\\end{tabular}
\\hrule
\\begin{enumerate}
\\entry{Examiner}{confirmation of outcomes. Enter for each outcome
  listed below (taken from the module specification)
  which question parts it is assessed in.}{
\\begin{description}
"
           (moduleid module)
           (title module)
           (year module)
           (level module)
           (fullname (owner module)))
   (mapcar
    #'(lambda(section)
        (let ((entries (mapcan
                        #'(lambda(e)
                            (when (equalp (curriculum-item-type e)
                                          (curriculum-section-type section))
                              (list e)))
                        outcomes)))
          (when entries
            (format stream
                    "\\item[~A.] ~A~%\\begin{enumerate}~%"
                    (curriculum-section-title section)
                    (curriculum-section-blurb section))
            (format stream "~{\\item ~A~%\\vspace{1.5em}~%~}"
                    (mapcar #'description entries))
            (format stream "\\end{enumerate}~%"))))
    curriculum-sections)
   (write-string
    "
\\end{description}}

\\entry{Moderator}{Comments}{
%Enter text here
\\vspace{2cm}}

\\entry{Examiner}{response to moderator's comments}{
%Enter text here
\\vspace{2cm}}

\\entry{External examiner}{comments}{
%Enter text here
\\vspace{2cm}}

\\entry{Examiner}{response to external examiner's comments}{
%Enter text here
\\vspace{2cm}}

\\end{enumerate}

\\section*{Guidance on the marking of Examination Papers}

The School document ``SEAS Procedures and Guidance for Assessment -
Undergraduate and Taught Postgraduate Programmes'' includes advice on
marking and setting examination questions; the relevant sections are
reproduced below.

\\begin{description}
\\item[3.1.3] During the marking process each page should be annotated
  by the marker to indicate it has been viewed.  The annotation can be
  a mark, a tick or written comments.

\\item[3.1.4] It should be made clear how the marks for a particular
  question have been arrived at; in particular it is not acceptable to
  simply give an overall mark for a long question, without further
  justification. Justification for a final mark can take the form of:
  marks for individual components in accordance with the mark scheme;
  ticks or comments associated with points made by the student; a
  short written justification covering the strengths and weaknesses of
  the answer.

\\item[3.1.5] For all Final Examination papers, the addition of
  contributory marks must be checked in a fashion approved by the
  Director of Undergraduate/Postgraduate Programmes. This is necessary
  to support the rejection of all appeals against academic judgement.

\\item[3.1.6]	A minimum of 20\\% of final year papers plus all coursework worth
  more than 10 credits must be second marked and
  any differences resolved. Where this does
  occur, it should be made clear that it has
  taken place, e.g. the second marker might use a
  different coloured ink to indicate that he has
  viewed each page of a script.
\\end{description}
\\end{document}"
    stream)))

(defun write-assessment-audit-trails(year directory &key (type "EXAM"))
  (mapcar
   #'(lambda(assessment)
       (let ((path
              (merge-pathnames
               (make-pathname :name (format nil "~A-~A"
                                            (moduleid assessment)
                                            (assessment-type assessment))
                              :type "tex")
               directory)))
         (with-open-file(os path
                         :direction :output
                         :if-does-not-exist :create
                         :if-exists :overwrite)
           (write-assessment-audit-trail assessment :stream os))
         path))
   (assessment-records *db* 'year year 'type type)))


#|temp code for setting up outcomes|#

(defun new-spec(moduleid &key
                (old-year 2007)
                (newyear (1+ old-year))
                (newmoduleid moduleid))
  (let* ((old
          (first (module-specifications *db*
                                        'moduleid moduleid 'year old-year)))
         (new (clone-module old newyear :newmoduleid newmoduleid)))
    (report-outcomes new)
    new))


(defun report-outcomes(module)
  (format t "~S: ~S (~D)~%===============~%"
          (moduleid module) (title module) (year module))
  (format t "Teaching~%--------~%")
  (dolist(teaching (teaching module))
    (format t "~,3D ~S ~S~%"
            (teachingid teaching)
            (teaching-type teaching)
            (description teaching))
    (dolist(item (curriculum-items teaching))
      (format t "  ~,4D ~A ~S~%"
              (curriculum-item-id item)
              (curriculum-item-type item)
              (description item))))
  (format t "Assessment~%--------~%")
  (dolist(assessment (assessments module))
    (format t "~,3D ~S ~S~%"
            (assessmentid assessment)
            (assessment-type assessment) (title assessment))
    (dolist(item (curriculum-items assessment))
      (format t "  ~,4D ~A ~S~%"
              (curriculum-item-id item)
              (curriculum-item-type item)
              (description item)))))

(clsql:locally-disable-sql-reader-syntax)

