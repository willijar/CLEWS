;;; $Id: tables.lisp,v 1.4 2005/05/16 20:20:45 willijar Exp $
;;; Copyright (c) 2004 Dr John A.R. Williams <J.A.R.Williams@aston.ac.uk>.
;;; Main class and handlers for the project-manager class
;;; part of the EWAS Application for Managing Student Projects

;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 2
;;; of the License, or (at your option) any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

(in-package :clews.publications)

(defvar *citation-specifications*
  '((:article "An article from a journal or magazine"
     (:author :title :journal :year)
     (:volume :number :pages :month :note))
    (:book "A book with an explicit publisher"
     (:author :title :publisher :year)
     (:editor :volume :number :series :address :edition :month :note))
    (:booklet "A work that is printed or bound, but without a named publisher or sponsoring institution"
     (:title)
     (:author :howpublished :address :month :year :note))
    (:incollection "A part of a book with its own title"
     (:author :title :booktitle :publisher :year)
     (:editor :volume :number :series :type :chapter :pages
      :address :edition :month :note))
    (:inproceedings "An article in a conference proceedings. If there are several papers in the same proceeding it is preferred to cross reference to the proceedings"
     (:author :title :booktitle :year)
     (:editor :volume :number :series :pages :address :month
      :organisation :publisher :note))
    (:manual "Technical Documentation"
     (:title)
     (:author :organisation :address :edition :month :year :note))
    (:mastersthesis "A masters thesis"
     (:author :title :school :year)
     (:type :address :month :note))
    (:misc "Use this type when nothing else fits"
     ()
     (:author :title :howpublished :month :year :note))
    (:patent "A  patent or patent application"
     (:author :title :year :number)
     (:type :month :note))
    (:phdthesis "A Ph.D. thesis"
     (:author :title :school :year)
     (:type :address :month :note))
    (:proceedings "The whole proceedings of a conference"
     (:title :year)
     (:booktitle :editor :volume :number :series :address
      :month :organisation :publisher :note))
    (:techreport "A report published by a school or other institution, usually numbered within a series"
     (:author :title :institution :year)
     (:type :number :address :month :note))
    (:unpublished
     "A document with an author and title but not formally published"
     (:author :title :note)
     (:month :year))
    (:inbook "A part of a book, usually untitled; it may be a chapter (or other sectional unit) and/or a range of pages"
     (:author :title :pages :publisher :year)
     (:editor :chapter :volume :number :series :type :address :edition
      :month :note)))
  "The list of citation types with a description, required and
optional fields")

(defvar *publication-fields*
  `((:id
     "Unique identifiying key used when referring to the reference."
     ((input :size 8 :datatype string)))
    (:citetype
     "Citation type - see Citation type table for allowed types"
     ((mcq :size 1 :style :dropdown :datatype symbol)
      ,@(mapcar #'car *citation-specifications*)))
    (:address
     "Usually the address of the publisher or other type of institution. For major publishing houses, van~Leunen recommends omitting the information entirely. For small publishers, on the other hand, you can help the reader by giving the complete address."
     ((input :size 80 :datatype string)))
    (:author
     "The name(s) of the author(s) seperated with commas, lastname last, spaces (but no stops) between initials"
     ((textarea :cols 80 :rows 2 :datatype author)))
    (:booktitle
     "Title of a book, part of which is being cited. For book entries, use the title field instead."
     ((input :size 80 :datatype string)))
    (:chapter
     "A chapter (or section or whatever) number."
     ((input :size 8 :datatype string)))
    (:crossref
     "The publicationid of the entry being cross referenced."
     ((mcq :style :dropdown :datatype (string :nil-allowed t)) ))
    (:editor
     "Name(s) of editor(s). If there is also an author field, then the editor field gives the editor of the book or collection in which the reference appears."
     ((input :size 80 :datatype string)))
    (:journal "A Journal abbreviated title"
     ((input :size 8 :datatype string)))
    (:key "Used for alphabetizing, cross referencing, and creating a label when the \"author\" information is missing. This field should not be confused with the citation key."
     ((input :size 8 :datatype string)))
    (:month
     "The month in which the work was published or, for an unpublished work, in which it was written. You should use the standard three-letter abbreviation"
     ((mcq  :datatype (integer :min 1 :max 12 :nil-allowed t) :style :dropdown)
      (nil . "-") (1 ."jan") (2 . "feb") (3 ."mar") (4 . "apr") (5 . "may")
      (6 . "jun") (7 ."jul") (8. "aug") (9. "sep") (10 ."oct") (11 ."nov")
      (12 . "dec")))
    (:note "Any additional information that can help the reader. The first word should be capitalised."
	   ((textarea :rows 2 :cols 80)))
    (:number
     "The number of a journal, magazine, technical report, or of a work in a series."
     ((input :size 4 :datatype string)))
    (:organisation
     "The organization that sponsors a conference or that publishes a manual or technical report."
     ((input :size 25 :datatype string)))
    (:pages "Page or page range (e.g. 78-84) or paper ref in a collection/proceedings"
     ((input :size 8 :datatype string)))
    (:publisher "The publisher's name." (input :size 20 :datatype string))
    (:ranking
     "The RAE ranking of the paper"
     ((mcq :datatype integer :value 3)
      (4 . "**** Quality that is world-leading in terms of originality,
significance and rigour.")
      (3 . "***  Quality that is internationally excellent in terms of
originality, significance and rigour but which nonetheless falls short
of the highest standards of excellence.")
      (2 . "**   Quality that is recognised internationally in terms of
originality, significance and rigour.")
      (1 . "*    Quality that is recognised nationally in terms of
originality, significance and rigour.")
      (0 . "     Quality that falls below the standard of nationally
recognised work. Or work which does not meet the published definition
of research for the purposes of this assessment.")))
    (:series "The name of a series or set of books. When citing an entire book, the the title field gives its title and an optional series field gives the name of a series or multi-volume set in which the book is published."
     ((input :size 8 :datatype string)))
    (:title "The work's title"
     ((textarea :cols 80 :rows 2 :datatype string)))
    (:volume "The volume of a journal or multivolume book."
     ((input :size 4 :datatype string)))
    (:year "The year of publication or, for an unpublished work, the year it was written. Generally it should consist of four numerals, such as 2004."
	   ((input :size 4
             :datatype (integer :nil-allowed t :min 1980 :max 2032 :value 2005) )))
    (:type "The type of a technical report---for example, \"Research Note\"."
	   (input :size 25 :datatype string))
    (:edition "The edition of a book---for example, \"Second\". This should be an ordinal, and should have the first letter capitalized."
     ((input :size 25 :datatype string)))
    (:howPublished "How something strange has been published. The first word should be capitalised."
     ((input :size 25 :datatype string)))
    (:school "The name of the school where a thesis was written."
     ((input :size 50 :datatype string)))
    (:status "The publication status"
     ((mcq :datatype (symbol :nil-allowed t) :style :dropdown)
      (nil . "In Preparation")
      (:submitted . "Submitted")
      (:accepted . "Accepted for publication")
      (:rejected . "Rejected for publication")
      (:published . "Published")
      (:public . "Published and public")))
    (:abstract "Abstract for this paper if available"
     ((textarea :rows 5 :cols 80))))
  "The list of field types with description and input element.")


(defclass publications-table(web-table access-controlled-entity)
  ((journals :type journals-table :initform nil :initarg :journals
             :reader journals
             :documentation "Journals source")
   (citation-specifications :initform *citation-specifications*
                            :reader citation-specifications
                            :initarg :citation-specifications))
  (:default-initargs :fields *publication-fields*)
  (:documentation "The table of publications"))

(defmethod generate-key((table publications-table) value)
  "Generate a unique key - next string of integer above largest"
  (if value
      (string (call-next-method))
      (write-to-string
       (1+ (reduce #'max
                   (mapcan
                    #'(lambda(k)
                        (when (stringp k)
                          (let ((v (parse-integer k :junk-allowed t)))
                            (when v (list v)))))
                    (dictionary-keys table)))))))

(defun can-edit-publication(publication table &optional (user *current-user*))
  (when (stringp publication)
    (setf publication (get-dictionary publication table)))
  (or (has-permission :admin table user)
      (and (has-permission :author table user)
           (is-publication-author publication table user))))

(defun missing-publication-fields(publication table)
  (let ((publication (expand-crossref publication table)))
    (mapcan #'(lambda(field) (unless (getf publication field) (list field)))
            (third (assoc (getf publication :citetype)
                          (citation-specifications table))))))

(defmethod field-markup(field (table publications-table) &optional record)
  (let ((markup (call-next-method))
        (required
         (member field (third (assoc field (citation-specifications table))))))
    (case field
      (:id
       `((input
          :name :id
          :datatype ,#'(lambda(s)
                         (when (get-dictionary s table)
                           (error 'invalid-format 'id
                            :type s :value "Record with this key already in table"))
                         s)
          :value ,(generate-key table nil))))
      (:journal
       `((mcq :name :journal :style :dropdown :value ,(getf record :journal)
          :datatype (string :nil-allowed t))
         (nil . "-")
         ,@(sort (dictionary-keys (journals table))  #'string-lessp)))
      (:status
       `((mcq :name :status :value nil
          :datatype (symbol :nil-allowed t) :style :dropdown)
         (nil . "In Preparation")
         (:submitted . "Submitted")
         (:accepted . "Accepted")
         (:rejected . "Rejected")
         ,@(when (publication-complete-p record table)
                 `((:published . "Published")
                   ,@(when (has-permission :admin table)
                           `((:public . "Published and Public")))))))
      (:crossref
       `((mcq :name :crossref :style :dropdown :datatype (string :nil-allowed t))
         ,@(let((list)
                (xref-type (case (getf record :citetype)
                             ((:inbook :incollection) :book)
                             (:inproceedings :proceedings))))
               (map-dictionary
                #'(lambda(k v)
                    (let ((booktitle (getf v :booktitle)))
                      (when (eql (getf v :citetype) xref-type)
                        (push
                         (cons
                          k
                          (format
                           nil "~A: ~A" k
                           (if (and booktitle (> (length booktitle) 68))
                               (concatenate 'string
                                            (subseq booktitle 0 65) "...")
                               booktitle)))
                         list))))
                table)
               (setf list  (sort list #'string-lessp :key #'car))
               (unless required (push '(nil . "-") list))
               list)))
      (t (if required
             (multiple-value-bind(tag attr content) (split-markup markup)
               (join-markup
                tag
                (progn
                  (when (eql (getf attr :datatype) 'string)
                    (setf (getf attr :datatype) '(string :min-word-count 1)))
                  attr)
                content) markup)
             markup)))))

(defmethod table-form((table publications-table) &optional record)
  (if record
      (let ((spec
             (assoc (getf record :citetype) (citation-specifications table))))
        `((form :method :post :enctype "multipart/form-data")
          ((input :type :hidden :name :citetype :datatype symbol
            :value ,(getf record :citetype)))
          ((input :type :hidden :name :id
            :value ,(getf record :id)))
          ((section :title "Required Fields")
           (table
            ,@(mapcan
               #'(lambda(field) (table-row-markup table field record))
               (if (member (getf record :citetype) '(:proceedings :collection))
                   (third spec)
                   (cons :status (third spec))))))
          ((section :title "Optional Fields")
           (table
            ,@(mapcar
               #'(lambda(field)(table-row-markup table field record))
               (cons :crossref (fourth spec)))))
          (p ((input :name :submit :type :submit :value "Update Record")))))
      `(section
        ((form :method :post :enctype "multipart/form-data")
         (table
          ,@(mapcar
             #'(lambda(field) (table-row-markup table field record))
             '(:id :citetype))
          (tr ((td :colspan 2 :align :center)
               ((input :name :submit :type :submit
                       :value "Create a new entry"))))))
        ((section :title "Available Citation Types")
         (p "You cannot change the citation type once a record is created.")
         (dl
          ,@(mapcan
             #'(lambda(spec)
                 `((dt ,(string-upcase (first spec))) (dd ,(second spec))))
             (citation-specifications table)))))))

(defmethod search-form((table publications-table))
  `((form :method :post :enctype "multipart/form-data")
    (p "Search"
     ((mcq :name :field :style :dropdown :datatype (symbol :nil-allowed t))
      (nil . "Any field")
      ,@(mapcar #'car (field-specifications table)))
     ((mcq :name :search-type :style :dropdown
           :datatype symbol :value :keyword-all)
      (:phrase . "containing phrase")
      (:keyword-all "containing all keywords")
      (:keyword-any "containing any keyword")
      (:matches . "exactly matching")
      (:lt . "less than")
      (:gt . "greater than"))
     ((input :name :value :size 30)))
    (p
     " order by "
     ((mcq :name :order-field :style :dropdown
           :datatype symbol)
      ,@(cons :date
              (mapcar #'car (field-specifications table))))
     ((mcq :name :order :style :dropdown :datatype symbol :value :desc)
      (:asc . "ascending") (:desc . "descending"))
     ((input :type :submit :name :submit :value "Search")))))

(defmethod table-search-func((term list) (table publications-table))
  (let ((func (call-next-method)))
    #'(lambda(a) (funcall func (expand-crossref a table)))))

(defmethod table-sort-func((term list) (table publications-table))
  "Given the values from search-form return a sorting function"
  (if (eql (getf term :order-field) :date)
      (let ((test (ecase (getf term :order) (:asc #'<) (:desc #'>))))
        #'(lambda(a b) (funcall test
                                (publication-date a table)
                                (publication-date b table))))
      (let ((func (call-next-method)))
        #'(lambda(a b)
            (funcall func
                     (expand-crossref a table)
                     (expand-crossref b table))))))

(defvar *journal-fields*
  '((:abbreviation
     "The abbreviated title, often used in citations"
     ((input :size 80 :datatype (string :min-word-count 1))))
    (:title
     "The full expanded title of the journal"
     ((input :size 80 :datatype (string :min-word-count 1))))
    (:issn
     "The ISSN code for the journal"
     ((input :size 9 :datatype (issn :nil-allowed t))))
    (:class "The library classification for the journal"
     ((input :size 9 :datatype string)))
    (:ranking
     "The RAE ranking of the journal"
     ((mcq :datatype integer :value 3)
      (4 . "**** Quality that is world-leading in terms of originality,
significance and rigour.")
      (3 . "***  Quality that is internationally excellent in terms of
originality, significance and rigour but which nonetheless falls short
of the highest standards of excellence.")
      (2 . "**   Quality that is recognised internationally in terms of
originality, significance and rigour.")
      (1 . "*    Quality that is recognised nationally in terms of
originality, significance and rigour.")
      (0 . "     Quality that falls below the standard of nationally
recognised work. Or work which does not meet the published definition
of research for the purposes of this assessment."))))
  "Specifications for hournal record fields")
#|(setf (first (slot-value (clews.publications::publications *publications*)
		   'clews.table::field-specifications))
      '(:ID "Unique identifiying key used when referring to the reference."
	((INPUT :SIZE 8 :DATATYPE STRING))))
 |#
(defmethod parse-input((spec (eql 'issn)) (s string)
                            &key nil-allowed )
  (let* ((s (string-trim '(#\Space #\Tab #\Newline) s)))
    (unless (and nil-allowed (is-nil-string s))
      (when (or (/= (length s) 9)
                (not (eql (char s 4) #\-))
                (notevery #'digit-char-p (subseq s 0 4))
                (notevery #'digit-char-p (subseq s 5 9)))
        (error 'invalid-format :type spec :value s
               :reason "ISSN does is not in the required format dddd-dddd"))
      s)))

(defun split-authors(authors)
  "Given an author field split it into a list of individual names"
  (regex:regex-split-string
   (load-time-value (regex:compile-str "\(,? +and +|, *\)"))
   (regex:regex-substitute-string
    (load-time-value (regex:compile-str "[:space:]+"))
    authors
    '(" "))
   :remove-empty-substr t))

(defmethod parse-input((spec (eql 'author)) (s string)
                       &key &allow-other-keys)
  "Normalises author list into Ian Bennions preferred style!!"
  (let* ((s (substitute  #\space #\.
                         (string-trim '(#\Space #\Tab #\Newline) s))))
    (when (is-nil-string s)
      (error 'invalid-format :type spec :value s
             :reason "Author list cannot be empty"))
    (let ((authors (split-authors s)))
      (if (rest authors)
          (let* ((end (last authors 2))
                 (last (cadr end)))
            (setf (cdr end) nil)
            (concatenate 'string
                         (join-strings authors ", ") " and " last))
          (first authors)))))

(defun in-author-list(authors lastname &optional firstnames)
  (let* ((authors (mapcar #'jarw.string:split-string (split-authors authors)))
         (match (find lastname authors
                      :test #'string-equal
                      :key #'(lambda(a) (car (last a))))))
    (if (and firstnames match)
        (let* ((initials
                (mapcar #'(lambda(s) (aref s 0))
                        (split-string firstnames
                                      :remove-empty-subseqs t)))
               (author-initials
                (mapcar #'(lambda(s) (aref s 0)) (butlast match)))
               (mismatch  (mismatch initials author-initials)))
          (not
           (and mismatch
                (< mismatch
                   (min (length initials) (length author-initials))))))
        match) ))

(defun is-publication-author(publication table &optional (user *current-user*))
  (declare (ignore table))
  (when (getf publication :author)
    (in-author-list (getf publication :author)
                    (property user :lastname)
                    (property user :firstname))))

(defun expand-crossref(pub table)
  (let ((xref (getf pub :crossref)))
    (when xref
      (loop
       :for a :on (expand-crossref (get-dictionary xref table) table)
       :by #'cddr
       :unless (getf pub (car a))
       :do (setf pub (nconc (list (car a) (cadr a)) pub)))))
  (setf pub (nconc (list :crossref nil) pub))
  (when (not (getf pub :journal-abbrev))
    (let ((journal (get-dictionary (getf pub :journal) (journals table))))
      (when journal
      (setf pub
            (nconc
             `(:journal  ,(getf journal :title)
               :journal-abbrev  ,(getf journal :abbreviation)
               :issn  ,(getf journal :issn))
             pub)))))
  pub)

(defun publication-complete-p(p table)
  (let ((p (expand-crossref p table)))
    (every #'(lambda(k) (getf p k))
           (third (assoc (getf p :citetype)
                         (citation-specifications table))))))

(defgeneric display-publication(publication style table)
  (:documentation
   "return markup for a publication formatted according to style")
  (:method ((key string) style  table )
    (call-next-method (get-dictionary key table) style table))
  (:method :around((publication list) style table)
           (call-next-method (expand-crossref (copy-list publication) table)
                             style table)))

(defconstant +months+
  (if (boundp '+months+)
      +months+
      #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")))

(defmethod display-publication((p list) (style (eql :bibtex)) table)
  `(pre
    ,(format nil "@~A{ph:~A,~%" (getf p :citetype) (getf p :id))
    ,@(loop for a on p by #'cddr
            unless (member (car a) '(:id :citetype :status))
            collect (format nil " ~A=\{~A\},~%" (string-downcase (car a))
                            (if (eql (car a) :month)
                                (elt +months+ (1- (cadr a)))
                                (cadr a))))
    "}"))

(defun bibtex-out(p &optional (stream *standard-output*))
  (format stream "@~A{ph:~A,~%" (getf p :citetype) (getf p :id))
  (loop :for a :on p :by #'cddr
     :unless (member (car a) '(:id :citetype :status :changed :changed-by :journal-abbrev :crossref))
     :do (when (cadr a) (format stream " ~A=\{~A\},~%" (string-downcase (car a))
                 (if (eql (car a) :month)
                     (elt +months+ (1- (cadr a)))
                     (cadr a)))))
  (write-string "}" stream))

(defmethod display-publication((p list) style  table)
  (let ((type (getf p :citetype)))
    `(span
      ,(getf p :author) ", \"" ,(getf p :title) "\","
      ,@(when (member type  '(:article :misc))
              `(,(when (getf p :journal) `(i ,(getf p :journal) " "))
                ,(when (getf p :volume) `(b ,(getf p :volume)))
                ,@(when (getf p :number) (list " (" (getf p :number)")"))))
      ,@(when (member type  '(:inproceedings :misc))
              `(,(when (getf p :booktitle) (list 'i (getf p :booktitle) ", "))
                ,@(when (getf p :organisation)
                        (list (getf p :organisation) ", "))
                ,@(when (getf p :volume) `((b ,(getf p :volume)) ", "))
                ,(getf p :address)))
      ,@(when (member type  '(:incollection :inbook))
              `(" in "
                ,(when (getf p :booktitle) `(i ,(getf p :booktitle) " "))
                ,@(when (getf p :editor) (list ", ed. " (getf p :editor)))
                ,@(when (getf p :publisher)
                        `(", " ,(getf p :publisher)
                          ,@(when (getf p :address)
                                  (list ", " (getf p :address)))))))
      ,@(when (eql type :patent)
              `((i "Patent " ,(getf p :number))))
      ,@(when (getf p :pages) (list ", pp. " (getf p :pages)))
      ,@(when (getf p :year)
              `(" ("
                ,@(when (getf p :month)
                        (list (elt +months+ (1- (getf p :month))) " "))
                ,(getf p :year) ")")))))

(defun publication-date(p table)
  (let ((year (getf p :year))
        (month (getf p :month)))
    (unless (and year month)
      (let ((xref (expand-crossref p table)))
        (unless year (setf year (getf xref :year)))
        (unless month (setf month (getf xref :month)))))
    (if year
        (encode-universal-time 0 0 0 1 (or month 1) year)
        0)))

(defun string-or-number<(a b)
  (handler-case
      (< (parse-input 'number a) (parse-input 'number b))
    (error(c) (declare (ignore c)) (string< a b))))

(defun string-or-number>(a b)
  (handler-case
      (> (parse-input 'number a) (parse-input 'number b))
    (error(c) (declare (ignore c)) (string> a b))))

(defmethod table-sort-func((term list) (table table-dictionary))
  "Given the values from search-form return a sorting function"
  (let* ((order (getf term :order))
         (order-field (getf term :order-field))
         (field-type (table-datatype order-field table))
         (numerical-field-p
          (when order-field
            (member field-type '(integer number)) ))
         (test
          (case order
            (:asc (if numerical-field-p #'< #'string-or-number<))
            (:desc (if numerical-field-p #'> #'string-or-number>))))
         (key
          (cond
            (numerical-field-p #'(lambda(r) (getf r order-field 0)))
            ((eql field-type 'string) #'(lambda(r) (getf r order-field)))
            (t  #'(lambda(r)  (format nil "~A" (getf r order-field)))))))
    #'(lambda(a b) (funcall test (funcall key (expand-crossref a table))
                            (funcall key (expand-crossref b table))))))

(defclass journals-table(web-table)
  ()
  (:default-initargs :fields *journal-fields* :primary-key :abbreviation)
  (:documentation "The table of publications"))

(defclass resident-file-dictionary(dictionary)
  ((filepath :type pathname
             :reader filepath
             :initarg :file
             :documentation "path to file in which data is to be stored")
   (cache :type hash-table :documentation "In memory cache")
   (lock :initform (jarw.port:make-mutex :name "lock for writing")))
  (:default-initargs :external-format :utf-8)
  (:documentation "More efficient transactions type dictionary for
publication storage - current values stored in memory, transactions
written at end of file"))

(defmethod initialize-instance :after((dictionary resident-file-dictionary)
                                      &key (test #'equal)
                                      (external-format :utf-8)
                                      &allow-other-keys)
  (let ((cache (make-hash-table :test test)))
    (setf (slot-value dictionary 'cache) cache)
    (jarw.io:do-file (item (filepath dictionary)
                           :external-format external-format)()
      ()
    (let ((k (car item))
          (v (cadr item))
          (present-p (consp (cdr item))))
      (if present-p (setf (gethash k cache) v) (remhash k cache))))))

(defmethod dictionary-test((dictionary resident-file-dictionary))
  (hash-table-test (slot-value dictionary 'cache)))

(defmethod get-dictionary(key (dictionary resident-file-dictionary)
                          &optional default)
  (gethash key (slot-value dictionary 'cache) default))

(defmethod (setf get-dictionary)(value key
                                 (dictionary resident-file-dictionary)
                                 &optional default)
  (declare (ignore default))
  (with-slots(lock cache filepath) dictionary
    (jarw.port:with-lock(lock)
      (jarw.io:append-to-file filepath (list key value))
      (setf (gethash key cache) value))))

(defmethod rem-dictionary(key (dictionary resident-file-dictionary))
  (with-slots(lock cache filepath) dictionary
    (jarw.port:with-lock(lock)
      (jarw.io:append-to-file filepath (list key))
      (remhash key cache))))

(defmethod map-dictionary((function function)
                          (dictionary resident-file-dictionary))
  (maphash function (slot-value dictionary 'cache)))

(defmethod get-transactions(key (dictionary resident-file-dictionary))
  (let ((test (dictionary-test dictionary))
        (results nil))
    (jarw.io:do-file (item (filepath dictionary))
        ()
      (let ((k (car item))
            (v (cadr item)))
        (when (funcall test k key) (push v results))))
    results))

(defun reverse-order(in out)
  (let ((items nil))
    (jarw.io:do-file (item in)
        ()
      (push item items))
    (dolist(item items)
      (jarw.io:append-to-file out item))))

(defclass journals-file-table(resident-file-dictionary journals-table)
  ())

(defclass publications-file-table(publications-table resident-file-dictionary)
  ())

(defmethod field-markup(field (table journals-table) &optional record)
  (let ((markup (call-next-method)))
    (if (and record (eql field :abbreviation))
        (multiple-value-bind(tag attr content) (split-markup markup)
          (setf (getf attr :readonly) :readonly)
          (setf (getf attr :size) (length (getf record :abbreviation)))
          (join-markup tag attr content))
        markup)))

;;;; TSV record handling for school publication database
(defun pub-no-pages(rec)
  (let* ((pages (split-string (getf rec :pages) :count 3 :delimiter #\-) ))
    (handler-case
        (- (parse-integer (first (last pages)))
           (parse-integer (first pages)))
      (error () 1))))

(defun citetype(rec)
  (intern (string-upcase (getf rec :citetype)) :keyword))

(defun pub-date(rec)
  (format nil "~A ~A" (getf rec :month) (getf rec :year)))

(defvar *tsv-mappings*
  `((:article
     :year :author :title :journal :volume :pages :issn)
    (:inproceedings
     :year :author :title :booktitle ,#'pub-date
     :organisation :page :publisher)
    (:book :year :author  :title :publisher ,#'pub-no-pages :isbn)
    (:inbook :year :author :title :booktitle :editor :publisher :isbn :pages)
    (:patent :year :title :number :author)))

(defun tsv-field(record field)
  (let ((field (etypecase field
                            (symbol (getf record field))
                            (function (funcall field record)))))
    (if (stringp field)
        (substitute-if-not #\space #'graphic-char-p field)
      (or field ""))))

(defmethod display-publication((p list) (style (eql :tsv)) table)
  (with-output-to-string(os)
    (let ((fields (cdr (assoc (citetype p) *tsv-mappings*))))
      (unless fields
        (error "No TSV mapping for citation type ~A" (citetype p)))
      (dolist(field fields)
        (unless (eql field (car fields)) (write-char #\tab os))
        (princ (tsv-field p field) os))
      (terpri os))))

#| get completed published papers - article and inproceedings |#
#|(defvar *publications* (publications aston::*publications*))
(map-dictionary #'(lambda(k p)
                      (when (eql (getf p :status) :public)
                        (bibtex-out
                         (expand-crossref (copy-list p) *publications*))))
                *publications*)
|#