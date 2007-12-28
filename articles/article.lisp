;; $Id: article.lisp,v 1.1 2007/07/26 08:53:35 willijar Exp willijar $
;; Article interface and base class implementation
;; Copyright (C) 2006 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of the CLEWS Article application library

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;; An article instance represents a single article in a collection of articles
;; it is cached in memory and should manage updating its own storage.

(in-package :clews.articles)

;;; external interface

(defgeneric id(article)
  (:documentation "Return a unique id for this article in its collection"))

(defgeneric collection(article)
  (:documentation "Return the collection in which an article is stored"))

(defgeneric text(article)
  (:documentation "Return the article content source text"))

(defgeneric (setf text)(text article)
  (:documentation "Set and store the article content text of given
type and to be stored used the specified encoding"))

(defgeneric errors(article)
  (:documentation "Return a list of errors in the article"))

(defgeneric last-modified(article)
  (:documentation "Return when this record was last modified"))

(define-condition article-error(error)
  ((collection
    :type article-collection :initarg :collection :reader collection)
   (id :type string :reader id :initarg :id)))

(define-condition article-not-found(article-error)
  ()
  (:report
   (lambda(c s)
     (format s "Article ~S not found in  ~S" (id c) (collection c)))))

(define-condition invalid-article-id(article-error)
  ()
  (:report
   (lambda(c s)
     (format s "~S is not a valid article id in ~S" (id c) (collection c)))))

(define-condition document-error(article-error)
  ((message :initform "" :reader message :initarg :message))
  (:report (lambda(c s) (write (message c) :stream s))))

;;; internal interface

(defgeneric instance-read-date(instance)
  (:documentation "Return the time when this instance was last read
from the record"))

(defgeneric record-write-date(instance)
  (:documentation "Time the instance was las written to permanent record"))

(defgeneric media-list(article)
  (:documentation "return the list of names of media files attached to
this article"))

(defgeneric (setf media)(data name article)
  (:documentation "Stored the data as media file name for article"))

(defgeneric rem-media(name article)
  (:documentation "Remove the media with given name from article"))

(defgeneric media-size(article)
  (:documentation "Return number of octets occupied by media for this
article"))

;; base implementation using the file system

(defclass article(access-controlled-entity parsed-fields)
  ((id :type string :reader id :initarg :id :initform nil
       :documentation "unique id/path for this article")
   (title :type string :field-name "title" :reader title :initform "no title")
   (author :reader author :initform nil
    :type list :field-name "author" :format rfc2822:addresses
    :documentation "The list of article authors allowed to edit this article")
   (collection :type article-collection :initarg :collection
               :accessor collection
               :documentation "Collection on which this article is stored")
   (content-file-position
    :type integer :reader content-file-position
    :documentation "Position of start of content in file")
   (instance-read-date :type integer :reader instance-read-date
                       :initform 0)
   (acl :format (acl :unbound-if-nil t) :field-name "acl"
        :documentation "Access control for this article")
   (document :documentation "Cached parsed document")
   (lock :initform (make-mutex) :reader mutex)
   (errors :type list :initform nil :reader errors
           :documentation "A list of errors to report for this document")
   (nameids :type list :reader nameids
            :documentation "A mapping of names to ids for this document"))
  (:metaclass  standard-parsed-class)
  (:documentation "An article"))

(defmethod field-missing((article article) field-name
                         (operation (eql 'setf)) &optional new-value)
  (push (cons field-name new-value) (unparsed-values article)))

(defmethod field-missing((article article) field-name
                         (operation (eql 'field-value)) &optional new-value)
  (declare (ignore new-value))
  (cdr (assoc field-name (unparsed-values article)
              :test (jarw.parse:field-test article))))

(defun path-to-article(id collection)
  (merge-pathnames
   (make-pathname :name id :type (article-file-type collection))
   (path-to-articles collection)))

(defmethod print-object((article article) stream)
  (print-unreadable-object (article stream :type t :identity t)
    (princ (id article) stream)))

(defmethod initialize-instance :after ((article article)
                                       &key &allow-other-keys)
  (update-instance-from-record article))

(defun is-author(user article)
  (member (clews::email-address user) (author article)
          :key #'clews::email-address :test #'string=))

(defmethod acl((article article))
  (if (slot-boundp article 'acl)
      (call-next-method)
      '((:view :all))))

(defmethod has-permission((action (eql :edit)) (article article)
                          &optional (user inet.acl:*current-user*))
  "Allow users with email address matching an author's to edit"
  (or (is-author user article)
      (call-next-method)
      (has-permission :admin article user)))

(defmethod has-permission((action (eql :view)) (article article)
                          &optional (user inet.acl:*current-user*))
  "Allow users with email address matching an author's to edit"
  (or (is-author user article)
      (call-next-method)
      (has-permission :admin article user) ))

(defgeneric path(instance)
  (:documentation "Return the pathname for this instance")
  (:method ((article article))
    (path-to-article (id article) (collection article))))

(defmethod record-write-date((article article))
  (file-write-date (path article)))

(defmethod last-modified((article article))
  (record-write-date article))

(defmethod (setf field-values) :around (alist (article article))
  (with-slots(errors) article
    (setf errors
          (delete-if #'(lambda(e) (typep e 'jarw.parse:invalid-field-value))
                     errors))
    (handler-bind((jarw.parse:invalid-field-value
                   #'(lambda(c)
                       (if (jarw.parse::parsed c)
                           (invoke-restart 'jarw.parse:unbind-slot)
                           (progn
                             (push c errors)
                             (invoke-restart 'jarw.parse:keep-old))))))
      (call-next-method))))

(defmacro with-article-file((character-stream octet-stream article
                                              &key direction)
                            &body body)
  (let ((garticle (gensym)))
    `(let((,garticle ,article))
      (with-lock((mutex ,garticle))
        (with-open-file(,octet-stream
                        (path ,garticle)
                        :direction ,direction
                        :if-does-not-exist :create
                        ,@(ecase direction
                             (:input '())
                             (:output '(:if-exists :supersede)))
                        :element-type 'unsigned-byte)
          (let ((,character-stream
                 (flexi-streams:make-flexi-stream
                  ,octet-stream  :external-format '(:utf-8 :eol-style :lf))))
            ,@body))))))

(defmethod update-instance-from-record((instance article))
  (with-article-file(is bs instance :direction :input)
    (setf (slot-value instance 'errors) nil)
    (setf (unparsed-values instance) nil)
    (dolist(slot (jarw.mop:class-slots (class-of instance)))
      (when (jarw.parse::slot-field-name slot)
        (slot-makunbound instance (jarw.mop::slot-definition-name slot))))
    (setf (jarw.parse:field-values instance)
          (jarw.io:read-headers :stream is :preserve-newlines-p t))
    (setf (slot-value instance 'instance-read-date)
          (file-write-date (path instance)))
    (setf (slot-value instance 'content-file-position)
          (file-position bs))
    (slot-makunbound instance 'document))
  instance)

(defmethod update-record-from-instance((instance article))
  (assert-permission :edit instance)
    (let ((text (text instance)))
      (with-article-file(os bs instance :direction :output)
        (jarw.io:write-headers
         (nconc (field-values instance)
                (unparsed-values instance))
         :stream os :preserve-newlines-p t)
        (terpri os)
        (setf (slot-value instance 'content-file-position)
              (file-position bs))
        (write-sequence text os)))
    (values))

(defmethod text((article article))
  (with-article-file(is bs article :direction :input)
    (file-position bs (content-file-position article))
    (let* ((text (make-string (- (file-length bs) (file-position bs))))
           (p (read-sequence text is)))
      (subseq text 0 p))))

(defgeneric matches(pattern article)
  (:documentation "Return true if articles matches pattern")
  (:method((regexp string) article)
    (matches (cl-ppcre::create-scanner regexp) article))
  (:method((regexp function) article)
    (cl-ppcre::scan
     regexp
     (with-article-file(is bs article :direction :input)
       (let* ((text (make-string (file-length bs)))
              (p (read-sequence text is)))
         (subseq text 0 p))))))

(defmethod (setf text)((text string) (instance article))
  (assert-permission :edit instance)
  (with-article-file(os bs instance :direction :output)
    (write-headers
         (nconc (field-values instance)
                (unparsed-values instance))
         :stream os :preserve-newlines-p t)
    (terpri os)
    (setf (slot-value instance 'content-file-position)
          (file-position bs))
    (write-sequence text os))
  (slot-makunbound instance 'document)
  text)

;; articles on a collection share reader and writer
(defmethod document-reader((article article))
  (document-reader (collection article)))

(defmethod document-writer(format (article article))
  (document-writer format (collection article)))

(defun resolve-article-reference(article node &optional (nameids #'nameids))
  (let ((collection (collection article))
        (refname (docutils:attribute node :refname)))
    (flet((resolve(article &optional id)
            (docutils:rem-attribute node :refname)
            (return-from resolve-article-reference
              (setf (docutils:attribute node :refuri)
                    (format nil "../~A/view~@[#~A~]" (id article) id)
                    (docutils:as-text (docutils:child node 0))
                    (format nil "~A" (title article) )
                    (docutils:resolved node) t))))
    (when refname
      (let ((other (get-dictionary refname collection )))
        (when other (resolve other)))
      (map-dictionary
       #'(lambda(dummy other)
           (declare (ignore dummy))
           (unless (eql other article)
             (let ((id (gethash refname (funcall nameids other))))
               (when id (resolve other id)))))
       collection)))))

(defun resolve-rfc-reference(node)
  (let ((refname (docutils:attribute node :refname)))
    (when refname
      (multiple-value-bind(start end starts ends)
          (cl-ppcre:scan "^rfc(\\d*)" refname)
        (declare (ignore end))
        (when start
          (docutils:rem-attribute node :refname)
          (setf (docutils:attribute node :refuri)
                (format nil "http://www.faqs.org/ftp/rfc/rfc~A.txt"
                        (subseq refname (elt starts 0) (elt ends 0)))
                (docutils:resolved node) t))))))

(defmethod (setf document)(document (article article))
  (setf (slot-value article 'document) document))

(defmethod (setf document) :after (document (article article))
  (setf (slot-value article 'errors)
        (nconc
         (mapcar
            #'(lambda(m)
                (make-instance 'document-error
                               :id (id article)
                               :collection (collection article)
                               :message (strcat
                                         "Document: "
                                         (docutils:as-text m))))
            (errors document))
         (delete-if #'(lambda(n) (typep n 'document-error))
                    (slot-value article 'errors)))))

(defmethod document((article article))
  (or (and (slot-boundp article 'document)
           (<= (last-modified article) (instance-read-date article))
           (slot-value article 'document))
      (let ((*search-path* (list (path-to-media article)))
            (*unknown-reference-resolvers*
             (cons #'resolve-rfc-reference
                   (cons #'(lambda(node) (resolve-article-reference article node))
                         *unknown-reference-resolvers*))))
        (setf (document article)
              (read-document article (document-reader article))))))

(jarw.debug::debug-on)
(defun bootstrap-articles(collection)
  "Efficiently bootstrap documents in articles.
It is highly recommended that this run after the collection is made."
  (let ((deferred (make-hash-table)))
    ;; read documents and keep track of unresolved targets
    ;; if no unresolved can store document in artcle
    (jarw.debug::debug-log "Bootstapping ~S ...." collection)
    (map-dictionary
     #'(lambda(id article)
         (jarw.debug::debug-log " ~A" id)
         (unless (slot-boundp article 'document)
           (let* ((unresolved nil)
                  (*unknown-reference-resolvers*
                   (cons #'resolve-rfc-reference
                         (cons
                          #'(lambda(node) (push node unresolved) t)
                          *unknown-reference-resolvers*)))
                  (*search-path* (list (path-to-media article))))
             (let ((document
                    (read-document article (document-reader article))))
             (if unresolved
                 (setf (gethash article deferred) (cons document unresolved))
                 (setf (document article) document))))))
     collection)
    ;; now try and resolve all those unresolved targets.
    (jarw.debug::debug-log " Resolving targets ...")
    (maphash
     #'(lambda(article record)
         (let* ((document (car record))
               (unresolved (cdr record))
               (nameids (slot-value article 'nameids))
               (ids (docutils::ids document))
               (refnames (docutils::refnames document))
               (refids (docutils::refids document))
               (*unknown-reference-resolvers*
                (cons #'(lambda(node)
                          (resolve-article-reference
                           article node
                           #'(lambda(a) (slot-value a 'nameids))))
                      *unknown-reference-resolvers*)))
           (jarw.debug::debug-log " ~A (~D)" (id article) (length unresolved))
           (docutils::do-transforms
               (list
                #'(lambda(document)
                    (declare (ignore document))
                    (dolist(target unresolved)
                      (unless (docutils:resolved target)
                        (docutils.transform::resolve-indirect-target
                         target nameids ids refnames refids)))))
             document)
           (setf (document article) document)))
     deferred)))
(jarw.debug::debug-off)

(defmethod read-document :around ((article article) reader)
  "Update errors and targets from a newly read document"
  (handler-bind ;; ALL errors here are terminal and are reported in document
      ((error #'(lambda(e) (docutils::report :terminal (format nil "~S" e)))))
    (let ((document (call-next-method)))
      ;; update nameids after reading document
      ;; thus read-document can be called without #'resolve-article-reference
      ;; to update nameids without infinite recursion
      (setf (slot-value  article 'nameids) (docutils:nameids document))
      document)))

(defmethod errors :before ((article article))
  ;; ensure errors from parsed document are loaded
  (unless (slot-boundp article 'document)
    (document article)))

(defmethod nameids :before ((article article))
  (unless (slot-boundp article 'nameids)
    (let* ((no-store nil)
           (*search-path* (list (path-to-media article)))
           (*unknown-reference-resolvers*
            (list #'resolve-rfc-reference
                  #'(lambda(node) (declare (ignore node)) (setf no-store t))))
           (document (read-document article (document-reader article))))
    (unless no-store
      (setf (document article) document)))))

(defmethod errors((document document))
  (let ((errors nil))
    (docutils:with-nodes(node document)
      (when (typep node 'docutils.nodes:system-message)
        (push node errors)))
    (nreverse errors)))

;; docutils reader interface for an article
(defmethod read-lines((article article))
  (read-lines (text article)))

(defmethod new-document((article article))
  ;; we reread settings only when cl-docutils.conf is modified
  (let* ((collection (collection article))
         (config-file (merge-pathnames "cl-docutils.conf"
                                       (root-path collection)))
         (config-modified (file-write-date config-file))
         (now (get-universal-time)))
  (make-instance
   'docutils.nodes:document
   :source-path (path article)
   :settings
   (if (and (slot-boundp collection 'document-settings)
            (or (not config-modified)
                (<= config-modified
                    (cdr (slot-value collection 'document-settings)))))
       (car (slot-value collection 'document-settings))
       (let ((settings (get-settings (path-to-articles collection))))
         (setf (slot-value collection 'document-settings)
               (cons settings now))
         settings)))))

;; we don't cache assessments currently - should we?
(defgeneric assessments (entity)
  (:documentation "Return the list of assessments in an entity")
  (:method ((document document))
    (let ((assessments nil))
        (docutils:with-nodes(node document)
          (when (typep node 'clews.assessment:assessment)
            (push node assessments)))
        (nreverse assessments)))
  (:method ((article article))
    (assessments (document article))))

;; output methods for article.
(defmethod markup:html(stream (article article) &optional attr content)
  (declare (ignore attr content))
  (assert-permission :view article)
  (write-sections (document-writer :html article)
                  (document article)
                  (list stream
                        'docutils.writer.html:body-pre-docinfo
                        'docutils.writer.html:docinfo
                        'docutils.writer.html:body )))

(defgeneric html-parts(article)
  (:documentation  "Return head and body parts for this article")
  (:method((article article))
    (assert-permission :view article)
    (let((head (make-string-output-stream))
         (body (make-string-output-stream)))
      (write-sections (document-writer :html article)
                      (document article)
                      `((,head docutils.writer.html:head)
                        (,body
                         docutils.writer.html:body-pre-docinfo
                         docutils.writer.html:docinfo
                         docutils.writer.html:body)))
      (values (get-output-stream-string head)
              (get-output-stream-string body)))))

(defmethod keywords ((article article) &key
                     (ignore-word-p #'jarw.search:ignore-word-p)
                     (stem #'jarw.search:stem)
                     (synonym #'jarw.search:synonym))
  (keywords (text article)
            :ignore-word-p ignore-word-p
            :stem stem
            :synonym synonym))

(defgeneric article-status(article user)
  (:documentation "Return a alist of state report
information for given user on article")
  (:method((article article) (user clews:user))
    (let ((state (user-state article user)))
    `(("Last Visited"
       ,(or (format-output '(jarw.parse:date :fmt :short)
                           (state-value :last-visit-time state))
            "Never")
       :view)
      ("Time Spent" ,(format-output 'jarw.parse:time-period
                                    (state-value :time-spent state)) :view))))
  (:method((article article) (state (eql nil)))
    "Return non user specific status information"
    `(("Last Modified" ,(format-output '(jarw.parse:date :fmt :short)
                                        (last-modified article)) :view)
      ("Created" ,(field-value "created" article) :view)
      ("Copyright" ,(field-value "copyright" article) :view)
      ,(let ((authors
              (mapcar
                   #'(lambda(a)

                       `((a :href ,(format nil "mailto:~A?subject=~A"
                                           (clews::email-address a)
                                           (uri-escape (title article))))
                         ,(clews:display-name a)))
                   (author article))))
            (list (format nil "Author~:[~;s~]" (> (length authors) 1))
                  `(markup::span ,@authors)
                  :view))
      ("Errors"
       ,(let ((n (length (errors article))))
             (if (> n 0) n nil))
       :edit))))

(defun status-line(status article &optional (user *current-user*))
  "Return list of inline elements for status line"
  (mapcan
   #'(lambda(rec)
       (when (and (second rec) (has-permission (third rec) article user))
         (list (first rec) ": " (second rec) "; ")))
   status))

(defgeneric article-list-item(article &key user prefix relative
                                      action status-line class abstract
                                      &allow-other-keys)
  (:documentation "Return a list itel element for this article")
  (:method ((article article)
            &key
            (user *current-user*)
            prefix
            (relative "./")
            (action "view")
            (status-line
             (status-line
              (append (article-status article user)
                      (article-status article nil))
              article))
            class
            (abstract (field-value "abstract" article))
            &allow-other-keys)
    `((markup:li ,@(when class (list :class class)))
      ,@(if (listp prefix) prefix (list prefix))
      ,(if (has-permission :view article)
           `((markup:a :href ,(strcat relative (uri-escape (id article))
                                      "/" action))
           ,(title article))
         (title article))
      ,(when abstract`((markup:p :class "abstract") ,abstract))
    ,@(typecase status-line
        (string
         (when (> (length status-line) 0)
            `(((p :class "status") ,@status-line))))
        (list status-line)))))

;; form handling

(defvar *form-field-width* 80 "Standard max field width for form elements")

(defun form-field-element(formatted-value attr &optional (min 1))
  (let ((rows (max min (min 20 (1+ (count #\newline formatted-value))))))
    (when (= 1 rows)
      (setf rows (ceiling (length formatted-value) *form-field-width*)))
    (if (> rows 1)
        `((markup:textarea  :rows ,rows :cols ,*form-field-width* ,@attr))
        `((markup:input  :size ,*form-field-width* ,@attr)))))

(defgeneric parsed-field-form-element(slot entity)
  (:documentation "Return the form element definition for slot in entity")
  (:method((slot jarw.parse::parsed-slot-mixin) (article article))
    (let* ((name (intern (string-upcase
                          (jarw.parse::slot-field-name slot))
                         :keyword))
           (datatype (or (jarw.parse::slot-format slot)
                         (jarw.mop::slot-definition-type slot) ))
           (value (let ((slot (jarw.mop:slot-definition-name slot)))
                    (when (slot-boundp article slot)
                      (format-output datatype
                                     (slot-value article slot)))))
           (attr `(:name ,name
                   :value ,value
                   ,@(when datatype `(:datatype ,datatype)))))
      (form-field-element value attr))))

(defgeneric parsed-fields(entity)
  (:documentation "Return a list of slots which have parsed names")
  (:method((entity jarw.parse:parsed-fields))
      (mapcan
       #'(lambda(slot)
           (when (jarw.parse::slot-field-name slot)
             (list slot)))
       (jarw.mop:class-slots (class-of entity)))))

(defgeneric article-form(article)
  (:documentation "Return the (initialised) form for given article")
  (:method ((article article))
    (flet((markup-entry(label value)
          `(tr
            ((th :class "form-label") ,label)
            ((td :class "form-value") ,value))))
    `((markup:form :method :post :enctype "multipart/form-data")
      ((p :align :right)
       ((markup:input :name "Submit Article" :type "submit"
                      :value "Update Article")))
      ((markup:table :class "form")
       ,@(mapcar
          #'(lambda(slot)
              (markup-entry
               (jarw.parse::slot-field-name slot)
               (parsed-field-form-element slot article)))
          (parsed-fields article))
       ,@(mapcar
        #'(lambda(pair)
            (let ((name (car pair)) (value (cdr pair)))
              (markup-entry
               `((markup:input
                  :name :unparsed-name
                  :size ,(max 12 (length name))
                  :value ,name))
               (form-field-element value
                                   `(:name :unparsed-value :value ,value)))))
        (unparsed-values article))
        ,(markup-entry
         `((markup:input :name :unparsed-name :size 12))
         `((markup:textarea :name :unparsed-value
            :rows 3 :cols ,*form-field-width*)))
       ,(let ((txt (text article)))
             (markup-entry
              "Body"
              (form-field-element txt `(:name :body :value ,txt) 20))))
      ((p :align :right)
       ((markup:input :name "Submit Article" :type "submit"
                      :value "Update Article")))))))

(defgeneric (setf form-data)(data article)
  (:method ((data list) (article article))
    (flet ((field(id)
             (let ((v
                    (getf data (intern (string-upcase id) :keyword)
                          :really-null)))
               (if (typep v 'condition) :really-null v)))
           (ensure-list(item) (if (listp item) item (list item))))
      (dolist(slot (parsed-fields article))
        (let ((v (field (jarw.parse::slot-field-name slot)))
              (unbound-if-nil
               (and (listp (jarw.parse::slot-format slot))
                    (getf (rest (jarw.parse::slot-format slot)) :unbound-if-nil))))
          (unless (eql v :really-null)
            (if (and (null v) unbound-if-nil)
                (slot-makunbound article (jarw.mop:slot-definition-name slot))
                (setf (slot-value article (jarw.mop:slot-definition-name slot))
                      v)))))
      (setf (jarw.parse::unparsed-values article)
            (mapcan #'(lambda(n v)
                        (when n
                          (let ((s (string-trim '(#\space #\tab) n)))
                            (when  (> (length s) 0)
                              (list (cons s v))))))
                    (ensure-list (getf data :unparsed-name))
                    (ensure-list (getf data :unparsed-value))))
      (let ((txt (field :body)))
        (unless (eql txt :really-null)
          (setf (text article)
                (ppcre:regex-replace-all
                 '(:sequence #\return #\linefeed)
                 (getf data :body)
                 "
"))))
      (slot-makunbound article 'document))))

;; it is a requirement that setting a user state dictionary value
;; persistently stores the state.

(defgeneric user-state(entity user)
  (:documentation "Return user state dictionary")
  (:method ((collection article-collection) (user user))
    (get-dictionary (clews:username user) (slot-value collection 'user-state)))
  (:method ((collection article-collection) key)
    (get-dictionary key  (slot-value collection 'user-state)))
  (:method ((article article) (user user))
    (state-value article (user-state (collection article) user)))
  (:method ((article article) collection-state)
    (get-dictionary (id article)  collection-state)))

(defgeneric state-value(key user-state)
  (:documentation "Return a keyed value from a user state")
  (:method ((article article) user-state )
    (or (get-dictionary  (id article) user-state)
        (setf (get-dictionary (id article) user-state)
              (list :time-spent 0))))
  (:method (key user-state)
    (get-dictionary key user-state)))

(defgeneric (setf state-value)(value id user-state)
  (:documentation "Store a user state value by key")
  (:method(value (id symbol) (user-state hash-table))
    (setf (gethash id user-state) value))
  (:method(value (id symbol) (user-state list))
    (assert (> (length user-state) 1))
    (loop :for a :on user-state :by #'cddr
          :when (eql (first a) id)
          :do (setf (second a) value)
              (return-from state-value value))
    (setf (cddr user-state) (nconc (list id value) (cddr user-state)))
    value)
  (:method(value id dictionary)
    (setf (get-dictionary id dictionary) value)))

(defgeneric time-required(article)
  (:documentation "Return the time in seconds required to read an article")
  (:method(article) (declare (ignore article)) 120))

(defgeneric leave-article(user-record collection)
  (:documentation "Record the leaving of an article by a user")
  (:method(user (article article))
    (leave-article user (collection article)))
  (:method((user user) (collection article-collection))
    (let* ((collection-state (user-state collection user))
           (last-article-id (state-value :last-article-id collection-state)))
      (when last-article-id
        (let ((last-article (get-dictionary last-article-id collection)))
          (when last-article
            (let ((now (get-universal-time))
                  (article-state (user-state last-article collection-state)))
              (incf (state-value :time-spent article-state)
                    (min
                     (- now
                        (or (state-value :last-visit-time article-state) 0))
                     (or (time-required last-article) 120)))))))
      (setf (state-value :last-article-id collection-state) nil))))

(defgeneric visit-article(user article)
  (:documentation "Record a visit to an article")
  (:method((user user) (article article))
    (let* ((state (user-state (collection article) user)))
      (setf (state-value :last-article-id state) (id article)
            (state-value :last-visit-time (user-state article state))
            (get-universal-time)))))

(defmethod path-to-media((article article))
  (ensure-directories-exist
   (subdir (root-path (collection article)) (list "media" (id article)))))

(defmethod media-list((article article))
  (mapcar
   #'(lambda(path)
       (format nil "~A.~A" (pathname-name path) (pathname-type path)))
   (directory (merge-pathnames "*.*" (path-to-media article)))))

(defmethod media-size((article article))
  (reduce #'+
          (directory (merge-pathnames "*.*" (path-to-media article)))
          :initial-value 0
          :key #'(lambda(p)
                   (with-open-file(os p :direction :input)
                     (file-length os)))))

(defmethod (setf media)(data (name string) (article article))
  (with-open-file(os (merge-pathnames name (path-to-media article))
                     :direction :output
                     :if-exists :overwrite
                     :if-does-not-exist :create
                     :element-type 'unsigned-byte)
    (write-sequence  data os)))

(defmethod rem-media((name string) (article article))
  (delete-file (merge-pathnames name (path-to-media article))))
