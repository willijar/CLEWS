; $Id: application.lisp,v 1.1 2007/07/26 08:53:41 willijar Exp willijar $
;; Article CLEWS Application Base Class
;; Copyright (C) 2006 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of the CLEWS Article Application Library

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;;

;;; Code:

(in-package :clews.articles)

(defvar *user-source* nil "User source for current application")

(defclass clews-articles(application)
  ((collection
    :type article-collection :reader collection :initarg :articles
    :documentation "The article collection managed by this application")
   (index :type keyword-index
          :initform (make-instance 'keyword-index)
          :reader keyword-index
          :documentation "maps keywords to concepts"))
  (:default-initargs :id :clews-articles)
  (:documentation "Articles Application Base Class"))

;;; action structure represents an action (new url) or a section on a page.

(defstruct action
  (name "" :type string) ; identifying name
  (permission :admin :type symbol) ; what permission is required
  (handler nil)  ; handler takes entity and request, returns body and headers
  (label "" :type string) ; label to be used
  (description "" :type string)) ; textual description

(defmethod has-permission((action action) entity
                          &optional (user *current-user*))
  (has-permission (action-permission action) entity user))

(declaim (inline filter-actions))
(defun filter-actions(actions entity &optional (user *current-user*))
  (if user
      (mapcan
       #'(lambda(a) (when (has-permission a entity user) (list a)))
       actions)
      actions))

(defgeneric actions(entity)
  (:documentation "return a list of a actions (pages) for given
article or collection of articles.")
  (:method :around (entity) (filter-actions (call-next-method) entity))
  (:method ((app clews-articles)) (actions (collection app))))

(defgeneric sections(entity)
  (:documentation "return a list of sections to display on main page.")
  (:method :around (entity) (filter-actions (call-next-method) entity))
  (:method ((app clews-articles)) (sections (collection app))))

(defmethod published-methods ((app clews-articles) baseurl)
  (declare (ignore baseurl))
  (nconc
   (call-next-method)
   (mapcar
    #'(lambda(action)
        (list (action-name action)
              #'(lambda(app request rest)
                  (declare (ignore rest))
                  (response-handler app request action))
              :stage :response
              :match :prefix
              :display-plugins-p t
              :role  (action-permission action)))
    (actions app))))

(defgeneric menus(app &optional article help)
  (:documentation "Return the menus data for current user. If
article return article menus on second row")
  (:method(app &optional article help)
    `((,@(mapcan #'(lambda(action)
                     (when (and (has-permission action app)
                                (> (length (action-label action)) 0))
                       (list
                        (list (action-name action) (action-label action)))))
                     (actions app))
       ,@(when help
               `((,(format nil "help-~A~:[~;/~]view"  help article) "Help")))
       ("preferences/" "Preferences"))
      ,@(when article
              (list
               (mapcan #'(lambda(action)
                           (when (and (has-permission action article)
                                      (> (length (action-label action)) 0))
                             (list
                             (list (format nil "~A/~A"
                                           (inet.uri:uri-escape (id article))
                                           (action-name action))
                                   (action-label action )))))
                       (actions article)))))))

(defmethod response-handler((app clews-articles) request (action action))
  "Response for collection actions"
  (response-handler (collection app) request action))

(defun get-style(collection)
  (let ((style  (user-preference :style collection *current-user* nil)))
    (when style
      `(((markup::style :escape nil :type "text/css")
        "<!--" #\newline ,style "-->")))))

(defmethod response-handler((articles article-collection)
                            request (action action))
  "Response for collection actions"
  (leave-article *current-user* articles)
  (multiple-value-bind(body head)
      (funcall (action-handler action) articles request)
    `(markup:html
      (markup:head
       (markup:title
        ,(string (clews::id articles)) ": "
        ,(action-label action))
       ,@head
       ,@(get-style articles))
      (markup::body
       ,(markup:navbar (menus articles nil (action-name action))
                       :relative "../"
                       :on-url (action-name action) )
       ((markup:section
          :id ,(action-name action)
          ,@(when (action-label action)
                  `(:title ,(action-description action))))
        ,@body)))))

(defmethod response-handler((article article) request (action action))
  "Response for an article action"
  (multiple-value-bind(body head)
      (funcall (action-handler action) article request)
    `(markup:html
      (markup:head
       (markup:title ,(string (clews::id (collection article))) ": "
        ,(title article) ": " ,(action-label action))
       ,@head
       ,@(get-style (collection article)))
      (markup::body
       ,(markup:navbar (menus (collection article) article (action-name action))
        :relative "../"
        :on-url (format nil "~A/~A" (uri-escape (id article))
                        (action-name action)))
       ,@body))))

(defmethod response-handler((app clews-articles) request path)
  "Default response - an article path."
  (let* ((parts (split-string path 2 #(#\/)))
         (articleid (first parts))
         (action (or (second parts) "view"))
         (user *current-user*)
         (*user-source* (clews:users app)))
    (with-delayed-updates((user-state (collection app) user) :deferred)
      (leave-article user (collection app))
      (when (= 0 (length articleid))
        (inet.http:redirect request "./home/"))
      (let* ((article (get-dictionary articleid (collection app))))
        (unless article (return-from response-handler :not-found))
        (let ((action
               (find action (actions article)
                     :key #'action-name
                     :test #'string-equal)))
          (unless action (return-from response-handler :not-found))
          (unless (has-permission action article)
            (return-from response-handler :forbidden))
          (prog1
              (response-handler article request action)
            (setf (get-dictionary (inet.acl:username user) (clews:users app))
                  user)))))))

(defgeneric collection-home(collection request)
  (:documentation"Handler for home page of application")
  (:method ((articles article-collection) request)
    (mapcar
     #'(lambda(action)
         (let ((name (action-name action))
               (label (action-label action))
               (handler (action-handler action)))
           `((markup:section :id ,name :escape nil
              ,@(when (and label (> (length label) 0))
                      `(:title ,label)))
             ,@(funcall handler articles request))))
     (sections articles))))

(defgeneric collection-directory(collection request)
  (:method ((collection article-collection) request)
    (let ((prefix (or (first (query-values "prefix" request)) "A"))
          (letters "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
          (results nil))
      (map-dictionary
       (if (string-equal prefix "*")
           #'(lambda(id article)
               (declare (ignore id))
               (when (not (find (char (title article) 0) letters
                                :test #'char-equal))
                 (push article results)))
           #'(lambda(id article)
               (declare (ignore id))
               (when (string-equal prefix (title article) :end2 (length prefix))
                 (push article results))))
       collection)
      `((markup:p "There are " ,(length results)
         " articles prefixed with \"" ,prefix
         "\" out of a total of " ,(dictionary-count collection)
         " articles.")
        (markup:p
         ,@(map 'list
                #'(lambda(c) `((a :href ,(format nil "?prefix=~C" c)) ,(string c) " "))
                letters)
         ((markup:a :href "*" :title "Others") "Others") )
        ((markup:ul :escape nil)
         ,@(mapcar
            #'(lambda(article)
                (article-list-item article :relative "../" :action "view"))
            (sort results #'string-lessp :key #'title)))))))

(defgeneric collection-search(collection request)
  (:method((collection article-collection) request)
    (let ((search-term (car (inet.http:form-values  "search-term" request))))
      `(((markup:form :method "POST" :enctype "multipart/form-data")
         (markup:p "Enter Regular Expression Search Pattern: ")
         (markup:p
          ((markup:input :type "text" :size 40 :name "search-term"
                         :value ,(or search-term "")))
          ((markup:input :type "submit" :name "Search"
                          :value "Search" ))))
        (hr)
        ,@(when search-term
           (let ((results(search-dictionary search-term collection)))
                  `((markup:p (markup:em ,(length results) " matches."))
                    ((markup:ol :class "condensed")
            ,@(mapcar
                #'(lambda(article)
                    (article-list-item
                     article
                     :abstract nil
                     :relative "../" :action "view"))
                results)))))))))

(defgeneric collection-new-article(collection request)
  (:method ((collection article-collection) request)
    (let ((form
           `((markup:form :method :post)
             (markup:p "Enter id for new article: "
              ((markup:input :name :id
                             :datatype
                             ,#'(lambda(id)
                                  (when (get-article (parse-input 'filename id)
                                                     collection
                                                     :if-does-not-exist nil)
                                    (invalid-input id
                                                   "An article with this id already exists -
choose another")) id)))
              ((markup:input :name :new :type :submit
                             :value "Create New Article"))))))
      (if (clews.form::submitted-action form request)
          (let ((id  (getf (form-data form request) :id)))
            (if (stringp id)
                (inet.http:redirect
                 request
                 (format nil "../~A/edit"
                         (uri-escape (id (get-article id collection
                                                      :if-does-not-exist :create)))))
                (list (markup-form form request))))
          (list form)))))

(defgeneric collection-error-list(collection request)
  (:method ((collection article-collection) request)
    `((p "The following articles for which you are an author have errors")
      ((markup:ol :class "condensed")
       ,@(mapcar
          #'(lambda(article)
              (article-list-item
               article
               :abstract nil
               :relative "../"
               :action "edit"
               :status-line
               `((markup:ul
                  ,@(mapcar
                     #'(lambda(e) `(markup:li ,(format nil "~A" e)))
                     (errors article))))))
          (search-dictionary
           #'(lambda(article)
               (and (errors article) (is-author *current-user* article)))
           collection))))))

(defgeneric article-media(article request)
  (:documentation "Return the article media management page")
  (:method ((article article) request)
    (let ((name (first (query-values "name" request)))
          (action (first (query-values "action" request)))
          (file  (first (clews.form::form-values "file" request)))
          (msg nil))
      (flet((action-uri(name action)
              (format nil "?name=~A&action=~A"
                      (uri-escape name) action)))
        (cond
          (file
           (let ((name (parse-input 'filename (car file)
                                    :if-invalid :replace)))
             (setf (media name article) (cdr file))
             (setf msg (format nil "Media ~S uploaded." (car file)))))
          (name
           (cond
             ((equal action "delete")
              (rem-media name article)
              (setf msg (format nil "Media ~S deleted" name)))
             (t ;; default is view article
              (throw 'inet.http:response
                (merge-pathnames name (path-to-media article)))))))
        `((markup:h1 "Managing Media for \"" ,(title article) "\"")
          (p ,(length (media-list article))
           " files occupying " ,(ceiling (/ (media-size article) 1024)) " KB")
          ((markup:form :method :post :enctype "multipart/form-data")
           (markup:p "Select File to Upload "
            ((markup:input :type "file" :name "file" :size 60))
            ((markup:input :type "submit" :name "Upload" :value "Upload"))))
          ((markup:table :width "50%" :border 1)
           (markup:tr ((markup:th :width "40%") "Name")
            ((markup:th :width "40%") "Last Modified") (markup:th))
           ,@(mapcar
              #'(lambda(name)
                  `(markup:tr
                    (markup:td
                     ((markup:a :href ,(action-uri name "view")) ,name))
                    (markup:td
                     ,(format-output
                       'jarw.parse:date
                       (file-write-date
                        (merge-pathnames name
                                         (path-to-media article)))))
                    (markup:td
                     ((markup:a :href  ,(action-uri name "delete"))
                      ((markup:img :src "/static/delete.gif"
                                   :alt "Delete"))))))
              (media-list article))))))))

(defvar *article-head* nil "Current article header")
(defvar *article-body* nil "Current article header")

(defgeneric article-view(article request)
  (:documentation "Return the main article view page by pulling together the
various article view sections together with a navigation bar.")
  (:method ((article article) request)
    (visit-article *current-user* article)
    (multiple-value-bind(*article-head* *article-body*) (html-parts article)
      (values
       (mapcar
        #'(lambda(action)
            (let* ((name (action-name action))
                   (label (action-label action))
                   (handler (action-handler action))
                   (section (funcall handler article request)))
              (when section
                `((div :id ,name :escape nil)
                  ,@(when (and label (> (length label) 0))
                          `(((div :class "title") ,label)))
                  ,@section))))
        (sections article))
       (head-section article request)))))

(defgeneric welcome-section(article request)
  (:documentation "Welcome for current user on article")
  (:method :around (entity request)
           `((dl
              ,@(mapcan
                 #'(lambda(rec)
                     (when (and (second rec)
                                (has-permission (third rec) entity))
                       `((dt ,(first rec)) (dd ,(second rec)))))
                 (call-next-method)))))
  (:method (entity request )
    `(("Welcome"  (b ,(clews:display-name *current-user*)) :view))))

(defgeneric title-section(article request)
  (:documentation "Welcome for current user on article")
  (:method (article request)
    `((h1 ,(title article)))))

(defgeneric article-status-section(article request)
  (:documentation "Return markup for the article status section - Main
method should return a list of a text (user) description, the value
and the role allowed to see it")
  (:method :around ((article article) request)
           `((dl
              ,@(mapcan
                 #'(lambda(rec)
                     (when (and (second rec)
                                (has-permission (third rec) article))
                       `((dt ,(first rec)) (dd ,(second rec)))))
                 (call-next-method)))))
  (:method ((article article) request )
    (declare (ignore request))
    (append (article-status article *current-user*)
            (article-status article nil))))

(defgeneric article-error-section(article request)
  (:documentation "Return a list of errors in this article")
  (:method((article article) request)
    (declare (ignore request))
    (when (errors article)
    `(((ul :class "error")
      ,@(mapcar
         #'(lambda(e) `(markup:li ,(format nil "~A" e)))
         (errors article)))))))

(defvar *form* nil)
(defvar *request* nil)
(defgeneric article-edit(article request)
  (:documentation "Edit interface for an article")
  (:method ((article article) request)
    (let ((form (article-form article)))
      (setf *form* form *request* request)
      (if (clews.form::submitted-action form request)
          (multiple-value-bind(data condition)
              (form-data form request)
            (setf (form-data article) data)
            (update-record-from-instance article)
            (setf form (article-form article))
            (do ()
                ((not (remf data :unparsed-name )))
              (remf data :unparsed-value ))
          `((markup:h1 ,(title article))
            ,@(when condition
                    '(((p :class error)
                       "Some fields are in error and have not been
updated. Please ammend and resubmit.")))
            ,@(when data `((p (em "Valid updates Stored"))))
            ,@(article-error-section article request)
            ,(markup-form form data)))
          `((markup:h1  ,(title article))
            ,@(article-error-section article request)
            ,form)))))

(defgeneric article-delete(article request)
  (:documentation "Edit interface for an article")
  (:method ((article article) request)
    (let ((form
           `((markup:form :method :post)
             (markup:h1 "Delete " ,(title article))
             (markup:p "Are you sure you want to delete article \""
              ,(title article) "\" "
              ((markup:input :type "submit" :name :delete :value "Yes"))
              ((markup:input :type "submit" :name :delete :value "No"))))))
      (let ((choice (car (inet.http::form-values :delete request))))
        (list
         (cond
           ((not choice) form)
           ((equal choice "Yes")
            (prog1
                `(p "Article \"" ,(title article) "\" is being deleted.")
              (rem-dictionary (id article) (collection article))))
           (t (inet.http::redirect  request "../"))))))))


(cl-ppcre::define-parse-tree-synonym role
    (:sequence skip-spaces (:register (:alternation token quoted-string))))

(defmethod parse-input((spec (eql 'roles)) (value string)
                       &key &allow-other-keys)
  (let ((roles nil))
    (cl-ppcre:do-register-groups(role) ('role value)
      (push
       (if (eql (elt role 0) #\")
           (unquoted role)
           (intern (string-upcase role) :keyword))
      roles))
    (nreverse roles)))

(defmethod format-output((spec (eql 'roles)) (value list)
                         &key &allow-other-keys)
  (format nil "~{~S ~}" value))

(defun group-select(collection request)
  "Return the form and list of users belonging to a specific group"
  (let* ((default
             (get-dictionary :group-selection
                             (user-state collection *current-user*)))
         (form
          `((markup:form :method "POST")
            (markup:p "Enter user names or groups: "
             ((markup:input :name :groups
                     :datatype roles
                     :value ,default))
             ((markup:input :name :submit :type :submit :value "Report")))))
         (data (form-data form request))
         (groups (getf data :groups default))
         (users
          (sort
           (delete-duplicates
            (nconc
             (mapcan
              #'(lambda(group)
                  (when (stringp group)
                    (when-bind(u (get-dictionary group *user-source*))
                      (list u))))
              (or groups default))
             (mapcar #'(lambda(uname) (get-dictionary uname *user-source*))
             (clews:get-users
              (mapcan #'(lambda(group) (when (symbolp group) (list group)))
                      (or groups default))
              *user-source*))))
           #'string<
           :key #'clews:username)))
    (when groups
      (setf (get-dictionary :group-selection
                          (user-state collection *current-user*))
            groups))
    (values users (markup-form form (when (getf data :groups) data)))))


(defun report-table(columns rows rowfunc &optional sort)
  "Construct a row table using columns (each column may have a column
name, a format for values, an aggregate function and a sort
function. Row values are determnined by calling rowfun on rows"
  (let* ((rows (mapcar rowfunc rows))
         (n -1))
    (multiple-value-bind(invert sortcol)
        (when sort
          (values (eql (char sort 0) #\-)
                  (abs (or (parse-integer sort :junk-allowed t) 0))))
    (when sortcol
      (when (and (< sortcol (length columns)) (fourth (elt columns sortcol)))
        (setf rows (sort rows (fourth (elt columns sortcol))
                         :key #'(lambda(row) (elt row sortcol))))
        (when invert (setf rows (nreverse rows)))))
    `((markup:table :class "report")
      ((markup:tr :class "header")
       ,@(let ((col -1))
          (mapcar #'(lambda(h)
                      (incf col)
                      (let ((v (if (listp h) (first h) h)))
                       (if (fourth h)
                            `(markup::th
                              ((markup::a
                                :href
                                ,(format nil "?sort=~:[~;-~]~D"
                                         (and (eql col sortcol) (not invert))
                                         col))
                               ,v))
                            `(markup::th ,v))))
                  columns)))
      ,@(mapcar
         #'(lambda(row)
             `((markup::tr :class ,(if (= (mod (incf n) 2) 0)
                                       "odd" "even"))
               ,@(mapcar
                  #'(lambda(columnspec value)
                      (let ((format (second columnspec)))
                        `(markup::td ,(format-output format value))))
                  columns row)))
         rows)
      ,@(when (and rows (some #'third columns))
        `(((markup:tr :class "footer")
          ,@(mapcar
             #'(lambda(column)
                 (let ((aggregate (third column)))
                   (prog1
                    `(td
                      ,@(when aggregate
                         (list
                         (format-output (second column)
                            (funcall aggregate (mapcar #'car rows))))))
                     (setf rows (mapcar #'cdr rows)))))
             columns))))))))

(defun users-report(columns users rowfunc &optional sort)
  (report-table
   (nconc (list (list "Name" nil nil #'string-lessp)
                (list "Username" nil nil #'string-lessp))
          columns)
   users
   #'(lambda(user)
       (nconc (list (clews:display-name user)
                    (clews:username user))
              (funcall rowfunc user)))
   sort))

(defun mean-with-nil(values)
  (jarw.math:mean (mapcan #'(lambda(v) (when v (list v))) values)))

(defun <-with-nil(a b)
  (if (numberp a)
      (if (numberp b) (< a b) t)
      nil))

(defgeneric visits-report(article request)
  (:documentation "Show basic per user report for article")
  (:method ((article article) request)
    (let ((sort (car (query-values "sort" request))))
    (multiple-value-bind(users form)
        (group-select (collection article) request)
      (list
       form
       '(markup::hr)
       (users-report
        (list
         (list "Time Spent" 'jarw.parse:time-period
               #'mean-with-nil #'<-with-nil)
         (list "Last Visited" '(jarw.parse:date :fmt :short :if-nil "Never")
               #'mean-with-nil #'<-with-nil))
        users
        #'(lambda(user)
            (let ((state (user-state article user)))
              (list
               (state-value :time-spent state)
               (state-value :last-visit-time state))))
        sort))))))


(defmethod actions((articles article-collection))
  (load-time-value
   (list
    (make-action
     :name "home/" :permission :view :handler #'collection-home
      :description "Articles Home Page" :label "Home")
    (make-action
     :name "directory/" :permission :view :handler #'collection-directory
     :label "Directory" :description "Directory of Articles")
    (make-action
     :name "search/" :permission :view :handler #'collection-search
     :label "Search" :description "Search Articles")
    (make-action
     :name "new/" :permission :edit :handler #'collection-new-article
     :label "New Article" :description "Add a new article")
    (make-action
     :name "errors/" :permission :edit :handler #'collection-error-list
     :label "Errors" :description "Articles with errors"))))

(defmethod actions((article article))
  (list
   (make-action
      :name "view" :permission :view :handler #'article-view
      :label "View" :description "View this article")
   (make-action
    :name "edit" :permission :edit :handler #'article-edit
    :label "Edit" :description "Edit this article")
   (make-action
    :name "media" :permission :edit :handler #'article-media
    :label "Manage Media" :description "Manage Media")
   (make-action
    :name "delete" :permission :edit :handler #'article-delete
    :label "Delete" :description "Delete this article")
   (make-action
       :name "visits" :permission :edit :handler #'visits-report
       :label "Visits Report"
       :description "Report on visits to this article")))

(defgeneric new-articles-section(articles request)
  (:method ((articles article-collection) request)
    (let* ((state (user-state articles *current-user*))
           (new-articles
            (dictionary:search-dictionary
            #'(lambda(article)
                (let ((visited
                       (state-value :last-visit-time
                                    (user-state article state))))
                  (or (not visited) (< visited (record-write-date article)))))
            articles)))
      `((markup:ul
         ,@(mapcar
            #'article-list-item
            (sort new-articles #'< :key #'record-write-date)))))))

(defmethod sections((articles article-collection))
  (list
   (make-action
      :name "welcome" :permission :view
      :handler #'welcome-section)
   #+nil(make-action
    :name "new" :permission :view :handler #'new-articles-section
    :label "New Articles")))

(defgeneric body-section(article request)
  (:documentation "Present the article body. *article-body* will be
bound to the parsed body for the article.")
  (:method (article request)
    (declare (ignore article request))
    (list *article-body*)))

(defgeneric head-section(article request)
  (:documentation "Present the article head. *article-head* will be
bound to the parsed head for the article.")
  (:method (article request)
    (declare (ignore article request))
    (list *article-head*)))

(defmethod sections((article article))
  (list
   (make-action
      :name "welcome" :permission :view
      :handler #'welcome-section)
   (make-action
      :name "status" :permission :view
      :handler #'article-status-section
      :description "View article status")
   (make-action
       :name "title" :permission :view :handler #'title-section
       :description "View article title")
   (make-action
       :name "body" :permission :view :handler #'body-section
       :description "View article body")
   (make-action
       :name "error" :permission :edit
       :handler #'article-error-section :label "Errors"
       :description "View errors in article")))

(defmethod components((app clews-articles))
  (list app (collection app)))

(defmethod user-component-preferences((self article-collection) user)
  (declare (ignore user))
  '((:style
     :text "Enter CSS to customise the presentation of this web
application. Examine the html to determine how to identify different
elements for selectors."
     :markup ((markup:textarea :cols 80 :rows 10))
     :type string
     :default "")
    (:deadline
     :text "Enter how many days warning you want for imminent deadlines."
     :markup ((markup:input))
     :default 14
     :type (integer :min 0 :max 365))))

(defmethod clews::preference-handler ((app clews-articles) request rest)
  (let* ((user *current-user*)
         (preference-pages
          (mapcan #'(lambda(item)
                      (let ((form (user-component-preferences item user)))
                        (when form (list (cons item form)))))
                  (components app)))
         (this-page
          (or
           (find rest preference-pages
                 :test #'string-equal
                 :key #'(lambda(p) (string (clews:id (car p)))))
           (first preference-pages)
           (return-from clews::preference-handler
             `(html
               (head (title "Preferences"))
               (body
                (p "No user preferences available for this application"))))))
         (form-elements (cdr this-page))
         (component (car this-page))
         (form (clews.form:make-form form-elements))
         (data (when (clews.form:submitted-action form request)
                 (clews.form:form-data form request)))
         (user-properties (user-component-properties component user)))
    (flet((title(c) (string-capitalize (clews:id c))))
      (loop for item on data by #'cddr ; map form data into user properties
            do (unless (or (eq (first item) :form)
                           (typep (second item) 'condition))
                 (setf (jarw.properties:property user-properties (first item)) (second item))))
      `(markup:html
        (markup:head (title "Preferences for " ,(title component)))
        ,@(get-style (collection app))
        (markup:body
         ,(markup:navbar
           (list (cons '("../" "Home")
                       (mapcar #'(lambda(p) (list (string-downcase (clews:id (car p)))
                                                  (title (car p))))
                               preference-pages)))
           :on-url rest)
         (markup:h1 "Preferences for " ,(title component))
         ,@(when data ;; store data
                 (setf (get-dictionary (clews:username user) (users app)) user)
                 '((markup:p (markup:em "You preferences have been updated"))))
         ,(clews.form:markup-form
           form
           (or data
               (mapcan #'(lambda(element) ;extract user properties
                           (let ((id (first element)))
                             (multiple-value-bind (v f-p)
                                 (jarw.properties:property user-properties id)
                               (when f-p (list id v)))))
                       form-elements)) ))))))

#|







(defconstant +week-interval+ 604800)
(defconstant +day-interval+ 86400)


|#

