;;;; CLEWS:  Peer Review Application component
;;;; Copyright (C) 2002-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; $Id: peer-review-handlers.lisp,v 1.2 2006/08/23 12:47:24 willijar Exp willijar $

(in-package :clews.peer-review)

(defmethod render-page ((app peer-review) stream markup)
  (with-markup-environment
      (:destination-path (image-directory app)
                         :math-cache (math-cache app)
                         :file-prefix "images/")
    (call-next-method app stream markup) ))

(defmethod directory-handler((app peer-review) request letter)
  (declare (ignore request))
  (if (= 0 (length letter)) (setq letter #\A) (setq letter (elt letter 0)))
  (let ((letters "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
    `(html (head (markup::title "List of Articles"))
      (body
       (navbar ,(menus app) :on-url "index" :relative "../")
       (h1 "All articles")
       (p "There are " ,(dictionary-count (articles app))
        " articles in total.")
       (p ,@(map 'list #'(lambda(c)
                           `((a :href ,(string c)) ,(string c) " "))
                 letters)
        ((a :href "*" :title "Others") "Others") )
       (ol ,@(mapcar #'(lambda(r)
                         `(li ((a :href
                                ,(concatenate 'string "../" (id r)))
                               ,(title r))))
                     (sort
                      (mapcan
                       (if (eq letter #\*)
                           #'(lambda(r)
                               (when (not
                                      (find (elt (title r) 0)
                                            letters :test #'char-equal))
                                 (list r)))
                           #'(lambda(r)
                               (when (char-equal letter (elt (title r) 0))
                                 (list r))))
                       (article-index app))
                      #'string-lessp :key #'title))) ))))

(defun add-hidden-element(form name value &optional (type 'string))
  (funcall
   (markup-transformation(tag attr content)
     (form (join-markup tag attr
                        (cons
                         `((input :type :hidden :name ,name :value ,value
                            :datatype ,type))
                         content))))
   form))

(defmethod response-handler((app peer-review) request rest)
  (when (or (= (length rest) 0) (string-equal rest ".")) ; Netscape 4 does this
    (return-from response-handler (home-handler app request rest)))
  (let* ((id (let ((p (position #\? rest)))
               (if p (subseq rest 0 p) rest)))
         (article (get-article id app))
         (user (remote-user request))
         (usernav
          `(,@(when (can-edit-article app article user)
                    `((,(concatenate 'string "edit/" id)
                       " Edit/Update article")
                      (,(concatenate 'string "edit/images/" id)
                       " Manage Article Images")))
            ,@(when (can-review-article app article user)
                    `((,(concatenate 'string "review/" id)
                        " Review Article")))
            ,@(when (can-delete-article app article user)
                `((,(concatenate 'string "delete/" id)
                   " Delete Article"))))))
    (unless article
      (return-from response-handler
        (cons :not-found (format nil "No article named ~S." id))))
    `(html
      (head (markup:title ,(title article))
       ,@(let ((refresh (user-preference :refresh app user nil)))
              (when refresh
                `(((meta :HTTP-EQUIV "refresh" :CONTENT ,refresh))))))
      (body
       (navbar ,(append (menus app) (when usernav (list usernav))) :on-url ,id)
       ((section :title  ,(title article))
        ,@(article-body-html app id
                             :is-tutor (has-permission :tutor app user))
        ,@(when usernav `((navbar (,usernav) :on-url ,id)))
        ((p :id "related-nodes")
         (b "Related Nodes: ")
         ,@(let ((threshold (user-preference
                             :related-threshold app user 0.1))
                 (this (get-index id app)))
                (mapcan
                 #'(lambda(entry)
                     (when (and (not (eql this (car entry)))
                                (> (cdr entry) threshold))
                       `(((a :href ,(id (car entry))) ,(title (car entry)))
                         ,(format nil " (~3,0F) " (* 100 (cdr entry))))))
                 (related-articles app article))))
        (hr)
        ,(multiple-value-bind(can-view-p reason)
             (can-view-reviews app article user)
           `((section :title "Reviews")
             ,@(if can-view-p
                 (or (mapcar
                      #'(lambda(r)
                          (let ((review-form (find-form r)))
                            (list
                             '(div :class "review")
                             (markup-form review-form r :text)
                             (author-html
                              r
                              :is-tutor (has-permission :tutor app user))
                             (when (has-permission :tutor app user)
                               `(p (b "Mark: " ,(round (form-mark r review-form)))))
                             (when (can-delete-review app article user
                                                      (author r))
                               `(p ((a :href
                                     ,(concatenate 'string
                                                   "delete-review/"
                                                   id "/" (author r)))
                                    "Delete this review")))
                             (let ((data (review-feedback r)))
                               (when (or data
                                         (and
                                          (can-provide-review-feedback
                                           app article user r)
                                          (review-feedback-form app)))
                                 (do-form
                                     data
                                   (add-hidden-element
                                    (if data
                                        (find-form (review-feedback r))
                                        (review-feedback-form app))
                                    :review-author (author r))
                                   (unless (and (not data)
                                                (can-provide-review-feedback
                                                 app article user r)) :text)
                                   #'(lambda(data)
                                       (when (equal (getf data :review-author)
                                                    (author r))
                                         (remf data :review-author)
                                         (setf (getf r 'review-feedback)
                                               data)
                                         (setf (reviews article)
                                               (mapcar
                                                #'(lambda(r2)
                                                    (if (eql (author r2)
                                                             (author r))
                                                        r
                                                        r2))
                                                (reviews article)))
                                         (reindex-article app id article)))
                                   request)))
                             '(hr))))
                      (reviews article))
                     '((p "There are no reviews of this article as yet.")) )
                 `((p ,reason ))))))))))

(defmethod help-handler((app peer-review) request rest)
  (declare (ignore request rest))
  `(html
    (head (markup:title "Peer Review Help Articles"))
    (body
     (navbar ,(menus app) :on-url "help")
     (p "Please read all of these articles which relate to how to use this
peer review system and to how it is assessed.")
     (ul
      ,@(mapcar
         #'(lambda(id)  `(li ((a :href ,id) ,(title (get-article id app)))))
         (sort
          (mapcan #'(lambda(id) (when (string= id "help" :end1 4) (list id)))
                  (article-ids app))
          #'string<))))))

(defmethod home-handler((app peer-review) request rest)
  (declare (ignore rest))
  (let* ((idx (article-index app))
         (user (remote-user request))
         (username (username user)))
    (flet ((make-links(idx &key (count 12) extra prefix suffix)
             (mapcan
              #'(lambda(entry)
                  (nconc
                   (list
                    `((a :href ,(concatenate
                                 'string prefix
                                 (slot-value entry 'id)
                                 suffix))
                      ,(slot-value entry 'title)))
                   (when extra
                     (list " (" (funcall extra entry) ")"))
                   (list " | ")))
              (subseq idx 0 count)))
           (entry-mark(e) (format nil "~D" (round (mark e))))
           (entry-date(e) (format-time nil  (created e) :fmt :date-only)))
      (multiple-value-bind (mark no-articles no-reviews average-article-mark
                                 average-review-mark)
          (contribution-statistics app username)
        (declare (ignore mark average-article-mark average-review-mark))
        `(html
          (head (markup:title "Peer Review Assessment")
           ,@(let ((refresh (user-preference :refresh app user nil)))
                  (when refresh
                    `(((meta :HTTP-EQUIV "refresh" :CONTENT ,refresh))))))
          (body
           (navbar ,(append (menus app)
                            (when (has-permission :tutor app user)
                              '((("marks/" "Marks")
                                 ("dregs" "Dregs")
                                 ("all" "All Articles")))))
            :on-url "/")
           ((section :title "Peer Review Assessment")
            ((section :title "Articles Requiring Reviews")
             (p ,@(make-links (articles-requiring-review idx username)
                              :prefix "review/")))
            ((section :title "Articles Requiring Writeup")
             (p ,@(multiple-value-bind (can-add-p reason)
                    (can-add-article app user
                                     :no-articles no-articles
                                     :no-reviews no-reviews)
                    (if can-add-p
                        (make-links (articles-requiring-writeup idx))
                        (list reason)))))
            ((section :title "Best Articles")
             (p ,@(make-links (best-articles idx) :extra #'entry-mark)))
            ((section :title "Your Articles")
             (p ,@(or (make-links
                       (sort (articles-authored-by idx username)
                             #'> :key #'created)
                       :count nil
                       :extra #'(lambda(idx)
                                  (format nil "~:[~;*, ~]~A"
                                          (needs-review-feedback idx)
                                          (entry-date idx))))
                      (list "None")))
             (p "(*) Review feedback required. You must provide feedback on all reviews before you can add a new article"))
            ((section :title "Newest Articles")
             (p ,@(make-links (newest-articles idx)
                              :extra #'entry-date)) ))))))))

(defmethod new-article-handler((app peer-review) request rest)
  (declare (ignore rest))
  (let* ((form '((form :method "post")
                 (p (b "Title: ")
                  ((input :datatype (string :min-length 3 :word-count 1)
                          :type "text" :size 40 :name :title))
                  ((input :type "submit" :name :submit
                          :value "Create New Article" )))))
         (title (or (getf (form-data form request) :title)
                    (car (query-values "title" request))))
         (user (remote-user request))
         (username (username user)))
    (multiple-value-bind (can-add-p reason)
        (can-add-article app user :title title)
      (if (and (stringp title)  can-add-p)
          (let ((new-id (generate-new-article-id app)))
            (add-article app new-id :title title :author username)
            (redirect request (concatenate 'string "edit/" new-id)))
          `(html
            (head (markup:title "New Article"))
            (body
             (navbar ,(menus app) :on-url "new")
             (h1  "New Article")
             ,@(if (not can-add-p)
                   `((p ,reason))
                   `((p "Please enter the title for the new article
below. If you are allowed to add this article you will then be taken
to a form where you can edit it. It does not count as your article
until you have submitted some content")
                     ,(markup-form
                       form
                       (when (submitted-action form request)
                         request))))))))))

(defmethod edit-article-handler((app peer-review) request id)
  (let* ((article (get-article id app))
         (user (remote-user request))
         (username (username user))
         (is-submission (form-values "CONTENT" request )))
    (unless article
      (return-from edit-article-handler (cons :not-found "No Such Article")))
    (multiple-value-bind (can-edit-p reason)
        (can-edit-article app article user)
      (multiple-value-bind (data condition)
          (form-data (article-form app) request)
        (if (and is-submission can-edit-p (not condition))
            (progn
              (when (not (content article))
                (setf (slot-value article 'author) username))
              (setf (slot-value article 'content)
                    (scan-content-for-new-topics
                     app (getf data :content) username))
              (setf (slot-value article 'anonymous) (getf data :anonymous))
              (setf (slot-value article 'title) (getf data :title))
              (setf (slot-value article 'created) (get-universal-time))
              (setf (slot-value article 'style) :rst)
              (reindex-article app id article)
              (redirect request
                        (if (form-values "Edit" request)
                            (concatenate 'string "../" id)
                            (concatenate 'string "images/" id))))
            `(html
              (head (markup:title "Edit Article"))
              (body
               (navbar
                ,(append
                  (menus app)
                  (when (can-edit-article app article user)
                    `(((,(format nil "~A" id) "Back to View")
                       (,(format nil "edit/images/~A" id) "Manage Images")))))
                :on-url "edit" :relative "../")
               (h1 ,(format nil "Edit: ~A (~A)" (title article) id))
               ,@(if can-edit-p
                     (list
                      (markup-form (article-form app)
                                   (if is-submission
                                       data
                                       (list
                                        :title (title article)
                                        :content (content article)
                                        :anonymous (anonymous article)))))
                     `((p ,reason)))))))))) ; else reason

(defmethod delete-article-handler((app peer-review) request id)
  (let ((article (get-article id app))
        (user (remote-user request)))
    (unless article
      (return-from delete-article-handler :not-found))
    (multiple-value-bind (can-delete-p reason)
        (can-delete-article app article user)
      `(html
        (head (markup:title "Delete Article"))
        (body
         (navbar ,(menus app) :relative "../")
         (h1 ,(format nil "Delete: ~A (~A)" (title article) id))
         ,(if can-delete-p
              (if (form-values "Delete" request)
                  (progn (rem-article app id)
                         `(p "Article Deleted"))
                  `((form :method "POST")
                    (p  "Delete the entire article?"
                     ((input :name "Delete" :type "submit" :value "YES")))))
              `(p ,reason)))))))        ; else reason

(defmethod delete-review-handler((app peer-review) request rest)
  (multiple-value-bind (id review-author)
      (values-list (split-string rest nil "/"))
    (let ((article (get-article id app))
          (user (remote-user request))
          (review (get-review app id review-author)))
      (unless (and article review)
        (return-from delete-review-handler
          (cons :not-found "No such review or article.")))
      (multiple-value-bind (can-delete-p reason)
          (can-delete-review app article user review-author)
        `(html
          (head (markup:title "Delete Review"))
          (body
           (navbar ,(menus app) :relative "../../")
           (h1 ,(format nil "Delete review by ~A on ~A (~A)"
                        review-author (title article) id))
           ,(if can-delete-p
                (if (form-values "Delete" request)
                    (progn (rem-review app id review-author)
                           `(p "Review Deleted"))
                    `((form :method "POST")
                      (p "Really delete this review?"
                       ((input :name "Delete" :type "submit"
                               :value "Yes")))))
                `(p ,reason))))))))     ; else reason

(defmethod review-article-handler((app peer-review) request id)
  (let* ((article (get-article id app))
         (user (remote-user request))
         (username (username user))
         (is-submission (form-values "Review" request)) )
    (unless article (return-from  review-article-handler :not-found))
    (multiple-value-bind (can-review-p reason)
        (can-review-article app article user)
      (multiple-value-bind (data condition)
          (when (and is-submission can-review-p)
            (form-data (review-form app) request))
        (if (and is-submission data (not condition))
            (progn
              (push (append (list :author username
                                  :created (get-universal-time))
                            data)
                    (reviews article))
              (reindex-article app id article)
              (redirect request (concatenate 'string "../" id)))
            `(html
              (head (markup:title "Review Article"))
              (body
               (navbar ,(menus app) :on-url "review" :relative "../")
               (h1 ,(format nil "Review: ~A (~A)" (title article) id))
               ((table :border 1 :cellspacing 0)
                (tr
                 ((td :valign "top" :width "60%")
                  ,@(article-body-html app id
                                       :path-to-root ".."
                                       :is-tutor
                                       (has-permission :tutor app user)))
                 ((td :valign "top")
                  ((section :level 3 :title "Review Form")
                   (p "Please complete the review below")
                   ,(if can-review-p
                        (markup-form (review-form app) data)
                        `(p ,reason)))))))))))))

(defmethod search-handler((app peer-review) request rest)
  (declare (ignore rest))
  (let ((search-term (car (form-values "search-term" request))))
    `(html
      (head (markup:title "Peer Review Article Search"))
      (body
       (navbar ,(menus app) :on-url "search")
       (h1 "Search")
       ((form :method "POST")
        (p "Enter Search Term: "
           ((input :type "text" :size 20 :name "search-term"
                   :value ,(or search-term "")))
           ((input :type "submit" :name "keyword" :value "Keyword" ))
           ((input :type "submit" :name "pattern" :value "Pattern" ))
           (p "Keyword searchs for articles containing the words in you
search term returning them in order of closest match. Pattern treats
the search term as a regular expression pattern returning all articles
which match"))
        ,(if search-term
             `(ol
               ,@(or
                  (if (form-values "pattern" request)
                      (handler-case
                          (let ((matcher (regex:compile-str search-term)))
                            (mapcan
                             #'(lambda(id)
                                 (let ((article (get-article id app)))
                                   (when (and (content article)
                                              (regex:scan-str matcher
                                                              (content article)))
                                     (list `(li ((a :href ,id)
                                                 ,(title article)))))))
                             (article-ids app)))
                        (error (condition)
                          `((p ,(format nil "Error in regular expression: ~S"
                                        condition)))))
                      (let ((threshold
                             (user-preference :search-threshold
                                              app (remote-user request) 0.1)))
                        (mapcan
                         #'(lambda(entry)
                             (let ((idx (car entry)))
                               (when (> (cdr entry) threshold)
                                 `((li ((a :href ,(id idx)) ,(title idx))
                                    ,(format nil " (~,3,0G)"
                                             (* 100 (cdr entry))))))))
                         (sort (search-index (keyword-index app) search-term)
                               #'>
                               :key #'cdr))))
                  '((li "No Matches Found"))))
             ""))))))

(defun normalise-filename(name)
  "Conver an upload name into a name suitable as a file suffix for storage"
  (let ((p (or (position #\/ name :from-end t)
               (position #\\ name :from-end t))))
    (when p (setf name (subseq name (1+ p)))))
  (with-output-to-string(os)
    (loop :for c :across name
          :if (and (typep c 'standard-char)
                   (or (alphanumericp c) (eql c #\.)) )
          :do (write-char c os)
          :else
          :do (write-char #\- os)
          :end)))

(defvar *t* nil)

(defmethod manage-images-handler((app peer-review) request id)
  (let ((article (get-article id app))
        (user (remote-user request)))
    (unless article (return-from manage-images-handler
                      (cons :not-found "No such article")))
    (multiple-value-bind (can-edit-p reason)
        (can-edit-article app article user)
      (flet ((image-name-to-path (name)
               (merge-pathnames
                (image-directory app)
                (format nil "~A-~A" id (normalise-filename name))))
             (list-images ()
               (mapcan
                #'(lambda(path)
                    (when (jarw.lib::is-prefix-p id (file-namestring path))
                      (list (subseq (file-namestring path) (1+ (length id))))))
                (directory (merge-pathnames
                            (make-pathname :name :wild :type :wild)
                            (image-directory app))))))
        (let ((actions
               (cond ((not can-edit-p) '("Illegal Action"))
                     ((form-values "Add" request)
                      (setf *t* request)
                      (let ((part (first (form-values "file" request))))
                        (if part
                            (let ((name (car part))
                                  (data (cdr part)))
                              (format t "~S -> (~D)~%" name (length data))
                              (with-open-file (os (image-name-to-path name)
                                                  :element-type
                                                  '(unsigned-byte 8)
                                                  :direction :output
                                                  :if-exists :supersede)
                                (write-sequence data os))
                              (list (format nil "Uploaded ~A (~D bytes)" name
                                            (length data))))
                            (list "No file uploaded"))))
                     ((form-values "Delete" request)
                      (let ((ids (form-values "ids" request)))
                        (mapcar
                         #'(lambda(image)
                             (delete-file (image-name-to-path image))
                             (format nil "Deleting  ~A " image))
                         (if (listp ids) ids (list ids)))))
                     (t '("")))))
          `(html
            (head (markup:title "Manage Images"))
            (body
             (navbar ,(menus app) :on-url "/edit/images/" :relative "../../")
             (h1 ,(title article) ": Manage Images" )
             (p "It is only useful to upload images in formats which
can be displayed by web browsers i.e. gif, jpg .  The filename suffix
should correspond to the file format." )
             (p ,@actions)
             ,(if can-edit-p
                  (let ((images
                         (mapcar
                          #'(lambda(image)
                              `(tr ((td :valign :top)
                                    ((input :name "ids" :type "checkbox"
                                            :value ,image))
                                    ,image)
                                (td
                                 ((img :src
                                       ,(format nil "../../images/~A-~A"
                                                id image))))))
                          (list-images))))
                    `((form :method "POST" :enctype "multipart/form-data") ;
                      (p "File: " ((input :name "file" :size 40 :type "file"
                                          :accept "gif"))
                       ((input :type "submit" :name "Add"
                               :value "Upload Image")))
                      ,(if images
                           `(table ,@images
                             (tr (td ((input :type "submit"
                                             :name "Delete"
                                             :value "Delete Images")))))
                           "")
                      (p ((a :href ,(concatenate 'string "../" id))
                          "Back to Editing Article"))))
                  `(p ,reason)))))))))

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

(defmethod marks-handler((app peer-review) request rolest)
  (let* ((allroles (cdr (assoc :student (inet.acl::acl app))))
         (roles (when (> (length rolest) 0)
                  (jarw.parse:parse-input 'roles  rolest))))
    (let* ((authors (sort (if roles
                              (get-users roles (users app))
                              (authors app))
                          #'string<))
           (fields '(:mark :raw-mark :no-articles :no-reviews :article-mark
                     :review-mark :feedback-mark-received :last-post))
           (form `((form :method "post")
                   (p "Moderate on"
                    ((mcq :name :moderate-by :style :dropdown
                          :datatype symbol :value  :subset)
                     (:mark . "Given Mark")
                     (:subset . "Subset Student Reviews")
                     (:all . "All Student Reviews"))
                    " to average review mark:"
                    ((input :name :mark :size 5
                            :datatype (number :format "~4,1f")))
                    ((input :type "Submit" :value "Recalculate")))))
           (data (form-data form request)))
      (flet ((format-value(v)
               (cond ((not (numberp v)) "-")
                     ((> v 10000)
                      (format-time nil  (floor v)))
                     ((not (integerp v)) (format nil "~4,1f" v))
                     (t v))))
        (multiple-value-bind (results moderation)
            (marks-analysis app authors
                            (case (getf data :moderate-by)
                              (:mark (getf data :mark))
                              (:all (authors app))
                              (t authors) ))
          (setf (getf data :mark) moderation)
          `(html
            (head (markup:title "Peer Review Marks"))
            (body
             (navbar ,(menus app) :on-url "marks/" :relative "../")
             ,(navbar
               (list
                (append '(("." "All"))
                        (mapcan
                         #'(lambda(role)
                             (list (list (string role) (string role))))
                         allroles )))
               :on-url (string-upcase rolest))
             (h1 "Marks for " ,(or roles "All Authors"))
             (p "To select a subset of students add their role onto
the url e.g. .../marks/it2002.")
             ,(markup-form form data)
             ((table :border 1)
              (tr (th "Student") (th "Mark") (th "Raw Mark")
                  (th "No" (br) "Articles")
                  (th "No" (br) "Reviews")
                  (th "Average"(br)"Article" (br) "Mark" (br) "Received")
                  (th "Average" (br) "Review" (br) "Mark" (br) "Given")
                  (th "Average" (br) "Review" (br) "Feedback" (br) "Mark")
                  (th "Last Article on"))
              ,@(mapcar
                 #'(lambda(username)
                     (let ((result (gethash username results)))
                       `(tr (td ((a :href ,(concatenate
                                            'string "../author/" username))
                                 ,username))
                         ,@(mapcar
                            #'(lambda(key)
                                `((td :align "right")
                                  ,(format-value (getf result key))))
                            fields))))
                 authors)
              ,@(let ((aggregate
                       (mapcan #'(lambda(field)
                                   (list field
                                         (mapcan
                                          #'(lambda(author)
                                              (let ((v (getf (gethash
                                                              author results)
                                                             field)))
                                                (when v (list v))))
                                          authors)))
                               fields)))
                     `(((tr :border 2) (th "Averages")
                        ,@(mapcar #'(lambda(field)
                                      `((th :align "right")
                                        ,(format-value
                                          (mean

                                           (mapcan
                                            #'(lambda(m) (when (and m (> m 0))
                                                           (list m)))
                                            (getf aggregate field))))))
                                  fields))
                       ((tr :border 2) (th "Std Deviations")
                        ,@(mapcar #'(lambda(field)
                                      `((th :align "right")
                                        ,(format-value
                                          (stddev (getf aggregate field)))))
                                  fields)))) )))) ))))


(defmethod dregs-handler((app peer-review) request rest)
  (declare (ignore rest))
  `(html
    (head (markup:title "The Dregs"))
    (body
     (navbar ,(menus app) :on-url "dregs")
     ((section :title "Duplicated titles")
      ,@(mapcar
         (lambda(entry)
           `(p ,@(mapcar
                  (lambda(idx)
                    `(span ((a :href ,(id idx)) ,(title idx))
                      (em " " ,(author idx))
                      " (" ,(format nil "~,1F" (mark idx))")" (br)))
                  entry)))
         (articles-with-duplicate-titles app)))
     ((section :title "Worst Articles")
      (ol
       ,@(mapcar
          #'(lambda(idx)
              `(li ((a :href ,(id idx)) ,(title idx))
                " (" ,(format nil "~,1f" (mark idx)) ")"
                ,(author idx) ))
          (subseq (best-articles (article-index app) :reverse t) 0 50))))
     ((section :title "Short Articles")
      (ol
       ,@(mapcar
          #'(lambda(idx)
              `(li ((a :href ,(id idx)) ,(title idx))
                " (" ,(content-length idx) ")"
                " " ,(author idx)  ))
          (subseq (shortest-articles (article-index app)) 0 50)))))))

(defmethod all-handler((app peer-review) request rest)
  (declare (ignore rest))
  `(html
    (head (markup:title "All articles listed by Mark"))
    (body
     (navbar ,(menus app) :on-url "all" :relative "../")
     (h1 "All articles listed by Mark")
     (table
      (tr (th "Mark") (th "No Reviews") (th "Title") (th "Author"))
      ,@(mapcan
         (lambda(idx)
           (when (has-content idx)
             `((tr (td ,(format nil "~0,1F" (mark idx)))
                (td ,(length (reviewers idx)))
                (td ((a :href ,(id idx)) ,(title idx)))
                (td ((a :href ,(concatenate 'string
                                            "../author/" (author idx)))
                     ,(author idx)))))))
         (best-articles (article-index app)) )))))

(defmethod author-handler((app peer-review) request author)
  (let* ((is-tutor (has-permission :tutor app (remote-user request)))
         (user (if (and is-tutor (> (length author) 0))
                   (get-dictionary author (users app))
                   (remote-user request)))
         (idx (article-index app))
         (username (username user)))
    (flet ((make-link (entry)
             `((a :href ,(concatenate 'string "../" (id entry)))
               ,(title entry)))
           (fmtmark(mark) `((td :align :right) ,(format nil "~0,1F" mark))))

      `(html
        (head (markup:title "Articles and reviews by " ,username))
        (body (navbar ,(menus app) :on-url "author/" :relative "../")
         (h1 "Articles authored by " ,username)
         ,(unless is-tutor
                  '(p "Marks will only be displayed if there at least 3
reviews for an article"))
         ((table :border 1 :cellspacing 0)
          (tr (th "Article") (th "Mark") (th "Created")
              ,(if is-tutor '(th "Reviewers") '(th "No reviews")))
          ,@(mapcar
             #'(lambda(entry)
                 (let ((reviewers (reviewers entry)))
                   `(tr
                     (td ,(make-link entry))
                     ,(if (or is-tutor (> (length reviewers) 2))
                          (fmtmark (mark entry))
                          '((td :align "center") "-"))
                     (td ,(format-time nil (created entry) :fmt :date-only))
                     ,(if is-tutor
                          `(td ,@(mapcar #'(lambda(name)
                                             `((a :href ,name) ,name " "))
                                         reviewers) )
                          `((td :align "right") ,(length reviewers))))))
             (articles-authored-by idx username)))
         (h1 "Reviews by " ,username)
         ((table :border 1 :cellspacing 0)
          (tr (th "Article Reviewed")  (th "Reviewed on")
              ,@(when is-tutor '((th "Mark") (th "Total Mark")
                                 (th "No reviews"))))
          ,@(mapcar
             #'(lambda(entry)
                 (let ((review (review (get-article entry app) username)))
                   `(tr
                     (td ,(make-link entry))
                     (td ,(format-time nil (created review) :fmt :date-only ))
                     ,@(when is-tutor
                             (list (fmtmark (form-mark review
                                                       (find-form review)))
                                   (fmtmark (mark entry))
                                   `((td :align :right)
                                     ,(length (reviewers entry))) )))))
             (articles-reviewed-by idx username))))))))