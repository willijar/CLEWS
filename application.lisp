;; $Id: application.lisp,v 1.5 2007/07/29 17:29:22 willijar Exp willijar $
;; The Web Application base class
;; Copyright (C) 2002-2006 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords: CLEWS

;; This file is part of the Common Lisp Educational Web System (CLEWS)

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;;

;;; Code:


(in-package :clews)

(defconstant +version+
  (if (boundp '+version+) +version+ "CLEWS/0.2")
  "Product token")

(defclass application (component)
  ((passwords :initform nil :initarg :pwd-source :reader passwords
              :documentation "Source of passwords for authentication")
   (users  :initform nil :initarg :user-dictionary :reader users
           :documentation "Source of user record objects for authorisaton")
   (authentication :type list
                   :reader authentication :initarg :authentication
                   :documentation "list, first item of which is the
Authentication function to be used. This takes 4 arguments, the
application entity, request, unmatched path and the rest of the list
which should be a property list of parameters, to which :baseurl
property is added during publication."
                   :initform (list #'cookie-authentication-handler))
   (plugins :type list :initform '() :initarg :plugins
            :reader plugins
            :documentation "List of plugins associated with the application"))
  (:documentation "Abstract Base Class for a CLEWS web application"))

(defgeneric groups(app)
  (:documentation "Return a list of the groups associated with this
application, the first element of each group is the group name and the
rest is a list of users in that group. A user may of course appear in
several groups")
  (:method((app application))
    (when (users app)
      (let ((groups ()))
        (map-dictionary
         (lambda(username user)
           (dolist (role (roles user))
             (let ((rec (find role groups :key #'first :test #'equal)))
               (if rec (pushnew username (rest rec) :test #'equal)
                   (push (list role username) groups)))))
         (users app))
        groups))))

(defgeneric publish(app url server)
  (:documentation
   "Publish given applications methods on given server, optionally specifying
   the path"))

(defgeneric authorization-handler(app request rest)
  (:documentation
   "Check user authentication and get user object if successful"))

(defgeneric response-handler(app request rest)
  (:documentation "Handle normal responses"))

(defgeneric application-handler(request rest app method display-plugins-p role)
  (:documentation "Main handler dispatches request
arguments to appropriate method"))

(defgeneric handle-plugins(request rest app markup)
  (:documentation "Add in the application pkugins to this markup."))

(defgeneric render-page(app stream markup)
  (:documentation "Function to render html markup as required by
this application. Default calls  html. Children should override this to add
in theming etc"))

(defgeneric published-methods(app baseurl)
  (:documentation "Return list giving published methods, a list of
lists each sublist is path, stage and a property list giving :match
:method(s) :display-plugins. baseurl will contain the url under which
the app is being published"))

(defgeneric log-handler(app request rest)
  (:documentation "Handler to log requests for a CLEWS application"))

(defgeneric su-handler(app request rest)
  (:documentation "Handler for super user access for a CLEWS application"))

(defgeneric preference-handler(app request rest)
  (:documentation "Handler for preference management for a CLEWS application"))

(defun make-authentication-handler(app baseurl)
  (when (and (passwords app) (authentication app))
    (let ((params (append (cdr (authentication app))
                          (list :baseurl baseurl)))
          (func (car (authentication app))))
      #'(lambda(app request rest)
          (funcall func app request rest params)))))

(defmethod published-methods((app application) baseurl)
  `(("." ,#'response-handler :stage :response :match :prefix
     :display-plugins-p t)
    ,@(when-bind(handler (make-authentication-handler app baseurl))
       `(("." ,handler :stage :authentication :match :prefix)))
    ,@(when (find-method #'log-handler nil (list (class-of app) t t) nil)
       `(("." ,#'log-handler :stage :log :match :prefix)))
    ,@(when (users app)
       `(("." ,#'authorization-handler :stage :authorization :match :prefix)
         ("su" ,#'su-handler :stage :response :match :exact :role :admin)
         ("preferences/" ,#'preference-handler
                         :stage :response :match :prefix
                         :display-plugins-p nil)))))

(defmethod response-handler(app request rest)
  (declare (ignore app rest))
  (cons :not-found
        (format nil "The url you requested ~S has not been found."
                (url request))))

(defmethod publish((app application) base-url server)
  "Exports handlers for this application onto server, on given path"
  (dolist (method-record (published-methods app base-url))
    (let* ((url (merge-url base-url (first method-record)))
           (handler (second method-record))
           (args (rest (rest method-record)))
           (stage (getf args :stage)))
      (etypecase handler
        (function
         (publish-handler
          (case stage
            (:log #'(lambda(request rest) (funcall handler app request rest)))
            (t
             #'(lambda(request rest)
                 (funcall #'application-handler request rest
                          app handler
                          (getf args :display-plugins-p nil)
                          (getf args :role nil)))))
          url
          server
          :stage (getf args :stage :response)
          :match (getf args :match :exact)
          :method (getf args :method t)))
        (pathname
         (publish-handler
          handler url server
          :recurse (getf args :recurse nil)
          :file-allowed (getf args :file-allowed
                              #'(lambda(pathname)
                                  (declare (ignore pathname)) t))))))))

(defmethod handle-plugins(request rest (app application) markup)
  ;; We assume we have a complete page of html, with two content elements
  ;; the head and body. We wrap the plugins into the body content using a
  ;; table if there are valid plugins
  (let ((user (remote-user request)))
    (flet ((filter-plugins(position)
             (mapcan
              #'(lambda (p)
                  (when (eq position
                            (user-preference :position p user :right))
                    (let ((markup (plugin-markup p request rest)))
                      (when markup
                        `(((section
                            :style ,(format
                                     nil "font-size:~A"
                                     (user-preference :font-size p user
                                                      "smaller"))
                            :level 3
                            :title ,(let* ((title (string-capitalize (id p)))
                                           (suffix (search  "-Plugin" title)))
                                          (if suffix
                                              (subseq title 0 suffix)
                                              title)))
                           ,@markup))))))
              (plugins app))))
      (let* ((left (filter-plugins :left))
             (right (filter-plugins :right))
             (top (filter-plugins :top))
             (bottom (filter-plugins :bottom))
             (no-rows (+ 1 (if top 1 0) (if bottom 1 0))))
        (unless (or left right top bottom) (return-from handle-plugins markup))
        (flet ((add-plugins(body)
                 `(((table :border ,(user-preference :border app user 0))
                    (tr
                     ,@(when left
                             `(((td :valign :top
                                 :class "border"
                                 :rowspan ,no-rows
                                 :width ,(user-preference :left-width
                                                          app user "200px"))
                                ,@left)))
                     ((td :valign :top) ,@(or top body))
                     ,@(when right
                             `(((td :valign :top
                                 :class "border"
                                 :rowspan ,no-rows
                                 :width ,(user-preference :right-width
                                                          app user "200px"))
                                ,@right))))
                    ,@(if (or top bottom)
                          `((tr ((td :valign :top) ,@(if top body bottom)))))
                    ,@(when (and top bottom)
                            `((tr ((td :valign :top) ,@bottom))))))))
          (multiple-value-bind (tag attrs content) (split-markup markup)
            (join-markup
             tag attrs
             (list
              (first content)
              (multiple-value-bind (tag attrs body)
                  (split-markup (second content))
                (unless (eql tag 'body)
                  (warn "Non BODY markup element ~A receiving plugins~%" tag))
                (join-markup
                 tag attrs
                 (if (eq (split-markup (first body)) 'navbar)
                     (cons (first body) (add-plugins (rest body)))
                     (add-plugins body))) )
              (rest (rest content))))))))))

(defmethod application-handler(request rest (app application)
                               method display-plugins-p role)
  "This is the handler called for all requests to an application. If
the method returns nil then nothing is done and control will be passed
to the next handler. If it returns t then it has completely dealt with
the response and the stream will be closed. Otherwise it returns 3
values - data, headers definition and possibly cookies. If the data is
a string it is written to a stream otherwise the render-page method is
called. If plugins is true and value returned is markup the html body
will be modified to include the plugins"
  (let ((*current-user* (remote-user request)))
    (if (and role (not (has-permission role app *current-user*)))
        :forbidden
        (multiple-value-bind (data headers no-plugins-p)
            (catch 'response (funcall method app request rest))
          (when data
            (let ((response
                   (if (and (listp data) (listp (cdr data)))
                       (make-instance
                        'response
                        :status 200
                        :content-type "text/html"
                        :content
                        (with-output-to-string(os)
                          (render-page
                           app os
                           (if (and display-plugins-p (not no-plugins-p))
                               (handle-plugins request rest app data)
                               data))))
                       (make-response data))))
              (dolist(header headers)
                (setf (header-field (car header) response) (cdr header)))
              (setf (header-field :server response)
                    (cons +version+ (content (header-field :server response))))
              response))))))

(defmethod render-page(app stream markup)
  (html stream markup))

(defun basic-authentication-handler(app request rest &optional params)
  (declare (ignore rest))
  (multiple-value-bind (uid pwd)
      (values-list (content (header-field :authorization request)))
    (setf (remote-user request) uid)
    (unless (credentials-valid-p pwd uid (passwords app))
      (cons 401 (getf params :realm "CLEWS")))))

;; THis is weak but better than basic digest
(defun authentication-digest(cpwd request)
  (with-output-to-string (digest)
    (map 'nil #'(lambda(item) (format digest "~2,'0X" item))
         (md5:md5sum-sequence
          (concatenate 'string cpwd (write-to-string
                                     (remote-addr request)))))))

(defun cookie-authentication-handler(app request rest &optional params)
  (let* ((cookie (cookie "authentication"  request))
         (fields (when cookie (split-string cookie :count 2 :delimiter #\:)))
         (uid (first fields))
         (digest (second fields)))
    (unless
        (and uid
             (let ((cpwd (stored-credentials uid (passwords app))))
               (when (and cpwd
                          (string= digest
                                   (authentication-digest cpwd request)))
                 (setf (remote-user request) uid))))
      ;; present and analyse login form
      (login-handler app request rest params))))

(defmethod authorization-handler((app application) request rest)
  "Default authorisation handler checks user dictionary if it exists.
It sets the remote-user field to the user record if found, otherwise
returns a Forbidden error"
  (let ((uid (remote-user request)))
    (when (users app)
      (let ((user (when uid (get-dictionary uid (users app)))))
        (when  (and (has-permission  :admin app user)
                    (not (string= rest "su"))) ; proxy handling
          (let ((proxy (property user :su)))
            (when proxy
              (setf user (get-dictionary proxy (users app))))))
        (setf (remote-user request) user)
        (unless (has-permission :view app user)
          (cons :forbidden "You are forbidden to access this web application.
Please contact the system administrator if you believe you should
have access."))))))

(defmethod su-handler((app application) request rest)
  (let ((user (remote-user request)))
    (unless (has-permission :admin app  user)
      (return-from su-handler
        (cons
         403 "You do not have admin authorisation for this web application.
Please contact the system administrator if you believe you should
have access.")))
    (let ((proxy (car (form-values "su" request))))
      (when (and proxy (get-dictionary proxy (users app)))
        (setf (property user :su) proxy)
        (setf (get-dictionary (username user) (users app)) user)
        (return-from su-handler
          (cons :see-other (merge-url (url request) "./")))))
    `(html
      (head (title "Superuser Proxy Access"))
      (body
       ((section :title "Superuser Proxy Access")
        ,(when (property user :su)
               (prog1
                   `(p "Previous proxy [" ,(property user :su) "] removed")
                 (rem-property user :su)
                 (setf (get-dictionary (username user) (users app)) user)))
        ((form :method "post")
         (p "Proxy for " ((mcq :name "su" :style :dropdown)
                          ,@(dictionary-keys (users app)))
            ((input :type "Submit" :value "Set Proxy")))))))))

;(defmethod log-handler ((app application) request rest )
  ;; After normal response write user object back to dictionary."
  ;; useful as it means you don't have to keep track of changes to the user
  ;; record in processing the response - buy inefficient if changes are rare
                                        ;  (declare (ignore rest))
                                        ;  (let ((user (remote-user request)))
                                        ;    (when (and (typep user 'user) (users app))
                                        ;      (setf (get-dictionary (username user) (users app)) user)))
 ; (values))

(defun login-handler (app request rest &optional params)
  (declare (ignore rest))
  (let ((username (car (form-values "USERNAME" request)))
        (persistence (car (form-values "PERSISTENCE" request)))
        (password (car (form-values "PASSWORD" request))))
    (cond
      ((and username (credentials-valid-p password username (passwords app)))
       (let ((response
              (make-instance
               'response
               :status :see-other
               :content
               (inet.http:simple-message-html
                "Redirect"
                (format nil "Resource has moved to ~S." (url request))))))
         (setf (header-field :location response) (url request))
         (setf (cookie "authentication" response
                       :domain (getf params :domain)
                       :path (path (getf params :baseurl))
                       :max-age
                       (when persistence
                         (parse-integer persistence :junk-allowed t)))
               (concatenate
                'string
                username ":" (authentication-digest
                              (stored-credentials username (passwords app))
                              request)))
         response))
      (;;failed in authentication - present form
        `(html
       (head (title "Web Login"))
       (body
        ((section :title "Web Login")
         ((form :method "POST")
          ((table :width "80%")
           (tr ((td :colspan 2)
                (p "Please enter your credentials to access this web below.")
                ,@(when username
                        `(((p :class "error")
                           "Login failed - the username and password do not match our records.")))))
           (tr (th "Username")
               (td ((input :name :username ,@(when username `(:value ,username))))))
           (tr (th "Password")
               (td ((input :type "password" :name :password))))
           (tr ((td :align "center" :colspan 2) "For how long do you
wish the browser to remember your creditentials?" (br) "It is
recommended that you choose \"This Session Only\" if accessing through
a public or shared computer system. Cookies must be enabled in you web
browser"))
           (tr (td) (td
                     ((mcq :style :dropdown :name :persistence)
                      (nil . "This session only")
                      (302400 . "1 Week")
                      (1209600 . "1 Month")
                      (31536000 . "1 Year") )))
           (tr (td)
               ((td :colspan 2)
                ((input :type "submit" :value "Submit Credentials"))))))
         ((p :class :error) (em "Firefox is the recommended broswer for using this web application. It is designed to web standards and not tested against bugs in other web browsers.")
))))))))

(defmethod preference-handler ((app application) request rest)
  (let* ((user (remote-user request))
         (preference-pages
          (mapcan #'(lambda(item)
                      (let ((form (user-component-preferences item user)))
                        (when form (list (cons item form)))))
                  (cons app (plugins app))))
         (this-page
          (or
           (find rest preference-pages
                 :test #'string-equal
                 :key #'(lambda(p) (string (id (car p)))))
           (first preference-pages)
           (return-from preference-handler
             `(html
               (head (title "Preferences"))
               (body
                (p "No user preferences available for this application"))))))
         (form-elements (cdr this-page))
         (component (car this-page))
         (form (make-form form-elements))
         (data (when (submitted-action form request)
                 (form-data form request)))
         (user-properties (user-component-properties component user)))
    (flet((title(c) (string-capitalize (id c))))
      (loop for item on data by #'cddr ; map form data into user properties
            do (unless (or (eq (first item) :form)
                           (typep (second item) 'condition))
                 (setf (property user-properties (first item)) (second item))))
      `(html
        (head (title "Preferences for " ,(title component)))
        (body
         ,(navbar
           (list (cons '("../" "Home")
                       (mapcar #'(lambda(p) (list (string-downcase (id (car p)))
                                                  (title (car p))))
                               preference-pages)))
           :on-url rest)
         (h1 "Preferences for " ,(title component))
         ,@(when data ;; store data
                 (setf (get-dictionary (username user) (users app)) user)
                 '((p (em "You preferences have been updated"))))
         ,(markup-form
           form
           (or data
               (mapcan #'(lambda(element) ;extract user properties
                           (let ((id (first element)))
                             (multiple-value-bind (v f-p)
                                 (property user-properties id)
                               (when f-p (list id v)))))
                       form-elements)) ))))))

(defmethod get-users((perm symbol) (app application))
  (reduce #'(lambda(a b) (union a b :test #'string=))
          (mapcar #'(lambda(role) (get-users role (users app)))
                  (cdr (assoc perm (acl app))))))

(defmethod user-component-preferences((self application) user)
  (declare (ignore user))
  '((:right-width
     :text "How wide do you want the right plugin column? Note if you
don't want a side column to appear at all, you need to ensure you
select no plugins for that side. These widths will be overriden if a
plugin is too wide."
     :markup ((mcq :style :dropdown)
              ("200px" . "200 pixels")
              ("300px" . "300 pixels")
              ("400px" . "400 pixels")
              ("500px" . "500 pixels"))
     :default "200px")
    (:left-width
     :text "Width of the left plugin column?"
     :markup ((mcq :style :dropdown)
              ("200px" . "200 pixels")
              ("300px" . "300 pixels")
              ("400px" . "400 pixels")
              ("500px" . "500 pixels"))
     :default "200px")
    (:border
     :text "Border width seperating page areas"
     :markup ((input :size 2))
     :type (integer :min 0 :max 5)
     :default 0)))