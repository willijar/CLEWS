;;; $Id: passwd.lisp,v 1.1 2007/10/03 07:48:05 willijar Exp $
;;;Copyright Dr John A.R. Williams (c) 2003. Please see LICENSE.txt

(in-package :clews.passwd)

(defclass passwd-manager (application)
  ((authenticators
    :type list :initform nil :initarg :authenticators
    :accessor authenticators
    :documentation "List of authenticators")
   (group-sources
    :type list :initform nil :initarg :group
    :accessor group-sources
    :documentation "List of dictionaries which return a list of users,
given a groupname")
   (admin :type string :initform "J.A.R.Williams@aston.ac.uk"
          :initarg :admin :accessor admin)
   (realm :type string :accessor realm :initarg :realm
          :initform "Aston Electronic Engineering"))
  (:default-initargs
      :id :passwd-manager
    :acl '((:view . (:all))
           (:admin . (:admin))))
  (:documentation "Password Manager"))

(defmethod published-methods ((app passwd-manager) baseurl)
  (declare (ignore baseurl))
  `(,@(call-next-method)
    ("admin" ,#'admin-handler :stage :response :match :exact :role :admin)
    #+nil("group" ,#'group-handler :stage :response :match :exact :role :admin)))

(defun reset-password(username authenticators &key (if-set :ask))
  (let* ((set-p
          (every #'(lambda(a) (stored-credentials username a)) authenticators))
         (random (crypt::random-salt 8))
         (new (if set-p
                  (ecase if-set
                    (:reset random)
                    (:ignore nil)
                    (:ask
                     (when
                     (y-or-n-p
                      "~%Password for ~S already set.~% Do you want to reset it"
                      username))))
                       random)))
    (format t "set-p= ~A New=~A~%" set-p new)
    (when new
      (map 'null #'(lambda(authenticator)
                     (change-credentials new username authenticator))
           authenticators)
      (format t "Updated authentication for ~S" username)
      (inet.rfc2821::send-mail
   "J.A.R.Williams@aston.ac.uk" username
   (format nil "New  Intranet Password for heisenberg.aston.ac.uk:8080")
   (format nil "
You have a new intranet password for accessing

Masters assessments  : http://heisenberg.aston.ac.uk:8080/MSc/grades/
Masters projects     : http://heisenberg.aston.ac.uk:8080/MSc/projects/
Research Publications: http://heisenberg.aston.ac.uk:8080/publications/

Username: ~S
New Password: ~S

To change this password go to http://heisenberg.aston.ac.uk:8080/pwd/

To access other EE intranet materials self register at
http://www.ee.aston.ac.uk/intranet/
After manual checking you will be then be given appropriate access.

Do NOT use your main university Unix password for general web site access."
           username new))
      (format t "Emailed authentication to ~S" username))))



(defun update-credentials(passwd username app request)
  (map 'null #'(lambda(authenticator)
                 (change-credentials passwd username authenticator))
       (authenticators app))
  (inet.rfc2821::send-mail
   (admin app) username
   (format nil "New ~A Intranet Password" (realm app))
   (format nil "~A~%

You have a new intranet password for accessing

Masters assessments  : http://heisenberg.aston.ac.uk:8080/MSc/grades/
Masters projects     : http://heisenberg.aston.ac.uk:8080/MSc/projects/
Research Publications: http://heisenberg.aston.ac.uk:8080/publications/

Username: ~S
New Password: ~S

To change this password go to http://heisenberg.aston.ac.uk:8080/pwd/

To access other EE intranet materials self register at
http://www.ee.aston.ac.uk/intranet/
After manual checking you will be then be given appropriate access.

Do NOT use your main university Unix password for general web site access."
           (realm app) username passwd))
  (format t "Emailed authentication to ~S" username))

(defmethod password-check(p1 &optional p2)
  "Returns a descriptive string if password not OK, null if it is OK"
  (cond
    ((and p2 (string/= p1 p2))
     "The two password fields do not match")
    ((< (length p1) 8)
     "Password must be at least 8 characters long")
    ((every #'alpha-char-p p1)
     "Passwords must contain at least one none alphabetic character")))

(defmethod admin-handler((app passwd-manager) request rest)
  (declare (ignore rest))
  (let ((form
         '((form :method :post)
           (p "Username: "
            ((input :name :username))
            ((input :type :submit :name :submit :value "Reset Password"))))))
    (multiple-value-bind (form-data condition) (form-data form request)
      (let ((username (getf form-data :username)))
        `(html
          (markup:head
           (markup:title "Reset Password for user"))
          (markup:body
           ((section :title "Reset Password for user")
            ,@(when (and (submitted-action form request)
                         (not condition)
                         form-data)
                    (update-credentials (crypt::random-salt 8) username app request)
                    `((p ,(format nil "Randomly generated
password has been sent to email address ~S." username))))
            ,(markup-form form (when username request)))))))))

(defmethod response-handler((app passwd-manager) request rest)
  (unless *current-user* (return-from response-handler :forbidden))
  (let ((form
         `((form :method :post)
           (table
            (tr (th "Username: ") (td ,(username *current-user*)))
            (tr (th "New Password:")
             (td ((input :type :password :name :p1))))
            (tr (th "Type New Password again:")
             (td ((input :type :password :name :p2))))
            (tr (th) (td ((input :type :submit :name :submit
                                 :value "Change Password"))))))))
    (multiple-value-bind (form-data condition) (form-data form request)
      `(html
        (markup:head
         (markup:title "Change Intranet Password"))
        (markup:body
         ((section :title "Change Intranet Password")
          ,(when (has-permission :admin app)
                 '(p ((a :href "admin") "Reset Password for another user")))
          ,@(if (submitted-action form request)
                (let* ((p1 (getf form-data :p1))
                       (err (or condition
                                (password-check p1 (getf form-data :p2)))))
                  (if err
                      `(((p :class :error) ,err)
                        ,(markup-form form form-data))
                      (progn
                        (update-credentials p1 (username *current-user*)
                                            app request)
                        `((p ,(format nil "Password for ~S successfully set."
                                      (username *current-user*)))))))
                (list (markup-form form)))))))))

(defmethod group-handler((app passwd-manager) request rest)
  #+nil(let* ((form
               `((form :method :post)
                 (table
                  (tr (th "Usernames: ") (td ((input :name :usernames :width 40))))
                  (tr (th "Set Passwords") (td ((boolean :name :pwd :valut t))))
                  (tr (th "Groups: ") (td ((input :name :groups))))
                  (tr (th) (td ((input :type :submit :name :submit
                                       :value "Update")))))))
              (form-data (form-data form request)))
         `(html
           (markup:head
            (markup:title "Password and Group Administration")
            (markup:body
             ((section :title "Password and Group Administration")
              ,(when (submitted-action form request)
                     (let ((usernames
                            (split-sequence #\space
                                            (getf form-data :usernames)
                                            :remove-empty-subseqs t))
                           (groups
                            (split-sequence #\space
                                            (getf form-data :groups)
                                            :remove-empty-subseqs t)))
                       (when (getf form-data :pwd)
                         (dolist(username usernames) (newpasswd app username)))
                       (set-groups app usernames groups)
                       `((p ,(format nil "users ~:{~S~} allocated to groups ~:{~S~} ~:[~; and sent new passwords~]"
                                     usernames groups (getf form-data :pwd))))))
              ,(markup-form form))
             ((section :title "Users Summary Table")
              (table
               (tr (th "Username") (th "Groups") (th "Errors"))
               ,@(mapcar
                  #'(lambda(username)
                      `(tr
                        (td ,username)))
                  (sort (all-usernames app) #'string>)))))))))
