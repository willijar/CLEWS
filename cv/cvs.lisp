;;; $Id: cvs.lisp,v 1.5 2005/03/10 19:55:14 willijar Exp $
;;;Peer review application class for EWAS
;;;Copyright Dr John A.R. Williams (c) 2003. Please see LICENSE.txt

(in-package :clews.cv)

(defclass cv-manager (application)
  ((cv-form
    :reader cv-form
    :initform "cv"
    :initarg :cv-form
    :documentation "CV Description Form"))
  (:default-initargs
      :id :cvs
    :acl '((:view . (:all))
           (:student . (:student))
           (:tutor  . (:staff :tutor :supervisor :admin))
           (:admin . (:admin))))
  (:documentation "Class for cv management system"))

;;; permissions for current user
(defmethod can-edit-cv((app cv-manager) cvname
                       &optional (user *current-user*))
  (or (has-permission '(:admin) app user)
      (string= cvname (username user))))

(defmethod can-view-cv((app cv-manager) cvname
                       &optional (user *current-user*))
  (or (has-permission '(:tutor :admin) app user)
      (string= cvname (username user))))

(defmethod response-handler((app cv-manager) request rest)
  (let* ((id (first
              (split-sequence #\/ (subseq rest 0 (position #\? rest))
                              :count 1 :remove-empty-subseqs t))))
    (if id
        (if (string= (subseq id (- (length id) 4)) ".pdf")
            (cv-pdf-handler app request (subseq id 0 (- (length id) 4)))
            (cv-display-handler app request id))
        (if (has-permission  '(:tutor :admin) app)
            (cv-directory app request rest)
            (redirect request (username *current-user*))))))

(defun cv-directory(app request rest)
  (declare (ignore request rest))
  "Present list of cvs"
  (unless (has-permission '(:admin :tutor) app)
    (return-from cv-directory :forbidden))
  `(html
    (head (title "CVs"))
    (body
     (navbar ,(menus app))
     ((section :title "Completed CVs")
      (dl
       ,@(mapcan
          #'(lambda(username)
              (let ((user (get-dictionary username (users app))))
                (when  user
                  (let((cv (property (user-component-properties app user)
                                     :cv)))
                    (when cv
                      `((dt ((a :href ,(username user))
                             ,(let ((title (getf cv :title)))
                                   (if (> (length title) 0)
                                       title
                                       (display-name user))))
                         " (" ,(username user) ")")))))))
          (sort (get-users :student app) #'string>)))))))

(defmethod menus((app cv-manager) &optional id)
  `((("."  "Home")
     ,(if id
          (when (can-edit-cv app id)
            (list (format nil "~A?edit=t" id)
                  (format nil "Edit ~A CV"  id)))
          (list (format nil "~A" (username *current-user*))
                (format nil "View ~A CV"  (username *current-user*))))
     ("preferences/" "Preferences") )))

(defmethod cv-display-handler((app cv-manager) request id)
  (unless (can-view-cv app id)
    (return-from cv-display-handler :forbidden))
  (let ((user (get-dictionary id (users app))))
    (unless user (return-from cv-display-handler :not-found))
    (let* ((data (property (user-component-properties app user) :cv))
           (form (or (find-form data) (cv-form app)))
           (edit-p (and (inet.http:query-values "edit" request)
                        (can-edit-cv app id)))
           (pagetitle (format nil "CV for ~A" (display-name user)))
           (report nil))
      (when (and edit-p (submitted-action form request))
        (multiple-value-bind(form-data condition)
            (form-data form request)
          (when condition
            (setf report
                  "CV update unsuccessful. Please correct errors below.") )
          (setf data form-data)
          (unless condition
            (setf (property (user-component-properties app user) :cv) data)
            (setf (get-dictionary id (users app)) user) )))
      `(html
        (head
         (title ,pagetitle))
        (body
         (navbar ,(menus app id)  :on-url ,id)
         ,(when report `((p :class :error) ,report))
         ,(markup-form form data (not edit-p))
         (p ((a :href ,(concatenate 'string id ".pdf")) "PDF Version"))
         (hr)
         ,(when (and edit-p (not report))
                `((section :title "Preview")
                  ,(markup-form form data :text))))))))

(defmethod cv-pdf-handler((app cv-manager) request id)
  (unless (can-view-cv app id) (return-from cv-pdf-handler :forbidden))
  (let ((user (get-dictionary id (users app))))
    (unless user (return-from cv-pdf-handler :not-found))
    (let* ((data (property (user-component-properties app user) :cv))
           (form (or (find-form data) (cv-form app)))
           (pagetitle (format nil "CV for ~A" (display-name user))))
      (make-instance 'response
                     :status 200
                     :content-type "application/pdf"
                     :content
                     (with-output-to-string(os)
                       (markup::pdf os
                                    `(html
                                      (head
                                       (title ,pagetitle)
                                       (markup::author ,(display-name user)))
                                      (body
                                       ,(markup-form form data t)))))))))
