;;;; CLEWS: Annotations plugin
;;;; Copyright (C) 2002-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; $Id: annotate.lisp,v 1.9 2005/03/10 20:13:09 willijar Exp $

(in-package :clews)

(defclass annotate-plugin (plugin)
  ((users :type  dictionary
          :initarg :user-dictionary
          :reader users
          :documentation "Source of user record objects")
   (cache :type hash-table
          :initform (make-hash-table :test #'equal)
          :documentation
          "Cache of which usernames that have written annotations for
which urls"))
  (:default-initargs :id :annotate-plugin)
  (:documentation
   "Plugin allows user to leave private and public notes on a page"))

(defmethod initialize-instance :after ((self annotate-plugin) &key)
  "We accumulate a list of which users have public annotations on which pages
   here"
  (map-dictionary
   #'(lambda (username user)
       (when user
         (let ((annotations
                (property (user-component-properties self user) :annotations)))
           (when annotations
             (maphash #'(lambda(url a)
                          (when (first a)
                            (push username
                                  (gethash url (slot-value self 'cache)))))
                      annotations)))))
   (users self)))

(defgeneric annotations(plugin entity)
  (:documentation "Return a list of annotations from plugin for entity"))

(defmethod annotations((self annotate-plugin) (user user))
  (property-subset (user-component-properties self user) :annotations
                   :maker #'(lambda(defaults)
                              (declare (ignore defaults))
                              (make-hash-table :test #'equal))))

(defmethod annotations((self annotate-plugin) (url string))
  (mapcar #'(lambda(username)
              (cons username
                    (property
                     (annotations self (get-dictionary username (users self)))
                     url)) )
          (gethash url (slot-value self 'cache))))

(defmethod plugin-markup((self annotate-plugin) request rest)
  (declare (ignore rest))
  (let* ((user (remote-user request))
         (url (path (url request)))
         (old-annotation
          (property (property (user-component-properties self user)
                              :annotations)
                    url))
         (private-annotation
          (second
           (if (car (form-values (id self) request))
               (with-form-values(text public) request
                 (let* ((submission
                         (when (> (length text) 0)
                           (list text (get-universal-time))))
                        (annotation
                         (if public
                             (list submission (second old-annotation))
                             (list (first old-annotation) submission))))
                   (setf (gethash url (slot-value self 'cache))
                         (append (when (first annotation)
                                   (list (username user)))
                                 (remove (username user)
                                         (gethash url (slot-value self 'cache))
                                         :test #'string-equal)))
                   (setf (property (annotations self user) url) annotation)))
               old-annotation))))
    `(((form :method "POST")
       ((textarea :name :text
                  :cols ,(user-preference :cols self user 30)
                  :rows ,(user-preference :rows self user 10))
        ,(if private-annotation (first private-annotation) ""))
       ((input :name ,(id self) :value "Annotate" :type "submit"))
       "Public: " ((input :name :public :type "checkbox" :value 1))
       ,@(mapcan
          #'(lambda (item)
              (let ((username (first item))
                    (annotation (second item)))
                (when annotation
                  `((p ,(first annotation) (br)
                     (em ,username " " ,(data-format-validation::format-time
                                         nil (second annotation)
                                         :fmt :date-only)))))))
          (sort (mapcan #'(lambda(item)
                            (when (second (second item)) (list item)))
                        (annotations self url))
                #'<
                :key #'(lambda (item) (second (second item))) ))))))

(defmethod user-component-preferences((self annotate-plugin) user)
  (declare (ignore user))
  (append (call-next-method)
          '((:rows
             :text "How many rows do you want in the form text area?"
             :markup ((input :size 5))
             :type (integer :min 1 :max 80)
             :default 10)
            (:cols
             :text "How many columns do you want in the form text area?"
             :markup ((input :size 5))
             :type (integer :min 1 :max 120)
             :default 30) )))

#|
(defmethod plugin-preferences-handler((plugin notes-plugin) request rest)
  (:documentation
   "handles preferences setting markup for this plugin"))
|#