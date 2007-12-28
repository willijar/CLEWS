;;;; CLEWS: Bug reporter plugin
;;;; Copyright (C) 2002-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; $Id: error-reporter.lisp,v 1.8 2005/03/10 20:13:09 willijar Exp $

(in-package :clews)

(defclass error-reporter-plugin (plugin)
  ((logfile :type pathname :initarg logfile
            :reader logfile-pathname
            :documentation "Path to where records are to be stored")
   (smtp-account :type string :initarg :smtp-account
                 :reader smtp-account
                 :documentation "Account on server from which email is to be sent")
   (smtp-server :type string :initarg :smtp-server
                :initform *smtp-server*
                :reader smtp-server
                :documentation "Server to use to send emails")
   (smtp-port :type fixnum :initarg :smtp-port
              :initform 25 :reader smtp-port
              :documentation "Port to use for smtp")
   (recipients :type list :initarg :recipients
               :initform '()
               :documentation "an ordered a-list mapping urls prefixes to email addresses - should finish with (\"\" . default)"))
  (:default-initargs :id :error-reporter-plugin)
  (:documentation "Plugin allows user to record errors on pages"))

(defun recipient(url error-reporter)
  (or (assoc-if
       #'(lambda(item) (string-equal item url
                                     :end1 (min (length item) (length url))))
       (slot-value error-reporter 'recipients))
      (smtp-account error-reporter)))

(defmethod plugin-markup((self error-reporter-plugin) request rest)
  (declare (ignore rest))
  (let* ((user (remote-user request))
         (url (path (url request)))
         (to (recipient url self))
         (body (when (form-values (id self) request)
                 (car (form-values "text" request)))))
    (list
     (if body
         (handler-case
             (let* ((username (username user))
                    (subject (format nil "Web Error Report for ~A" url))
                    (headers `((:from . "ee-www-no-reply@aston.ac.uk")
                               (:reply-to . ,username)
                               (:to . ,to)
                               (:cc . ,(user-preference
                                        :email self user
                                        (property user :email username))))))
               (send-mail
                (smtp-account self)
                (list to username)
                subject
                body
                :server (smtp-server self)
                :port (smtp-port self)
                :signature "Web Bug Report"
                :headers headers)
               '(p (em "Your report has been successfully sent,
and a copy has been sent to you")) )
           (error (condition)
             `((p :class :error)
               ,(format nil "An error occured in sending you report.
Please send an email directly to ~S giving as much information as possible
about what lead up to the error and quoting the condition ~A" to condition))))
         '(p "Please report any errors, or suggestions for improvements relating to this web page using the form below"))
     `((form :method "POST" :action ,url)
       ((textarea :name "text"
         :cols ,(user-preference :cols self user 30)
         :rows ,(user-preference :rows self user 10)))
       ((input :name ,(id self) :value "Send Report" :type "submit"))))))

(defmethod user-component-preferences((self error-reporter-plugin) user)
  (append (call-next-method)
          `((:rows
             :text "How many rows do you want in the form text area?"
             :markup ((input :size 5))
             :type (integer :min 1 :max 80)
             :default 10)
            (:cols
             :text "How many columns do you want in the form text area?"
             :markup ((input :size 5))
             :type (integer :min 1 :max 120)
             :default 30)
            (:email
             :text "Your preferred email address for report copies"
             :markup ((input :size 40))
             :type (string :min-length 5 :max-length 127)
             :default ,(property user :email (username user))) )))

#|
(defmethod plugin-preferences-handler((plugin notes-plugin) request rest)
  (:documentation
   "handles preferences setting markup for this plugin"))
|#