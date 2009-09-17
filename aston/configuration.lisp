;;;; Aston Web Applications
;;;; Copyright (C) 2002-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; $Id: configuration.lisp,v 1.18 2005/03/10 19:54:25 willijar Exp $

(in-package :aston)


(import 'clews.grades::db-connect)

;; db-spec should return connection args for given database

(defparameter *pwd-source*
  (make-instance 'clews::password-file-authenticator
                 :file (translate-logical-pathname #p"clews:pwd")
                 :acl '((:admin . ("willijar")))))

(defclass aston-users(clews:user-source filesystem-dictionary)
  ())

(defun reset-password(username &key (if-set :ask))
  (clews.passwd::reset-password username (list *pwd-source*) :if-set if-set))

(defparameter *user-source*
  (make-instance
   'aston-users
   :directory (translate-logical-pathname #p"clews:users;*.lisp"))
  "The usual source of user records for aston")

(setf sb-ext::*short-site-name*
      (concatenate 'string (machine-instance) ".aston.ac.uk"))

(setf inet.rfc2821::*smtp-server*
      (if  (equal "lucifer" (machine-instance))
           "smtp.jarw.org.uk"
           "mailhub.aston.ac.uk"))

(defvar *annotate-plugin*
  (make-instance 'clews:annotate-plugin :user-dictionary *user-source*))

(defvar *chatterbox-plugin* (make-instance 'clews:chatterbox-plugin))
(defvar *who-plugin* (make-instance 'clews:who-plugin))

(defvar *error-reporter-plugin*
  (make-instance 'clews:error-reporter-plugin
                 :smtp-account (if (equal "lucifer" (machine-instance))
                                   "J.A.R.Williams@jarw.org.uk"
                                   "J.A.R.Williams@aston.ac.uk")))
(defvar *quotes-plugin*
  (make-instance 'clews:quotes-plugin
                 :file (translate-logical-pathname #p"clews:quotes.txt")))

(defvar *polls-plugin*
  (make-instance 'clews:polls-plugin
                 :file (translate-logical-pathname #p"clews:polls.lisp")))

(defun add-poll(id description choices deadline &key
                (roles '(:authenticated)))
  (clews::add-poll *polls-plugin* id description choices deadline
                   :roles roles))

(defun close-poll(id) (clews::remove-poll *polls-plugin* id))

(defun apply-aston-style(markup &key navbar)
  (multiple-value-bind (tag attrs content) (split-markup markup)
    (join-markup
     tag attrs
     (list
      (multiple-value-bind (tag attrs content) (split-markup (first content))
        (unless (eq tag 'head)
          (warn "Applying Aston head style to a ~A tag" tag))
                                        ;head
        (join-markup tag (append (list :escape nil) attrs)
                     (cons "
<LINK HREF=\"/static/local.css\" TYPE=\"text/css\" REL=\"stylesheet\">
<META HTTP-EQUIV=\"Content-Style-Type\" CONTENT=\"text/css\" />" content)))
      (multiple-value-bind (tag attrs content) (split-markup (second content))
        (unless (eq tag 'body)
          (warn "Applying Aston body style to a ~A tag" tag))
        (join-markup
         tag
         (append (list :class "seas")
                 attrs)
         (append
          `(((table :escape nil :width "100%" :cellpadding 0 :cellspacing 1
              :border 0)
             "
<tr>
</tr>"))
          (multiple-value-bind(tag attr rest) (split-markup (first content))
            (cond
              ((functionp navbar)
               (cons (funcall navbar tag attr rest) (rest content)))
              ((listp navbar) (cons navbar content))
              (t content)))
          `(((table :escape nil :width "100%" :border 0 )
             "<style type=\"text/css\"><!--A {text-decoration:none }--></style>
")))))))))

(defmethod clews::render-page((app clews:application) stream markup)
  (html stream (apply-aston-style markup)))



#|
(add-poll :itrep2003 "Vote for your student representative for the Internet Technology Programme" '("Mohammed Imran" "Ritesh Taank") "2003-12-04 17:00" :roles '(:it2003 "willijar"))
(add-poll :ttrep2003 "Vote for your student representative for the Telecommunications Technology Programme" '("Yogesh Sharm" "Romil Prasad") "2003-12-04 17:00" :roles '(:tt2003 "willijar"))
(defun close-poll(id)
  (clews::remove-poll *polls-plugin* id))
(defun polls() (clews::polls *polls-plugin*))
(add-poll :itrep2003 "Vote for your student representative for the Internet Technology Programme" '("Mohammed Imran" "Ritesh Taank") "2003-12-04 17:00" :roles '(:it2003 "willijar"))
(add-poll :ttrep2003 "Vote for your student representative for the Telecommunications Technology Programme" '("Yogesh Sharm" "Romil Prasad") "2003-12-04 17:00" :roles '(:tt2003 "willijar"))
(defun close-poll(id)
  (clews::remove-poll *polls-plugin* id))

(defun polls() (clews::polls *polls-plugin*))
|#
