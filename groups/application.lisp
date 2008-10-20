;;;; Application entity -*- Lisp -*- Copyright (C) 2008
;;;; John A.R. Williams <J.A.R.Williams@jarw.org.uk> Released under
;;;; the GNU General Public License (GPL) See
;;;; <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;

(in-package :clews.coursework)

(defclass components-application(application module)
  ((modules :initarg :modules :reader modules))
  (:documentation "An application module which uses command processor logs"))

(defmethod initialize-instance :after ((app components-application)
                                       &key directory)
  (when directory
    (if (slot-boundp app 'modules)
        (error "Directory specified and modules already initialised.")
        (setf (slot-value app 'modules)
              (make-instance 'command-processor-directory
                             :directory directory)))))
#|
(defvar *app*
  (make-instance 'components-application
                 :id :coursework
                 :title "EE Masters Coursework"
                 :description "System for coursework assessment"
                 :user-dictionary aston::*user-source*
                 :pwd-source aston::*pwd-source*
                 :directory #p"/home/willijar/www/clews/modules/groups/*"))
(defvar *m* (modules *app*))
(defvar *c* (get-dictionary "EE4008-2008" *m*))
(aston::publish *app* "/coursework/")
|#



