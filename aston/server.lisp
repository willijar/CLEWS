;;;; Aston Web Applications
;;;; Copyright (C) 2002-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; $Id: server.lisp,v 1.11 2005/03/10 19:54:25 willijar Exp $

(in-package :aston)

(defvar *hostname*
  (cond ((equal "cld045026" (machine-instance)) "heisenberg.aston.ac.uk")
        ("localhost")))


(defvar *base-url*
  (make-instance
   'url
   :scheme :http
   :port 8080
   :path "/"
   :hostname *hostname*)
  "Base url under which all content is served")

(defvar *server*
  (make-instance
   'httpd
   :default-hostname (hostname *base-url*)
   :port (port *base-url*)
   :timeout 60
   :max-requests 25
   :max-connections 25
   :admin "J.A.R.Williams@aston.ac.uk"
   :stderr (translate-logical-pathname #p"clews:error.log")
   :stdlog (translate-logical-pathname #p"clews:clews.log"))
  "Aston Server Instance")

(defun start()
  (stop-server *server*)
  (reload)
  (start-server *server*))

(defun publish(app path)
  (clews:publish app (merge-url *base-url* path) *server*))

(defun reload()
  (publish-handler (translate-logical-pathname #p"clews:static;")
                   (merge-url *base-url* "/static/")
                   *server*)
  (publish-handler (translate-logical-pathname #p"clews:static;favicon.ico")
                   (merge-url *base-url* "/favicon.ico")
                   *server*)
  (publish (make-instance 'aston-directory :id :aston-directory) "/index/")
  ;(publish *tutorial* "/adaptive-tutorial/")
  (publish *tutorial-app* "/tutorials/")
  (publish *discussions* "/discussions/")
  (publish *assessments* "/assessments/")
;;  (publish *cvs* "/cv/")
  (publish *peer-review* "/peer-review/")
  (publish *cs-peer-review* "/cs-peer-review/")
  (publish *publications* "/publications/")
  #+nil(publish *beng-projects* "/BEng-projects/")
  #+nil(publish *meng-projects* "/MEng-projects/")
  (publish *passwd* "/pwd/")
  (publish *aston-forms* "/forms/")
  (publish jarw.media:*media-server* "/media/")
  (publish *grades* "/MSc/")
  (publish *msc-projects* "/MSc/projects/")
  ;;  (load-tutorials)
  (load-forms (translate-logical-pathname #p"clews:forms;"))
  (inet.http::publish-handler #'lab-page "/labs/"  *server* :match :exact) )

(defun stop()
  (stop-server *server*))

(defun sigreload(signal code scp)
  (declare (ignore signal code scp))
  (format t "Reloading at ~A~%" (format-time nil))
  (reload)
  (format t "Reloading Completed~%"))

(defun sigstop(signal code scp)
  (declare (ignore signal code scp))
  (stop)
  (cl-user::quit))

(defun sigpipe (a b c)
  (declare (ignore a b c))
  (warn "Aborting Connection"))

#+cmu(system:enable-interrupt Unix:SIGPIPE #'sigpipe)
#+sbcl(sb-sys:enable-interrupt sb-unix:sigpipe #'sigpipe)
#+sbcl(sb-sys:enable-interrupt sb-unix:sighup #'sigreload)

(eval-when(:load-toplevel :execute) (reload))

(defun con() (slot-value *server* 'inet.server::connections))
(let ((old-con nil))
  (defun conmon()
    (loop
     (let ((con (slot-value *server* 'inet.server::connections)))
       (unless (equal old-con con)
         (setf old-con con)
         (princ con) (terpri) (terpri))
       (sleep 1)))))
