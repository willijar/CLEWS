;;;; Aston Web Applications
;;;; Copyright (C) 2002-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; $Id: server.lisp,v 1.11 2005/03/10 19:54:25 willijar Exp $

(in-package :aston)

(defvar *hostname* (cond ((or (equal "edmund" (machine-instance))
                              (equal "eas-web" (machine-instance)))
                          "www.ee.aston.ac.uk")
                         ((equal "ee-dt07" (machine-instance))
                          "heisenberg.aston.ac.uk")
                         ("localhost")))

(defvar *base-url*
  (make-instance
   'inet.uri:url
   :scheme :http
   :port 8080
   :path "/"
   :hostname *hostname*)
  "Base url under which all content is served")

(defvar *server*
  (make-instance
   'inet.http:httpd
   :default-hostname (inet.uri:hostname *base-url*)
   :port (inet.uri:port *base-url*)
   :admin "J.A.R.Williams@aston.ac.uk")
  "Aston Server Instance")

(defun redirector(request rest)
  (declare (ignore request))
  (inet.http::make-response
   (cons 301
         (concatenate 'string "http://edmund.aston.ac.uk:8080/" rest))))

(inet.http:publish-handler #'redirector "/" *server*)

(setf (cdr
       (assoc :response
              (slot-value (car (slot-value *server* 'inet.http::hosts)) 'inet.http::handlers))
)
`(("/" (T ,#'redirector :prefix))))

