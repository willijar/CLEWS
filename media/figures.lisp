;; $Id$
;; Implementation for figures and math server
;; Copyright (C) 2006 Dr. John A.R. Williams
;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords: LaTeX, figures

;; This file is part of CLEWS

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;; A CLEWS web application using the docutils.figures image server

;;; Code:

(in-package :clews.media)

(defclass media-manager(application jarw.media::media-server)
  ((base-url-string :reader jarw.media::media-base-url))
  (:default-initargs
      :id :figures
    :acl '((:view :all)
           (:add . (:student :staff :admin))
           (:admin . (:admin))))
  (:documentation "Class for figure management system"))

(defmethod publish :after((app media-manager) base-url server)
  (unless *media-server* (setf *media-server* app))
  (setf (slot-value app 'base-url-string) (inet.uri:url-string base-url)))

(defmethod response-handler((server media-manager) request rest)
  (if (has-permission '(:view) server)
      (let ((variant-key (jarw.media::string-to-variant-key rest)))
        (if variant-key
            (inet.http::file-response request
                                      (media-variant-path variant-key server))
            :not-found))
      :forbidden))

(defun submit-handler(app request rest)
  (declare (ignore rest))
  ;(unless (has-permission '(:add) app)
  ;  (return-from submit-handler :unauthorized))
  (let* ((form '((markup:form :method "post")
                 (p (b "Latex expression: "))
                 (p ((markup:textarea :datatype (string :word-count 1)
                                      :name :latex :cols 80 :rows 5)))
                 (p ((markup:input :type :submit :name :submit
                                   :value "Submit" )))))
         (latex (getf (form-data form request) :latex)))
    `(html
      (head (markup:title "Math and Figures"))
      (body
       (h1 "Math and Figures")
       (p "Enter a latex expression below and press submit. The server
will convert it into a png file suitable for linking to from a web
page.")
       (div ,(markup-form form (if latex request nil)))
       (hr)
       ,@(when latex
               `((div)
                 (p "Please wait ...")
                 (br)
                 ((img :alt ,latex
                   :src ,(media-variant-url
                    (media-variant
                     (register-media latex app
                                     :content-type "text/x-eqn")
                     app "image/png")
                    app)))))))))

(defmethod published-methods ((app media-manager) baseurl)
  `(("." ,#'response-handler :stage :response :match :prefix
     :display-plugins-p t)
    ("submit" ,#'submit-handler :stage :response :match :exact)
    ,@(jarw.lib:when-bind(handler (clews::make-authentication-handler
                                   app baseurl))
         `(("submit" ,handler :stage :authentication :match :exact)))
    ,@(when (find-method #'log-handler nil (list (class-of app) t t) nil)
            `(("." ,#'log-handler :stage :log :match :prefix)))
    ,@(when (users app)
       `(("submit" ,#'authorization-handler :stage :authorization
          :match :exact)))))