;;; $Id: publications.lisp,v 1.6 2005/05/16 20:19:46 willijar Exp $
;;; Copyright (c) 2004 Dr John A.R. Williams <J.A.R.Williams@aston.ac.uk>.
;;; Main class and handlers for the publications-manager class
;;; part of the EWAS Application for Managing Student Projects

;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 2
;;; of the License, or (at your option) any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

(in-package :clews.publications)

(defclass publications-manager (application)
  ((directory :type pathname :initarg :directory :reader publication-directory
              :documentation "Directory where publications are stored")
   (publications
    :type publications-table :reader publications :initarg :publications-source
    :documentation "Source of publications"))
  (:default-initargs
      :id :publications
    :authentication (list #'publications-authentication-handler)
    :acl '((:view . (:all))
           (:author . (:staff :fellow :author))
           (:admin . (:admin))))
  (:documentation "Class for project management system"))

(defmethod published-methods ((app publications-manager) baseurl)
  (declare (ignore baseurl))
  `(,@(call-next-method)
    ("journals/" ,#'journals-handler :stage :response :match :prefix)
    ("edit/" ,#'edit-publication-handler :stage :response :match :prefix)
    ("tsv/" ,#'tsv-handler :stage :response :match :prefix)
    ("files/" ,(publication-directory app)
		 :recurse nil
		 :file-allowed
		 ,#'(lambda(pathname)
		      (let ((type (pathname-type pathname)))
            (or (string-equal type "gif")
                (and
                 (get-dictionary (pathname-name pathname)
                                 (publications app))
                 (string-equal type "pdf"))))))
    ("admin/" ,#'admin-handler :stage :response :match :exact :role :admin)
    ("stats/" ,#'stats-handler :stage :response :match :exact :role :author)))

(defun publications-authentication-handler(app request rest params)
  (when (or (cookie  "authentication" request)
            (and (> (length rest) 0)
                 (not (is-prefix-p "files/" rest))))
    (cookie-authentication-handler app request rest params)))

(defmethod initialize-instance :after ((app publications-manager)
                                       &key &allow-other-keys)
  (when (and (publication-directory app) (not (slot-boundp app 'publications)))
    (setf (slot-value app 'publications)
          (make-instance
           'publications-file-table
           :acl (acl app)
           :file (merge-pathnames (publication-directory app) "publications")
           :journals (make-instance
                      'journals-file-table
                      :file (merge-pathnames
                             (publication-directory app)
                             "journals"))))))

(defun publication-view-allowed-p(app p)
  (or (has-permission '(:author :admin) app)
      (and (not (eql (getf p :citetype) :proceedings))
           (eql (getf p :status) :public))))

(defmethod response-handler((app publications-manager) request rest)
  (let* ((publications (publications app))
         (form (search-form publications))
         (action (submitted-action form request)))
    (multiple-value-bind (form-data condition) (form-data form request)
      (let ((results
             (if (and action (not condition))
                 (sort
                  (search-dictionary
                   (let ((test (table-search-func form-data publications)))
                     #'(lambda(p)
                         (when (publication-view-allowed-p app p)
                           (funcall test p))))
                   publications)
                  (table-sort-func form-data publications))
                 (mapcar
                  #'car
                  (subseq
                   (sort
                    (let ((recs nil))
                      (map-dictionary
                       #'(lambda(k p)
                           (declare (ignore k))
                           (when (publication-view-allowed-p app p)
                             (push (cons p (publication-date p publications))
                                   recs)))
                           publications)
                       recs)
                    #'> :key #'cdr) 0 25)))))
        `(html
          (head (title "Publications"))
          (body
           (navbar
            ((("stats/"
               ,(if *current-user* "Your publication status" "Author Login"))
              ,@(when (has-permission :admin app)
                      `(("admin/" "Administration")))
              ,@(when (has-permission :author app)
                      `(("bibtex" "BibTeX records")))
              ,@(when (has-permission :author app)
                      `(("tsv/article" "Journal TSV records")
                        ("tsv/inproceedings" "Conference TSV records")
                        ("tsv/inbook" "Book Chapter TSV records")
                        ("tsv/book" "Book TSV records")
                        ("tsv/patent" "Patent TSV records")))
              )))
           ((section :title ,"Search Publications")
            ,(markup-form (search-form (publications app))
                          (cond
                            (action
                             (when (and *current-user* (not condition))
                               (setf (property (user-component-properties
                                                app  *current-user*) :search)
                                     form-data))
                             request)
                            (*current-user*
                             (property (user-component-properties
                                        app  *current-user*) :search))))
            (hr)
            ,(unless action '(h2 "Some Recent Publications"))
            (p (em ,(length results) " results"))
            ,(when (has-permission :author publications)
                   '(p "Authors can click on the reference to edit a publication. Only published, checked by an editor are publically viewable."))
            (table
             ,@(mapcar
                #'(lambda(p)
                    `(tr ((td :align :right :valign :top)
                          ,(if (can-edit-publication p publications)
                               `((a :href ,(format nil "./edit/~A"
                                                   (getf p :id)))
                                 ,(getf p :id))
                               (getf p :id)) )
                      (td ,(display-publication p
                                                (when (equalp rest "bibtex")
                                                  :bibtex) publications))
                      ((td :align :right :valign :top)
                       ,(when (probe-file (publication-filename p app))
                              `((a :href ,(format nil "files/~A.pdf"
                                                  (getf p :id)))
                                ((img :alt "PDF" :src "files/pdf.gif")))) )))
                results)) )))))))

(defun journals-handler(app request id)
  (let* ((journals (journals (publications app)))
         (id (or (car (form-values :abbreviation request))
                 (when (> (length id) 0)
                   (inet.uri:uri-unescape id))))
         (journal (when id (get-dictionary id journals)))
         (form (table-form journals journal))
         (action (submitted-action form request)))
    (multiple-value-bind (form-data condition) (form-data form request)
      (when (and (has-permission :author app) action (not condition))
        (accept-form-data journals form-data)
        (setf journal (get-dictionary id journals)))
      (when (and id (not journal))
        (return-from journals-handler
          (cons :not-found (format nil "There is no journal ~A" id))))
      `(html
        (head (title "Journals"))
        (body
         (navbar
          ((("../" "Search Papers")
            ("../stats/" "Your papers")
            ("./" "Add a paper")
            ,@(when (has-permission :admin app)
                    `(("../admin/" "Administration"))) )))
         ((section :title ,"Journals")
          ,(when form
                 `((section :title ,(if journal
                                        (format nil "Edit journal ~S" id)
                                        "Add a new Journal"))
                   ,(when action
                          (if condition
                              '((p :class "error")
                                "Submission failed. Correct the errors below")
                              '(p (em "Submission Successful"))))
                   ,(markup-form form (if action form-data journal)
                                 (not (has-permission :author app)))))
          ,(when journal
                 `((section :title "Changes to journal" )
                   ,(changes-markup id journals)))
          (p "Clink on a link below to view or edit journal details")
          (table
           ,@(mapcar
              #'(lambda(v) `(tr (td ((a :href
                                        ,(inet.uri:uri-escape
                                          (getf v :abbreviation)))
                                     ,(getf v :abbreviation)))))
              (sort
               (search-dictionary #'identity journals)
               #'string< :key #'(lambda(v) (getf v :title))))))
         )))))

(defun edit-publication-handler(app request id)
  (let* ((publications (publications app))
         (action (car (form-values :id request)))
         (id (or action (when (> (length id) 0) id)))
         (publication (when id (get-dictionary id publications)))
         (form (table-form publications publication)))
    (multiple-value-bind (form-data condition) (form-data form request)
      (when (and (has-permission :author app) action (not condition))
        (accept-form-data publications form-data)
        (setf publication (get-dictionary id publications))
        (setf form (table-form publications publication)))
      (when (and id (not publication))
        (return-from edit-publication-handler
          (cons :not-found (format nil "There is no publication ~A" id))))
      `(html
        (head (title "Edit/Add Publication"))
        (body
         (navbar
          ((("../" "Search Papers")
            ("../stats/" "Your papers")
            ("./" "Add a paper")
            ("../journals/" "Journals")
            ,@(when (has-permission :admin app)
                    `(("../admin/" "Administration"))) )))
         ,(when form
                `((section :title ,(if publication
                                       (format nil "Edit ~A ~A"
                                               (getf publication :citetype) id)
                                       "Add a new publication"))
                  ,(when action
                         (if condition
                             '((p :class "error")
                               "Submission failed. Correct the errors below")
                             '(p (em "Submission Successful."))))
                  ,@(when publication
                          (let ((fields
                                 (missing-publication-fields publication publications)))
                            `((p (display-publication publication nil publications))
                              ,(if fields
                                   `((p :class :error) "Missing fields: "
                                     ,(format nil "~{~S ~}" fields) ". These fields must be completed.")
                                   '(p "Record complete.")))))
                  ,(markup-form form (if action form-data publication)
                                (not (has-permission :author app)))))
         ,(when id
                (let* ((filename (publication-filename publication app))
                       (file-exists (probe-file filename)))
                  `((section :title "PDF File")
                    ,(let ((data (car (form-values "file" request))))
                          (when data
                            (with-open-file (os filename
                                                :direction :output :if-exists :supersede)
                              (write-sequence data os))
                            `(p "File " ,(if file-exists "Replaced" "Uploaded"))))
                    ,(if (probe-file filename)
                         `((a :href ,(format nil "../files/~A.pdf" id))
                           "Current PDF file")
                         '(p "No file stored."))
                    ((form :method "POST" :enctype "multipart/form-data")
                     (p "File: " ((input :name "file" :size 40 :type "file"
                                         :accept "pdf"))
                      ((input :type "submit" :name "Add" :value "Upload file")))))))
         ,(when publication
                `((section :title "Changes to record" )
                  ,(changes-markup id publications))))) )))

(defun publication-filename(publication app)
  (merge-pathnames (publication-directory app)
                   (make-pathname :name (getf publication :id)
                                  :type "pdf")))
#|
(defmethod files-handler((app publications-manager) request rest)
  (let ((filename (merge-pathnames (publication-directory app) rest)))
    (unless (and (pathname-name filename)
		 (probe-file filename)
		 (or (string-equal (pathname-type filename) "gif")
		     (and
		      (get-dictionary (pathname-name filename)
				      (publications app))
		      (string-equal (pathname-type filename) "pdf"))))
      (error 'http-error :code 404))
    (httpd::send-file request filename :content-type
		      (if (string-equal (pathname-type filename) "gif")
			  "image/gif"
			  "application/pdf"))))
|#

(defun stats-handler(app request rest)
  (declare (ignore request rest))
  (let* ((publications (publications app))
         (records
          (mapcar
           #'(lambda(p) (setf (getf p :date)
                              (publication-date p publications)) p)
           (search-dictionary
            #'(lambda(p) (is-publication-author p publications))
            publications))))
    (labels ((sort-func(a b) (> (getf a :date) (getf b :date)))
             (row(p) `(tr ((td :align :right :valign :top)
                           ((a :href ,(format nil "../edit/~A"
                                              (getf p :id)))
                            ,(getf p :id)) )
                       (td ,(display-publication p nil publications))
                       ((td :align :right :valign :top))))
             (table(condition)
               (let ((subset
                      (sort
                       (mapcan
                        #'(lambda(p) (when (funcall condition p) (list p)))
                        records)
                       #'sort-func)))
                 `(table
                   (tr ((td :colspan 2) (em ,(length subset) " results")))
                   ,@(mapcar #'row subset)))))
      `(html
        (head (title "Your Publication Status"))
        (body
         (navbar
          ((("../" "Search Papers")
            ("../edit/" "Add a paper")
            ("../journals/" "Journals")
            ,@(when (has-permission :admin app)
                    `(("../admin/" "Administration"))))))
         ((section :title ,(format nil "Publication Status for ~S"
                                   (display-name *current-user*)) )
          (p (em ,(length records) " records in total."))
          ((section :title "Papers published but not yet pulic")
           (p "These must be checked by an editor to be made public.")
           ,(table #'(lambda(p) (eql (getf p :status) :published))))
          ((section :title "Papers accepted but not yet published")
           (p "Update information as soon as you have the complete
publication details on the paper")
           ,(table #'(lambda(p) (eql (getf p :status) :accepted))))
          ((section :title "Papers submitted but not yet accepted")
           (p "Update acceptance/rejection status as soon as the editors have informed you of their decision.")
           ,(table #'(lambda(p) (eql (getf p :status) :submitted))))
          ((section :title "Papers in progress but not yet submitted")
           ,(table #'(lambda(p) (not (getf p :status)))))
          ((section :title "Published Public Papers")
           ,(table #'(lambda(p) (eql (getf p :status) :public))))
          ((section :title "Rejected Papers")
           ,(table #'(lambda(p) (eql (getf p :status) :rejected))))  ))))))

(defun admin-handler(app request rest)
  (declare (ignore request rest))
  (let* ((publications (publications app))
         (records
          (mapcar
           #'(lambda(p) (setf (getf p :date)
                              (publication-date p publications))
                    p)
           (search-dictionary
            #'(lambda(p) (can-edit-publication p publications))
            publications))))
    (labels ((sort-func(a b) (> (getf a :date) (getf b :date)))
             (row(p) `(tr ((td :align :right :valign :top)
                           ((a :href ,(format nil "../edit/~A"
                                              (getf p :id)))
                            ,(getf p :id)) )
                       (td ,(display-publication p nil publications))
                       ((td :align :right :valign :top))))
             (table(condition)
               (let ((subset
                      (sort
                       (mapcan
                        #'(lambda(p) (when (funcall condition p) (list p)))
                        records)
                       #'sort-func)))
                 `(table
                   (tr ((td :colspan 2) (em ,(length subset) " results")))
                   ,@(mapcar #'row subset)))))
      `(html
        (head (title "Publications Administration"))
        (body
         (navbar
          ((("../" "Search Papers")
            ("../edit/" "Add a paper")
            ("../stats/" "Your papers"))))
         ((section :title "Administration")
          ((section :title ,"Papers Published but not yet public")
           ,(table #'(lambda(p) (eql (getf p :status) :published))))
          ((section :title ,"Papers accepted but not yet published")
           ,(table #'(lambda(p) (eql (getf p :status) :accepted))))
          ((section :title ,"Papers submitted but not yet accepted")
           ,(table #'(lambda(p) (eql (getf p :status) :submitted))))
          ((section :title "Papers in progress but not yet submitted")
           ,(table #'(lambda(p) (not (getf p :status)))))))))))

(defun tsv-handler(app request rest)
  (declare (ignore request))
  (let* ((publications (publications app))
         (citetype (intern (string-upcase rest) :keyword)))
    (unless (member citetype *tsv-mappings* :key #'car)
      (return-from tsv-handler (cons :not-found (format nil "No TSV output available for ~S" citetype))))
    (make-instance
     'inet.http:response :status 200
     :content-type "text/plain"
     :content
     (with-output-to-string(os)
       (dolist(rec
                (sort
                 (search-dictionary
                    #'(lambda(p) (and (is-publication-author p publications)
                                      (eql (citetype p) citetype)
                                      (getf p :year)
                                      (> (getf p :year) 2000)))
                    publications)
                 #'<
                 :key #'(lambda(a) (publication-date a publications))))
         (write-string (display-publication rec :tsv publications) os))))))
