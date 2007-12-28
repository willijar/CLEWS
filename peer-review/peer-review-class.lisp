(in-package :clews.peer-review)
;;; $Id: peer-review-class.lisp,v 1.1 2006/07/30 17:43:18 willijar Exp willijar $
;;;Peer review application class for CLEWS
;;;Copyright Dr John A.R. Williams (c) 2002. Please see LICENSE.txt

(defclass peer-review (application)
  ((articles :type dictionary
             :reader articles
             :initarg :article-dictionary
             :initform (make-hash-table :test #'equal)
             :documentation "Dictionary for storing article objects")
   (review-form :reader review-form
                :initarg :review-form
                :initform (find-form "peer-review-2000")
                :documentation "The form to be used for reviews.")
   (article-form :allocation :class
                 :initform (find-form "peer-review-article")
                 :documentation "The form used for adding/editing articles")
   (review-feedback-form :reader review-feedback-form
                         :initarg :review-feedback-form
                         :initform nil
                         :documentation
                         "The form to be used for review feedback.")
   (max-no-writeups
    :type integer :reader max-no-writeups :initform 2 :initarg :max-no-writeups
    :documentation
    "The maximum number of writeups allowed with the same title")
   (no-articles-required
    :type integer :reader no-articles-required
    :initform 10 :initarg :no-articles-required
    :documentation "Number of writeups required per student")
   (min-words
    :type integer :initform 1000 :reader min-words :initarg :min-words
    :documentation "Min word count for student articles")
   (review-ratio :type rational :reader review-ratio
                 :initform 3 :initarg :review-ratio
                 :documentation "How many reviews per article required")
   (max-no-reviews :type integer :reader max-no-reviews
                   :initform 5 :initarg :max-no-reviews
                   :documentation "Max no reviews allowed per article")
   (article-index :type list :initform nil :reader article-index
                  :documentation "Index of articles")
   (keyword-index :type keyword-index
                  :documentation "The search index"
                  :reader keyword-index)
   (image-directory :type pathname :reader image-directory
                    :initarg :image-directory
                    :documentation "Pathname to where images are to be stored")
   (math-cache :type hash-table :initform (make-hash-table :test #'equal)
               :reader math-cache
               :documentation "Caching index for rendered math")
   (menus :type list :reader menus :allocation :class
          :initform '((("."  "Home")
                       ("help"  "Help")
                       ("new" "New")
                       ( "search" "Search")
                       ("directory/A" "Directory")
                       ("preferences/" "Preferences") ))
          :documentation "Top level menus for this application")
   (reviewer-delay
    :type integer :initform 7200 :initarg :reviewer-delay
    :reader reviewer-delay
    :documentation
    "How long after an article is submitted before it is available to
review"))
  ;; permissions are fairly obvious.
  ;; :view as usual to be allowed to see the application
  ;; :student to be allowed to add articles and reviews etc
  ;; :tutor as :student but without limitations, and can see scores
  (:default-initargs
      :id :clews.peer-review
    :acl '((:view . (:all))
           (:student . (:all))
           (:tutor . nil )))
  (:documentation "Class for a peer review system"))

(defmethod article-ids((app peer-review))
  (dictionary-keys (articles app)))

(defmethod get-article((id string) (app peer-review))
  (get-dictionary id (articles app)))

(defmethod get-article((idx article-index-entry) (app peer-review))
  (get-dictionary (id idx) (articles app)))

(defmethod (setf get-article) ((new-value article)
                               (id string) (app peer-review))
  (setf (get-dictionary id (articles app)) new-value))

(defun generate-new-article-id(app)
  (loop
   (let ((new-id (format nil "~5,'0D" (random 99999))))
     (if (not (get-article new-id app)) (return new-id)))))

(defmethod article-form((app peer-review))
  (let* ((form (slot-value app 'article-form))
        (content-element (clews.form:get-form-element :content form)))
    (setf (clews.form::datatype content-element)
      `(string
        ,@(unless (has-permission :tutor app)
                  `(:word-count ,(min-words app)))
        :skip-return t))
    (setf (clews.form::default-value content-element)
          (format nil "Not less than ~D words" (min-words app)))
    form))
