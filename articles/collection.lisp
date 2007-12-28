;; $Id: collection.lisp,v 1.1 2007/07/26 08:53:56 willijar Exp willijar $
;; Articles and article collections implementation
;; Copyright (C) 2006 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of the CLEWS Article application library

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

(in-package :clews.articles)

(defgeneric get-article(id collection &key if-does-not-exist)
  (:documentation "Retrieve an article with given id from a
collection.  if-does-not exist may take the values :error which will
result in an error if there is no article, :create which will ctreate
a new article and return it or another value which will be returned if
there is no article"))

(defclass user-state(logfile-dictionary deferrable-dictionary)
  ()
  (:default-initargs :test 'equal)
  (:documentation "user state is stored as a logfile with deferred updates"))

(defmethod dictionary::dictionary-internalise
    ((external-key string) (external-value list) (dictionary user-state))
  (if external-value
      (values external-key
              (make-instance 'child-dictionary
                             :storage external-value
                             :key external-key
                             :parent dictionary))
      (call-next-method)))

(defmethod dictionary::dictionary-externalise
    ((key string) (value child-dictionary) (dictionary user-state))
  (values key (dictionary::storage value)))

(defclass article-collection(dictionary:dictionary clews::component)
  ((path :type pathname :initarg :path :reader root-path
         :documentation "Root Path")
   (article-class :type symbol :initform 'article :initarg :class
                  :reader article-class
                  :documentation "Class of article to be constructed")
   (article-file-type :initarg :file-type :type string :initform "rst"
                      :reader article-file-type)
   (articles :type hash-table :initform (make-hash-table :test #'equal)
             :reader articles)
   (user-state :type directory-dictionary)
   (document-reader
    :type docutils:reader
    :reader document-reader
    :documentation
    "document reader to be used for articles in this collection")
   (document-writers
    :initform nil
    :type docutils:writer
    :documentation
    "document writers to be used for articles in this collection")
   (document-settings
    :documentation "Cons of Settings for documents and when last read"))
  (:documentation "A collection of articles stored on the file system"))

(defun subdir(root subpath)
  (merge-pathnames
   (make-pathname :directory (append (pathname-directory root) subpath))
   root))

(defmethod initialize-instance :after ((collection article-collection)
                                       &key &allow-other-keys)
  (dolist(subdir '("articles" "media" "users"))
    (ensure-directories-exist
     (subdir (root-path collection) (list subdir))))
  (setf (slot-value collection 'document-reader)
        (document-reader (find-class (article-class collection))))
  (flet ((make-user-state(path)
           (make-instance 'user-state :file path)))
    (setf (slot-value collection 'user-state)
          (make-instance
           'directory-dictionary
           :directory (subdir (root-path collection) '("users"))
           :reader #'make-user-state
           :writer #'(lambda(value path) (declare (ignore value path)))
           :default #'make-user-state))))

(defmethod document-writer(format (collection article-collection))
  (or (getf (slot-value collection 'document-writers) format)
      (setf  (getf (slot-value collection 'document-writers) format)
             (document-writer format
                              (find-class (article-class collection))))))

(defgeneric path-to-articles(collection)
  (:method((collection article-collection))
    (subdir (root-path collection) '("articles"))))

(defgeneric path-to-media(collection)
  (:method((collection article-collection))
    (subdir (root-path collection) '("media"))))

(defmethod print-object((c article-collection) stream)
  (print-unreadable-object (c stream :type t :identity t)
    (format stream "~D articles at ~S"
            (length (dictionary-keys c))
            (root-path c))))

(defmethod dictionary-keys((collection article-collection))
  (mapcar ;; fix problem with simple-base-strings
   #'(lambda(p)  (map 'string #'character (pathname-name p)))
   (directory
    (merge-pathnames
     (make-pathname :name :wild :type (article-file-type collection))
     (path-to-articles collection)))))

(defmethod get-article((id string) (collection article-collection)
                       &key (if-does-not-exist :error))
  (let ((article (gethash id (articles collection)))
        (path (path-to-article id collection)))
    (unless (probe-file path)
      (remhash id (articles collection))
      (return-from get-article
        (case if-does-not-exist
          (:create (let ((new
                          (setf (gethash id (articles collection))
                                (make-instance (article-class collection)
                                               :id id
                                               :title id
                                               :collection collection))))
                     (update-record-from-instance new)
                     new))
          (:error (error 'article-not-found :collection collection :id id))
          (t if-does-not-exist))))
    (unless article
      (return-from get-article
        (setf (gethash id (articles collection))
                         (make-instance (article-class collection)
                                        :id id
                                        :collection collection))))
    (when  (< (instance-read-date article) (record-write-date article))
      (update-instance-from-record article))
    article))

(defmethod get-dictionary((key string) (instance article-collection)
                          &optional default)
  (get-article key instance :if-does-not-exist default))

(defmethod rem-dictionary(key (collection article-collection))
  (let ((article (get-article key collection :if-does-not-exist nil)))
    (when article
        (assert-permission :edit article)
        (delete-file (path-to-article key collection))
        (remhash key (articles collection)))))

(defmethod map-dictionary(func (collection article-collection))
  (dolist(name (dictionary-keys collection))
    (funcall func name (get-article name collection))))

(defmethod dictionary-count((collection article-collection))
  (length (dictionary-keys collection)))

(defmethod search-dictionary((regexp string) (collection article-collection))
  (let ((scanner (cl-ppcre::create-scanner regexp)))
    (search-dictionary
     #'(lambda(a) (matches scanner a))
     collection)))



