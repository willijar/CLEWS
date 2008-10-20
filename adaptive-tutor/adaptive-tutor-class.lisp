;;;; Base Adaptive Tutor class - implementation
;;;; Copyright (C) 2002-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; $Id: adaptive-tutor-class.lisp,v 1.1 2006/07/30 17:41:29 willijar Exp willijar $

(in-package :clews.adaptive-tutor)

;; knowledge domain data stored in a hash of string ids and concept objects
;; this means that concepts can be referred to before the actual
;; concept-assoc is created

(defvar *adaptive-tutor* nil
  "Tutorial object being processed for new concepts")

(defclass adaptive-tutor(application)
  ((concept-map :type hash-table
                :reader concept-map
                :initform (make-hash-table :test #'equalp)
                :documentation "The concept map")
   (bibliography :type hash-table
                 :initform (make-hash-table :test #'equalp)
                 :reader bibliography
                 :documentation "The bibliography")
   (index :type keyword-index
          :initform (make-instance 'keyword-index)
          :reader keyword-index
          :documentation "maps keywords to concepts")
   (references ::type hash-table
               :initform (make-hash-table :test #'equalp)
               :reader references
               :documentation "table mapping labels to content")
   (menus :type list :reader menus :allocation :class
          :initform '((("."  "Home / Logout")
                       ("help"  "Help")
                       ("search" "Search")
                       ("directory/" "Directory")
                       ("bibliography/" "Bibliography")
                       ("preferences/" "Preferences")))))
  (:default-initargs :id :adaptive-tutor)
  (:documentation "Adaptive Tutor Class"))

(defmethod concept((tutor adaptive-tutor) id &key (if-not-exist :error))
  "Return concept association for concept with given name.
if-not-exist specified what to do if not concept of this name
has yet been created, either flag an :error, :ignore it and return nil
or create the concept instance."
  (declare (type (or symbol string) id)
           (type (member :error :create :ignore) if-not-exist))
  (let ((id (string-downcase id)))
    (multiple-value-bind (c present-p)
        (get-dictionary id (concept-map tutor))
      (cond (present-p c)
            ((eq if-not-exist :error)
             (cerror "Create new concept ~s"
                     "No concept ~S defined." id)
             (concept tutor id :if-not-exist :create))
            ((eq if-not-exist :create)
             (write-log :load-concept "  Creating concept ~S~%" id)
             (setf (get-dictionary id (concept-map tutor))
                   (make-instance 'concept :concept-id id) ))))))

(defmethod concepts((tutor adaptive-tutor))
  "return a list of all of the concepts in the tutor"
  (let (c)
    (map-dictionary
     #'(lambda (key p) (declare (ignore key)) (push p c))
     (concept-map tutor))
    c))

(defvar *path-to-root* "."
  "Path from current place to top level")

(defmacro with-tutor ((tutor) &body body)
  "Macro sets up dynamic context for defining concepts
   by binding special variables
   the tutor is established as the default destination for concepts
   definined in the body. Addionally some other special control variables
   are set via  keys e.g."
  `(let ((*adaptive-tutor* ,tutor))
    (declare (special *adaptive-tutor* *path-to-root*))
    (with-markup-environment
        (:references (references ,tutor)
                     :section-level 0
                     :file-prefix (concatenate 'string *path-to-root* "/files/")
                     :bibliography (bibliography ,tutor)
                     ))
      ,@body))

(defmethod load-concepts((path pathname) (tutor adaptive-tutor)
                         &key (run-external-programs t))
  (with-markup-environment
      (:run-external-programs run-external-programs
                              :source-path path
                              :rerendered-math nil)
    (with-tutor (tutor)
      (with-structured-text-syntax
        (mapc #'(lambda(fname)
                  (write-log :load-concept "~%Loading concept file~% ~S~%"
                             fname)
                  (load fname :verbose nil))
              (directory path))))))

(defmethod compile-concepts((path pathname) (tutor adaptive-tutor)
                            &key (run-external-programs t))
  (with-markup-environment (:run-external-programs run-external-programs
                                                   :source-path path)
    (with-tutor (tutor)
      (mapc #'(lambda(fname)
                (write-log :load-concept "~%Compiling concept file ~S~%" fname)
                (compile-file fname :verbose nil))
            (directory path)))))

(defmethod published-methods ((app adaptive-tutor) baseurl)
  (declare (ignore baseurl))
  `(,@(call-next-method)
    ("search" ,#'search-handler :stage :response :match :prefix)
    ("bibliography/" ,#'bibliography-handler :stage :response :match :prefix)
    ("directory/" ,#'directory-handler :stage :response :match :prefix)))

(defmethod initialize-instance :after ((app adaptive-tutor) &key)
  "Install the server handlers for the Adaptive Tutorial"
  (push (make-instance 'status-plugin :tutor app)
        (slot-value app 'clews::plugins)))

