;;;; Group assessment entity -*- Lisp -*- Copyright (C) 2008
;;;; John A.R. Williams <J.A.R.Williams@jarw.org.uk> Released under
;;;; the GNU General Public License (GPL) See
;;;; <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;

(in-package :clews.coursework)

(defclass component(self-describing-component)
  ((deadline :accessor deadline
             :documentation "Deadline date or nil for none")
   (weighting :initform 1 :type number :initarg :weighting :reader weighting
              :documentation "Relative weighting of this coursework")
   (review-elements :type list :initarg :review-elements :reader review-elements
                    :documentation "List of descriptions and markup types")
   (accept-files-p :initarg :accept-files :initform nil
                   :reader accept-files
                   :documentation "If true student file uploads will go here"))
  (:documentation "Class representing courseworks for submitted work"))

(defmethod initialize-instance :after ((component component)
                                       &key deadline)
  (setf (slot-value component 'deadline) (parse-input 'date deadline)))

(defgeneric component-form(component)
  (:documentation "Return the form markup for a given component")
  (:method((component component))
    `((form :method :post)
      ,(review-elements component)
      ((p :align :center)
       ((:input :type :submit :name ,(id component) :value "Submit"))))))

(defgeneric component-mark(controller component data)
  (:documentation "Given data return the mark for this component")
  (:method(controller (component component) data)
    (declare (ignore controller))
    (form-mark data (component-form component))))

(defgeneric component-completed-p(component data)
  (:documentation "Return true if componet has been completed")
  (:method((component component) data) (when data t)))

(defclass group-component(component)
  ())

(defclass individual-component(component)
  ())

;; (defun system-path(pathname)
;;   "Return a pathname where the type also contains the verion number - for
;;    file systems without version control"
;;   (merge-pathnames
;;    (make-pathname
;;     :type (format nil "~A,~D"
;;                   (pathhname-type pathname)
;;                   (pathname-version pathname)))
;;    pathname))

;; (defun lisp-path(pathname)
;;   "Given a physical pathname with a type engind ,version return the correctly coded lisp-pathname"

;;   (let* ((type (pathname-type pathname))
;;          (p (position #\, type)))
;;     (if p
;;         (merge-pathnames
;;          (make-pathname :type (subseq type 0 p) :version (parse-integer (subseq type (1+ p))))
;;          pathname)
;;         pathname)))

;; (defun directory-all-versions(pathname)
;;   (mapcar #'lisp-path (directory pathname)))

;; (defun directory-recent-versions(pathname)
;;   (let ((paths nil)) ; path strings and versions
;;     (dolist(path (directory-all-versions pathname))
;;       (let ((name (namestring path))
;;             (version (pathname-version path))
;;             (previous (assoc name paths :key #'car :test #'string-equal)))
;;         (if previous
;;             (when (> version (cdr previous)) (setf (cdr previous) version))
;;             (push (cons name version paths)))))
;;     (mapcar
;;      #'(lambda(entry)
;;          (let ((pathname (pathname (car entry))))
;;            (setf (pathname-version pathname) (cdr entry))
;;            pathname))
;;      paths)))
