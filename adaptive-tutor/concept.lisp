;;;; Adaptive Tutor Concepts
;;;; Copyright (C) 2002-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; knowledge domain data stored in a hash of string ids and concept objects
;;;; this means that concepts can be referred to before the actual
;;;; concept-assoc is created
;;;; $Id: concept.lisp,v 1.1 2006/07/30 17:39:46 willijar Exp $

(in-package :clews.adaptive-tutor)

(defmacro defconcept(id properties content &optional assessment)
  "Define a concept. properties are quoted, content is quoted unless the first element is lambda and assessment is evaluated"
  `(make-concept
    ,id
    ',properties
    ,(if (or (eq (first content) 'function)
             (eq (first content) 'lambda))
         content `(quote ,content))
    ,(when assessment `
           (make-instance ',(first assessment)
                          ,@(rest assessment)))))

(defun make-concept(id properties content assessment &optional
                    (tutor *adaptive-tutor*))
  "create a new concept of given named id, prerequisits, children and outcomes
   prerequisits and outcomes are alists of concept names and floating point
   weighting factors. children is a list of required children names."
  (write-log :load-concept "  Defining concept ~S~%" id)
  (let ((self (concept tutor id :if-not-exist :create)))
                                        ; reinitialise slots for redefinition
    (dolist (slot '(prerequisites outcomes children))
      (setf (slot-value self slot ) nil))
    (with-new-document id
      (etypecase content
        (function (mapc #'preprocess-html  (funcall content))
                  (setf (content self) content))
        (cons (setf (content self) (mapcar #'preprocess-html content)))
        (string (setf (content self)
                      (preprocess-html `(structured-text ,content))))
        (null (setf (content self) nil))))
    (dolist (p (getf properties :prerequisites))
      (setf (direct-prerequisite self)
            (cons (concept tutor (car p) :if-not-exist :create)
                  (cdr p))))
    (dolist (child (reverse (getf properties :children)))
      (setf (direct-child self)
            (concept tutor child :if-not-exist :create)))
    (dolist (o (getf properties :outcomes))
      (setf (direct-outcome self)
            (cons (concept tutor (car o) :if-not-exist :create) (cdr o))))
    (setf (slot-value self 'assessment) assessment)
    (loop for item on properties by #'cddr
          unless (member (car item) '(:prerequisites :outcomes :children))
          do (setf (property self (first item)) (second item)))
    (setf (property self :load-time) (get-universal-time))
    (setf (property self :source-path) markup::*source-path*)
    (reindex-concept self tutor)
    (setf (get-dictionary id (references tutor))
          (list (or (property self :link-text)
                    (property self :title))
                (string-downcase id)
                #+nil(base-urlstring tutor
                                     (concatenate 'string
                                                  "/" (string-downcase id)))))
    self))

(defmethod reindex-concept((concept concept) (tutor adaptive-tutor))
  "Ensure we are correctly indexed by keywords"
  (index (keyword-index tutor) concept))

;public accessor functions get a particular element in concept cons
(defun direct-prerequisite(self other)
  "returns the prerequisit cons element of self if other
   is a direct prerequisite of self, otherwise nil"
  (cdr (assoc other (direct-prerequisites self))))

(defun direct-child(self other)
  "returns the child concept element if concept
   is a direct prerequisite of self, false otherwise"
  (declare (type concept self other))
  (find other (direct-children self)))

(defun direct-outcome(self other)
  "returns the outcome cons element of self if concept
   is a direct outcome of self, false otherwise"
  (declare (type concept other self))
  (or (cdr (assoc other (direct-outcomes self)))
      (when (eq self other)
        (if (direct-children self) 0.0 1.0))))

;precepts checking for relationships between concepts
(defmethod is-prerequisite ((self concept) (other concept))
  "Returns true if other is a prerequisite of self
   either directly or indirectly"
  (when (or (direct-prerequisite self other)
            (some (lambda (p) (is-prerequisite (car p) other))
                  (direct-prerequisites self)))
    t))

(defmethod is-child ((self concept) (other concept))
  "Returns true if other is a child of self
   either directly or indirectly"
  (when (or (direct-child self other)
            (some (lambda (p) (is-child p other))
                  (direct-children self)))
    t))

(defmethod is-outcome ((self concept) (other concept))
  "Returns true if other is an outcome of self
   either directly or indirectly"
  (when (or (direct-outcome self other)
            (some (lambda (p) (is-outcome (car p) other))
                  (direct-outcomes self)))
    t))

(defmethod is-allowed-prerequisite ((self concept) (other concept))
  "return true if other is allowed as a prerequisite for self"
  (not (or (eq self other)
           (is-prerequisite other self)
           (is-child self other))))

(defmethod is-allowed-child ((self concept) (other concept))
  "true if other is  allowed as a child for self"
  (not (or (eq self other)
           (is-child other self)
           (is-prerequisite self other))))

(defmethod is-allowed-outcome ((self concept) (other concept))
  "true if other is allowed as an outcome for self"
  t)

; methods to set relationships between concepts
(defmethod (setf direct-prerequisite) (p (self concept))
  "Set p as a prerequsite (probability relationship) of self"
  (declare (type probability-relationship p))
  (unless (is-allowed-prerequisite self (car p))
    (error "~S is not allowed as a prerequisite of ~S"
           (concept-id (car p))
           (concept-id self) ))
  (push p (slot-value self 'prerequisites)))

(defmethod (setf direct-child) ((child concept) (self concept))
  "Adds child as a child of self."
  (unless (is-allowed-child self child)
    (error "~S is not allowed as a child of ~S"
           (concept-id child)
           (concept-id self) ))
  (pushnew child (slot-value self 'children)))

(defmethod (setf direct-outcome) (o (self concept))
  "Adds  o as an outcome (probability relationship) of self."
  (declare (type probability-relationship o))
  (unless (is-allowed-outcome self (car o))
    (error "~S is not allowed as an outcome of ~S"
           (concept-id (car o))
           (concept-id self) ))
  (push  o (slot-value self 'outcomes)))

; methods to get and set concept properties, and store defaults etc
; note that concept properties don't have to be explicitely defined however
; it is recommended, and at the very least, you should define a default value
; This idiom should probable be factored out into a pattern

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((concept-property-definitions '() ))

    (defun def-concept-property (name default &optional documentation example)
      "Used to define and document a new concept property, its default
value, a description of it and en example definition"
      (pushnew (list name default documentation example)
               concept-property-definitions))

    (defun concept-property-default(name)
      "Return the default value for a given concept property or NIL"
      (second (assoc name concept-property-definitions)))

    (defun concept-property-documentation(name)
      "Return the documentation string for given concept property or NIL"
      (third (assoc name concept-property-definitions)))

    (defun concept-property-example(name)
      "Return an example definition for a given concept property or NIL"
      (fourth (assoc name concept-property-definitions)))

    (defun all-concept-properties()
      "Return a list of all of the defined concept properties"
      (mapcar #'car concept-property-definitions))))

; TODO: should also include prequisites of children which aren't also direct
; children

(defmethod prerequisites((self concept))
  "return a list with all prequisite concepts for self
   and their calculated weights"
  (let ((result-list (copy-alist (direct-prerequisites self))))
    (labels ((accumulate-weight (c w)
               (let ((a (assoc c result-list)))
                 (if a (rplacd a (- 1 (* (- 1 (cdr a)) (- 1 w))))
                     (push (cons c w) result-list)))))
      (dolist (p (direct-prerequisites self))
        (dolist (pp (prerequisites (car p)))
          (accumulate-weight (car pp)
                             (* (cdr p) (cdr pp))))))
    result-list))

(defmethod children((self concept))
  "returns all the required children (direct and indirect) for self
   in proper depth first order"
  (mapcan #'(lambda (c) (cons c (children c)))
          (direct-children self)))

(defmethod assessment-p((self concept))
  "True if concept or its children has assessment available "
  (or (assessment self) (some #'assessment (children self))))

(defmethod direct-outcomes-for((tutor adaptive-tutor) (self concept))
  "return a list of all of those concepts which have a direct outcome for
   self, together with their weights"
  (mapcan #'(lambda (other)
              (let ((a (cdr (assoc self (direct-outcomes other)))))
                (when a (list (cons other a)))))
          (concepts tutor)))

(defmethod outcomes-for((tutor adaptive-tutor) (self concept))
  "returns a list of all concepts, direct and indirect, which can contribute
  to c together with their weights, including self"
  (let ((outcomes ())                   ; accumulate the outcomes here
        (stack ()))   ; stack used to prevent circular reference loops
    (labels ((collect-outcomes (c w)
               (unless (find c stack)
                 (push c stack)
                 (let ((a (assoc c outcomes)))
                   (if a (rplacd a (- 1 (* (- 1 w)
                                           (- 1 (cdr a)))))
                       (push (cons c (* w (direct-outcome c c))) outcomes)))
                 (dolist (rec (direct-outcomes-for tutor  c))
                   (collect-outcomes (car rec) (* w (cdr rec))))
                 (pop stack))))
      (collect-outcomes self 1))
    (rplacd (assoc self outcomes)  (direct-outcome self self))
    outcomes))

(defun concept-content(tutor concept user request)
  (declare (ignore tutor))
  (let ((content (content concept)))
    (etypecase content
      (null '(p "Concept has not been defined yet"))
      (cons content)
      (function
       (funcall content concept user
                #'(lambda(fieldname)
                    (form-values fieldname request)))))))

(defmethod ancestors((ancestor concept)(child concept))
  "Returns the first sequence of children leading from ancestor
    concept to child
   concept (excluding child) or nil if no such path exists"
  (let ((stack ()))                     ; stack used to acculate paths
    (labels ((recurse-children (ancestor)
               (cond ((eq ancestor child) t)
                     (t (push ancestor stack)
                        (dolist (ancestor (direct-children ancestor))
                          (when (recurse-children ancestor) (return t))
                          (pop stack)
                          nil)))))
      (when (recurse-children ancestor) stack))))

(defmethod html(stream (tag (eql 'concept-content))
                &optional attrs content)
  "New tag to allow referencing or reuse of content sepersate from
the originating concept e.g. without its children"
  (let* ((concept (concept *adaptive-tutor*
                           (first content) :if-not-exist :ignore))
         (content (content concept)))
    (if concept
        (html stream `(,(if (getf attrs :section t)
                            `(section :title ,(concept-title concept))
                            'div)
                       ,@(when (and (getf attrs :summary t)
                                    (property concept :summary))
                               `((p (em ,(property concept :summary)))))
                       ,@(when (consp content) content)))
        (html stream 'p '(:class :error)
              `("Content referenced from non existent concept \""
                ,(first content)"\"")))))

(defmethod latex(stream (tag (eql 'concept-content))
                 &optional attrs content)
  "New tag to allow referencing or reuse of content sepersate from
the originating concept e.g. without its children"
  (let* ((concept (concept *adaptive-tutor*
                           (first content) :if-not-exist :ignore))
         (content (content concept)))
    (if concept
        (latex stream `(,(if (getf attrs :section t)
                             `(section :title ,(concept-title concept))
                             'div)
                        ,@(when (and (getf attrs :summary t)
                                     (property concept :summary))
                                `((p (em ,(property concept :summary)))))
                        ,@(when (consp content) content)))
        (latex stream 'p '(:class :error)
               `("Content referenced from non existent concept \""
                 ,(first content)"\"")))))

(defun tree-heirarchy(ancestor child)
  "Returns a tree giving the folder view from ancestor to child. Customise
   this if you want a different view."
  (let* ((ancestors (if ancestor (nreverse (ancestors ancestor child))
                        (list child)))
         (parent (first (last ancestors)))
         (peers (when parent (direct-children parent)))
         (children (mapcar #'(lambda (c) (cons c nil))
                           (direct-children child))))
    (if ancestors             ; no ancestors means we are at top level
        (reduce #'list (butlast ancestors)
                :from-end t ;; idiom to create a tree from a list
                :initial-value
                (cons parent
                      (if peers
                          (mapcar #'(lambda(c)
                                      (cons c (when (eq c child) children)))
                                  peers)
                          (cons child children))))
        (cons ancestor children) )))

(defun remove-circularity(topnode get-children &key (key #'car))
  "Operate on a circular tree of relationships . topnode is the object
to start from and get-children is the function which when called on an
object will return the children nodes. returns a tree"
  (let ((stack ()))   ; stack used to prevent circular reference loops
    (labels ((collect-children (c)
               (let ((key (funcall key c)))
                 (unless (member key stack)
                   (push key stack)
                   (let ((r (cons c (mapcan
                                     #'(lambda(child)
                                         (let ((tree (collect-children child)))
                                           (when tree (list tree))))
                                     (funcall get-children c)))))
                     (pop stack)
                     r)))))
      (collect-children topnode))))


