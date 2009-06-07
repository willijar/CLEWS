;; $Id: tutorial-articles.lisp,v 1.1 2007/07/26 08:55:10 willijar Exp willijar $
;; Tutorial Articles
;; Copyright (C) 2006 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of CLEWS Articles

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;;

;;; Code:

(in-package :clews.articles)

(cl-ppcre:define-parse-tree-synonym article-id
    (:sequence skip-spaces (:register (:alternation token quoted-string))))

(cl-ppcre:define-parse-tree-synonym article-id-weight
    (:sequence article-id skip-spaces #\= skip-spaces
               (:register
                (:sequence (:greedy-repetition 1 nil :digit-class)
                           (:greedy-repetition 0 nil #\.)
                           (:greedy-repetition 0 nil :digit-class)))))

(cl-ppcre:define-parse-tree-synonym deadlines
    (:sequence role skip-spaces #\= skip-spaces
               (:register
                 (:greedy-repetition
                  10 nil
                  (:char-class :digit-class #\+ #\- #\space #\: #\.)))
               skip-spaces
               (:alternation :end-anchor #\;)))

(defmethod parse-input((spec (eql 'articles)) (value string)
                                   &key &allow-other-keys)
  (let ((ids nil))
    (cl-ppcre:do-register-groups (id) ('article-id value)
      (push (unquoted id) ids))
    (nreverse ids)))

(defmethod parse-input((spec (eql 'article-weights)) (value string)
                                   &key &allow-other-keys)
    (let ((weights nil))
      (cl-ppcre:do-register-groups (id weight) ('article-id-weight value)
        (push (cons (unquoted id) (parse-number  weight))
              weights))
      (nreverse weights)))

(defmethod format-output ((spec (eql 'articles)) (value list)
                                      &key &allow-other-keys)
  (with-output-to-string(os)
    (dolist(item value)
      (unless (eql item (first value)) (write-char #\newline os))
      (write-string  (quoted item) os))))

(defmethod format-output ((spec (eql 'article-weights)) (value list)
                                      &key &allow-other-keys)
  (with-output-to-string(os)
    (dolist(item value)
      (unless (eql item (first value)) (write-char #\newline os))
      (format os "~A=~D;" (quoted (car item)) (cdr item)))))

(defmethod parse-input ((spec (eql 'deadlines)) (value string)
                        &key (fmt :short) &allow-other-keys)
  (let ((deadlines nil))
    (cl-ppcre:do-register-groups (role date) ('deadlines value)
      (let ((role (if (eql (elt role 0) #\")
                      (subseq role 1 (1- (length role)))
                      (intern (string-upcase role) :keyword)))
            (date (parse-input 'jarw.parse::date date :fmt fmt)))
        (push (cons role date) deadlines)))
    (nreverse deadlines)))

(defmethod format-output((spec (eql 'deadlines)) (value list)
                         &key (fmt :short) &allow-other-keys)
  (with-output-to-string(os)
    (dolist(item value)
      (unless (eql item (first value)) (write-char #\newline os))
      (format os "~:[~A~;~S~]=~A;" (stringp (car item)) (string (car item))
              (format-output 'jarw.parse::date (cdr item)
                             :fmt fmt)))))

(defclass tutorial-article(article)
  ((prerequisites-field
    :type list
    :format (article-weights :unbound-if-nil t)
    :field-name "prerequisites"
    :documentation  "parsed field of direct prerequisites")
   (outcomes-field
    :type list
    :format article-weights :field-name "outcomes"
    :documentation "Parsed field of direct-outcomes")
   (children-field  :type list
             :format (articles :unbound-if-nil t) :field-name "children"
             :documentation "parsed field of children")
   (time-required :type integer
                  :field-name "time-required"
                  :format (integer :nil-allowed t :unbound-if-nil t)
                  :documentation "Time which must be spent on this
page to consider it completed.")
   (deadlines
    :type list :format (deadlines :unbound-if-nil t)  :field-name "deadlines"
    :documentation "Mapping of usernames or roles to deadline dates")
   (feedback-dates
    :type list :format (deadlines :unbound-if-nil t)
    :field-name "feedback-dates"
    :documentation "Mapping of usernames or roles to feedback dates")
   #+nil(status
    :field-name "status" :initform :draft
    :type symbol
    :format (member :type (symbol :nil-allowed nil :convert string-upcase)
                    :set (:draft :active :deferred :accepted :withdrawn
                                 :rejected :final :replaced))
    :documentation "Status of this document in work flow")
   (completion-threshold
    :type number
    :field-name "completion-threshold"
    :format (number :nil-allowed t :unbound-if-nil t)
    :documentation "Threshold of completion before this article is to
be considered completed")
   (direct-prerequisites
    :type list
    :documentation
    "an alist of direct prerequisite articles and their weightings")
   (direct-outcomes
    :type list
    :documentation "alist of outcome concepts and their weightings")
   (direct-children
    :type list
    :documentation "list of direct children concepts of this concept")
   (assessments
    :type list :reader assessments
    :documentation "list of assessment stubs in this article"))
   (:metaclass  standard-parsed-class)
   (:documentation "An article for a tutorial"))



(defmethod parsed-field-form-element((slot jarw.parse::parsed-slot-mixin)
                                     (article tutorial-article))
  (case (jarw.mop:slot-definition-name slot)
    (status
     (let* ((name (intern (string-upcase (jarw.parse::slot-field-name slot))
                         :keyword))
           (allowed-values
            (or (mapcan
               #'(lambda(s) (when (has-permission s article *current-user*)
                              (list s)))
               '(:draft :active :deferred :accepted :withdrawn
                 :rejected :final :replaced))
                (list :draft)))
             (datatype `(member
                         :type (symbol :nil-allowed t :convert string-upcase)
                         :set ,allowed-values))
             (value (let ((slot (jarw.mop:slot-definition-name slot)))
                      (when (slot-boundp article slot)

                                       (slot-value article slot)))))
         `((markup:mcq :name ,name :value ,value :datatype ,datatype)
           ,@allowed-values)))
    (t (call-next-method))))

(define-condition relationship-error(article-error)
  ((relationship :type symbol :reader relationship :initarg :relationship)
   (message :type string :reader message :initarg :message))
  (:report
   (lambda(c s)
     (format s "Invalid ~A in ~S: ~A"
             (relationship c) (id c) (message c)))))

(defgeneric update-relationships(article)
  (:documentation "Update the cached relationships for an article"))

(defmethod update-instance-from-record :after ((article tutorial-article))
  (unless (bootstrapping-p (collection article))
    (update-relationships article)))

(defmethod (setf form-data):after ((data list) (article tutorial-article))
  (update-relationships article))

(defgeneric direct-outcomes(article)
  (:method ((article tutorial-article))
    (unless (slot-boundp article 'direct-outcomes)
      (update-relationships article))
    (slot-value article 'direct-outcomes)))

(defgeneric direct-children(article)
  (:method ((article tutorial-article))
    (unless (slot-boundp article 'direct-children)
      (update-relationships article))
    (slot-value article 'direct-children)))

(defgeneric direct-prerequisites(article)
  (:method ((article tutorial-article))
    (unless (slot-boundp article 'direct-prerequisites)
      (update-relationships article))
    (slot-value article 'direct-prerequisites)))

(defmethod time-required((article tutorial-article))
  (if (slot-boundp article 'time-required)
      (slot-value article 'time-required)
      (call-next-method)))

#+nil(defgeneric status(article)
  (:method((article tutorial-article))
    (if (slot-boundp article 'status)
        (slot-value article 'status)
        :draft)))

(defgeneric completion-threshold(article)
  (:method((article tutorial-article))
    (if (slot-boundp article 'completion-threshold)
        (slot-value article 'completion-threshold)
        0.8)))

(defmethod read-document :around ((article tutorial-article) (reader rst-reader))
  (let ((document (call-next-method)))
    (setf (slot-value  article 'assessments)
          (mapcar
           #'(lambda(assessment)
               (change-class assessment
                             'article-questionnaire :article article)
               assessment)
           (assessments document)))
    document))

(defmethod read-document :around ((article tutorial-article) (reader recursive-rst-reader))
  (let ((document (call-next-method)))
    (map 'nil
         #'(lambda(assessment)
             (change-class
              assessment
              'article-questionnaire
              :article (dictionary:get-dictionary
                        (docutils.utilities:namespace assessment)
                        (collection article))))
         (assessments document))
    document))

(defmethod assessments :before ((article tutorial-article))
  (unless (slot-boundp article 'document) (document article)))

(defun is-prerequisite (self other)
  "Returns true if other is a prerequisite of self
   either directly or indirectly"
  (or (member other (direct-prerequisites self) :key #'car)
      (some #'(lambda(p) (is-prerequisite (car p) other))
            (direct-prerequisites self))))

;;(member self (direct-prerequisites other) :key #'car)
;;      (some #'(lambda(p) (is-prerequisite (car p) other))
;;           (direct-prerequisites self))))

(defun is-child(parent child)
  "Returns true if other is child of self
   either directly or indirectly"
  (or (member child (direct-children parent))
      (some #'(lambda (p) (is-child p child))
            (direct-children parent))))

(defgeneric allowed-prerequisite-p(self other)
  (:documentation "return true if other is allowed as a prerequisite for self")
  (:method (self other)
    (not (or (eq self other)
             (is-prerequisite other self)
             (is-child self other)))))

(defgeneric allowed-child-p (self other)
  (:documentation "true if other is  allowed as a child for self")
  (:method (self other)
    (not (or (eq self other)
             (is-child other self)
             (is-prerequisite self other)))))

(defgeneric allowed-outcome-p(self other)
  (:documentation  "true if other is allowed as an outcome for self")
  (:method (self other) t))

(defmethod update-relationships((article tutorial-article))
  (with-slots(errors direct-prerequisites direct-outcomes direct-children)
      article
    (setf direct-prerequisites nil direct-outcomes nil direct-children nil)
    (setf errors
          (delete-if #'(lambda(e) (typep e 'relationship-error)) errors))
    (let ((relationship)
          (collection (collection article)))
      (labels ((err(&rest args)
                 (push (make-instance
                        'relationship-error
                        :id (id article)
                        :collection collection
                        :relationship relationship
                        :message (apply #'format `(nil ,@args)))
                       errors)
                 nil)
               (article(id)
                 (if (equalp id (id article))
                     article
                     (or (get-article id collection :if-does-not-exist nil)
                         (err "Related article ~S does not exist." id)))))
        (when (slot-boundp article 'prerequisites-field)
          (setf relationship 'direct-prerequisites)
          (dolist(item (slot-value article 'prerequisites-field))
            (let ((other (article (car item))))
              (when other
                (if (allowed-prerequisite-p article other)
                    (push (cons other (cdr item)) direct-prerequisites)
                    (err "~S not allowed as a prerequisite of ~S"
                         (id other) (id article)))))))
        (when (slot-boundp article 'outcomes-field)
          (setf relationship 'direct-outcomes)
          (dolist(item (slot-value article 'outcomes-field))
            (let ((other (article (car item))))
              (when other
                (if (allowed-outcome-p article other)
                      (push (cons other (cdr item)) direct-outcomes)
                    (err "~S not allowed as an outcome of ~S"
                         (id other) (id article)))))))
        (when (slot-boundp article 'children-field)
          (setf relationship 'direct-children)
          (dolist(item (slot-value article 'children-field))
            (let ((other (article item)))
              (when other
                (if (allowed-child-p article other)
                    (push other  direct-children)
                    (err "~S not allowed as a child of ~S"
                         (id other) (id article))) ))))))
    (setf direct-prerequisites (nreverse direct-prerequisites)
          direct-outcomes (nreverse direct-outcomes)
          direct-children (nreverse direct-children))))

(defgeneric prerequisites(article)
  (:documentation "return a list with all prequisite articles for self
   and their calculated weights")
  (:method((article tutorial-article))
    (let ((results (copy-list (direct-prerequisites article))))
      (flet ((accumulate-weight (c w)
                   (let ((a (assoc c results)))
                     (if a (rplacd a (- 1 (* (- 1 (cdr a)) (- 1 w))))
                         (push (cons c w) results)))))
        (dolist (p (direct-prerequisites article))
          (dolist (pp (prerequisites (car p)))
            (accumulate-weight (car pp) (* (cdr p) (cdr pp)))))
        results))))

(defgeneric children(article)
  (:documentation "returns all the required children (direct and
indirect) for self in proper depth first order")
  (:method((article tutorial-article))
    (mapcan #'(lambda (c) (cons c (children c)))
            (direct-children article))))

(defgeneric outcomes-for(article)
  (:documentation "returns a list of all concepts, direct and
indirect, which can contribute to c together with their weights,
including self")
  (:method((article tutorial-article))
    (let ((collection (collection article))
          (dofmem (make-hash-table :test 'eq))
          outcomes
          stack)  ; stack used to prevent circular reference loops
      ;; memoize direct-outcomes-for - very important optimization
      (map-dictionary
       #'(lambda(k v)
           (declare (ignore k))
           (dolist(o (direct-outcomes v))
             (push (cons v (cdr o)) (gethash (car o) dofmem)  )))
       collection)
      (labels ((direct-outcomes-for(self) (gethash self dofmem))
               (self-outcome(article)
                 (cdr (assoc article (direct-outcomes article))))
               (collect-outcomes (c w)
                   (let ((a (assoc c outcomes)))
                       (if a
                           (rplacd a (- 1 (* (- 1 w) (- 1 (cdr a)))))
                           (push (cons c (* w (or (self-outcome c) 1)))
                                 outcomes)))
                   (dolist (rec (direct-outcomes-for c))
                     (unless (or (eql (car rec) c) (find (car rec) stack))
                       (push (car rec) stack)
                       (collect-outcomes (car rec) (* w (cdr rec)))
                       (pop stack)))))
          (collect-outcomes article 1.0d0)
          (rplacd (assoc article outcomes)
                  (or (self-outcome article)
                      (if (> (length outcomes) 1) 0 1))))
      outcomes)))

(defun ancestors(ancestor child)
  "Returns the first sequence of children leading from ancestor
concept to child concept (excluding child) or nil if no such path
exists"
  (let ((stack ()))                     ;; stack used to accumulate paths
    (labels ((recurse-children (ancestor)
               (cond ((eq ancestor child) t)
                     (t (push ancestor stack)
                        (dolist (ancestor (direct-children ancestor))
                          (when (recurse-children ancestor) (return t))
                          (pop stack)
                          nil)))))
      (when (recurse-children ancestor) stack))))

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

(defclass tutorial-collection(article-collection)
  ((article-class :type symbol :initform 'tutorial-article :allocation :class
                  :reader article-class
                  :documentation "Class of article to be constructed"))
  (:documentation "Collection of tutorial articles"))


;; work flow pattern

#|
digraph workflow {
 	node [fontname="Helvetica"];
  Draft -> Deferred;
  Deferred -> Draft;
  Draft -> Accepted;
  Draft -> Rejected;
  Draft -> Withdrawn;
  Accepted -> Final;
  Final -> Replaced [style=dashed];
  Accepted -> Rejected [style=dashed];
  Active;
}


(defmethod has-permission((status (eql :draft)) (article tutorial-article)
                          &optional (user *current-user*))
  "True if can set status to draft"
  (and (has-permission :edit article user)
       (member (status article) '(:draft :deferred))))

(defmethod has-permission((status (eql :active)) (article tutorial-article)
                          &optional (user *current-user*))
  "True if can set status to draft"
  (and (has-permission :edit article user)
       (eql (status article) :active)))

(defmethod has-permission((status (eql :deferred)) (article tutorial-article)
                          &optional (user *current-user*))
  "True if can set status to draft"
  (and (has-permission :edit article user)
       (member (status article) '(:draft :deferred))))

(defmethod has-permission((status (eql :accepted)) (article tutorial-article)
                          &optional (user *current-user*))
  (and (has-permission :admin (collection article) user)
       (eql (status article) :draft)
       (not (errors article))))

(defmethod has-permission((status (eql :withdrawn)) (article tutorial-article)
                          &optional (user *current-user*))
  (and (has-permission :edit article user)
       (eql (status article) :draft)))

(defmethod has-permission((status (eql :rejected)) (article tutorial-article)
                          &optional (user *current-user*))
  (and (has-permission :admin (collection article) user)
       (member (status article) '(:draft :accepted))))

(defmethod has-permission((status (eql :final)) (article tutorial-article)
                          &optional (user *current-user*))
  (and (has-permission :admin (collection article) user)
       (eql (status article) :accepted)
       (not (errors article))))

(defmethod has-permission((status (eql :replaced)) (article tutorial-article)
                          &optional (user *current-user*))
  (and (has-permission :edit article user)
       (eql (status article) :final)))
|#

(defgeneric (setf status)(status article)
  (:documentation "Set the status of an article if allowed for current user")
  (:method((status symbol) (article tutorial-article))
    (assert-permission status article)
    (setf (slot-value article 'status) status)))

;; user properties for articles

(defun accumulate-scores(article &key score (test #'identity)
                         (outcomes (outcomes-for article)))
  "Accumulate a score function across all outcomes towards concept
which satisfy the test. If score is nil then the weighting is
returned. If test is nil then all outcomes are examined. if outcomes
is provided it will be used as the list of outcome records"
  (flet ((accumulate-weighting(&optional a b)
           (if a (- 1 (* (- 1 a) (- 1 b))) 0)))
  (reduce #'accumulate-weighting
          (mapcan #'(lambda(o)
                      (when (funcall test (car o))
                        (list
                         (if score
                             (* (funcall score (car o)) (cdr o))
                             (cdr o)))))
                  outcomes))))

(defun assessment-completed(article-state assessment)
  (clews.assessment:completed
   (state-value (docutils.assessment::name assessment) article-state)))

(defun assessment-knowledge(article-state assessment)
  (state-value (docutils.assessment::name assessment) article-state))

(defgeneric article-self-completion(collection-state article)
  (:documentation "Completion level achieved for this article only.")
  (:method((user clews:user) article)
    (article-self-completion (user-state (collection article) user) article))
  (:method(collection-state (article tutorial-article))
    (let ((assessments (assessments article))
          (article-state (user-state article collection-state)))
      (if assessments
          (/ (count-if #'(lambda(a) (assessment-completed article-state a))
                       assessments)
             (length assessments))
          (min 1 (/ (or (state-value :time-spent article-state) 0)
                    (or (time-required article) 300)))))))

(defgeneric article-completion(collection-state article)
  (:documentation "Percentage completion level achieved by user
   of this concept (including outcomes towards it)")
  (:method ((user clews:user) article)
    (article-completion (user-state (collection article) user) article))
  (:method(collection-state (article tutorial-article))
   (accumulate-scores
          article
          :score #'(lambda(article)
                     (article-self-completion collection-state article)))))

(defun article-self-completed-p(data article)
  "True if user-data has actually directly completed this concept, ignoring its
   children"
  (>= (article-self-completion data article)
      (completion-threshold article)))

(defun article-completed-p(data article)
  "Returns true if the user-data is considered to have completed this
concept (and its children)"
  (and (article-self-completed-p data article)
       (every #'(lambda(c) (article-completed-p data  c))
              (direct-children article))))

(defun incomplete-children(collection-state article)
  "returns list of children of given concept not directly completed by
the user. Will be in correct sequential ordering"
  (mapcan
   #'(lambda(a)
       (when (not (article-self-completed-p collection-state a))
         (list a)))
   (children article)))

(defun incomplete-prerequisites(user article)
  "returns all incomplete prerequsites of given concept for given user
   in order of importance * how much student has left to do"
  (let ((collection-state (user-state (collection article) user)))
    (mapcar
     #'cdr
     (sort
      (mapcan
       #'(lambda(p)
           ;;list weight w * (1-completion level) and prerequisit concept
           (let ((prerequisite (car p))
                 (w (cdr p)))
             (unless
                 (if (is-child prerequisite article)
                     (article-self-completed-p collection-state prerequisite)
                     (article-completed-p collection-state prerequisite))
               (list
                (cons (* w (- 1 (article-completion collection-state
                                                    prerequisite)))
                      prerequisite)))))
       (prerequisites article))
      #'> :key #'car))))

(defgeneric user-tasks(user entity)
  (:documentation "Return a list of incomplete user tasks")
  (:method((user user) (collection tutorial-collection))
    (delete-duplicates
     (mapcan #'(lambda(goal) (user-tasks user goal))
             (goals collection user))))
  (:method((user user) (article tutorial-article))
    (let ((collection-state (user-state (collection article) user)))
      (if (article-self-completed-p collection-state article)
          (incomplete-children collection-state article)
          (cons article (incomplete-children collection-state article))))))

(defgeneric next-article(user article)
  (:documentation "Return next article to be visited after this article")
  (:method ((user user) (article tutorial-article))
    (let ((collection-state (user-state (collection article) user)))
      (flet((check-it(a)
              (let ((next (first (delete article (incomplete-children
                                                  collection-state a)))))
                (when next
                  (or (first (delete article
                                     (incomplete-prerequisites user next)))
                      next)))))
        ;; first check current article children then goals children
        (or (check-it article)
            (check-it (first (goals (collection article) user)))))))
  (:method ((user user) (collection tutorial-collection))
    (let ((collection-state (user-state collection user)))
      (some #'(lambda(g) (next-article collection-state g))
            (goals collection user)))))

(defmethod article-list-item((article tutorial-article)
                             &key
                             (user *current-user*)
                             current-article
                             prefix
                             (relative "./")
                             (action "view")
                             (status-line
                              (status-line
                               (append (article-status article user)
                                       (article-status article nil))
                               article))
                             class
                             (abstract (field-value "abstract" article))
                             &allow-other-keys)
  (let ((state (user-state (collection article) user))
        (assessment (assessments article)))
    (call-next-method article
                      :state state
                      :current-article current-article
                      :prefix prefix
                      :relative relative
                      :action action
                      :status-line status-line
                      :abstract abstract
                      :class
                      (format nil "~A-~A~@[ ~A~]"
                              (if (direct-children article)
                                  "folder"
                                  "document")
                              (cond
                                ((eq current-article article) "this")
                                ((article-completed-p state article)
                                 (if assessment "completed-q" "completed"))
                                ((incomplete-prerequisites user article)
                                 (if assessment "notok-q" "notok"))
                                (t (if assessment "ok-q" "ok")))
                              class))))

(defmethod article-status((article tutorial-article) (user clews:user))
  (let* ((collection-state (user-state (collection article) user))
        (state (user-state article collection-state)))
    (nconc
     (delete "Time Spent" (call-next-method) :key #'first :test #'equal)
     `(("Time Spent"
        ,(format nil "~D sec~@[ / ~D sec~]"
                 (state-value :time-spent state)
                 (time-required article))
        :view)
       ("Completion"
        ,(format nil "~D%/~D%"
                 (floor
                  (* 100 (article-completion collection-state article)))
                 (floor (* 100 (completion-threshold article))))
        :view)))))

(defmethod article-status((article tutorial-article) (state (eql nil)))
   `(#+nil("Status" ,(string (status article)) :edit)
     ,@(call-next-method)))

(defgeneric goals(collection user)
  (:documentation "Return the list of goal articles for given user in
collection")
  (:method((collection tutorial-collection) (user user))

     (mapcan
     #'(lambda(id)
         (let ((a (get-dictionary id collection)))
           (when a (list a))))
     (state-value :goals (user-state collection user)))))

(defgeneric (setf goals)(goals collection user)
  (:documentation "Set the goals for a particular user")
  (:method((goals list) (collection tutorial-collection) (user user))
    (let ((user-state (user-state collection user)))
      (let ((new-goals
             (delete-duplicates
              (mapcar
               #'(lambda(g) (etypecase g (string g) (article (id g))))
               goals)
              :test #'equal)))
        (unless (equal new-goals (state-value :goals user-state))
          (setf (state-value :goals user-state) new-goals ))
       (goals collection user)))))

(defgeneric child-heirarchy(top child)
  (:documentation "Return a heirarchical tree of nodes from top down
to and including the child and its siblings")
  (:method(top child)
    (let* ((ancestors  (nreverse (ancestors top child)))
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
          (cons top children) )))
  (:method(top (child (eql nil)))
    "All children of top"
    (labels((heirarchy(top)
              (cons top (mapcar #'heirarchy (direct-children top)))))
      (heirarchy top))))

(defun updated-goals(collection user)
  "Update and return goals by removing completed ones and adding in
possible new ones"
  (let ((collection-state (user-state collection user)))
    (let ((goals (mapcan
                  #'(lambda(a)
                      (unless (article-completed-p collection-state a)
                        (list a)))
                  (delete-duplicates
                   (nunion
                    (goals collection user)
                    (search-dictionary
                     #'(lambda(article) (has-permission :goal article user))
                     collection))))))
      (map nil
           #'(lambda(goal)
               (delete-if #'(lambda(other) (is-child goal other)) goals))
           goals)
      (setf (goals collection user) goals))))

(defmethod leave-article((user user) (collection tutorial-collection))
  (call-next-method))

(defmethod visit-article((user user) (article tutorial-article))
  ;; put current goal from current article to top of list
  ;; or add this article as a new goal
  (call-next-method)
  (let* ((goals (goals (collection article) user))
         (goal (find-if
                #'(lambda(goal) (or (eql goal article)
                                    (is-child goal article)))
                goals)))
    (setf (goals (collection article) user)
          (if goal
              (cons goal (delete article goals))
              (cons  article goals)))))

;; questionnaires in articles inherit some article info

(defclass article-questionnaire(docutils.assessment::questionnaire)
  ((article
    :initarg :article :reader article :type article
    :documentation "Article in which assessment is stored")
   (errors :documentation "Error in the assessment")))

(defmethod title((q article-questionnaire))
  (title (article q)))

;; (defmethod docutils.writer.latex::visit-node
;;     ((writer docutils.writer.latex::latex-writer)
;;      (assessment article-questionnaire))
;;   (let ((render (setting :latex-assessment-render (document assessment))))
;;     (when render
;;       (let ((knowledge
;;              (when *articles*
;;                (dictionary:get-dictionary
;;                 (name assessment)
;;                 (clews.articles::user-state
;;                  (dictionary:get-dictionary
;;                   (docutils.utilities:namespace assessment) *articles*)
;;                  inet.acl::*current-user*)))))
;;         (setq *assessment* assessment
;;               *knowledge* knowledge)
;;         (break)

;;         (ccase render
;;           (:status
;;            (part-append
;;             (format nil "\\begin{assessment}
;; ~@[\\item ~A~%~]~@[\\item[\\em Deadline:] ~A~%~]\\item[\\em No Questions:] ~A
;; ~@[\\item[\\em Time Allowed:] ~A~~min~%~]\\end{assessment}~%"
;;                     (clews.assessment::description assessment)
;;                     (clews.assessment::deadline-date knowledge assessment)
;;                     (clews.assessment::set-no-questions nil assessment)
;;                     (let ((tt (clews.assessment::timelimit nil assessment)))
;;                       (when tt (/ tt 60)))))))))))


(defun lookup-date(user article fieldname)
  (when user
    (flet((return-if-deadline(article)
            (when (slot-boundp article fieldname)
              (let*((deadlines  (slot-value article fieldname))
                    (match
                     (or (cdr (assoc (clews:username user) deadlines
                                     :test #'equal))
                       (cdr (find-if #'(lambda(d)
                                         (clews::has-role user (car d)))
                                     deadlines)))))
                (when match (return-from lookup-date match))))))
      (return-if-deadline article)
      (dolist (goal (goals (collection article) user))
        (when goal
          (dolist(parent (ancestors goal article))
            (return-if-deadline parent)))))))

(defmethod deadline-date(user (article tutorial-article))
  (lookup-date user article 'deadlines))

(defmethod feedback-date(user (article tutorial-article))
  (or (lookup-date user article 'feedback-dates)
      (deadline-date user article)))

(defmethod default-deadline-date((questionnaire article-questionnaire))
  (or (call-next-method)
      (deadline-date *current-user* (article questionnaire))
      ))

(defmethod default-feedback-date((questionnaire article-questionnaire))
  (or (call-next-method)
      (feedback-date *current-user* (article questionnaire))))

(defmethod deadline-date(knowledge (questionnaire article-questionnaire))
  (declare (ignore knowledge))
  (default-deadline-date questionnaire))

(defmethod feedback-date(knowledge (questionnaire article-questionnaire))
  (declare (ignore knowledge))
  (default-feedback-date  questionnaire))

(defmethod errors :before ((article tutorial-article))
  ;; ensure errors from parsed document are loaded
  (unless (slot-boundp article 'direct-outcomes)
    (update-relationships article)))

(defmethod errors((assessment article-questionnaire))
  (if (slot-boundp assessment 'errors)
      (slot-value assessment 'errors)
      (let ((*current-user* nil)
            (knowledge (list nil))) ;; dummy knowledge and no user
        (setf (slot-value assessment 'errors)
              (handler-case
                  (progn
                    (clews.assessment::assessment-status-table
                     knowledge assessment)
                    (clews.assessment:assessment-attempt-markup
                     knowledge assessment nil)
                    (clews.assessment:assessment-feedback-markup
                     knowledge assessment nil)
                    nil)
                (error(e)
                  (list (make-instance
                         'document-error
                         :collection (collection (article assessment))
                         :id (id (article assessment))
                         :message (format
                                   nil "Assessment:~%  ~S~%~A"
                                   (docutils.assessment::name assessment)
                                   e)))))))))

(defmethod (setf document) :after (document (article tutorial-article))
  (let ((errors (mapcan #'errors (assessments article))))
    (when errors
      (setf (slot-value article 'errors)
            (nconc (slot-value article 'errors)
                   errors)))))

(defmethod clews.assessment::assessment-attempt-p-reason
    or (knowledge  (assessment article-questionnaire))
    (when (and (slot-boundp assessment 'errors)
               (slot-value assessment 'errors))
      "There is an error in this assessment.
Please advise tutor to correct it."))

(defgeneric mark-weighting(item)
  (:documentation "Return the weighting for a given item")
  (:method((questionnaire clews.assessment:questionnaire))
    (clews.assessment::weighting questionnaire))
  (:method((article tutorial-article))
0    (reduce #'+ (mapcar #'mark-weighting (assessments article))
             :initial-value 0)))

(defun weighted-average(weights marks)
  (let ((w (reduce #'+ weights))
        (m (reduce #'+ (mapcar #'* weights
                               (mapcar #'(lambda(m) (if m m 0)) marks)))))
    (if (= w 0)
        0
        (/ m w))))

(defgeneric mark(userdata item)
  (:documentation "Return the mark for given user on item")
  (:method(knowledge (questionnaire clews.assessment:questionnaire))
    (if (clews.assessment::assessment-count-p knowledge questionnaire)
        (clews.assessment:assessment-mark knowledge questionnaire)
        0.0))
  (:method(article-state (article tutorial-article))
    (weighted-average
     (mapcar #'mark-weighting (assessments article))
     (mapcar
      #'(lambda(assessment)
          (mark (get-dictionary
                 (docutils.assessment::name assessment) article-state)
                assessment))
      (assessments article))))
  (:method((user user)  (article tutorial-article))
    (call-next-method (user-state article user) article))
  (:method((user user) (articles list))
    (let ((articles (mapcan #'(lambda(a) (when (assessments a) (list a)))
                            articles)))
    (weighted-average
     (mapcar #'mark-weighting articles)
     (mapcar #'(lambda(article) (mark user article)) articles)))))

(defvar *state* nil)
(defun assessment-deadlines(collection user nodays
                            &optional (now (get-universal-time)))
  "Return a list of missed, imminent and feedback available deadlines
in order for *current-user* collection up to nodays in the future"
  (let* ((then (+ now (* nodays 24 60 60)))
         (state (user-state collection user))
         missed imminent feedback
         (*current-user* user))
    (map-dictionary
     #'(lambda(k article)
         (declare (ignore k))
         (let ((astate (user-state article state)))
           (setf *state* astate)
           (dolist (a (assessments article))
             (let* ((knowledge (assessment-knowledge astate a))
                   (d (deadline-date knowledge a))
                   (completed (assessment-completed astate a)))
               (cond
                 ((and d (<= now d then))
                  (push (cons d a) imminent))
                 ((and d (not completed)  (<= d now))
                  (push (cons d a) missed))
                 ((and (clews.assessment:assessment-feedback-p knowledge a)
                       d
                       (> d
                          (state-value :last-visit-time astate)))
                  (push (cons d a) feedback)))))))
     collection)
    (values
     (sort missed #'< :key #'car)
     (sort imminent #'< :key #'car)
     (sort feedback #'< :key #'car))))

(defun article-deadlines(collection user nodays
                         &optional (now (get-universal-time)))
  "Return a list of articles to be completed soom and not not completed
articles missed"
  (let* ((then (+ now (* nodays 24 60 60)))
         missed imminent
         (*current-user* user))
    (map-dictionary
     #'(lambda(k article)
         (declare (ignore k))
         (when (slot-boundp article 'deadlines)
           (let ((d (deadline-date user article)))
             (when (and d (<= d then)
                        (find-if
                         #'(lambda(goal) (is-child goal article))
                         (goals collection user))
                        (not (article-completed-p user article)))
               (cond
                 ((< d now) (push (cons d article) missed))
                 ((< d then) (push (cons d article) imminent)))))))
     collection)
    (values
     (sort missed #'< :key #'car)
     (sort imminent #'< :key #'car))))

(defun deadlines-report-text(collection user nodays)
    "Return a textual deadlines report for user on collection - upto
nodays ahead and as a second value if it is different to previous
report."
    (multiple-value-bind(assessment-missed assessment-imminent
                                           assessment-feedback)
        (assessment-deadlines collection user nodays)
      (multiple-value-bind(article-missed article-imminent)
          (article-deadlines collection user nodays)
    (let* ((reports
            `(("Missed Assessments" ,assessment-missed)
              ("Imminent Assessments" ,assessment-imminent)
              ("Assessments with Feedback Available" ,assessment-feedback)
              ("Topics you should have read" ,article-missed)
              ("Topics you need to read soon" ,article-imminent))))
      (values
       (with-output-to-string(os)
         (dolist(report reports)
           (when (second report)
             (terpri os)
             (write-line (first report) os)
             (write-line (make-string  (length (first report))
                                       :initial-element #\=) os)
             (dolist(line (second report))
               (format os "~A ~A~%"
                       (format-output '(date :fmt :short) (car line))
                       (title (cdr line)))))))
       (some #'second reports))))))

;; (defun set-deadlines(collection user articlename newdeadline
;;                      &key (recurse t))
;;   (let* ((article (get-dictionary articlename collection))
;;          (deadline (jarw.parse:parse-input 'date newdeadline))
;;          (userstate (user-state collection user))
;;          (articles (if recurse
;;                       (cons article (children article))
;;                       (list article))))
;;     (mapcar
;;      #'(lambda(article)
;;          (let ((astate (user-state article userstate)))
;;            (mapcar
;;             #'(lambda(assessment)
;;                 (let* ((knowledge (assessment-knowledge astate assessment)))
;;             (assessments article))))
;;      articles)))

(defmethod docutils.parser.rst::title((source article))
  (title source))

(defmethod docutils.parser.rst::subsections((source tutorial-article))
  (direct-children source))

(defmethod docutils.utilities:namespace((source article))
  (id source))

(defun insert-tutorial-metadata(source reader parent-node)
  (when (typep parent-node 'docutils.nodes:document)
    (docutils.parser.rst::insert-metadata source reader parent-node)))

(defun full-document(article)
  (let ((*search-path* (list (path-to-media article)))
        (*unknown-reference-resolvers*
         (cons #'resolve-rfc-reference
               (cons #'(lambda(node)
                         (resolve-article-reference
                          article node #'nameids #'resolve-to-id))
                     *unknown-reference-resolvers*))))
    (read-document article
                   (make-instance 'docutils.parser.rst::recursive-rst-reader))))






#|

(in-package :clews.articles)
(defparameter *a* (get-dictionary "digital-transmission" aston::*tutorials*))
(defparameter *d* (full-document *a*))
(defparameter *w* (make-instance
             'latex-writer
             :settings '((:latex-document-class . "article")
                         (:latex-document-options . "10pt,a4paper,twocolumn")
                         (:use-latex-docinfo . t)
                         (:latex-stylesheet . "jarw-math.tex"))))

(defparameter *u* (setf inet.acl::*current-user*
                        (get-dictionary "willijar" aston::*user-source*)))

(setq docutils.assessment::*articles* aston::*tutorials*)
(updated-goals docutils.assessment::*articles* *current-user*)

(defparameter (docutils.assessment::*user-data*
               (user-state article *u*))

(with-open-file(os #p"/home/willijar/dev/lisp/src/docutils/tests/tmp.tex" :direction :output :if-does-not-exist :create :if-exists :supersede) (docutils:write-document *w* *d* os))


To Fix:


clean oven and hobb

* article-questionnaire specialise rendering.

* redraw msk-transmitter

* Tables - line and hlines?

* small non-breaking spaces for units and other non-breaking spaces??

|#