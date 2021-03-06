;;;; Web components -*- Lisp -*- Copyright (C) 2008
;;;; John A.R. Williams <J.A.R.Williams@jarw.org.uk> Released under
;;;; the GNU General Public License (GPL) See
;;;; <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;

;;;; modules correspond to a specific application unit (which may contain
;;;; other modules)
;;;; views provide the handlers on those modules

(in-package :clews.views)

(defclass self-describing-component()
  ((id :type symbol :initarg :id :reader id
         :documentation "context unique if of this component")
   (title :type string :initarg :title :reader title
          :documentation "Title - (used to make a link)")
   (description :initarg :description
                :documentation "Text or markup elements description"))
  (:documentation "Basic base class for self describing components"))

(defmethod print-object((object self-describing-component) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (when (slot-boundp object 'id) (princ (id object) stream))))

(defgeneric description(component)
  (:documentation "Return description as a markup element")
  (:method((component self-describing-component))
    (with-slots(description) component
      (etypecase description
        (null "")
        (string `(p ,description))
        (list `((div :class :description) ,@description))))))

(defclass view(self-describing-component)
  ((permission-check
    :type (or function symbol)
    :initform #'(lambda(&rest args) (declare (ignore args)) t)
    :initarg :check :reader permission-check
    :documentation "Given a module and user returns t
if access permitted, otherwise nil and an explanation why not")
   (handler :type function :initarg :handler :reader handler
            :documentation "handler takes view, module and request and
            returns a markup element"))
  (:documentation "Class representing a view of a module"))

;; we may want to add in some caching here for efficiency
(defvar *views* (make-hash-table)
  "Hash mapping views to classes")

(defgeneric views(entity)
  (:documentation "Return a list of all the views associated with an
  entity.")
  (:method((entity standard-object))
    (let ((views nil))
      (dolist(class (class-precedence-list (class-of entity)))
        (dolist(view (gethash class *views*))
          (pushnew view views :key #'id :test #'equal)))
      views)))

(defun allowed-views(entity &optional (user *current-user*))
  (let ((*current-user* user))
    (mapcan #'(lambda(v) (when (can-view-p v entity) (list v)))
            (views entity))))

(defgeneric get-view(path entity)
  (:documentation "Return named view for entity or nil if none")
  (:method((path string) entity)
    (find path (views entity) :key #'id :test #'equal)))

(defun add-view(classname view)
  (let ((class (find-class classname)))
    (setf (gethash class *views*)
          (cons view
                (delete (id view) (gethash class *views*)
                        :key #'id :test #'equal)))))

(defmacro defview(id (classname) &rest args)
  `(add-view ',classname
     (make-instance
      'view
      :id ,(string id)
      :title ,(or (second (assoc :title args))
                  (error "No title defined for view"))
      :description ,(second (assoc :description args))
      :handler (lambda,@(rest (assoc :handler args)))
      ,@(when (assoc :check args)
              `(:check
                        (lambda,@(rest (assoc :check args))))))))

(defun can-view-p(view module)
  (let ((condition (permission-check view)))
    (multiple-value-bind(p d)
        (etypecase condition
          (function
           (funcall condition module))
          (symbol
           (or (has-permission condition module)
               (values nil (format nil "~A forbidden to ~A ~A"
                                   (username *current-user*)
                                   condition
                                   module)))))
      (unless p (return-from can-view-p (values p d)))))
    t)

(defclass module(self-describing-component)
  ()
  (:documentation "A module is a self describing which will typically
  have views and may have other submodules"))

(defgeneric modules(module)
  (:documentation "Return the dictionary of submodules
of an application module or nil if none")
  (:method(m) (declare (ignore m)) nil))

;; response handler for a top level module
;; breaks path down to recurse down modules and display view
(defgeneric markup(view module request navigation)
  (:documentation "Return the full html documentation for given view and module
with given navigation menus"))

(defmethod response-handler((module module) request rest)
  "Response handler provides application top level framework"
  (let* ((path (subseq rest 0 (position #\? rest)))
         (parts (or (split-string path :delimiter '(#\/ #\.)
                                  :remove-empty-subseqs nil)))
         (modules (list module)) ;; built up in reverse order from parts
         view)
    ;; find modules by depth and view on bottom module
    (cond
      ((modules module)
       (loop
          :for remainder :on parts :by #'rest
          :with part = (first remainder)
          :with module = (first modules)
          :if (not (rest remainder))
          :do (cond
                ((zerop (length part)) (redirect request "STATUS"))
                ((setf view (get-view part module)))
                ((throw 'response :not-found)))
          :else
          :do (push (get-dictionary part (modules module))
                    modules)))
      ((rest path)
       (throw 'response :not-found))
      ((setf view (get-view (first path) module)))
      ((throw 'response :not-found)))
    (let ((module (first modules)))
      ;; check we are allowed this view
      (unless (can-view-p view module)
        (error 'permission-denied-error :action view :service module))
      ;; now construct navagation data - two rows
      ;; first is breadcrumbs, second is available views
      (let ((navigation
             (list
              ;; breadcrumbs
              (let ((relative-url ""))
                (mapcar
                 #'(lambda(name module)
                     (setf relative-url (strcat relative-url "../"))
                     (list (strcat relative-url name "/STATUS")
                           (title module)))
                 (butlast parts 2)
                 (rest modules)))
              ;; other available views on this module
              (mapcar
               #'(lambda(view)
                   (list (id view) (title view)))
               (allowed-views module)))))
        (markup view module request navigation)))))

(defmethod markup(view (module module) request navigation)
  `(html
    (head (title ,(title module) " - " ,(title view)))
    (body
     ,(navbar navigation)
     ((section :title ,(strcat (title module) " - " (title view)))
      ,(description module)
      ,(description view)
      ,@(funcall (handler view) module request))
     ,(navbar navigation))))

;; all modules should have at least a status module
(defview status(module)
  (:title "Status")
  (:description "")
  (:check(module)
      (or (some #'(lambda(v)
                    (unless (equal (id v) "STATUS") (can-view-p v module)))
                (views module))
          (let ((modules (modules module)))
            (and modules
                 (search-dictionary
                  #'(lambda(m)
                      (let ((v (get-view "STATUS" m)))
                        (and v (can-view-p v m))))
                   modules)))))
  (:handler(module request)
     (declare (ignore request))
     (let ((a (delete "STATUS" (allowed-views module) :test #'equal :key #'id))
           (m (dictionary-keys (modules module))))
       (list
        (when a
          `((section :title "Actions Available")
            (dl
             ,@(mapcar
                #'(lambda(v)
                    `((dt ((a :href ,(id v)) ,(title v)))
                      (dd ,(description v))))
                a))))
        (when m
          `((section :title "Modules Available")
            (dl
             ,@(mapcan
                #'(lambda(modulename)
                    (let* ((module (get-dictionary modulename (modules module)))
                           (views (views module)))
                      (when views
                        `((dt ,(title module))
                          (dd
                           ,(description module)
                           (p ,@(mapcan
                                 #'(lambda(v)
                                     (list " | "
                                           `((a
                                              :href
                                              ,(strcat modulename "/" (id v)))
                                             ,(title v))))
                                 views)
                              "|"))))))
                m))))))))
