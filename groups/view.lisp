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
  ((path :type string :initarg :path :reader path
         :documentation "Path to this component")
   (title :type string :initarg :title :reader title
          :documentation "Title - (used to make a link)")
   (description :type string :initarg :description :reader description
                :documentation "Short 1 paragraph description"))
  (:documentation "Basic base class for self describing components"))

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

(defgeneric views(entity &optional user)
  (:documentation "Return a list of the view objects associated with an
  entity. If optional user argument is given return only views
  available for that user")
  (:method-combination nconc)
  (:method :around((entity standard-object) &optional (user *current-user*))
     (let ((views nil))
       (dolist(view (call-next-method))
         (unless (find (path view) views :key #'path :test #'equal)
           (push view views)))
       (if user
           (let ((*current-user* user))
             (mapcan
              #'(lambda(v)
                  (when (can-view-p v entity)
                    (list v)))
              views))
           views)))
  (:method nconc (entity &optional user)
    (declare (ignore entity user))
    nil))

(let ((views (make-hash-table)))
  (defgeneric get-view(path entity)
    (:documentation "Return named view for entity or nil if none")
    (:method((path string) entity)
      (find path (views entity) :key #'path :test #'equal)))
  (defmethod views nconc((entity standard-object) &optional user)
    (declare (ignore user))
    (copy-list (gethash (class-of entity) views)))
  (defun add-view(classname view)
    (let ((class (find-class classname)))
      (setf (gethash class views)
            (cons view
                (delete (path view) (gethash class views)
                        :test #'equal :key #'path)))))
  (defmacro defview(path (classname) &rest args)
    `(add-view ',classname
               (make-instance
                'view
                :name ,(string path)
                :title ,(or (second (assoc :title args))
                            (error "No title defined for view"))
                :description ,(second (assoc :description args))
                :handler (function (lambda,@(rest (assoc :handler args))))
                ,@(when (assoc :check args)
                        `(:check (function
                                  (lambda,@(rest (assoc :check args))))))))))

(defun can-view-p(view module)
  (let ((condition (permission-check view)))
    (multiple-value-bind(p d)
        (etypecase condition
          (function (funcall condition module))
          (symbol (or (has-permission condition module)
                      (values nil (format nil "~A forbidden to ~A ~A"
                                          (username *current-user*)
                                          condition
                                          module)))))
      (unless p (return-from can-view-p (values p d)))))
    t)

(defclass module(self-sescribing-component)
  ()
  (:documentation "A module is a self describing which will typically have views and may have other submodules"))

(defgeneric modules(module)
  (:documentation "Return the dictionary of submodules
of an application module or nil if none")
  (:method(m) (declare m) nil))

;; response handler for a top level module
;; breaks path down to recurese down modules and display view

(defmethod response-handler((module module) request rest)
  "Response handler provides application top level framework"
  (let ((path (subseq rest 0 (position #\? rest)))
        (parts (or (split-string path nil #(#\/ #\.)
                                 :remove-empty-subseqs nil)))
        (modules (list module)) ;; built up in reverse order from parts
        view)
    ;; find modules by depth and view on bottom module
    (loop
       :for remainder :on parts :by #'rest
       :with part = (first remainder)
       :with module = (first modules)
       :if (not (rest remainder))
       :do (cond
             ((zerop (length part)) (redirect request "status"))
             ((setf view (getview part module)))
             ((throw 'response :not-found)))
       :else
       :do (push
            (or (get-dictionary part (or (modules module)
                                         (throw 'response :not-found)))
                (throw 'response :not-found))
            modules))
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
                     (list (strcat relative-url name "/status")
                           (title module)))
                 (butlast parts 2)
                 (rest modules))))
              ;; other available views on this module
            (views
             (mapcar
              #'(lambda(view)
                  (list (name view) (title view)))
              (views module)))))
       `(html
         (head (title ,(title module)))
         (body
          ,(navbar navigation)
          ((section :title ,(title module))
           (p ,(description module))
           ((section :title ,(title view))
            (p ,(description view))
            ,@(funcall (handler view) module request)))
          ,(navbar navigation)))))))

;; all modules should have at least a status module
(defview |status|(module)
  (:title "Status")
  (:check (module)
      (or (some #'(lambda(v) (can-view-p v module))
                (remove "status" (views module nil) :key name :test #'equal))
          (let ((modules (modules module)))
            (and modules
                 (search-dictionary modules
                                    #'(lambda(m)
                                        (let ((v (get-view "status" m)))
                                          (and v (can-view-p v m)))))))))
  (:handler(module request)
       `(((section :title "Actions Available")
          (dl
          ,@(mapcar
             #'(lambda(v)
                 `((dt ((a :href ,(name v)) ,(title v)))
                   (dd ,(description v))))
             (views module))))
         ((section :title "Modules Available")
          (dl
           ,@(mapcar
              #'(lambda(modulename)
                  (let* ((module (get-dictionary module-name (modules module)))
                         (views (views module)))
                    (when (views module)
                      `((dt ,(title module))
                        (dd
                         (p ,(description module))
                         (p ,@(mapcar
                               #'(lambda(v)
                                   `((span :class "link")
                                     ((a
                                       :title ,(description v)
                                       :href ,(strcat modulename "/" (name v)))
                                      ,(title v))))
                               (views module))))))))
              (dictionary-keys (modules module))))))))

