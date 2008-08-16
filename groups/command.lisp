;;;; Commands with logging and undo -*- Lisp -*- Copyright (C) 2008
;;;; John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL) See
;;;; <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;

;;; commands are functions which take a command processor as the first
;;; argument and which may be logged to a stream for future playback
;;; the log file records the username, date and the command name and args
;;; on playback username is set. They may also define an undo.

(in-package :jarw.command)

(defconstant command-property 'command)

(defclass command()
  ((documentation :initarg :documentation :initform nil
                  :documentation "User defined documentation on this command")
   (permission-check
    :type function :reader permission-check :initarg :check
    :initform #'(lambda(&rest args) (declare (ignore args)) t)
    :documentation "Return a function to check if *current-user* is allowed ")
   (command-action
    :type function :reader command-action :initarg :action
    :documentation "Return Function which takes entity and args and
    carries out action")
   (undo-action :type function :initform nil :reader undo-action :initarg :undo
                :documentation "Return a function which given entity
                and args would undo the changes on entity - called
                before command-action. If nil command cannot be
                undone."))
  (:documentation "Class representing a command. Each of the slots
  contains a function which takes the entity and other args."))

(defmethod describe-object((object command) stream)
  (format stream "~&~S is a command.~@[~%~A~]"
             object (slot-value object 'documentation)))

(defclass command-processor()
  ((command-log
    :initform nil :accessor command-log :initarg :command-log
    :documentation "pathname or stream for logging commands")
   (undo-stack :accessor undo-stack :initform nil
               :documentation "Undo stack.")
   (redo-stack :accessor redo-stack :initform nil
               :documentation "Redo stack")
   (max-undos :accessor max-undos :initform 0 :initarg :max-undos
              :Documentation "Maximum length of undo stack"))
  (:documentation "A command processor entity"))

(defmethod initialize-instance :after
    ((command-processor command-processor) &key initialise-log)
  (when initialise-log
    (playback-commands initialise-log command-processor)))

(defgeneric log-command(command entity args)
  (:documentation "Record the command and args in a command log")
  (:method(command (command-processor command-processor) args)
    (let ((*package*
           (symbol-package (class-name (class-of command-processor))))
          (log (command-log command-processor)))
      (when log (log-command command log args))))
  (:method(command (command-log null) args)
    (declare (ignore command args)) ) ;; do nothing if no command log
  (:method(command (command-log stream) args)
    (print
     (list (when *current-user* (username *current-user*))
            (format-output 'date (get-universal-time))
      (cons command args))
     command-log))
  (:method(command (command-log pathname) args)
    (with-open-file(os command-log
                       :direction :output
                       :if-exists :append
                       :if-does-not-exist :create)
      (log-command command os args))))

(defstruct undo
  (command nil)
  (function nil)
  (user *current-user*)
  (args))

(defun record-undo(command entity undo args)
  (when (> (max-undos entity) 0)
    (push (make-undo :command command :undo undo :args args)
          (undo-stack entity))
    (setf (redo-stack entity) nil)
    (when (> (length (undo-stack entity)) (max-undos entity))
      (setf (rest (nthcdr (max-undos entity) (undo-stack entity))) nil))))

(defun can-execute-command-p(commandname entity &rest args)
  (apply (permission-check (get commandname command-property ))
         (cons entity args)))

(defun execute-command(commandname command-processor args &key (log t))
  (let ((command (get commandname command-property ) )
        (allargs (cons command-processor args)))
    (unless (apply (permission-check command) allargs)
      (error 'permission-denied-error
             :action commandname
             :service command-processor))
    (let ((undo-action (undo-action command)))
      (if undo-action
          (let ((undo (apply undo-action allargs)))
            (restart-case
                (prog1
                  (apply (command-action command) allargs)
                  (when log
                    (handler-bind
                        ((error #'(lambda(c)
                                    (declare (ignore c))
                                  (invoke-restart 'undo))))
                      (log-command commandname command-processor args)))
                  (record-undo commandname command-processor undo args))
              (undo()
                :report (lambda(stream) (format stream "Undo ~A" commandname))
                (funcall undo command-processor))))
          (prog1
              (apply (command-action command) allargs)
            (when log (log-command commandname command-processor args))
            (case commandname
              (undo)
              (redo)
              (t (record-undo command command-processor nil args))))))))

(defmacro defcommand(name (&rest args) &rest parts)
  (let ((check (rest (assoc :check parts)))
        (action (rest (assoc :action parts)))
        (undo (rest (assoc :undo parts)))
        (doc (second (assoc :documentation parts)))
        (entity (gensym)))
    (unless action (error "No command action defined for ~A" name))
    `(setf (get ',name ',command-property)
           (make-instance
            'command
            ,@(when check `(:check (function (lambda(,@args) ,@check))))
            ,@(when undo `(:undo (function (lambda(,@args) ,@undo))))
            :action (function (lambda(,@args) ,@action))
            :documentation ,doc)
           (symbol-function ',name)
           #'(lambda(,entity &rest args)
               (execute-command ',name ,entity args)))))

(defgeneric playback-commands(source command-processor &key log)
  (:documentation "Playback commands on command processor from a
  source - if log-commands is true (default nil) commands will also be
  recorded")
  (:method((is stream) command-processor &key log)
    (do((entry (read is nil nil) (read is nil nil)))
       ((not entry))
      (let ((command (car (third entry)))
            (*current-user* (first entry))
            (args (cdr (third entry))))
        (execute-command command command-processor args :log log))))
  (:method((command-log pathname) command-processor &key log)
    (with-open-file(is command-log :direction :input)
      (playback-commands is command-processor :log log)))
  (:method((is pathname) (command-processor command-processor) &key log)
    (declare (ignore log))
    (let ((*package*
           (symbol-package (class-name (class-of command-processor)))))
      (call-next-method))))

;; undo and redo are commands but are handled differently in execute-command
(defcommand undo(entity)
  (:documentation "Undo last action")
  (:check
   (let ((u (first (undo-stack entity))))
     (when u
       (and (undo-function u)
            (or (has-permission :admin entity)
                (equal *current-user* (undo-user u)))))))
  (:action
   (let ((undo (pop (undo-stack entity))))
     (funcall (undo-function undo) entity)
     (push undo (redo-stack entity))  )))

(defcommand redo(entity)
  (:documentation "Redo last action (undo last undo)")
  (:check
   (let ((redo (first (redo-stack entity))))
     (when redo
       (or (has-permission :admin entity)
           (equal *current-user* (undo-user redo))))))
  (:action
   (let ((redo (pop (redo-stack entity))))
     (execute-command
        (undo-command redo) entity (undo-args redo)))))

(defcommand configure(entity classname &rest args)
  (:documentation "Reconfigure assessment according to some initargs -
  only allowed to be called once on a command-processor object.")
  (:check
   (declare (ignore classname args))
   (eql (class-of entity) #.(find-class 'command-processor)))
  (:action
   (change-class entity classname)
   (setf *package* (symbol-package classname))
   (apply #'reinitialize-instance (cons entity args))))

