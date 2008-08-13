;;;; Commands with logging -*- Lisp -*- Copyright (C) 2008
;;;; John A.R. Williams <J.A.R.Williams@jarw.org.uk> Released under
;;;; the GNU General Public License (GPL) See
;;;; <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;

;;; commands are functions which take a command processor as the first
;;; argument and which may be logged to a stream for future playback
;;; the log file records the username, date and the command name and args
;;; on playback username is set.

(in-package :jarw.command)

(defconstant command-symbol 'command)

(defparameter *log-commands* t
  "If tru record command on an entities command log")

(defgeneric command-log(command-processor)
  (:documentation "Return the command-log entity for a command processor")
  (:method(command-processor) *standard-output*))

(defgeneric log-command(command entity args)
  (:documentation "Record the command and args in a command log")
  (:method(command command-processor args)
    (log-command command (command-log command-processor) args))
  (:method(command (command-log stream) args)
    (print
     (cons
      (cons (when *current-user* (username *current-user*))
            (format-output 'date (get-universal-time)))
      (cons command args))
     command-log))
  (:method(command (command-log pathname) args)
    (with-open-file(os command-log
                       :direction :output
                       :if-exists :append
                       :if-does-not-exist :create)
      (log-command command os args))))

(declaim (inline execute-command))
(defun execute-command(command command-processor args
                       &optional (log-commands *log-commands*))
  (apply (get command command-symbol) (cons command-processor args))
  (when log-commands (log-command command command-processor args)))

(defmacro defcommand(name (entity &rest args) &body body)
  `(setf (get ',name ',command-symbol)
         #'(lambda(,entity ,@args) ,@body)
         (symbol-function ',name)
         #'(lambda(,entity &rest args)
             (execute-command ',name ,entity args))))

(defgeneric playback-commands(source command-processor &optional log-commands)
  (:documentation "Playback commands on command processor from a
  source - if log-commands is true (default nil) commands will also be
  recorded")
  (:method((is stream) command-processor &optional log-commands)
    (do((entry (read is nil nil)))
       ((not entry))
      (let ((command (cadr entry))
            (*current-user* (caar entry))
            (args (cddr entry)))
        (execute-command command command-processor args log-commands))))
  (:method((command-log pathname) command-processor &optional log-commands)
    (with-open-file(is command-log :direction :input)
      (playback-commands is command-processor log-commands))))
