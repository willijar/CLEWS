;;;; CLEWS: Polls plugin
;;;; Copyright (C) 2003-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; $Id: polls.lisp,v 1.5 2005/03/10 20:13:09 willijar Exp $

(in-package :clews)

(defun make-poll(description choices deadline &key (roles '(:authenticated)))
  (list :text description
        :roles roles
        :casters '()
        :closed nil
        :deadline (ctypecase deadline
                    (integer deadline)
                    (string (parse-input 'date deadline)))
        :choices (make-array (length choices)
                             :initial-contents (mapcar
                                                #'(lambda(c) (cons c 0))
                                                choices))))

(defun description(poll) (getf poll :text))
(defun deadline(poll) (parse-input 'date (getf poll :deadline) :fmt :short))
(defun choices(poll) (getf poll :choices))
(defun casters(poll) (getf poll :casters))
(defun no-votes-caste(poll) (reduce #'+ (choices poll) :key #'cdr))
(defun closed(poll) (getf poll :closed))
(defun can-view-poll(poll user)
  (and (or (has-role user (getf poll :roles))
           (member (username user) (getf poll :roles) :test #'equal))
       (not (closed poll))))

(defun deadline-passed(poll) (> (get-universal-time) (getf poll :deadline)))
(defun has-caste-vote(poll user)
  (member (username user) (casters poll) :test #'equal))

(defun can-caste-vote(poll user)
  (and (not (or (deadline-passed poll)
                (has-caste-vote poll user)))
       (can-view-poll poll user)))

(defun can-view-results(poll user)
  (and (deadline-passed poll)
       (can-view-poll poll user)))

(defun caste-vote(poll user option)
  (when (can-caste-vote poll user)
    (incf (cdr (elt (choices poll) option)))
    (push (username user) (getf poll :casters))))

(defun close-poll(poll) (setf (getf poll :closed)(close-poll t)))

(defclass polls-plugin (plugin)
  ((file :initarg :file :reader file
         :documentation "File where poll information is to be stored")
   (polls :type list :initform '() :accessor polls
          :documentation "A list of polls objects"))
  (:default-initargs :id :poll-plugin)
  (:documentation "Plugin allows user to vote on polls"))

(defun save-data(polls)
  (with-open-file(out (file polls) :if-exists :supersede
                      :direction :output)
    (prin1 (polls polls) out)))

(defun add-poll(polls id description choices deadline &key
                    (roles '(:authenticated)))
  (push (cons id (make-poll description choices deadline :roles roles) )
        (polls polls))
  (save-data polls))

(defun remove-poll(polls id)
  (close-poll (rest (assoc id (polls polls))))
  (save-data polls))

(defmethod initialize-instance :after ((self polls-plugin) &key)
  "We accumulate a list of which users have public annotations on which pages
   here"
  (setf (polls self)
        (with-open-file(in (file self)
                           :if-does-not-exist :create
                           :direction :input)
          (read in nil))))

(defmethod plugin-markup((self polls-plugin) request rest)
  (declare (ignore rest))
  (let* ((user (remote-user request))
         (submitted (form-values (id self) request))
         (polls (mapcan #'(lambda(rec)
                            (when (or (can-caste-vote (cdr rec) user)
                                      (can-view-results (cdr rec) user))
                              (list rec)))
                        (polls self)))
         (forms-displayed 0))
    (when polls
      `(((form :method "POST")
         ,@(mapcan
            #'(lambda(record)
                (let ((id (first record))
                      (poll (rest record)))
                  `((hr)(p ,(description poll))
                    ,(cond
                      ((can-caste-vote poll user)
                       (let ((choice (car (form-values id request))))
                         (if (and submitted choice)
                             (handler-case
                                 (progn
                                   (caste-vote poll user
                                               (parse-integer choice))
                                   (save-data self)
                                   '(p "Vote caste successfully"))
                               (error () '((p :class :error)
                                           "Vote caste unsuccessful")))
                             `((mcq :datatype 'integer :name ,id)
                               ,@(let ((n -1))
                                      (incf forms-displayed)
                                      (map 'list
                                           #'(lambda(c)
                                               (cons (incf n) (car c)))
                                           (choices poll)))))))
                      ((can-view-results poll user)
                       (let ((n (no-votes-caste poll)))
                         `(table
                           ,@(map 'list
                                  #'(lambda(c)
                                      `(tr (th ,(car c))
                                        ((td :align :right) ,(cdr c))
                                        ((td :align :right)
                                         ,(floor
                                           (* 100
                                              (if n (/ (cdr c) n) 0))) "%")))
                                  (choices poll))
                           (tr (th "Total") (td ,n)))))
                      (t '((p "unavailable")))))))
            polls)
         ,(when (> forms-displayed 0)
                `((input :name ,(id self) :value "Vote" :type :submit))))))))
