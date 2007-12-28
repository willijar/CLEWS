;;;; CLEWS: Who-is plugin
;;;; Copyright (C) 2003-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; $Id: who.lisp,v 1.9 2005/03/10 20:13:09 willijar Exp $

(in-package :clews)

(defclass who-plugin (plugin)
  ((who :type list
        :initform nil
        :documentation
        "List of who has been on recently and their times"))
  (:default-initargs :id :who-plugin)
  (:documentation "Who is recenty online plugin"))

(defmethod plugin-markup((self who-plugin) request rest)
  (declare (ignore rest))
  (let* ((now (get-universal-time))
         (start (- now 86400 ))
         (user (remote-user request))
         (username (username user))
         (max-age (user-preference :max-age self user 3600))
         (who (setf (slot-value self 'who)
                    (sort
                     (cons (cons username now)
                           (mapcan
                            #'(lambda(w)
                                (when (and (string-not-equal username (car w))
                                           (> (cdr w) start))
                                  (list w)))
                            (slot-value self 'who)))
                     #'string-lessp :key #'car))))
    `((p "Users here in the last "
       ,(round (/ max-age 60)) " mins.")
      ,(if who
           `(table
             (tr
              ,@(mapcan
                 #'(lambda(w)
                     (let* ((age (- now (cdr w)))
                            (min (round (/ age 60))))
                       (when (and (<= age max-age)
                                  (not (equal (username user) (car w))) )
                         `((tr
                            ,@(if (> min 0)
                                  `((td ,(car w)) ((td :align "right") ,min " min"))
                                  `((td ,(car w)) ((td :align "right") ,age " sec"))))))))
                 who)))
           '(p (em "There is no one in"))))))

(defmethod user-component-preferences((self who-plugin) user)
  (declare (ignore user))
  (append (call-next-method)
          '((:max-age
             :text "What is the maximum age of user access you want displayed?"
             :markup ((input :size 5))
             :markup ((mcq)
                      (300 . "5 minutes")
                      (900 . "15 minutes")
                      (3600 . "1 hour")
                      (21600 . "6 hours")
                      (86400 . "1 Day"))
             :type (integer :min 1 :max 86400)
             :default 900))))