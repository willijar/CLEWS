;;;; CLEWS:  Chatterbox plugin
;;;; Copyright (C) 2002-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; $Id: chatterbox.lisp,v 1.8 2005/03/10 20:13:09 willijar Exp $

(in-package :clews)

(defclass chatterbox-plugin (plugin)
  ((messages :type list
             :initform nil
             :documentation
             "List of messages - which are lists of a username, some
text and a datetime"))
  (:default-initargs :id :chatterbox-plugin)
  (:documentation "Instant messaging type plugin"))

(defun chatterbox-messages(self)
  "returns active list of messages in a chatterbox -  also removes old ones"
  (let* ((start (- (get-universal-time)  86400 ))
         (max 99)
         (messages (slot-value self 'messages))
         (last-cons
          (do ((i 0 (1+ i)) (a messages (rest a)))
              ((or (not a) (>= i max) (< (third (first a)) start)) a))) )
    ;;truncate list
    (when last-cons (setf (rest last-cons) nil))
    messages))

(defmethod plugin-markup((self chatterbox-plugin) request rest)
  (declare (ignore rest))
  (let* ((user (remote-user request))
         (max (user-preference :max-no-messages self user 100))
         (start (- (get-universal-time)
                   (user-preference :message-duration self user 86400))))
    (when (form-values (id self) request)
      (push (list (username user)
                  (car (form-values "text" request))
                  (get-universal-time))
            (slot-value self 'messages)))
    (let ((my-messages '()))
      (do* ((c 1 (1+ c))
            (messages (chatterbox-messages self) (rest messages))
            (msg (first messages) (first messages)))
           ((or (not msg) (> c max) (< (third msg) start)))
        (push msg my-messages))
      `((p ,@(if my-messages
                 `((em ,(multiple-value-bind(se mi ho)
                                            (decode-universal-time (third (first my-messages)))
                                            (format nil "Last message: ~2,'0d:~2,'0d:~2,'0d"
                                                    ho mi se)))
                   (br)
                   ,@(mapcan
                      #'(lambda (msg)
                          (list `(strong ,(first msg) ">") (second msg) '(br)) )
                      my-messages))
                 '((em "and all is quiet ..."))))
        ((form :method "POST")
         ((textarea :name "text"
                    :cols ,(user-preference :cols self user 20)
                    :rows ,(user-preference :rows self user 2)))
         ((input :name ,(id self) :value "Send" :type "submit")))))))

(defmethod user-component-preferences((self chatterbox-plugin) user)
  (declare (ignore user))
  (append (call-next-method)
          '((:max-no-messages
             :text "What is the maximum number of messages you want displayed?"
             :markup ((input :size 5))
             :type (integer :min 1 :max 100)
             :default 10)
            (:message-duration
             :text "What is the maximum age of message you want displayed?"
             :markup ((mcq)
                      (300 . "5 minutes")
                      (900 . "15 minutes")
                      (3600 . "1 hour")
                      (21600 . "6 hours")
                      (86400 . "1 Day"))
             :type (integer :min 1 :max 86400)
             :default 3600)
            (:rows
             :text "How many rows do you want in the form text area?"
             :markup ((input :size 5))
             :type (integer :min 1 :max 20)
             :default 2)
            (:cols
             :text "How many columns do you want in the form text area?"
             :markup ((input :size 5))
             :type (integer :min 1 :max 80)
             :default 30) )))