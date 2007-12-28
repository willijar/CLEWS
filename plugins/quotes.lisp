;;;; CLEWS: Quotes plugin
;;;; Copyright (C) 2002-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; reads fortune type files, quotes delimited by % on a line by itself
;;;; $Id: quotes.lisp,v 1.5 2005/03/10 20:13:09 willijar Exp $

(in-package :clews)

(defclass quotes-plugin (plugin)
  ((file :type pathname	:initarg :file
         :reader file
         :documentation "File where quotes are stored")
   (no-quotes :type integer :initform 0
              :documentation "No of quotes in file")
   (file-write-date :type integer
                    :initform 0
                    :documentation "When the file was last modified")
   (quote :type string :documentation "Current quote")
   (update-period :type integer
                  :initarg :update-period
                  :initform (* 60 60 24)
                  :documentation "How often quote is to be updated")
   (next-update :type integer
                :initform (get-universal-time)
                :documentation "Time after which quote is to be updated"))
  (:default-initargs :id :quotes-plugin)
  (:documentation "Quote of the dat type plugin"))


(defmethod initialize-instance :after ((quotes quotes-plugin) &key)
  (assert (probe-file (file quotes)) () "Quotes file ~S does not exist"
          (file quotes)))

(defun no-quotes(quotes)
  (when (> (file-write-date (file quotes))
           (slot-value quotes 'file-write-date))
    (with-open-file (s (file quotes))
      (let ((line nil) (c 0))
        (handler-case
            (while (setq line (read-line s nil nil))
              (when (string-equal line "%") (incf c)))
          (error () "Error reading quote file ~S, last line read ~S"
                 (file quotes) line))
        (setf (slot-value quotes 'file-write-date)
              (file-write-date (file quotes)))
        (setf (slot-value quotes 'no-quotes) c))))
  (slot-value quotes 'no-quotes))

(defun get-quote(quotes index)
  (assert (< index (no-quotes quotes)) ()
          "Index ~D exceeds number of quotes in ~S" index (file quotes))
  (with-open-file (s (file quotes))
    (do ((c 0))
        ((= index c))
      (when (string-equal (read-line s) "%") (incf c)))
    (with-output-to-string(out)
      (while (let ((line (read-line s)))
               (when (not (string-equal line "%"))
                 (write-line line out)))))))

(defun current-quote(quotes)
  (let ((now (get-universal-time)))
    (when (> now (slot-value quotes 'next-update))
      (setf (slot-value quotes 'quote)
            (get-quote quotes (random (no-quotes quotes))))
      (incf (slot-value quotes 'next-update)
            (slot-value quotes 'update-period))))
  (slot-value quotes 'quote))

(defmethod plugin-markup((self quotes-plugin) request rest)
  (declare (ignore request rest))
  `(((pre),(current-quote self))))

(defmethod user-component-preferences((self quotes-plugin) user)
  (declare (ignore user))
  '((:position
     :text "Where on the the page do you wish to view this plugin?"
     :markup ((mcq)
              (:top . "Top")
              (:bottom . "Bottom")
              (:no . "Disable it") )
     :type symbol
     :default :bottom)
    (:font-size
     :text "What size do you want the text ?"
     :type string
     :markup ((mcq)
              ("100%" . "same size as main page")
              ("smaller" . "smaller")
              ("x-small" . "small")
              ("xx-small" . "very small"))
     :default "smaller")))