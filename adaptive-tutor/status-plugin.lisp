;;;; Status plugin for adaptive tutor
;;;; Copyright (C) 2002-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; $Id: status-plugin.lisp,v 1.1 2006/07/30 17:39:09 willijar Exp $

(in-package :clews.adaptive-tutor)

(defclass status-plugin (plugin)
  ((tutor :type adaptive-tutor :initarg :tutor :reader tutor
          :documentation "The parent adaptive tutor"))
  (:default-initargs :id :clews.adaptive-tutor-status )
  (:documentation "Who is recenty online plugin"))

;;; we have the same id as the tutor - i.e. preferences are in
;;; same property subset - is this a good idea?

(defmethod plugin-markup((self status-plugin) request rest)
  (declare (ignore rest))
  (let* ((user (remote-user request))
         (tutor (tutor self))
         (conceptid
          (property (user-component-properties tutor user) :concept)))
    (when conceptid
      (let* ((concept (concept tutor conceptid))
             (user-data (user-component-properties tutor user))
             (knowledge (knowledge user-data concept))
             (vertical-p
              (member (user-preference :position self user :right)
                      '(:right :left))))
        `(((p :escape nil)
           ,(format nil
                    (if vertical-p
                        "Welcome <b>~A</b><br>
<em>Last Visited:</em> ~A<br><em>Time Spent:</em> ~A sec<br>
<em>Completion:</em> ~4,1F%
~:[~;<b>Completed</b>~]<br>
~@[~{<em>This Mark:</em> ~4,1F%~@[<br><b>Not counted:</b> ~A~]~}~]<br>
~:[~;<em>Understanding:</em> ~4,1F%~]"
                        "Welcome <b>~A</b> |
<em>Last Visited:</em> ~A <em> |
Time Spent:</em> ~A sec |
<em>Completion:</em> ~4,1F%~:[~;<b>Concept Completed</b>~] |
~@[~{<em>This Mark:</em> ~4,1F%~@[<br><b>Not counted:</b> ~A~]~}~] |
 ~:[~;<em>Understanding:</em> ~4,1F% |~]")
                    (display-name user)
                    (if (property knowledge :last-visit-time)
                        (format-time  nil (property knowledge :last-visit-time)
                                      :fmt :short)
                        "First Visit")
                    (or (property knowledge :time-spent) 0)
                    (when-bind (c (concept-completion user-data tutor concept))
                      (* 100 c))
                    (completed-p user-data tutor concept)
                    (let ((assessment (assessment concept)))
                      (when (and assessment
                                 (assessment-feedback-p knowledge assessment))
                        (list
                         (* 100 (assessment-mark knowledge assessment))
                         (second (multiple-value-list
                                  (assessment-count-p knowledge assessment))))))
                    (and (direct-children concept)
                         (assessment-p concept))
                    (when-bind (c (concept-understanding user-data tutor concept))
                      (* 100 c)))
           (br)
           ((a :title "Meta Information about this concept"
               :href ,(format nil "~A/outcomes" (concept-id concept)))
            "Concept Metadata") (br)
           ((a :title "Expanded (for printing)"
               :href ,(format nil "~A.expanded" (concept-id concept)))
            "Expand Page") (br)
           ((a :title "Adobe Acrobat version including children (for printing)"
               :href ,(format nil "~A.pdf" (concept-id concept)))
            "PDF Format")
           ,@(when (has-permission :tutor tutor user)
                   `((br)
                     ((a :title "Marks analysis for this concept"
                       :href ,(format nil "~A/marks" (concept-id concept)))
                      "Marks")
                     (br)
                     ((a :title "LaTeX form of this concept and its children"
                       :href ,(format nil "~A.tex" (concept-id concept)))
                      "Latex Format") ))) )))))