;;;; CLEWS Plugins API
;;;; Copyright (C) 2002-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; $Id: plugin.lisp,v 1.2 2007/07/21 07:44:25 willijar Exp $

(in-package :clews)

(defclass plugin (component)
  ()
  (:documentation "Abstract Base Class for plugins"))

(defgeneric plugin-markup(plugin request rest)
  (:documentation "Returns the markup for this plugin"))

(defmethod user-component-preferences((self plugin) user)
  (declare (ignore user))
  '((:position
     :text "Where on the the page do you wish to view this plugin?"
     :markup ((mcq)
              (:left . "Left Side")
              (:right . "Right Side")
              (:bottom . "Bottom")
              (:top . "Top")
              (:no . "Disable it") )
     :type symbol
     :default :right)
    (:font-size
     :text "What size do you want the text ?"
     :type string
     :markup ((mcq)
              ("100%" . "same size as main page")
              ("smaller" . "smaller")
              ("x-small" . "small")
              ("xx-small" . "very small"))
     :default "smaller")))
