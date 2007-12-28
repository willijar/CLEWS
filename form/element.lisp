;;;; CLEWS Form Handling
;;;; Copyright (C) 2002-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; The basic API for form element handling
;;;; $Id: element.lisp,v 1.4 2007/07/16 07:28:18 willijar Exp willijar $

(in-package :clews.form)

(defgeneric name(element)
  (:documentation "Element name - corresponds to the data field name"))

(defgeneric element-markup(element &optional value disabled error-msg)
  (:documentation "The list of markup-sexp template for the element
with given value if supplied, to default value otherwise"))

(defgeneric datatype(element)
  (:documentation "The data type for this element - used
to dispatch a validation /conversion function"))

(defgeneric default-value(element)
  (:documentation "The default value for this element"))

(defgeneric numerical-element-p(element)
  (:documentation "Return true if the element will result in a numerical
value"))

(defgeneric is-form-element-p(element)
  (:documentation "Return true if this is a form element - used to sleect form elements ambedded in markup"))

(defgeneric element-mark(element value)
  (:documentation "Return the normalised mark out of 1.0
associated with this value and element."))

(defgeneric element-weighting(element)
  (:documentation "Return the relative weighting for this element in a form if it is numeric"))

(defgeneric element-max(element)
  (:documentation "Return the maximum numerical value that can be returned by this element or nil"))

(defgeneric element-min(element)
  (:documentation "Return the minimum numerical value that can be
returned by this element or nil"))

(defmethod element-mark((element list) value)
  "return the normalised mark for this element with given value"
  (let ((max (element-max element)))
    (when (and (numberp max) (numberp value) (not (zerop max)))
      (/ value max))))

(defmethod numerical-element-p((element list))
  (let ((type (datatype element)))
    (when (and  (listp type) (eql (car type) 'member))
      (setf type (getf (rest type) :type)))
    (subtypep (if (listp type) (first type) type) 'number)))

(defmethod element-max((element list))
  (let ((type (datatype element)))
    (when (eql (car type) 'member) (setf type (getf (rest type) :type)))
    (when (and type (listp type) (subtypep (first type) 'number))
      (getf (rest type) :max))))

(defmethod element-min((element list))
  (let ((type (datatype element)))
    (when (eql (car type) 'member) (setf type (getf (rest type) :type)))
    (when (and type (listp type) (subtypep (first type) 'number))
      (getf (rest type) :min))))

(defmethod element-weighting(element)
  (or (element-max element) 0))

(defmethod element-weighting((element list))
  (when (listp (car element))
    (or (getf (rest (first element)) :weighting)
        (call-next-method))))

;;; Methods relating to form elements as markup s-exp
(defvar *form-element-tags* '(mcq maq input textarea select boolean)
  "A list of markup element tags which are to be processed as form elements")
(defvar *reserved-attributes* '(:value :datatype :disabled)
  "A list of attributes reserved for form element processing
 - these will be removed prior to rendering")
(declaim (inline is-reserved-attribute-p))
(jarw.debug::debug-off)
(defmethod is-form-element-p((element list))
  (let ((a (first element)))
    (when (listp a)
    (jarw.debug:debug-log "~A [~A] -> ~A" a  (symbol-package (first a)) (member (first a) *form-element-tags*)))
    (and (listp a)
         (member (first a) *form-element-tags*)
         (getf (rest a) :name)
         )))             ;(not (eql (getf (rest a) :disabled) :text) )
(jarw.debug::debug-off)

(defmethod is-form-element-p(element) (declare (ignore element)) nil)

(defun is-reserved-attribute-p(attr)
  (member attr *reserved-attributes*))

(defmethod name((markup list))
  (when (is-form-element-p markup)
    (return-from name (getf (rest (first markup)) :name)))
  (process-markup-when
   #'(lambda(element)
       (when (and (listp element) (listp (first element)))
         (eq (first (first element)) 'form)))
   markup
   #'(lambda(element)
       (return-from name (getf (rest (first element)) :name)))))

(defmethod default-value((element list))
  (getf (rest (first element)) :value
        (let ((v (second element)))
          (if (and (member (first (first element)) '(mcq maq)) (consp v))
              (car v)
              v) )))

(defmethod datatype((element list))
  (multiple-value-bind(tag attr content) (split-markup element)
    (let*((datatype (getf attr  :datatype))
          (type (if (listp datatype) (first datatype) datatype)))
      (if (and (or (eq tag 'mcq) (eq tag 'maq))
               (not (eq type 'member)))
          (let ((m `(member :type ,datatype
                     :set ,(mapcar #'(lambda(e) (if (listp e) (car e) e))
                                   content))))
            (if (eq tag 'mcq) m `(list :type ,m)))
          datatype))))

(defmethod element-markup((element list)
                          &optional (value (default-value element))
                          disabled error-msg)
  (append
   (list
    (multiple-value-bind (tag attr content) (split-markup element)
      (unless (and disabled (eql (getf attr :type) :hidden))
        (join-markup tag
                     (append (list :value value :datatype (getf attr :datatype))
                             (when-bind(d (or disabled  (getf attr :disabled)))
                               (list :disabled d))
                             (remove-attribute-if #'is-reserved-attribute-p
                                                  attr))
                     content))))
	 (when error-msg `(((P :class "error") ,error-msg)))))

;;; A form element object - for presenting in a list of such objects
(defclass form-element ()
  ((name :type string :initarg :name :reader name
         :documentation "Element name")
   (text :type string :initarg :text :initform "" :reader text
         :documentation "Prefix text - usually asking a question")
   (suffix :type string :initarg :suffix :initform "" :reader suffix
           :documentation "Suffix text - usually stating units")
   (markup-template :type list :initarg :markup :reader markup-template
                    :initform '((INPUT :type "text"))
                    :documentation "The markup-sexp for the element")
   (default :initarg :default :initform "" :accessor default-value
            :documentation "The default value for this element")
   (type :initarg :type :initform nil :accessor datatype
         :documentation "The data type for this element - used
to dispatch a validation /conversion function"))
  (:documentation "Record used to record information on a form element"))

(defmethod initialize-instance :after ((element form-element) &key)
  "Since we can, why not add in membership for mcq/maq validation"
  (multiple-value-bind(tag attr content)
      (split-markup (markup-template element))
    (let*((datatype (datatype element))
          (type (if (listp datatype) (first datatype) datatype )))
      (when (and (or (eq tag 'mcq) (eq tag 'maq))
                 (not (eq type 'member)))
        (setf (slot-value element 'type)
              (let ((m `(member :type ,datatype
                         :set ,(mapcar #'(lambda(e) (if (listp e) (car e) e))
                                       content))))
                (if (eq tag 'mcq) m `(list :type ,m))))))
    (setf (slot-value element 'markup-template)
          (join-markup tag
                       (append (list :name (name element))
                               attr)
                       content)) ))

(defmethod element-markup((element form-element)
                          &optional (value (default-value element))
                          disabled error-msg)
  `((P ,(text element) (BR)
     ,@(element-markup (markup-template element) value disabled)
     ,(suffix element))
    ,@(when error-msg `(((P :class "error") ,error-msg))) ))


(defvar +uk-countries+
  '("England" "Falkland Islands" "Gibraltar" "Great Britain" "Northern Ireland"
    "Scotland" "United Kingdom" "Virgin Islands (British)" "Wales"))

(defvar +eu-countries+
  (sort (append +uk-countries+
                '("Belgium" "Cyprus" "Denmark" "France" "Germany"
                  "Greece" "Ireland" "Italy" "Luxembourg" "Netherlands"
                  "Netherlands Antilles" "Portugal" "Spain"))
        #'string<))
(defvar +countries+
  '("Albania" "Algeria" "American Samoa" "Andorra" "Angola" "Anguilla"
    "Antarctica" "Antigua and Barbuda" "Argentina" "Armenia" "Aruba"
    "Australia" "Austria" "Azerbaijan" "Bahamas" "Bahrain" "Bangladesh"
    "Barbados" "Belarus" "Belgium" "Belize" "Benin" "Bermuda" "Bhutan"
    "Bolivia" "Bosnia and Herzegowina" "Botswana" "Bouvet Island" "Brazil"
    "British Indian Ocean Territory" "Brunei Darussalam" "Bulgaria"
    "Burkina FASO" "Burma" "Burundi" "Cambodia" "Cameroon" "Canada"
    "Cape Verde" "Cayman Islands"
    "Central African Republic" "Chad" "Chile" "China" "Christmas Island"
    "Cocos (Keeling) Islands" "Colombia" "Comoros" "Congo" "Cook Islands"
    "Costa Rica" "Cote d'Ivoire" "Croatia" "Cyprus" "Czech Republic" "Denmark"
    "Djibouti" "Dominica" "Dominican Republic" "East Timor" "Ecuador" "Egypt"
    "El Salvador" "England" "Equatorial Guinea" "Eritrea" "Espana" "Estonia"
    "Ethiopia" "Falkland Islands" "Faroe Islands" "Fiji" "Finland" "France"
    "French Guiana" "French Polynesia" "French Southern Territories" "Gabon"
    "Gambia" "Georgia" "Germany" "Ghana" "Gibraltar" "Great Britain" "Greece"
    "Greenland" "Grenada" "Guadeloupe" "Guam" "Guatemala" "Guinea"
    "Guinea-Bissau"
    "Guyana" "Haiti" "Heard and Mc Donald Islands" "Honduras" "Hong Kong"
    "Hungary" "Iceland" "India" "Indonesia" "Iran" "Ireland" "Israel" "Italy"
    "Jamaica" "Japan" "Jordan" "Kazakhstan" "Kenya" "Kiribati" "Korea (South)"
    "Korea, Republic of" "Kuwait" "Kyrgyzstan" "Latvia" "Lebanon" "Lesotho"
    "Liberia" "Liechtenstein" "Lithuania" "Luxembourg" "Macau" "Macedonia"
    "Madagascar" "Malawi" "Malaysia" "Maldives" "Mali" "Malta"
    "Marshall Islands" "Martinique" "Mauritania" "Mauritius" "Mayotte"
    "Mexico" "Moldova, Republic of" "Monaco" "Mongolia" "Montserrat" "Morocco"
    "Mozambique"
    "Myanmar" "Namibia" "Nauru" "Nepal" "Netherlands" "Netherlands Antilles"
    "New Caledonia" "New Zealand" "Nicaragua" "Niger" "Nigeria" "Niue"
    "Norfolk Island" "Northern Ireland" "Northern Mariana Islands" "Norway" "Oman"
    "Pakistan" "Palau" "Panama" "Papua New Guinea" "Paraguay" "Peru" "Philippines"
    "Pitcairn" "Poland" "Portugal" "Puerto Rico" "Qatar" "Reunion" "Romania"
    "Russia" "Russian Federation" "Rwanda" "Saint Kitts and Nevis" "Saint Lucia"
    "Samoa (Independent)" "San Marino" "Sao Tome and Principe" "Saudi Arabia"
    "Scotland" "Senegal" "Seychelles" "Sierra Leone" "Singapore" "Slovakia"
    "Slovenia" "Solomon Islands" "Somalia" "South Africa" "South Korea" "Spain"
    "Sri Lanka" "St. Helena" "St. Pierre And Miquelon" "Suriname"
    "Svalbard and Jan Mayen Islands" "Swaziland" "Sweden" "Switzerland" "Taiwan"
    "Tajikistan" "Tanzania" "Thailand" "Togo" "Tokelau" "Tonga" "Trinidad"
    "Trinidad and Tobago" "Tunisia" "Turkey" "Turkmenistan"
    "Turks and Caicos Islands" "Tuvalu" "U.S.A." "Uganda" "Ukraine"
    "United Arab Emirates" "United Kingdom" "United States" "Uruguay" "Uzbekistan"
    "Vanuatu" "Vatican City State (Holy See)" "Venezuela" "Viet Nam"
    "Virgin Islands (British)" "Virgin Islands (U.S.)" "Wales"
    "Wallis and Futuna Islands" "Western Sahara" "Yemen" "Yugoslavia" "Zambia"
    "Zimbabwe"))
