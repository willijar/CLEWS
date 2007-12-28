';;;; CLEWS Form handling
;;;; Copyright (C) 2002-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; Validation for Form data Handling
;;;; $Id: validation.lisp,v 1.1 2006/07/19 10:26:48 willijar Exp $

(in-package :clews.form)

(defparameter +uk-countries+
  '("England" "Falkland Islands" "Gibraltar" "Great Britain" "Northern Ireland"
    "Scotland" "United Kingdom" "Virgin Islands (British)" "Wales")
  "List of countries in the united kingdom")

(defparameter +eu-countries+
  (sort (append +uk-countries+
                '("Belgium" "Cyprus" "Denmark" "France" "Germany"
                  "Greece" "Ireland" "Italy" "Luxembourg" "Netherlands"
                  "Netherlands Antilles" "Portugal" "Spain"))
        #'string<)
  "List of countries in EU region")

(defparameter +countries+
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
    "Hungary" "Iceland" "India" "Indonesia" "Iran" "Ireland" "Israel" "Italy" "Jamaica"

    "Japan" "Jordan" "Kazakhstan" "Kenya" "Kiribati" "Korea (South)"
    "Korea, Republic of" "Kuwait" "Kyrgyzstan" "Latvia" "Lebanon" "Lesotho"
    "Liberia" "Liechtenstein" "Lithuania" "Luxembourg" "Macau" "Macedonia"
    "Madagascar" "Malawi" "Malaysia" "Maldives" "Mali" "Malta" "Marshall Islands"
    "Martinique" "Mauritania" "Mauritius" "Mayotte" "Mexico"
    "Moldova, Republic of" "Monaco" "Mongolia" "Montserrat" "Morocco" "Mozambique"
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
    "Zimbabwe")
  "List of country names for forms")