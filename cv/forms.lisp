;;; $Id: forms.lisp,v 1.3 2005/03/10 19:55:14 willijar Exp $
;;;Peer review application class for EWAS
;;;Copyright Dr John A.R. Williams (c) 2003. Please see LICENSE.txt

(in-package :clews.cv)

(register-form
 "cv"
 `((form :name "cv" :method "POST")
   ((section :title "Name")
    ((input :name :title :size 60)))
   ((section :title "Education")
    ((section :title "Qualifying Degree")
     (p "If you hold a degree or equivalent please give details below.
This should be the qualification most relevant prior to studying on
this program")
     (table
      (tr
       ((th :align :right)  "Subject: ")
       (td ((input :name :subject :size 40)))
       ((th  :align :right) "Award (e.g. BEng):")
       (td ((input :name :degree-award :size 10))))
      (tr
       ((th  :align :right) "University:")
       (td ((input :name :university :size 20)))
       ((th  :align :right) "Degree Class, GPA or equivalent:")
       (td ((input :name :degree-class :size 5))))
      (tr
       ((th  :align :right) "Country: ")
       (td ((mcq :style :dropdown :name :country :value "United Kingdom")
            ,@+countries+))
       ((th  :align :right) "Year Awarded: ")
       (td ((input :name :degree-date :size 12
                   :datatype (integer :min 1950 :max 2010)))))))
    ((section :title "Alternative Education")
     (p "If you do not hold a degree, or have other relevant
qualifications that might be of interest to a potential employer,
please provide comprehensive information about your technical
education, to include dates and places of attendance, examinations
passed and the marks or grades awarded to you")
     ((textarea :structured-text t :name :education-other :cols 80 :rows 10)))
    ((section :title "English")
     (p "Please indicate you knowledge/training of english. If
you studied English as a foreign language give the date and test score")
     (table
      (tr (td ((mcq :name :english :style :dropdown)
               "First Language" "TOEFL" "ELTS" "GCSE" "Other"))
          ((th :align :right) "Test Score:")
          (td ((input :name :english-score :size 5
                      :datatype (string :nil-allowed t))))
          ((th :align :right) "Test Date")
          (td ((input :name :english-date :size 12
                      :datatype (date :nil-allowed t))))))
     (p "If other give details below" (br)
        ((textarea :name :english-other :datatype (string :nil-allowed t)
                   :cols 80 :rows 2)))))
   ((section :title "Professional Qualifications and Experience")
    (p "Give details of any relevant professional experience and professional
qualifications held including the names of the institutions and dates of awards. You can use structured text formating.")
    ((textarea :structured-text t :name :professional
               :datatype (string :nil-allowed t)
               :cols 80 :rows 10)))
   ((section :title "Placement Preferences and Statement")
    (p "Please indicate your preferences in terms of the project
placement, and provide a statement about why you want to do this type
of project and what particular qualities you will bring to it.  You
should indicate whether you would prefer a software, hardware or
management type project, if there are specific subject preferences
 (mobile, photonics, e-commerce etc) and if you have preferences in
terms of where the placement should take place.  If you are a foreign
national you may wish to indicate that you are willing to take up, if
offered, full time employment in the UK.This will aid the supervisors
decide on what projects would be most suitable for you. Obviously make
sure when choosing your placements that your preferences match its
activities.  You can use structured text formating. A minimum of 150 words is required.")
    ((textarea :structured-text t :name :placement-preferences
               :datatype (string :nil-allowed t :word-count 150)
               :cols 80 :rows 10)))
   ((section :title "Further Information")
    (p "If you wish to create your own personal CV web page please
enter its URL here")
    ((input :name :url :size 40 :datatype (string :nil-allowed t))))
   ((input :type :submit :name "submit" :value "Update CV"))))

(defmethod markup-form((form (eql (find-form "cv")))
                       &optional data (disabled nil)
                       (element-markup #'element-markup))
  (let ((data (if (typep data 'inet.http:request) (form-data form data) data)))
    (if disabled
        `((section :nonumber t :title ,(getf data :title))
          ((section :nonumber t :title "Education")
           ((section :nonumber t :title "Qualifying Degree")
            (table
             (tr
              ((th :align :right)  "Subject: ")
              (td ,(getf data :subject))
              ((th  :align :right) "Award:")
              (td ,(getf data :degree-award)))
             (tr
              ((th  :align :right) "University:")
              (td ,(getf data :university))
              ((th  :align :right) "Degree Class, GPA or equivalent:")
              (td ,(getf data :degree-class)))
             (tr
              ((th  :align :right) "Country: ")
              (td ,(getf data :country))
              ((th  :align :right) "Year Awarded: ")
              (td ,(getf data :degree-date)))))
           ,(when (getf data :education-other)
                  `((section :nonumber t :title "Alternative Education")
                    ,(markup::parse-structured-text
                      (getf data :education-other) ))))
          ((section :nonumber t :title "English")
           (p ,(getf data :english))
           ,(when (or  (getf data :english-score) (getf data :english-date))
                  `(table
                    (tr
                     ,@(when (getf data :english-score)
                             `(((th :align :right) "Test Score:")
                               (td ,(getf data :english-score))))
                     ,@(when (getf data :english-date)
                             `(((th :align :right) "Test Date")
                               (td ,(format-time nil (getf data  :english-date)
                                                 :fmt :date-only)))))))
           ,(when (getf data :english-other)
                  `(p ,(getf data :english-other))))
          ,(when (getf data :professional)
                 `((section :nonumber t
                    :title "Professional Qualifications and Experience")
                   ,(markup::parse-structured-text
                     (getf data :professional))))
          ,(when (getf data :placement-preferences)
                 `((section :nonumber t :title "Placement Preferences and Statement")
                   ,(markup::parse-structured-text
                     (getf data :placement-preferences))))
          ,(when (getf data :url)
                 `((section :nonumber t :title "Further Information")
                   (p "See " ((a :href ,(getf data :url))
                              ,(getf data :url))))))
        (call-next-method form data disabled element-markup))))



