;;;; Aston Web Applications
;;;; Copyright (C) 2004-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; $Id: discussions.lisp,v 1.2 2005/03/13 09:33:21 willijar Exp $

(in-package :aston)

(defvar *assessments*
  (make-instance
   'assessment-application
   :assessment-path (translate-logical-pathname #p"clews:assessments;*.quiz")
   :user-dictionary *user-source*
   :pwd-source *pwd-source*
   :acl '((:view . (:all))
          (:student :student :staff "test")
          (:tutor "willijar" :staff)
          (:admin  "willijar"))))

(defvar *labgroups*
  '((:msc-2008 "akanircc" "chhabrad" "dodiakn" "gagardo" "kapolonc" "lawalao1"
     "malikma" "miottio" "ntezilrj" "ogundegb" "olssonj" "otisic"
      "rainalda" "salmanh" "seyedebm" "sharafs1" "tariqa3" "tewarirs" "tiany2"
     "udumacn")
    (:ee3dtr-2008 "bockettb" "campbell" "chuakv" "clarkda" "griffigg" "hjborhny" "kassinig" "lausw" "leegcb" "mcculloh" "mubarakm" "patelak3" "pgahmdmz" "reesbosl" "stonecm" "tchamrrk" "vassilic" "warnerm") ))

;(add-aston-users '("bockettb" "campbell" "chuakv" "clarkda" "griffigg" "hjborhny" "kassinig" "lausw" "leegcb" "mcculloh" "mubarakm" "patelak3" "pgahmdmz" "reesbosl" "stonecm" "tchamrrk" "vassilic" "warnerm") :roles '(:ug :ee3dtr-2008 :student))

(defvar *labqs*
  '("dtr-lab-qam1" "dtr-lab-qam2" "dtr-lab-pcm1" "dtr-lab-pcm2"))

(defun lab-table(group &optional (questionnaires *labqs*))
  `(table
    (tr (th "Student")
     ,@(mapcar #'(lambda(q) `(th ,q)) questionnaires)
     (th "Total"))
    ,@(mapcar
       #'(lambda(username)
           (when username
             (unless (get-dictionary username *user-source*)
               (error "username ~S unknown" username))
             (let ((marks nil))
               `(tr
                 (th ,(clews.assessments::display-name
                       (get-dictionary username *user-source*))
                  " (" ,username ")")
                 ,@(mapcar
                    #'(lambda(q)
                        `((td :align :right)
                          ((a :href ,(format
                                      nil
                                      "/assessments/~A/attempt/~A"
                                      q username))
                           ,(let* ((assessment
                                    (clews.assessments::get-assessment
                                     q *assessments*))
                                   (knowledge
                                    (if assessment
                                        (clews.assessments::knowledge
                                         username assessment)
                                        (error "Assessment ~S not known" q))))
                                  (if (clews.assessment::completed knowledge)
                                      (let ((mark (clews.assessment::assessment-mark
                                                   knowledge assessment)))
                                        (push mark marks)
                                        (format nil "~,1F" (* 100 mark)))
                                      (progn
                                        (push 0 marks)
                                        "X")))
                           )))
                    questionnaires)
                 ((td :align :right)
                  ,(format nil "~,1F" (* 100 (jarw.math::mean marks)))) ))))
       group)))


(defun lab-page(request rest &optional (groups *labgroups*) (questionnaires *labqs*))
  (declare (ignore request rest))
  (html nil
  `(html
    (head (title "Lab Groups"))
    (body
     ((section :title "Lab Groups")
      ,@(mapcar
	#'(lambda(g)
	    `((section :title ,(string (car g)))
	      ,(lab-table (cdr g) questionnaires)))
	groups))))))

(defun aggregate-marks(usernames assessmentnames)
  (dolist(username usernames)
    (format t "~9A ~,2F~%"
	    username
	    (* 100
               (/
                (reduce #'+
                        (mapcar
                         #'(lambda(q)
                             (let* ((assessment (clews.assessments::get-assessment
                                                 q *assessments*))
                                    (knowledge (clews.assessments::knowledge
                                                username assessment)))
                               (or (clews.assessment::assessment-mark
                                    knowledge assessment) 0)))
                         assessmentnames))
                (length assessmentnames))))))

(in-package :clews.assessment)

(defun lspline(x y)
  "Helper function for splining experimental erfc data on log-log
space - returns plist of the min, max value and the splining closure"
  (let ((spline (jarw.math::cubic-spline
                 (map 'vector #'log x)
                 (map 'vector #'log y))))
    (list :min (reduce #' min x)
          :max (reduce #'max x)
          :spline #'(lambda(x) (exp (funcall spline (log x)))))))

;(defparameter *tims-ber-coeff*
;  '(((:psk . 4)  :p #(0.34065 3.03467) :min 5.3054e-01 :max 9.3847e-01)
;    ((:psk . 8)  :p #(0.34905 1.79675) :min 5.7510e-01 :max 1.8440e+00)
;    ((:psk . 16) :p #(0.28697 0.99033) :min 5.4564e-01 :max 4.2834e+00)
;    ((:qam . 4)  :p #(0.57195 2.91395) :min 7.5417e-01 :max 1.0100e+00)))
;    ((:qam . 8)  :p #(0.12750 0.31982) :min 5.8921e-01 :max 2.6071e+01)
;    ((:qam . 16) :p #(0.13857 0.26348) :min 5.4938e-01 :max 3.1731e+01)))

(defparameter *tims-ber-coeff*
  `(((:psk . 4)
     ,@(lspline
        #(0.42000 0.54000 0.66000 0.67000 0.84000 0.85000 1.07000)
        #(2.0100e-02 5.3300e-03 9.1000e-04 1.4100e-03 6.1000e-05
          5.1000e-05 2.0000e-06)))
    ((:psk . 8)
     ,@(lspline
        #( 0.42000 0.53000 0.66000 0.84000 1.04000
          1.33000 1.70000 2.11000 2.54000 )
        #( 9.2400e-02 5.0700e-02 2.5300e-02 9.9700e-03
          3.1600e-03 4.5000e-04 5.7000e-05 8.0000e-06 1.0000e-06 )))
    ((:psk . 16)
     ,@(lspline
        #(0.42000 0.54000 0.68000 0.81000 1.05000 1.34000
          1.71000 2.12000 2.66000 3.03000 4.39000 5.23000)
        #(1.6300e-01 1.1900e-01 8.4300e-02 6.8000e-02 3.8200e-02
          2.2200e-02 1.0200e-02 4.2800e-03 1.3300e-03 3.0900e-04
          2.3000e-05 3.0000e-06 )))
    ((:qam . 4)
     ,@(lspline
        #(0.42000 0.54000 0.69000 0.85000 1.06000
          1.34000 1.71000)
        #(6.3700e-02 3.2300e-02 1.4000e-02 4.2300e-03
          1.2800e-03 1.7200e-04 1.7000e-05)))))

(defun tims-ber-question(record)
  (unless (init-data record)
    (let* ((rec (elt *tims-ber-coeff* (random (length  *tims-ber-coeff*))))
           (min (getf (cdr rec) :min))
           (max (getf (cdr rec) :max)))
      (setf (getf (init-data record) :type) (car rec)
            (getf (init-data record) :snr) (+ min (random (- max min))))))
  (let* ((type (getf (init-data record) :type))
         (snr (getf (init-data record) :snr))
         (rec (cdr (assoc type *tims-ber-coeff* :test #'equal)))
         (ber (funcall (getf rec :spline) snr)))
    `(NUMERIC-Q
      :QUESTION
      (P ,(format nil "What is the bit-error-rate (BER) measured using the TIMS
      equipment with the ~A-~D modulation
      scheme and a signal-to-noise (SNR) ratio of ~,3F?"
                  (symbol-name (car type)) (cdr type) snr))
      :answer ,ber
      :weighting 1/3
      :tol 0.6
      :format "~,1E" )))