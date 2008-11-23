;;;; CLEWS Form Analysis handling
;;;; Copyright (C) 2002-2008 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;

(in-package :clews.form)

(defun mark-form-data(data)
  (form-mark data (find-form data)))

(defgeneric form-mark(data form)
  (:documentation "Calculate a mark from data - by default calculates it
   on the basis of all elements which are number types and have a maximum
   value set.")
  (:method (data form)
    (let ((sum 0) (total 0))
      (dolist(element (elements form))
        (let ((v (or (element-mark element (getf data (name element))) 0))
              (w (element-weighting element)))
          (when (and w v)
            (incf total w)
            (incf sum (* w v)))))
      (when (> total 0)
        (/ sum total)))))

(defun form-mark-evaluator(expr)
  "Given a form mark evaluation expression return a lambda for evaluation
The following values are lexically bound fopr expr
fields - list of form field names
weighting - function which returns the weighting"
  `(lambda(data form)
     (let* ((elements (elements form))
            (fields (mapcar #'name elements)))
       (labels ((element(name) (find name elements :test #'equal :key #'name))
                (value(fields)
                  (if (consp fields)
                      (mapcar #'value fields)
                      (or (element-mark (element fields) (getf data fields)) 0)))
                (weighting(fields)
                  (if (consp fields)
                      (mapcar #'weighting fields)
                      (element-weighting (element fields))))
                (weighted-sum(fields)
                  (let ((sum 0) (total 0))
                    (dolist(field fields)
                      (let ((v (value field))
                            (w (weighting field)))
                        (format t "~S ~A ~A ~%" field v w)
                        (when (and (numberp v) (numberp w))
                          (incf total w)
                          (incf sum (* w v)))))
                    (when (> total 0) (/ sum total)))))
         ,expr))))

#| e.g. standard form mark using form-mark-evaluator
(form-mark-evaluator '(clews.form::weighted-sum clews.form::fields))
|#

(defmethod form-mark(data (form list))
  (let ((expr (getf (form-attrs form) :form-mark)))
    (if expr
        (funcall (eval (form-mark-evaluator expr)) data form)
        (call-next-method))))

(defgeneric form-analysis-markup(form data)
  (:documentation "Produce markup of analysis of given item for given data"))

(defmethod form-analysis-markup(form dataset)
  (let* ((elements (elements form))
         (values
          (mapcan
           #'(lambda(element)
               (let ((name (name element)))
                 (list name (mapcar #'(lambda(item)
                                        (getf item name))
                                    dataset))))
           elements)))
    (markup-form form values t #'element-analysis-markup)))

(defgeneric element-analysis-markup(element values &rest rest)
  (:documentation "List of sexp marking up question with analysis"))

(defmethod element-analysis-markup :around (element values &rest rest)
  (declare (ignore element rest))
  (append
   (call-next-method)
   (let ((values (mapcan #'(lambda(n) (when n (list n)))
                         values)))
     (when (and (> (length values) 0) (every #'numberp values))
       `((br)((table :border 1 :cellspacing 0)
              (tr (th "Counted")  (th "Mean") (th "Std Dev")
               (th "Maximum") (th "Minimum"))
              (tr (td ,(length  values))
               ,@(let ((mean (mean values)))
                      `((td ,mean) (td ,(stddev values mean))
                        (td ,(apply #'max values))
                        (td ,(apply #'min values)))))))))))

(defmethod element-analysis-markup((element list) value &rest rest)
  (declare (ignore rest))
  (multiple-value-bind (tag attr content) (split-markup element)
    (markup-analysis value tag (datatype element) attr content)))

(defmethod element-analysis-markup((element form-element) values &rest rest)
  (declare (ignore rest))
  `((P ,(text element))
    ,@(multiple-value-bind (tag attr content)
        (split-markup (markup-template element))
        (markup-analysis values tag (datatype element) attr content))))

(defgeneric markup-analysis(values tag type &optional attr content)
  (:documentation "Function generates analysis of given on the basis
   of a specific markup tag and data type.
   attr is the concatenation of tag and type attributes
   content is the tag content"))

(defmethod markup-analysis(values tag type  &optional attr content)
  (declare (ignore tag  type attr content))
   `((ul ,@(mapcan #'(lambda(value) (when value
				      `((li ,(princ-to-string value)))))
		  values))))

(defmethod markup-analysis(values (tag (eql 'mcq))  type
                           &optional attr content)
  (declare (ignore type))
  (let ((values-count (mapcar #'(lambda(item) (cons (car item) 0)) content))
        (other 0))
    (dolist (value values)
      (let ((a (assoc value values-count :test #'equal)))
        (if a (incf (cdr a)) (incf other))))
    (case (getf attr :style)
      (:horizontal
       (let ((cwidth (format nil "~A%" (/ 100.0 (length content)))))
         `(((table :border 1 :cellspacing 0 :width ,(getf attr :width "80%"))
            (tr ,@(mapcar
                   #'(lambda(item)
                       `((td :width ,cwidth :align "center" :valign "top")
                         ,(string (cdr item))))
                   content))
            (tr ,@(mapcar
                   #'(lambda(item)
                       `((td :align "center")
                         ,(cdr (assoc (car item)
                                      values-count :test #'equal))))
                   content))))))
      (:dropdown
       `(((table :border 1 :cellspacing 0)
          ,@(mapcan #'(lambda(item)
                        (let ((v (cdr (assoc (car item)
                                             values-count :test #'equal))))
                          (when (> v 0)
                            `((tr (td ,(cdr item))
                               ((td :align "center") ,v))))))
                    content)
          (tr (td "Other") (td ,other)))))
      (otherwise
       `(((table)
          ,@(mapcar #'(lambda(item)
                        `(tr (td ,(cdr item))
                          ((td @align "center")
                           ,(cdr (assoc (car item)
                                        values-count :test #'equal))) ))
                    content)
          (tr (td "Other") (td ,other))))))))

