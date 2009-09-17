(in-package :cl-user)
(load-system :s-xml)
(load-system :markup)

(s-xml::define-xml-entity "ldquo" "``")
(s-xml::define-xml-entity "rdquo" "``")

(defun module-dom(moduleid)
  (with-input-from-string
      (is
       (let ((text (load-file (format nil "/home/willijar/www/modules/~A.xml"
				      moduleid))))
	 (subseq text (search "<section" *t*))))
    (s-xml:parse-xml-dom is :lxml)))

(defun extract-dom(dom test)
  "Extract list of components matching test from a dom. Test is a
function taking 3 arguments, the tag, attr and content"
  (let ((results nil))
    (funcall
     (markup::markup-scanner(tag attr content markup)
			    ((funcall test tag attr content)
			     (push markup results) nil))
     dom)
    (nreverse results)))

(defun normalise-string(string)
  "return a string with whitespace normalised"
  (let ((last-wsp nil)
	(wsp '(#\newline #\return #\newline #\space #\tab)))
    (with-output-to-string(os)
      (with-input-from-string(is (string-trim wsp string))
	(do((c (read-char is nil) (read-char is nil)))
	   ((not c))
	  (cond
	    ((member c wsp)
	     (when (not last-wsp) (write-char #\space os))
	     (setf last-wsp t))
	    (t (write-char c os)
	       (setf last-wsp nil))))))))

(defun extract-outcomes(dom)
  (let ((outcome-rows
	 (extract-dom
	  (first
	   (extract-dom
	    (first
	     (extract-dom
	      dom
	      #'(lambda(tag a c)
		  (and (eql tag :|varlistentry|)
		       (listp (first c))
		       (equalp (second (first c)) (normalise-string "Module Outcomes"))))))
	    #'(lambda(tag a c) (eql tag :|tbody|))))
	  #'(lambda(tag a c)  (eql tag :|row|)))))
	  
	      
      
  