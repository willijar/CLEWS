
(in-package :clews.articles)

(defvar *tutorial*
  (make-instance 'article-collection
                 :directory
                 #p"/home/willijar/teaching/modules/EE401B/rst/"
                 :default #'(lambda(path) (namestring path))
                 :acl '((:view :all)
                        (:add :tutor)
                        (:admin :admin))))

(in-package :clews.adaptive-tutor)

(defun make-key(title)
  (let ((last-wsp-p t))
    (with-output-to-string(os)
      (loop :for c :across title
            :do (setf last-wsp-p
                     (cond
                       ((docutils::wsp-char-p c)
                        (unless last-wsp-p (write-char #\- os))
                        t)
                       ((member c '(#\* #\? #\, #\.)) nil)
                       (t (write-char (char-downcase c) os) nil)))))))


(let ((markup::*references* (make-hash-table :test #'equal)))
  (dolist(concept (concepts aston::*tutorial*))
    (setf (gethash (concept-id concept) markup::*references*)
          (list (concept-title concept))))
  (dolist(concept (concepts aston::*tutorial*))
    (let ((key (make-key (concept-title concept)))
          (content (content concept)))
      (when (and content (listp content))
        (format t "~A -> ~A~%" (concept-id concept) key)
        (let ((doc `(markup:document
                   (title ,(concept-title concept))
                   (dl
                    (dt "author") (dd "Dr. John A.R. Williams")
                    (dt "authorids") (dd "willijar")
                    ,@(when (property concept :created)
                            `((dt "date")
                              (dd ,(property concept :created))))
                    ,@(when (property concept :summary)
                            `((dt "summary")
                              (dd ,(property concept :summary))))
                    ,@(when (property concept :keywords)
                            `((dt "keywords")
                              (dd ,(jarw.parse:format-output
                                    'jarw.parse:separated
                                    (property concept :keywords)
                                    :type 'read))))
                    ,@(when (property concept :level)
                            `((dt "level")
                              (dd ,(property concept :level))))
                    ,@(let ((preq (mapcan
                                   #'(lambda(p)
                                       `((dt (ref ,(concept-id (car p))))
                                         (dd ,(cdr p))))
                                   (direct-prerequisites concept))))
                           (when preq `((dt "prerequisites")
                                        (dd (dl ,@preq)))))
                    ,@(let ((out (mapcan
                                  #'(lambda(p)
                                      `((dt (ref ,(concept-id (car p))))
                                        (dd ,(cdr p))))
                                  (direct-outcomes concept))))
                           (when out `((dt "outcomes")
                                       (dd (dl ,@out)))))
                    ,@(let ((ch (mapcan
                                 #'(lambda(a) `(" " (ref ,(concept-id a))))
                                 (direct-children concept))))
                           (when ch `((dt "children")
                                      (dd  ,@ch)))))
                     ,@content)))
        (with-open-file(os (merge-pathnames
                            key
                            "/home/willijar/teaching/modules/EE401B/rst/*.rst")
                           :direction :output :if-exists :supersede)
          (markup::rst os doc)))))))
