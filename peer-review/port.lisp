
(in-package :clews.articles)

(defvar *tutorial*
  (make-instance 'article-collection
                 :directory
                 #p"/home/willijar/teaching/modules/EE401B/rst/"
                 :default #'(lambda(path) (namestring path))
                 :acl '((:view :all)
                        (:add :tutor)
                        (:admin :admin))))


(defvar *review*
  (make-instance 'article-collection
                 :directory
                 #p"/home/willijar/teaching/modules/EE401B/rst/"
                 :default #'(lambda(path) (namestring path))
                 :acl '((:view :all)
                        (:add :tutor)
                        (:admin :admin))))

(in-package :clews.peer-review)


(dolist(id (article-ids aston::*ee4000-peer-review*))
  (let* ((article (get-article id aston::*ee4000-peer-review*))
         (content (content article)))
    (when (eql (style article) :structured-text)
      (format t "~A...~%" id)
        (let ((doc `(markup:document
                     (title ,(title article))
                   (dl
                    (dt "authorids") (dd ,(author article))
                    (dt "date") (dd ,(created article))
                    (dt "anonymous") (dd ,(anonymous article)))

                     ,@content))
              (reviews (reviews article)))
        (with-open-file(os (merge-pathnames
                            key
                            "/home/willijar/teaching/modules/EE4000/rst/*.rst")
                           :direction :output :if-exists :supersede)
          (markup::rst os doc))
        (with-open-file(os (merge-pathnames
                            key
                            "/home/willijar/teaching/modules/EE4000/rst/*.state")
                           :direction :output :if-exists :supersede)
          (write (list (cons :reviews reviews)) os))))))
