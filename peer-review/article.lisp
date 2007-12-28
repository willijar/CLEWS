;;;; $Id: article.lisp,v 1.1 2006/07/30 17:42:14 willijar Exp $
;;; Extra methods for articles which require info from application
;;; environment as well.

(in-package :clews.peer-review)

(defmethod find-links-out((app peer-review) (article article))
  "return the index entries for links out of given article - slow"
  (mapcan
   #'(lambda(indx)
       (when (and (not (string-equal (title article) (title indx)))
                  (search (title indx) (content article) :test #'char-equal))
         (list indx)))
   (article-index app)))

(defmethod find-links-in((app peer-review) (article article))
  "return index entries for links into this article
- has to read all articles currently so slow."
  (mapcan
   #'(lambda(indx)
       (when (search (title article) (content (get-article (id indx) app))
                     :test #'string-equal)
         (list indx)))
   (article-index app) ))

(defmethod crosslinked-content((app peer-review) (id string))
  (let ((article (get-article id app)))
    (when (content article)
      (let* ((style (style article))
             #+nil(links (links-out (get-index id app)))
             #+nil(titles (mapcar
                           #'regex-quote-string
                           (delete-duplicates (mapcar #'title links)
                                              :test #'string-equal)))
             (titles
              (mapcar
               #'regex-quote-string
               (delete-duplicates (mapcar #'title (article-index app))
                                  :test #'string-equal))))
        (if titles
            (flet ((find-id(regs)
                     (id (find-if #'(lambda(idx)
                                      (string-equal (elt regs 2)
                                                    (title idx)))
                                  (article-index app)))))
              (let ((matcher
                     (compile-str
                      (format nil
                              "([^:alnum:])(~A~{|~A~})([^:alnum:])"
                              (first titles) (rest titles)))))
                (regex-substitute-string matcher (content article)
                                         (if (eq style :structured-text)
                                             (list "\"" 2 "\":" #'find-id " ")
                                             (list "<a href=\"" #'find-id "\">" 2 "</a>"))
                                         :match-register 2)))
            (content article))))))

(defun format-authorship-html(author created anonymous &key override)
  `(p (em ,(if anonymous "Anonymous" author) " "
       ,(if (and anonymous override)
            (concatenate 'string " (" author ") ")
            " ")
       " " ,(format-time nil created))))

(defmethod author-html((article article) &key (is-tutor nil))
  (format-authorship-html
   (author article) (created article) (anonymous article) :override is-tutor))

(defmethod author-html((review list) &key (is-tutor nil))
  (format-authorship-html
   (getf review :author)  (getf review :created) (getf review :anonymous)
   :override is-tutor))

(defun article-image-references(things prefix)
  (markup::process-inline-text
   things
   #'(lambda(txt)
       (regex:regex-process-string
        (load-time-value
         (compile-str"\"([^\"]+)\":image:([^\\ \\n\\r]+)") t) txt
        #'(lambda(regs)
            (list (list 'IMG :src (concatenate 'string prefix (elt regs 2))
                        :alt (elt regs 1)))) ))))

(defmethod article-body-html((app peer-review) (id string)
                             &key  is-tutor (path-to-root "."))
  (let ((article (get-article id app))
        (prefix (format nil "~A/images/~A-" path-to-root id)))
    `(((div :class "article" :escape nil)
       ,@(when (> (length (summary article)) 0) `((p (em ,(summary article)))))
       ,(if (not (content article))
            "No writeup provided as yet."
            (case (style article)
              (:structured-text
               (parse-structured-text
                (crosslinked-content app id)
                :inline-hooks
                (cons #'(lambda(things)
                          (article-image-references things prefix))
                      markup::*inline-hooks*)))
              (:rst (parse-rst app id :path-to-root path-to-root))
              (t (crosslinked-content app id)))))
      ,(author-html article :is-tutor is-tutor))))
