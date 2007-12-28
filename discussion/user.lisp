;;;; CLEWS Discussion System
;;;; Copyright (C) 2004-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL) 
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; $Id: user.lisp,v 1.1 2006/08/21 07:12:17 willijar Exp $

(in-package :clews.discussion)

;; article marks
(defun group-data(app groupname &optional (user *current-user*))
  (property-subset
   (property-subset (user-component-properties app user) :groups
		    :maker #'(lambda(defaults)
			       (declare (ignore defaults))
			       (make-hash-table :test #'equal)))
   groupname))

;; integer sets - an ordered list of article nmbers or
;; cons's representing a range of article numbers.

(defun integer-set-member-p(n lst)
  "Returns true if n is in integer set lst"
  (dolist(x lst)
    (etypecase x
      (cons
       (cond
	 ((< n (car x)) (return nil))
	 ((<= n (cdr x)) (return t))))
      (integer
       (cond
	 ((< n x) (return nil))
	 ((= n x) (return t)))))))

(defun integer-set-add(n lst)
  "Add n to integer set lst returning new integer set. lst may be modified"
  (unless lst (return-from integer-set-add (list n)))
  (do ((sublist lst (rest sublist)))
      ((let* ((x (first sublist)) min max)
	 (if (consp x)
	     (setf min  (car x) max (cdr x))
	     (setf min x max x))
	 (cond ((not sublist)
		(setf lst (nconc lst (list n)))) ; reached end - extend
	       ((> n (1+ max)) nil) ; beyond this range - continue loop
	       ((< n (1- min)) ; prior to this element - insert n in place
		(setf (cdr sublist) (cons (car sublist) (cdr sublist))
		      (car sublist) n))
	       ((> n max) ; extend range up 1 and maybe merge with next
		(let ((next (second sublist)) nmin nmax)
		  (if (consp next)
		      (setf nmin (car next) nmax (cdr next))
		      (setf nmin next nmax next))
		  (if (and next (>= n (1- nmin))) ;
		      (setf (first sublist) (cons min nmax)
			    (rest sublist) (cddr sublist))
		      (setf (first sublist) (cons min n)))
		  lst))
	       ((< n min)		; extend range down 1
		(setf (first sublist) (cons n max)))
	       (t lst)))		; already in range - terminate
       lst)))

(defun integer-set-remove(n lst)
  (do* ((sublist lst (rest sublist))
	(x (first sublist) (first sublist)))
       ((cond ((not sublist))		; finished
	      ((not (consp x))		; if integer
	       (when (= n x)	       ; and equal - remove and finish
		 (setf (car sublist) (cadr sublist)
		       (cdr sublist) (cddr sublist))))
	      ((> n (cdr x)) nil)	; not there yet
	      ((< n (car x)) (return lst)) ; past it and not found
	      ((= n (cdr x))
	       (let ((max (1- n)))
		 (if (= max (car x))
		     (setf (first sublist) max)
		     (setf (cdr x) max))))
	      ((= n (car x))
	       (let ((min (1+ n)))
		 (if (= min (cdr x))
		     (setf (first sublist) min)
		     (setf (car x) min))))
	      (t			; must be in middle of x
	       (setf (cdr sublist) (cons (cons (1+ n) (cdr x))
					 (cdr sublist))
		     (cdr x) (1- n))))
	lst)))

(defun article-mark(mark n app groupname &optional (user *current-user*))
  (integer-set-member-p
   n
   (property (group-data app groupname user) mark)))

(defun (setf article-mark) (bool mark n app groupname
			    &optional (user *current-user*))
  (let ((data (group-data app groupname user)))
    (setf (property data mark)
	  (if bool
	      (integer-set-add n  (property data mark))
	      (integer-set-remove n (property data mark))))
    (setf (get-dictionary (username user) (users app)) user) ))
