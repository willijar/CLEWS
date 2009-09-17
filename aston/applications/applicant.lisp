;;;; $Id: applicant.lisp,v 1.1 2003/10/12 10:54:54 willijar Exp $
;;;; Copyright (C) 2003 Dr. John A.R. Williams, J.A.R.Williams@blueyonder.co.uk

;;;; This file is part of the Common Lisp Applications EWAS application

;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2 of the License, or
;;;; (at your option) any later version.

;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.

;;;; You should have received a copy of the GNU General Public License
;;;; along with this program; if not, write to the Free Software
;;;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA
;;;; or retreive a copy from http://www.gnu.org/licenses/gpl.txt

;;;; Base Adaptive Tutor class - implementation

(in-package :aston-applications)

(defmethod registration-handler((app aston-applications) request rest)
  (declare (ignore rest))
  (let* ((username (request-data request :username))
	 (path (request-url request))
	 (user (and username (get-dictionary username (users app)))))
    (format t "username:~A~%path:~A~%user:~A~%" username path user)
    (when user		       ;already registered - go somewhere else
      (return-from registration-handler
	(request-redirect
	 request
	 (cond ((has-permission user :admin app) "admin/")
	       ((has-permission user :referee app) "referee/")
	       (t (princ-to-string (property user :app-id)))))))
    (when username			;register a new applicant
      (let ((pwd (request-data request :password)))
	(when (and pwd (> (length 6)))
	  (return-from registration-handler
	    (let* ((cpwd (ewas::create-crypt-pwd pwd))
		   (id (new-application app username))
		   (path (merge-url path id)))
	      (setf (get-dictionary username (users app))
		    (make-instance 'user :username username
				   :roles '(:applicant)
				   :properties
				   `((:app-id . ,id)
				     (:pwd . ,cpwd))))
	      (send-mail app username "Aston University Masters Application Registration"
			 (format nil "This email was automatically sent to you because you have successfully
registered to apply online for the Masters programmes in the School of
Engineering and Applied Science at Aston University.

You can now complete and check the status of your application on the
web at

~A

using the credentials

Username: ~S
Password: ~S
"
				 (urlstring path)
				 username
				 pwd))
	      (values
	       `(html
		 (head (title ,(title app) "  Successful Registration"))
		 (body
		  ((section :title ,(concatenate 'string (title app)
						 " Successful Registration"))
		   (p "You have successfully registered to apply online. A
confirmational email has been sent to " ,username)
		   (p "Your application id is " ,id)
		   (p ((a :href ,id)
		       "Start entering your application details on line.")))))
	       (list
		:set-cookie 
		(cookie "authentication"
			(authentication-cookie username cpwd request)
			:expires
			(let ((persistence
			       (parse-integer
				(request-data request :persistence))))
			  (when (> persistence 0)
			    (+ persistence (get-universal-time))))
			:path (url-path (merge-url path "../"))))))))))
    `(html
      (head (title ,(title app) " Registration"))
      (body
       ((section :title ,(concatenate 'string (title app) " Registration"))
	(p "In order to complete an online application you will need to
register you email address (which will use as your user name to log in)
 and a password which you may choose.
 A confirmational email stating your password will be sent to you
once registered.")
	((form :method "POST" :action ,(urlstring path))
	 (table
	  (tr (th "Email Address:")
	      (td ((input :name :username))))
	  (tr (th "Password:")
	      (td ((input :name :password :type "password"))))
	  (tr (td) (td
		      ((mcq :name :persistence :value 0)
		       (0 . "Public/Shared computer")
		       (31536000 . "Keep me signed in") )))
	  (tr (td)
	      (td
	       ((input :type "submit" :value "Apply online")))))))))))

(defmethod response-handler((app aston-applications) request rest) 
  (let* ((user (request-user request))
	 (rest (split-string rest 2 '(#\/)))
	 (form (when (> (length (first rest)) 0)
		 (intern (first rest) :keyword)))
	 (app-id (if (has-permission user :admin app)
		     (second rest)
		   (property user :app-id) )) 
	 (application (get-dictionary app-id (applications app))))
    (unless application
      (return-from response-handler
	(request-send-error
	 request 404
	 (format nil "Application ~S not found.
<a href=\"~A\">Back to referring Page</a>"
		 app-id
		 (request-header request :referrer)))))
    (if form
	(form-display app request application form)
      (status-display app request application))))

(defun status-display(app request application)
  (let ((status (property application :status)))
    `(html
      (head (title "Application Status " ,(property application :id))
      (body
       ((section
	 :title ,(concatenate 'string 
			      "Application " (property application :id)
			      " " (applicant-name application)))
	((table :align "center")
	 ,@(mapcan
	    #'(lambda(key)
		(let ((date (getf status (car key))))
		  (if date
		   `((tr (td ,(cdr key))
			 (td ,(format-date nil date :fmt :short)))))))
	    +status-checks+)
	 ,(if (getf status :decision-made)
	      `(tr  ((td :colspan 2)
		     ((section :title "Decision ")
		      `(p (strong ,(if (getf status :accept) "Accept" "Reject"))
			  ": "
			  ,(getf status :decision-basis)))))))))))))

#|
|#

#|
  (get-form form-name application)


For the manual inputting of the postal applications, I'd like all the
personal details fields and what programme applied for. 
> The complete list of personal info includes name, 
> nationality, date-of-birth, home and correspondence 
> addresses. Would this to be too much to type in?
No - that'll be OK. I will need to move this info over to SITS in cases
where we've made a definite offer anyway and a copy and paste job will
do the trick nicely.

On the form - you know where it says when would you like to start?
Instead of just the year - can we put in October or Sept plus the year?
Just leaving it open as 2003 or 2004 etc means that I do get emails
saying, please process my application quickly coz I want to start in
April! If we put Oct 2003 or Oct 2004, then they'll be in no doubt as to
when the course starts.

Can you amend something in the General page on the MSc webpages? At the
bottom in the fees section, where it says Living expenses, can we put
"Estimated Living expenses £500 per month" instead of the breath-taking
figure of £8500?! I checked with our International office and made a few
phone calls to colleagues in other universities and the general
statement tends to be rough estimation of £500 per month.

I'm going to draw up a brief document simply outlining what we discussed
in general terms for our meeting with AKK - I tend to find that he likes
having a bit of paper with stuff written on it to look at. I will if I
may, "borrow" bits from your emails to Geof and myself.


	   table align=center border=1 cellspacing=0 cellpadding=3>
<tr><th>Stage</th><th>Date</th><th>Result</th>
<dtml-if isAdmin><th>Action</th></dtml-if>
<tr><td>Created</td><td><dtml-var created></td><td></td></tr>
<dtml-if lit_req>
<tr><td>Literature Requested</td><td><dtml-var lit_req></td><td></td>
<dtml-unless lit_sent>
<dtml-if isAdmin>
  <td><a href="<dtml-var absolute_url>?appl_id=<dtml-var appl_id>&action=sendLiterature">Literature Sent</a></td>
</dtml-if>
</dtml-unless>
</dtml-if>

<dtml-if lit_sent>
<tr><td>Literature Sent</td><td><dtml-var lit_sent></td><td></td></tr>
</dtml-if>

<tr><td>Application Received</td><td><dtml-var rcvd></td><td></td>

<dtml-if qualification_check>
</tr><tr><td>Qualifications Checked</td><td><dtml-var qualification_check></td><td></td>
<dtml-elif isAdmin>
  <td><a href="<dtml-var absolute_url>?appl_id=<dtml-var appl_id>&action=checkQualifications">Qualifications Checked</a></td>
</dtml-if>

<dtml-if refs_req>
</tr><tr><td>References Requested</td><td><dtml-var refs_req></td><td></td>
  <dtml-if refs_rcvd>
    </tr><tr><td>References Received</td><td><dtml-var refs_rcvd></td><td></td>
  <dtml-elif isAdmin>
    <td><a href="<dtml-var absolute_url>?appl_id=<dtml-var appl_id>&action=receiveReferences">References Received and checked</a></td></tr>
  </dtml-if>
<dtml-elif isAdmin>
  <td><a href="<dtml-var URL>?appl_id=<dtml-var appl_id>&action=requestReferences">Requested References</a></td>
</dtml-if>
</tr>

<dtml-if interview>
 <tr><td>Interview at</td><td><dtml-var interview></td><td></td>
 <dtml-unless decision><dtml-if isAdmin>
   <td><a href="<dtml-var absolute_url>?appl_id=<dtml-var appl_id>&when=&action=requestInterview">Clear Interview</a></td>
 </dtml-if></dtml-unless>
</td></tr>
</dtml-if>

<dtml-if decision>
<tr bgcolor=#FF0000 text=#00FFFF><td>Decision Made<td><dtml-var decision>
<td><dtml-if reject>Reject<dtml-else>Accept</dtml-if>

<dtml-if isAdmin>
<td><a href="<dtml-var absolute_url>?appl_id=<dtml-var appl_id>&action=clearDecision">Clear Decision</a></td>   
</dtml-if>

<tr><td valign=top>Decision</td>
<td colspan=3><dtml-var basis_of_entry></td></tr>
<dtml-elif isAdmin>
   <dtml-if "(interview and _.DateTime(interview)<_.DateTime()) or not interview">
     <tr><td valign=top>Basis of Decision</td><td colspan=3>
        <form method="POST" action="<dtml-var absolute_url>">
        <input type=hidden name="appl_id" value="<dtml-var appl_id>">
        <textarea cols=40 name=basis_of_entry><dtml-if basis_of_entry><dtml-var basis_of_entry></dtml-if>
        </textarea><br>
        <input type=submit name=action value="accept">
        <input type=submit name=action value="reject">
        </form>
     </td></tr>
   <dtml-else>
  <tr><td colspan=4>
  <form method="POST">
   <input type=submit name=action value="requestInterview"> at 
   <input type=text name="when"> (yyyy/mm/dd HH:MM)
   <input type=hidden name="appl_id" value="<dtml-var appl_id>">
   </form></td></tr>
   </dtml-if>
</dtml-if>

<dtml-if "decision and reject=='0'">

<tr><td>SUN Number</td>
<dtml-if SUN>
<td><dtml-var SUN></td>
<dtml-if isAdmin>
<td></td><td>
  <a href="?appl_id=<dtml-var appl_id>&SUN=&action=allocateSUN">Clear SUN</a>   
</td>
</dtml-if>
<dtml-elif isAdmin>
  <td colspan=3>
  <form method="POST">
   <input type=text name="SUN">
   <input type=submit name=action value="allocateSUN">
   <input type=hidden name="appl_id" value="<dtml-var appl_id>">
   </form></td>
</dtml-if>
</tr>

<tr>
<dtml-if sship_form_sent>
<td>Studentship Form Sent</td><td><dtml-var sship_form_sent></td>

<dtml-if sship_form_rcvd>
</tr><tr><td>Studentship Form Received</td><td><dtml-var sship_form_rcvd></td>
<dtml-elif isAdmin>
<td></td>
<td><a href="?appl_id=<dtml-var appl_id>&action=receiveStudentship">Studentship Form Received</a></td>
</dtml-if>
</tr>

<dtml-elif isAdmin>
<tr><td colspan=3></td>
<td><a href="?appl_id=<dtml-var appl_id>&action=sendStudentship">Studentship Form Sent</a></td>
</dtml-if>
</tr>

<tr>
<dtml-if accm_form_sent>
<td>Accomodation Form Sent</td><td><dtml-var accm_form_sent></td>

<dtml-if accm_form_rcvd>
</tr><tr><td>Accomodation Form Received<td><dtml-var accm_form_rcvd></td>
<dtml-elif isAdmin>
<td></td>
<td><a href="?appl_id=<dtml-var appl_id>&action=receiveAccomodation">Accomodation Form Received</a></td>
</dtml-if>
</tr>

<dtml-elif isAdmin>
<tr><td colspan=3></td>
<td><a href="?appl_id=<dtml-var appl_id>&action=sendAccomodation">Accomodation Form Sent</a></td>
</dtml-if>
</tr>

</dtml-if>
<dtml-if isAdmin>
<tr><td valign=top colspan=4>Notes:<br>
<form method=POST>
<textarea cols=60 rows=5 name=notes><dtml-if notes><dtml-var notes></dtml-if></textarea>
<br><input type=submit name=action value="updateNotes">
<input type=hidden name="appl_id" value="<dtml-var appl_id>">
</form>
</td>
</dtml-if>
</table>
|#