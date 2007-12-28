:Date: 2005-07-02
:Version: $Id$
:Authors: Dr John Williams
:License: GPL
:Copyright: Dr John Williams 2005

=================
Assessments
=================

CLEWS supports a range of assessments sharing a common interface for
use in Web applications.


----------------
Assessment Types
----------------

Ephemeral
=========

Ephemeral assessments are those which are only available between a
specified start date or end date.

Parameters
----------

:start-date 
	If specified, the date from which assessment access is to
	be allowed for students. 

:end-date
	If specified, access to the assessment will not be allowed
	for students after this date.


Timelimited
===========

Timelimited assessments are those for which a time limit between first
accessing and submitting results is permitted.

Parameters
----------

:timelimit
	If specified the time allowed (in seconds) to complete the
	assessment

:strict-timelimit
	If true submissions outside the time limit are not allowed,
	otherwise submissions are allowed but a proportional penalty
	is applied to the mark if the time limit is exceeded i.e. if a
	student takes twice as long as the time limit their mark will
	be halved.

Scheduled
=========

Scheduled assessments are those for which a submission deadline is
applied.

:deadline-date
	The deadline date (and time) for submissions. Submissions
	after the deadline will receive no marks.

:feedback-date
	The date at which feedback is allowed on the submission. If
	not specified the deadline date is used.

:strict-deadline
	If true submissions are not accepted after the deadline,
	otherwise they will be accepted but the mark will be zero.

:multiple-attempt
	If true multiple attempts are allowed, even after the feedback
	is available. Only Useful for formative assessments.

Questionnaire
=============

Questionnaire is an online questionnaire type assessment which
supports a range of different question types.

Parameters
----------

:questions
	A list of question specifications

:randomise-questions
	If true question selection and order will be randomised

:set-no-questions
	By defualt all questions will be set. If this is specified
	(and less than the total number of questions) then a subset
	selection of questions will be set. TO be useful the questions
	should also be randomised so students get different sets of
	questions

:no-questions-counted
	By default all questions set for a student are counted in the
	final mark. If this is specified and less than the no of
	questions set then only the best questions will be counted.
 
Socratic
========

Socratic is an implementation of a Socratic (question lead/discursive)
approach to learning. The principle is that students attempt a
questionnaire individually. They then discuss the questionnaire and
their answers in small groups (with the tutor). After the discussion
they do the questionnaire again individually.

As an assessment the mark allocated will be a combination of their two
assessment attempts.

Socratic assessments may be free form or scheduled. If run free form
the students will move between stages at the pace of the group. If
scheduled each stage will have a deadline. Students who do not
complete a stage before its deadline will not be able to progress to
the next stage.

Parameters
----------

:schedule
	If nil the assessment is free form. Otherwise the assessment
	is scheduled. It is a p-list specifying either dates or
	intervals (in days) for	each stage of the assessment. If given
	all three stages must be specified e.g. 
	``'(attempt1 7 :discussion 7 :attempt2 3)``. If dates are
	given they must be in sequence. If a start date is provided
	then the intervals are relative to the start date, other wise
	they are relative to the assessment deadline date.

:min-no-posts
	The minimum number of posts expected from students in the
	discussion component before they are considered to have
	completed it.

:groups
	A list of lists of students usernames. Each sub list
	represents a discussion group of students.

:discussion
	A string holding the discussion mailbox. This is normally
	not set by the assessment author but could be used to lead the
	discussion. In this case the message From field should contain the
	username of the tutor and the Message-ID field must be
	specified.

