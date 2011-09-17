; Problem Set 7 Solutions
;   by Michael Allen

; ============
;  Exercise 1
; ============

; The difference between our version and Louis' suggestion is 
; subtle but very important. In our version, the expression
;   ((get-method mobile-obj 'install) self)))
; tells the person's embedded mobile-obj to use its knowledge
; of how to INSTALL to install the person.


; Louis suggests replacing this code with
;   (ask mobile-obj 'INSTALL)
; which asks the mobile-obj to install itself. The trouble occurs
; when the embedded mobile-object tries to install the person into
; its birthplace. Instead of placing the person there, the
; embedded mobile-obj is placed there instead. When the person
; tries to move out of the room, the mobile-obj will be left
; behind, and the person will appear in the new room. Thus, the
; character can wind up in two places in a way. Alyssa is correct
; in asserting the Louis' implementation will not work.

; ============
;  Exercise 2
; ============

; a. John is more restless than Holly.

; b. They move on the same clock tick about 1 in every 6 ticks.

; ============
;  Exercise 3
; ============

(define mike (make&install-person 'mike student-lab 1024))
(define late-homework (make&install-thing 'late-homework student-lab))

; Transcript...

; Mike has late homework to give to John

(ask mike 'take late-homework)
; At student-lab : mike says -- I take late-homework 
;Value: #t

(ask mike 'go 'east)
; mike moves from student-lab to foundation-office 
;Value: #t

(ask mike 'go 'east)
; mike moves from foundation-office to faculty-office 
; At faculty-office : mike says -- Hi john 
;Value: #t

(ask mike 'lose late-homework)
; At faculty-office : mike says -- I lose late-homework 
;Value: #t

(ask john 'take late-homework)
; At faculty-office : john says -- I take late-homework 
;Value: #t

; We don't need Mike anymore
(go-to-heaven mike)

; ============
;  Exercise 4
; ============

(show holly)

; #[compound-procedure 2]
; Frame:
;   #[environment 3]
; Body:
;   (lambda (message)
;     (cond ((eq? message ...) (lambda ... true))
;           ((eq? message ...) (lambda ... possessions))
;           ((eq? message ...) (lambda ... ... possessions))
;           ...))

; Just follow all the hash numbers and create an environment diagram with them.
; Of course, this is not as easy as it looks.

; ============
;  Exercise 5
; ============

; MAKE-CARD-LOCKED-PLACE creates locked rooms
(define (make-card-locked-place name)
  (let ((place (make-place name)))
    (lambda (message)
      (cond ((eq? message 'card-locked-place?) (lambda () #t))
	    ((eq? message 'accept-person?)
	     (lambda (self person)
	       (filter (lambda (thing) (is-a thing 'id-card?))
		       (ask person 'possessions))))
	    (else (get-method place message))))))

; Add a shiny new locked conference room
(define conference-room (make-card-locked-place 'conference-room))
(can-go-both-ways room-with-elevator 'east 'west conference-room)

; Transcript...

; John takes his ID and heads for the new conference room

(ask john 'take john-card)
; At faculty-office : john says -- I take john-card 
;Value: #t

(ask john 'go 'west)
(ask john 'go 'south)
(ask john 'go 'south)
(ask john 'go 'east)

; john moves from room-with-elevator to conference-room 
;Value: #t

; Holly can't get in without her ID.

(ask holly 'go 'north)
(ask holly 'go 'east)
; holly can't move to conference-room 
;Value: message-displayed

; Grendel finds her and eats her.

(ask grendel 'go 'up)
(ask grendel 'go 'out)
(ask grendel 'eat-person holly)

; At room-with-elevator : grendel says -- Growl.... I'm going to eat you, holly 
; At room-with-elevator : holly says -- 
;                      Dulce et decorum est 
;                      pro computatore mori! 
; holly moves from room-with-elevator to heaven 
; At room-with-elevator : grendel says -- Chomp chomp. holly tastes yummy! 
;Value: *burp*

; Holly should have brought her ID :)

; We don't need John or Grendel anymore
(go-to-heaven john)
(go-to-heaven grendel)

; ============
;  Exercise 8
; ============

; BIG-BROTHER is part of exercise 8.
; Pay no attention to him now.

; BIG-BROTHER watches IDs used to access the labs
(define big-brother
  (let ((card-list '()))
    (lambda (message)
      (cond ((eq? message 'inform)
	     (lambda (self card-id place)
	       (let ((report (list card-id place (current-time))))
		 (if (filter (lambda (card)
			       (and (eqv? (car report) (car card))
				    (not (eqv? (cadr report) (cadr card)))
				    (eqv? (caddr report) (caddr card)))) card-list)
		     (begin
		       (report-stolen-card card-id)
		       (display-message (list "Big Brother is watching"))
		       #t)
		     (begin
		       (set! card-list (cons report card-list))
		       #t)))))))))

; ============
;  Exercise 6
; ============

; MAKE-PROTECTED-STUDENT-LAB makes rooms
; that can only be entered with registered IDs
(define (make-protected-student-lab name)
  (let ((cards '())
	(place (make-place name)))
    (lambda (message)
      (cond ((eq? message 'protected-student-lab?) (lambda (self) #t))
	    ((eq? message 'register)
	     (lambda (self card)
	       (if (eq? self (ask card 'place))
		   (begin
		     (set! cards (cons (ask card 'id) cards))
		     (display "ACCESS GRANTED\n"))
		   (display "ACCESS DENIED\n"))))
	    ((eq? message 'accept-person?)
	     (lambda (self person)
	       (filter (lambda (thing)
			 (and (is-a thing 'id-card?)
			      (filter (lambda (card) 
					(if (eqv? card (ask thing 'id))
					    ; WARNING: Pretend this is #t until exercise 8
					    (ask big-brother 'inform (ask thing 'id) self)
					    #f)) cards)))
		       (ask person 'possessions))))
	    (else (get-method place message))))))

; Dividing up the lab
(define west-lab (make-protected-student-lab 'west-lab))
(define south-lab (make-protected-student-lab 'south-lab))

(can-go-both-ways south-lab 'north 'south student-lab)
(can-go-both-ways west-lab 'east 'west student-lab)

; Dimitri is a TA for south-lab
(define dimitri (make&install-person 'dimitri south-lab 1024))
(define dimitri-card (make&install-id-card 'dimitri-card south-lab 6173862102))

(ask south-lab 'register dimitri-card)
; ACCESS GRANTED
;Value: #[unspecified-return-value]

; Transcript...

; Dimitri wanders the lab.

(ask dimitri 'take dimitri-card)
(ask dimitri 'go 'north)
(ask dimitri 'go 'west)
; dimitri can't move to west-lab 
;Value: message-displayed

(ask dimitri 'go 'south)
; dimitri moves from student-lab to south-lab 
;Value: #t

; ============
;  Exercise 7
; ============

; MAKE-OGRE creates ogres which eat people holding a particular ID
(define (make-ogre name birthplace idnum threshold)
  (let ((person (make-person name birthplace threshold)))
    (lambda (message)
      (cond ((eq? message 'act)
	     (lambda (self)
	       (for-each (lambda (person) (ask self 'frisk person))
			 (other-people-at-place self (ask self 'place)))
	       ((get-method person 'act) self)))
	    ((eq? message 'frisk)
	     (lambda (self person)
	       (if (filter 
		    (lambda (thing)
		      (and (is-a thing 'id-card?)
			   (eqv? idnum (ask thing 'id)))) (ask person 'possessions))
		   (ask self 'eat-person person))))
	    ((eq? message 'eat-person)
	     (lambda (self person)
	       (ask self 'say 
		    (list "Fe Fi Fo Fum... I smell a stolen IDnum."))
	       (ask self 'say
		    (list "I'm going to eat you" (ask person 'name) "."))
	       (go-to-heaven person)
	       (ask self 'say
		    (list "Mmm Mmm Good."))
	       (ask self 'move-to heaven)
	       (remove-from-clock-list self)))
	    (else (get-method person message))))))

; REPORT-STOLEN-CARD dispatches an ogre to eat the thief
(define (report-stolen-card card-id)
  (ask (make-ogre card-id dungeon card-id 1) 'install))

; Dimitri steals Holly's ID so he can be the SICP professor.

(ask dimitri 'go 'north)
(ask dimitri 'go 'east)
(ask dimitri 'go 'east)
(ask dimitri 'take holly-card)
; At faculty-office : dimitri says -- I take holly-card 
;Value: #t

; The card is reported stolen.

(report-stolen-card (ask holly-card 'id))

; Dimitri runs to the supply closet.

(ask dimitri 'go 'west)
(ask dimitri 'go 'south)
(ask dimitri 'go 'south)
(ask dimitri 'go 'west)

; But the Ogre finds him there.

(clock)
(clock)
; ---Tick---
; At supply-closet : 2000-32 says -- Fe Fi Fo Fum... I smell a stolen IDnum. 
; At supply-closet : 2000-32 says -- I'm going to eat you dimitri . 
; At supply-closet : dimitri says -- I lose holly-card 
; At supply-closet : dimitri says -- I lose dimitri-card 
; At supply-closet : dimitri says -- 
;                      Dulce et decorum est 
;                      pro computatore mori! 
; dimitri moves from supply-closet to heaven 
; At heaven : dimitri says -- Hi holly 
; At supply-closet : 2000-32 says -- Mmm Mmm Good. 
;Value: tick-tock

; ============
;  Exercise 8
; ============

; BIG-BROTHER is defined between exercises 5 & 6
; And a line has been magically added to MAKE-CARD-LOCKED-PLACE
; in exercise 6 (find the comment in the code)

; Two hackers
(define alyssa (make&install-person 'alyssa south-lab 1024))
(define ben (make&install-person 'ben west-lab 1024))

; and their ID cards (Ben's is a forged version of Alyssa's)
(define alyssa-id (make&install-id-card 'alyssa-id south-lab '0110-a))
(ask south-lab 'register alyssa-id)
(ask alyssa 'take alyssa-id)

(define ben-id (make&install-id-card 'forged-id west-lab '0110-a))
(ask west-lab 'register ben-id)
(ask ben 'take ben-id)

; They meet in the main lab and then go back to their labs.

(ask alyssa 'go 'north)
(ask ben 'go 'east)

; ben moves from west-lab to student-lab 
; At student-lab : ben says -- Hi alyssa 
;Value: #t

(ask alyssa 'go 'south)
; alyssa moves from student-lab to south-lab 
;Value: #t

(ask ben 'go 'west)
; Big Brother is watching

; ben moves from student-lab to west-lab 
;Value: #t

; Big Brother takes notice of Ben's forged ID,
; so Ben makes a break for it

(ask ben 'go 'east)
(ask ben 'go 'north)
(ask ben 'go 'secretly)

; But Big Brother's Ogre finds them there.

(clock)
(clock)

; At supply-closet : 0110-a says -- Fe Fi Fo Fum... I smell a stolen IDnum. 
; At supply-closet : 0110-a says -- I'm going to eat you ben . 
; At supply-closet : ben says -- I lose forged-id 
; At supply-closet : ben says -- 
;                      Dulce et decorum est 
;                      pro computatore mori! 
; ben moves from supply-closet to heaven 
; At heaven : ben says -- Hi dimitri holly 

; We don't need Alyssa anymore
(go-to-heaven alyssa)

; ============
;  Exercise 9
; ============

; MAKE-STUDENT creates student who like to wander around collecting IDs
(define (make-student name birthplace threshold)
  (let ((person (make-person name birthplace threshold)))
    (lambda (message)
      (cond ((eq? message 'act)
	     (lambda (self) 
	       (let ((wanted (pick-random 
			      (filter (lambda (thing) (and (is-a thing 'ownable?)
							   (not (is-a thing 'owned?))))
				      (ask (ask self 'place) 'things))))
		     (forsale (pick-random (ask self 'possessions))))
		 (cond ((and wanted (= (random 2) 0))
			(ask self 'take wanted))
		       ((and forsale (= (random 4) 0))
			(ask self 'lose forsale))
		       (else ((get-method person 'act) self))))))
	     (else (get-method person message))))))

; Installs new students
(define (make&install-student name birthplace threshold)
  (let ((student (make-student name birthplace threshold)))
    (ask student 'install)
    student))

; Some students

(make&install-student 'prof-plum west-lab 2)
(make&install-student 'col-mustard west-lab 2)
(make&install-student 'mr-green west-lab 3)
(make&install-student 'mrs-peacock south-lab 3)
(make&install-student 'miss-scarlet south-lab 2)
(make&install-student 'ms-white south-lab 3)

; Some ID cards

(ask west-lab 'register (make&install-id-card 'west-0 west-lab 'west-0))
(ask west-lab 'register (make&install-id-card 'west-1 west-lab 'west-1))
(ask south-lab 'register (make&install-id-card 'south-0 south-lab 'south-0))
(ask south-lab 'register (make&install-id-card 'south-1 south-lab 'south-1))

; And some forged cards

(ask west-lab 'register (make&install-id-card 'forged-0 west-lab 'south-0))
(ask south-lab 'register (make&install-id-card 'forged-1 south-lab 'west-1))
(make&install-id-card 'forged-2 student-lab 'south-0)
(make&install-id-card 'forged-3 student-lab 'west-1)

; Use (run-clock 1000) to get your own transcript