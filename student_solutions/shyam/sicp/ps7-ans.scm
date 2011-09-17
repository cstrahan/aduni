;;; SICP PS-7
;;; Shyam Visweswaran

;;;===================================================
;;; Warm-up 1
;;;===================================================

;;flip returns 1 or 0 everytime it is evaluated
;;note: the value of flip is a procedure

(define flip
  (let ((state 0))
    (lambda ()
      (if (= state 0)
	  (set! state 1)
	  (set! state 0))
      state)))

;;make-flip creates a flip procedure
;;note: the value of make-flip and (make-flip) are procedures

(define (make-flip)
  (let ((state 0))
    (lambda ()
      (if (= state 0)
	  (set! state 1)
	  (set! state 0))
      state)))

(define flip1 (make-flip))
;(flip1)
;Value: 1
;Value: 0
;Value: 1
;Value: 0

(define flip2 (make-flip))
;(flip2)
;Value: 1
;Value: 0
;Value: 1
;Value: 0

;;flip1 and flip2 flip independently of each other

;;;=================================================
;;; Warm-up 2
;;;=================================================

(define flip (make-flip))
(define flap1 (flip))
(define (flap2) (flip))
(define flap3 flip)
(define (flap4) flip)

flap1
;Value: 1

flap2
;Value: #[compound-procedure 10 flap2]

flap3
;Value: #[compound-procedure 9]

flap4
;Value: #[compound-procedure 11 flap4]

(flap1)
;The object 1 is not applicable.
;Type D to debug error, Q to quit back to REP loop: q
;Quit!

;(flap2)
;Value: 0

;(flap3)
;Value: 1

;(flap4)
;Value: #[compound-procedure 9]

;flap1
;Value: 1

;(flap3)
;Value: 0

;(flap2)
;Value: 1

;((flap4))
;Value: 0

;;;=======================================================
;;; Warmup 3 4 5 6 - done by hand
;;;======================================================

;;;=====================================================
;;; Exercise 1
;;;=====================================================

;;With Louis' method, at the time of installing Holly we lose
;;track of the fact that she is a person and she is installed
;;as a mobile object. so the birthplace lists Holly as a
;;mobile object and when Holly leaves and sends amessage
;;that she is leaving, the room does not know who she is.
;;On arriving in a new room Holly reports to the new room
;;which adds her to its list. so she is now in both the
;;old room and the new room.

;;;=====================================================
;;; Exercise 2
;;;=====================================================
;; john is more restless than holly
;; both of them move about every 6th time
;; did 40 moves

;;;=====================================================
;;; Exercise 3
;;;=====================================================

;(define shyam   (make&install-person 'shyam   student-lab  100))
;(define late-homework
  (make&install-thing 'late-homework   student-lab))

;(ask shyam 'look-around)
;At student-lab : shyam says -- I see late-homework computer-manual 
;Value: (late-homework computer-manual)

;(ask shyam 'take late-homework)
;At student-lab : shyam says -- I take late-homework 
;Value: #t

;(ask john 'look-around)
;At faculty-office : john says -- I see john-card holly-card 
;Value: (john-card holly-card)

;(ask shyam 'go 'east)
;shyam moves from student-lab to foundation-office 
;Value: #t

;(ask shyam 'go 'east)
;shyam moves from foundation-office to faculty-office 
;At faculty-office : shyam says -- Hi john 
;Value: #t

;(ask shyam 'lose late-homework)
;At faculty-office : shyam says -- I lose late-homework 
;Value: #t

;(ask john 'take late-homework)
;At faculty-office : john says -- I take late-homework 
;Value: #t

;;;=======================================================
;;; Exercise 4 on sheet
;;;======================================================

;;;=======================================================
;;; Exercise 5
;;;=======================================================

;;make-card-locked-place uses a filter to look for id-card
;;on a person; since this is list we will check for a non-empty
;;list and then apply filter

(define (make-card-locked-place name)
  (let ((place (make-place name)))
    (lambda (message)
      (cond ((eq? message 'accept-person?)
             (lambda (self person)
	        (let ((person-list (ask person 'possessions)))
		  (define (helper lst)
		   (cond ((null? lst) #f)
			 ((is-a (car lst) 'id-card?)
			  (ask big-brother 'inform self (ask (car lst) 'id))
			  #t)
			 (else helper (cdr lst))))
		  (helper person-card-list))))
	    (else (get-method place message))))))


;;here student-lab is card-locked
;;and holly is able to get in after getting an id-card
;;while john is unable to get in since he has no id-card

;(ask student-lab 'accept-person? holly)
;Value: #f

;(ask holly 'move-to student-lab)
;holly can't move to student-lab 
;Value: message-displayed

;(ask holly 'to-move faculty-office)
;holly moves from classroom to faculty-office 
;At faculty-office : holly says -- Hi john 
;Value: #t

;(ask holly 'take holly-card)
;At faculty-office : holly says -- I take holly-card 
;Value: #t

;(ask holly 'move-to student-lab)
;holly moves from faculty-office to student-lab 
;At student-lab : holly says -- Hi shyam 
;Value: #t

;(ask john 'move-to student-lab)
;john can't move to student-lab 
;Value: message-displayed


;;;=================================================================
;;; Exercise 6
;;;=================================================================

;;Protected-student-lab does the following:
;;'accept-person -- gets a list of person's possessions, checks for
;; id-card objects and then checks the id-card's symbol against the
;; id-card-list it maintains
;;'register-card -- when asked to register a id-card, it checks to
;; to see if the object is an id-card, if it is in the room and
;; if it has alrady been registered. If non of these apply the card
;; is registered and added to the id-card-list of the room.

(define (protected-student-lab name)
  (let ((id-card-list '())
	(place (make-place name)))
    (lambda (message)
      (cond ((eq? message 'accept-person?)
	     (lambda (self person)
	       (let ((person-list (ask person 'possessions)))
		 (define (helper lst)
		   (cond ((null? lst) #f)
			 ((not (is-a (car lst) 'id-card)) (helper (cdr lst)))
			 ((memq (ask (car lst) 'id) id-card-list)
			  (ask big-brother 'inform self (ask (car lst) 'id))
			  #t)
			 (else helper (cdr lst))))
		 (helper person-card-list))))
	    ((eq? message 'register-card)
	     (lambda (self id-card)
	       (cond ((not (is-a id-card 'id-card?))
		      (display-message (list (ask id-card 'name) "is not an id-card")))
		     ((memq (ask id-card 'id) id-card-list)
		      (display-message (list (ask id-card 'name) "has already been registered")))
		     ((not (eq? self (ask id-card 'place)))
		      (display-message (list (ask 'id-card 'name) "is not present in" (ask self 'name))))
		     (else (set! id-card-list (cons (ask id-card 'id) id-card-list))
			    (display-message (list (ask id-card 'name) "is now registered"))
			    #t))))
	    (else (get-method place message))))))


;;here student-lab has been protected

;(define shyam-card (make&install-id-card 'shyam-card student-lab '2000-50))
;(ask student-lab 'register-card shyam-card)
;shyam-card has already been registered 
;(ask shyam 'take shyam-card)
;At student-lab : shyam says -- I take shyam-card 
;(ask shyam 'go 'east)
;shyam moves from student-lab to foundation-office 
;(ask shyam 'move-to student-lab)
;shyam moves from foundation-office to student-lab 
;(ask holly 'move-to student-lab)
;holly can't move to student-lab 


;;;=================================================================
;;; Exercise 7
;;;=================================================================
;; ogre has an inner troll and an inner person. It checks in a room
;; if the stolen-card is present. If the card is owned by the ogre
;; it ignores it, if the card is on the floor it picks it up and
;; if a person owns the card it eats the person.

(define (make-ogre name birthplace threshold stolen-id)
  (let ((troll (make-troll name birthplace threshold))
	(person (make-person name birthplace threshold)))
    (lambda (message)
      (cond ((eq? message 'ogre?) (lambda (self) true))
	    ((eq? message 'person) (lambda (self) false)) ; to prevent grendal from eating ogre
	    ((eq? message 'act)
		  (lambda (self)
		    (let ((id-list (filter (lambda (x) (is-a x 'id-card?))
					   (ask (ask self 'place) 'things))))
		      (let ((stolen-list (filter (lambda (x) (eq? stolen-id (ask x 'id)))
						id-list)))
			(define (helper lst)
			  (cond ((null? lst) ((get-method person 'act) self))
				((eq? self (ask (car lst) 'owner))
				 (helper (cdr lst)))
				((eq? 'nobody (ask (car lst) 'owner))
				 (ask self 'take (car lst)))
				(else ((get-method troll 'eat-person) self
								      (ask (car lst) 'owner)))))
			(helper stolen-list)))))
	    (else (get-method person message))))))

;(define (make&install-ogre name birthplace threshold stolen-id)
;  (let ((ogre (make-ogre name birthplace threshold stolen-id)))
;    (ask ogre 'install)
;    ogre))

;; report-stolen-card on getting a id-card number, generates an
;; ogre in the dungeon to hunt the particular id-card

;(define (report-stolen-card id)
;  (make&install-ogre (string->symbol (string-append "ogre" (symbol->string id))) dungeon 1 id)
;  (display-message (list "OGRE has been dispatched to hunt down" id)))


;;report shyam-card and blake-card as stolen; OGREs dispatched
;;finally OGRE eats blake
		      
(ask shyam-card 'id)
;Value: 2000-34
(report-stolen-card '2000-34)
OGRE has been dispatched to hunt down 2000-34 

---Tick---
ogre2000-34 moves from dungeon to supply-closet 
blake chose to stay put 
albert chose to stay put 
shyam chose to stay put 
john moves from faculty-office to foundation-office 
holly chose to stay put 
;Value: tick-tock

(ask blake-card 'id)
;Value: 2000-36
(report-stolen-card '2000-36)
OGRE has been dispatched to hunt down 2000-36 

---Tick---
ogre2000-36 moves from dungeon to supply-closet 
At supply-closet : ogre2000-36 says -- Hi ogre2000-34 
ogre2000-34 moves from supply-closet to room-with-elevator 
blake chose to stay put 
albert chose to stay put 
shyam chose to stay put 
john moves from foundation-office to bike-storage-hallway 
holly chose to stay put 
;Value: tick-tock

---Tick---
ogre2000-36 moves from supply-closet to room-with-elevator 
At room-with-elevator : ogre2000-36 says -- Hi ogre2000-34 
ogre2000-34 moves from room-with-elevator to bike-storage-hallway 
At bike-storage-hallway : ogre2000-34 says -- Hi john 
blake chose to stay put 
albert chose to stay put 
shyam chose to stay put 
john moves from bike-storage-hallway to room-with-elevator 
At room-with-elevator : john says -- Hi ogre2000-36 
holly moves from classroom to room-with-elevator 
At room-with-elevator : holly says -- Hi john ogre2000-36 
;Value: tick-tock

---Tick---
ogre2000-36 moves from room-with-elevator to classroom 
ogre2000-34 moves from bike-storage-hallway to lower-stairwell 
blake chose to stay put 
albert chose to stay put 
shyam chose to stay put 
john chose to stay put 
holly chose to stay put 
;Value: tick-tock

(ask blake 'take blake-card)
At student-lab : blake says -- I take blake-card 
;Value: #t

---Tick---
ogre2000-36 moves from classroom to room-with-elevator 
At room-with-elevator : ogre2000-36 says -- Hi holly john 
ogre2000-34 moves from lower-stairwell to bike-storage-hallway 
blake chose to stay put 
albert moves from student-lab to network-closet 
shyam chose to stay put 
john moves from room-with-elevator to elevator 
holly moves from room-with-elevator to supply-closet 
;Value: tick-tock

---Tick---
ogre2000-36 moves from room-with-elevator to supply-closet 
At supply-closet : ogre2000-36 says -- Hi holly 
ogre2000-34 moves from bike-storage-hallway to foundation-office 
blake chose to stay put 
albert chose to stay put 
shyam chose to stay put 
john moves from elevator to room-with-elevator 
holly chose to stay put 
;Value: tick-tock

---Tick---
ogre2000-36 moves from supply-closet to room-with-elevator 
At room-with-elevator : ogre2000-36 says -- Hi john 
ogre2000-34 can't move to student-lab 
blake chose to stay put 
albert chose to stay put 
shyam chose to stay put 
john moves from room-with-elevator to bike-storage-hallway 
holly chose to stay put 
;Value: tick-tock

(ask blake 'move-to room-with-elevator)
blake moves from student-lab to room-with-elevator 
At room-with-elevator : blake says -- Hi ogre2000-36 
;Value: #t

---Tick---
At room-with-elevator : ogre2000-36 says -- Growl.... I'm going to eat you, blake 
At room-with-elevator : blake says -- I lose blake-card 
At room-with-elevator : blake says -- 
                     Dulce et decorum est 
                     pro computatore mori! 
blake moves from room-with-elevator to heaven 
At room-with-elevator : ogre2000-36 says -- Chomp chomp. blake tastes yummy! 
ogre2000-34 moves from foundation-office to bike-storage-hallway 
At bike-storage-hallway : ogre2000-34 says -- Hi john 
blake chose to stay put 
albert moves from network-closet to supply-closet 
At supply-closet : albert says -- Hi holly 
shyam chose to stay put 
john moves from bike-storage-hallway to room-with-elevator 
At room-with-elevator : john says -- Hi ogre2000-36 
holly chose to stay put 
;Value: tick-tock

      
	      
;;;========================================================
;;; Exercise 8
;;;========================================================

;;make-master-brother gets a list of all creatures that are
;;moving on each clock tick. the new list generated on each
;;tick replaces the old list. in the list any duplicate ids
;;are checked and an ogre is dispatched as needed.

(define (make-master-brother name)
  (let ((master-list '()))
     (lambda (message)
	  (cond ((eq? message 'inform)
		 (lambda (self place id)
		   (define (helper lst)
		     (cond ((null? lst)
			    (set! master-list (cons (cons id place) master-list))
			    (display-message (list (ask self 'name) " -- hello"
						   (ask place 'name) ", got your information")))
			   ((and (eq? (caar lst) id)
				 (eq? (cdar lst) place))
			    (display-message (list (ask self 'name) " -- hello"
						   (ask place 'name) ", got your information")))
			   ((and (eq? (caar lst) id)
				 (not (eq? (cdar lst) place)))
			    (display-message (list "hello, this is a duplicate card!"))
			    (report-stolen-card id))
			   (else (helper (cdr lst)))))
		   (helper master-list)))
		((eq? message 'install)
		 (lambda (self) (add-to-clock-list self)))
		((eq? message 'move)
		 (set! master-list '()))
		((eq? message 'name) (lambda (self) name))
		(else (no-method name))))))

;(define big-brother (make-master-brother 'big-brother))
;(ask big-brother 'install)

;;this is a limited test showing that every time a person
;;moves to a protected-student-lab room, the big-brother is
;;informed

;(ask shyam 'go 'east)
;shyam moves from student-lab to foundation-office
;(ask shyam 'go 'west)
;shyam moves from foundation-office to student-lab
;big brother - hello, student-lab got your information

;;;=========================================================
;;; Exercise 9
;;;=========================================================

;;the new act proc for student has the student pick-up stuff
;;randomly (1 in 3 chance) and drop stuff randomly (1 in 4
;;chance). also picks up any id-cards in the room.

(define (make-student name birthplace threshold)
  (let ((person (make-person name birthplace threshold)))
    (lambda (message)
      (cond ((eq? message 'act)
	     (lambda (self)
	       (let ((other-stuff-in-room (filter (lambda (x) (is-a x 'ownable?))
						  (ask (ask self 'place) 'things))))
		 (let ((id-list (filter (lambda (x) (is-a x 'id-card?))
					other-stuff-in-room)))
		   (cond ((not (null? id-list))
			  (ask self 'take (pick-random id-list)))
			 ((and (= (random 3) 0) (not (null? other-stuff-in-room)))
			  (ask self 'take (pick-random other-stuff-in-room)))
			 ((and (= (random 4) 0) (not (null? (ask self 'possessions))))
			  (ask self 'lose (pick-random (ask self 'possessions))))
			 (else ((get-method person 'act) self)))))))
	    (else (get-method person message))))))

;;works but not fully tested

;;;==========================================================
;;; Exercise 11
;;;==========================================================

;The actual quote is:
;"Dulce et decorum est
;Pro patria mori."

;which appears in the poem "Dulce et Decorum Est" by Wilfred Owen
;describing World War I.

;Translated form Latin it means:
;"It is sweet and becoming to die for your country".