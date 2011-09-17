; Problem Set 5 Solutions
;   by Dimitri Kountourogiannis

; ============
;  Exercise 1
; ============

; List union returns the list l1, minus the elements in it that
; also appear in l2, appended to the list l2.
; Some Examples:

(list-union '(1 2 3) '(4 5 6))
;Value: (1 2 3 4 5 6)

(append '(1 2 3) '(4 5 6))
;Value: (1 2 3 4 5 6)

(list-union '(1 2 3 4) '(1 4 5 6))
;Value: (2 3 1 4 5 6)

(append '(1 2 3 4) '(1 4 5 6))
;Value: (1 2 3 4 1 4 5 6)

(list-union '(1 1 1 3 2 2) '(4 4 3 5 5))
;Value: (1 1 1 2 2 4 4 3 5 5)

; ============
;  Exercise 2
; ============

; (reduce * 1 lst)
; Computes the product of a list of numbers lst

(reduce * 1 '(1 2 3 4 5))
;Value: 120

; ============
;  Exercise 3
; ============

; Computes the sum of squares of a list of numbers lst

(reduce * 1 (map square lst))

; ============
;  Exercise 4
; ============

; Reduce Appends the elements of a list of lists together

(reduce append '() '((1 2) (2 3) (4 6) (5 4)))
;Value: (1 2 2 3 4 6 5 4)

; If list union is used instead of append, then repeats are removed
; (assuming there are no repeats already in the given lists) 

(reduce list-union '() '((1 2) (2 3) (4 6) (5 4)))
;Value: (1 2 3 6 5 4)

(define (list-union l1 l2)
  (cond ((null? l1) l2)
	((member (car l1) l2)
	 (list-union (cdr l1) l2))
	(else
	 (cons (car l1)
	       (list-union (cdr l1) l2)))))
; ============
;  Exercise 5
; ============

; proc returns the list symbols with any repetitions
; removed. If a symbol occurs n times, the first n-1
; instances will be removed and the last one will remain

(define (proc symbols)
  (reduce list-union
	  '()
	  (map list symbols)))
(proc '(a a b a c d c e f g c))
;Value: (b a d e f g c)

; remove-repeats would be a more descriptive name

; ============
;  Exercise 6
; ============

; There are many things you could add. Here are some examples
; (thanks to John West)

(define beginnings   
  '((you say)     
    (why do you say)     
    (i am glad to hear that)     
    (I nearly fainted from joy the last time someone said)     
    (can you stand on this chair and repeat)     
    (how dare you say)     
    (it is interesting you think)     
    (it is pitiful you think)    
    (My dog is hard of hearing. Can you shout)     
    (are you angry when you say)     
    (not even my lover says)     
    (bugger me. you are saying)     
    (i am sad you say)     
    (my dog does not like you saying)     
    ()))        

(define general-advice
  '((make sure to take some humanities)
    (Burgundy has a lot of interesting departments)
    (make sure to get time to explore the Boston area)
    (how about a freshman seminar)
    (take the time to explore your sexuality. I am sure you will find it
worthwhile)
    (do stop prattling and try and think before you speak)
     (One of the Burgundy customs you may not know about is hitting members
of faculty very hard on the chin when you see them)
    (Perhaps you could dress a little better than you do now, and not spit
on the floor)
    (Some 15% of our students think that they are Elvis)
    (The professor of molecular biology is eminently bribeable. All his
students get top marks)
    (personal hygiene does not really matter here so save time by not
washing)
    (Say that again and I will call the police)))


; ============
;  Exercise 7
; ============

               
** (blah blah eecs101 tra la la)
(eecs101 is too much work for freshmen -- wait until next year)

** (fee fie fo fum eecs101)
(eecs101 is too much work for freshmen -- wait until next year)

** (phys101 tastes yummy)
(students really enjoy phys101)

** (Know of any seminar worth taking?)
(i hear that snorkeling in boston harbor is a really exciting seminar)

** (I want to take off next week)
(too bad -- off is not offered next week)

** (I want to double major in sex, drugs and rock n roll)
(sex, drugs is fascinating and you can make a living doing it if rock n roll does not work out)

** (I want to double major in sex, drugs)
(doing a double major is a lot of work)

** (What is math101 about)
(math101 is about elementary differential and integral calculus)

** (What are math101 and math102 about)
(math101 is about elementary differential and integral calculus)
(math102 is about multivariate calculus)

** (How many units are math101 and math102)
(math101 is a 4 unit subject)
(math102 is a 4 unit subject)

** (What are the prerequisites for math102)
(the prerequisites for math102 are math101)

** (Can I take eecs101)
(the prerequisites for eecs101 are true-grit)


; ============
;  Exercise 8
; ============

; ALL-PREREQUISITES returns the prerequistes of a subject in the generalized 
; sense: {prereqs of subject} U {prereqs of prereqs of subject} U ...
 
(define (all-prerequisites subject)
  (let ((entry (find subject catalog)))
    (if (null? entry) '()
	(let ((a (entry-prerequisites entry)))
	  (if (null? a) '()
	      (list-union a
			  (reduce list-union
				  '()
				  (map all-prerequisites a))))))))
			
(all-prerequisites 'geo104)
;Value: (math203 phys102 math102 phys101 math101)

(all-prerequisites 'eecs101)
;Value: (true-grit)


; ============
;  Exercise 9
; ============

; A rule for checking prerequisites

   (make-rule
    `(can I take (? s ,in-catalog) if I have not taken (? t ,in-catalog))
    (lambda (dict)
      (let ((course1 (entry-subject (value 's dict)))
	    (course2 (entry-subject (value 't dict))))
	(if (member course2 (all-prerequisites course1))
	    (write-line (append '(No way Jose!)
				(list course2)
				'(is a prerequisite for)
				(list course1)))
	    (write-line '(Sure! Be my guest.))))))

;
; ** (can i take math101 if i have not taken math101)
; (sure! be my guest.)
;
; ** (can i take geo104 if i have not taken math101)
; (no way jose! math101 is a prerequisite for geo104)


; =============
;  Exercise 10
; =============

; CHECK-CIRCULAR-PREREQUISITES makes sure a course-list does not
; simultaneously countain a course and a (generalized) prerequisite

(define (check-circular-prerequisites lst)
  (null? (list-intersection lst
			    (reduce list-union '() (map all-prerequisites lst)))))

; =============
;  Exercise 11
; =============

; TOTAL-CREDITS sums up the total credit value of the list courses  passed to it

(define (total-credits lst)
  (define (get-credits course)
    (entry-units (find course catalog)))
  (reduce + 0 (map get-credits lst)))

; =============
;  Exercise 12
; =============

; CHECK-SUBJECT-LIST takes a subject list and makes sure that 
; it doesn't go over the credit limit, and it
; doesn't contain circular prerequisites .


(define (check-subject-list lst)
  (define (get-prereqs course)
    (entry-prerequisites (find course catalog)))
  (cond ((> (total-credits lst) 18)
	 (write-line '(Now wait a minute, son. 
		       Your credit load is over the limit of 18. 
		       Lookie here, you better drop some courses!)) false)
	 ((not (check-circular-prerequisites lst))
	  (write-line '(What kind of a fool are you, boy?))
	  (write-line '(You are not
			allowed to take a course and a prerequisite
			at the same time!))
	  (write-line '(Do I have to give you a
			whuppin?)) false)
	 (else (write-line (append '(Listen up son, you need these-here 
				    prerequisites:)
				  (reduce list-union '() (map get-prereqs lst)))) true)))

; =============
;  Exercise 13
; =============

; A rule for checking a subject list.

   (make-rule
    `(I want to take (?? s ,subjects))
    (lambda (dict)
      (check-subject-list (map entry-subject (value 's dict)))))

; =============
;  Exercise 14
; =============


; There are many things you can add/modify
; Here is a really nice extension from Jeffrey Radcliffe

;;; george.scm
;;;
;;; Implements a Presidental Candidate.
;;;
;;; Modified from the original aDuni problem "ps5adv.scm" by 
;;; Jeffrey M. Radcliffe, October 2000.

;;; Top-level procedure

(define (debate-george name)
  (write-line (list 'hello name))
  (write-line '(it is a pleasure to debate you today))
  (write-line '(let us talk about some issues))
  (george-driver-loop name)
  '#t)

;;; Driver loop

(define (george-driver-loop name)
  (let ((user-response (prompt-for-command-expression "** ")))
    (cond ((equal? user-response '(goodbye))
	   (write-line (list 'goodbye name))
	   (write-line '(be sure to vote bush 2000!)))
	  (else (reply-to user-response)
		(george-driver-loop name)))))


;;; Select method for generating the reply

(define (reply-to input)
  (cond ((translate-and-run input subject-knowledge))
	((translate-and-run input conventional-wisdom))
	((with-odds 1 3)
	 (write-line (reflect-input input)))
	(else
	 (write-line (pick-random general-advice)))))


;;; First, let's do the simple, stupid(er) things.

;;; This returns true n1 out of n2 times
(define (with-odds n1 n2)
  (< (random n2) n1))

;;; One simple response method is to repeat the user's reponse, after 
;;; changing first-person words to second person

(define (reflect-input input)
  (append (pick-random beginnings)
	  (change-person input)))

(define (pick-random list)
  (list-ref list (random (length list))))

(define beginnings
  '((you claim)
    (why do you say)
    (it is suprising that)
    ()))

(define (change-person phrase)
  (sublist '((i you) (me you) (am are) (my your))
	   phrase))

(define (sublist replacements list)
  (map (lambda (elt) (substitute replacements elt)) list))

(define (substitute replacements item)
  (cond ((null? replacements) item)
	((eq? item (caar replacements)) (cadar replacements))
	(else (substitute (cdr replacements) item))))

;;; Another simple response method is to just reply with with some
;;; general advice chosen at random

(define general-advice
  '((i am a compassionate conservate you know)
    (my father was the president)
    (democrats are evil and i am not a democrat)
    (i do not have a cold heart)
    (rats .... erm nevermind)))

;;; More sophisticated methods match the input against a set of rules.
;;; Translate-and-run will try the rules, and return FALSE if no rules
;;; match.  If some rule matched, then the rule action is run, and
;;; translate-and-run returns TRUE.

(define (translate-and-run input rules)
  (try-rules input rules
	     (lambda () false)	            
	     (lambda (result fail) true)))  


;;; The simplest rules just test for patterns in the input, and print
;;; some conventional wisdom.

;;; this procedure ignores the match dictionary and just produces knee-jerk
;;; reactions

(define (simple-response text)
  (lambda (dict) (write-line text)))

(define conventional-wisdom
  (list
;   (make-rule
;    '((?? x) gun control (?? y))
;    (simple-response '(i will defend our second amendment rights to bear arms)))
   (make-rule
    '((?? x) clinton (?? y))
    (simple-response '(president clinton is morally bankrupt)))
   (make-rule
    '((?? x) policy (?? y))
    (simple-response '(i will strive to make good policy with my advisors)))
   (make-rule
    '((?? x) tax (?? y))
    (simple-response '(my tax plan will benefit all americans)))
   (make-rule
    '((?? x) taxes (?? y))
    (simple-response '(my tax plan will benefit all americans)))
   (make-rule
    '((?? x) buddhist (?? y))
    (simple-response '(democrats are abusers of campaign funding rules)))
   (make-rule
    '((?? x) texas (?? y))
    (simple-response '(texas is the greatest state in nation)))
   (make-rule
    '((?? x) rats (?? y))
    (simple-response '(did you say something about my opponent)))
   (make-rule
    '((?? x) cocaine (?? y))
    (simple-response '(i would rather not talk about my past)))
   (make-rule
    '((?? x) economy (?? y))
    (simple-response '(i will do for america what i did for the texas economy)))
    (make-rule
    '((?? x) drugs (?? y))
    (simple-response '(i would rather not talk about my past)))
    (make-rule
    '((?? x) children (?? y))
    (simple-response '(i will leave no child behind)))
    (make-rule
     '((?? x) arsdigita (?? y))
     (simple-response '(arsdigita is a threat to democracy and apple pie)))
    (make-rule
     '((?? x) philip greenspun (?? y))
     (simple-response '(all hail the grand and benevolent philip greenspun!)))
))

;;; More "sophisticated" responses depend on knowledge drawn from the
;;; Party Platform(TM) catalog.

;;; Here is a simple catalog data structure with a few entries:

(define (make-entry subject department summary units satisfies prerequisites)
  (list subject department summary units satisfies prerequisites))
(define (issue-subject issue) (list-ref issue 0))
(define (issue-department issue) (list-ref issue 1))
(define (issue-summary issue) (list-ref issue 2))
(define (issue-units issue) (list-ref issue 3))
(define (issue-satisfies issue) (list-ref issue 4))
(define (issue-prerequisites issue) (list-ref issue 4))

(define platform
  (list
   (make-entry 'guns    'domestic  '(gun control) 6 '()
	       '(lobbyist monies and deer hunting))
   (make-entry 'israel  'foreign   '(the middle east) 1 '()
	       '(pride and independence))
   (make-entry 'palestine  'foreign   '(the middle east) 1 '()
	       '(independence and pride))
   (make-entry 'schools    'domestic '(the school system) 1 '()
	       '(welfare exploitation and integrity))
   (make-entry 'abortion   'domestic '(abortion) 1 '()
	       '(single moms and religion))
   (make-entry 'internet   'international '(the internet) 1 '()
	       '(dark hearted space monkeys evil cults and pornographers))
   (make-entry 'napster    'internet '(file trading) 1 '()
	       '(metallica and that other band))
   (make-entry 'reform     'poticial '(reform) 2 '()
	       '(big spending and soft money))
   (make-entry 'stuff        'rats '(that thing) 1 '()
	       '(respect and dignity))
))

;;;Using the platform, we can define some match restrictions

;;;Given a subject, succeeds with the platform entry for that subject if it
;;;is in the catalog, otherwise fails

(define (in-platform subject fail succeed)
  (let ((entry (find subject platform)))
    (if entry
	(succeed entry fail)
	(fail))))

;;;Matches a list of subjects of the form s1 s2 .... and sk

(define (subjects words fail succeed)
  (try-rules
   words
   (list
    (make-rule
     `((? subject ,in-platform))
     (lambda (dict)
       (list (value 'subject dict))) )
    (make-rule
     `((?? list-of-subjects ,subject-seq) and (? final-subject ,in-catalog))
     (lambda (dict)
       (append (value 'list-of-subjects dict)
	       (list (value 'final-subject dict)))) )
     )
   fail
   succeed))

;;; Subprocedure used by SUBJECTS.  Matches a sequence of subjects 
;;; s1 s2 ... sn

(define (subject-seq words fail succeed)
  (try-rules
   words
   (list
    (make-rule
     `((? subject ,in-platform))
     (lambda (dict)
       (list (value 'subject dict))) )
    (make-rule
     `((? subject1 ,in-platform) (?? more-subjects ,subject-seq))
     (lambda (dict)
       (cons (value 'subject1 dict) (value 'more-subjects dict))) )
     )
   fail
   succeed))
	     
;;; Now we can define some actual reponses based upon catalog knowledge

(define subject-knowledge 
  (list
   (make-rule
    `(what do you think about (? s ,in-platform))
    (lambda (dict)
      (let ((issue (value 's dict)))
	(write-line
	 (append  (issue-summary issue)
		 '(is a very important)
		 (list (issue-department issue))
		 '(issue))))))
   (make-rule
    `(what do you think about the (? s ,in-platform))
    (lambda (dict)
      (let ((issue (value 's dict)))
	(write-line
	 (append  (issue-summary issue)
		 '(is a very important)
		 (list (issue-department issue))
		 '(issue))))))
   (make-rule
    `(what is your stance on (? s ,in-platform))
    (lambda (dict)
      (let ((issue (value 's dict)))
	(write-line
	 (append  (issue-summary issue)
		 '(is a very important)
		 (list (issue-department issue))
		 '(issue))))))
   (make-rule
    `(what are (?? s ,subjects) about)
    (lambda (dict)
      (for-each (lambda (entry)
		  (write-line
		   (append (list (entry-subject entry))
			   '(is about)
			   (entry-summary entry))))
		(value 's dict))) )
   (make-rule
    `(do you think (?? s ,subjects) are important)
    (lambda (dict)
      (for-each (lambda (issue)
		  (write-line
		   (append '(i consider)
			   (list (issue-subject issue))
			   '(to be a number)
			   (list (issue-units issue))
			   '(priority))))
		(value 's dict))))
   (make-rule
    `(how important is (?? s ,subjects))
    (lambda (dict)
      (for-each (lambda (issue)
		  (write-line
		   (append '(i consider)
			   (list (issue-subject issue))
			   '(to be a number)
			   (list (issue-units issue))
			   '(priority))))
		(value 's dict))))
   (make-rule
    `(how important are (?? s ,subjects))
    (lambda (dict)
      (for-each (lambda (issue)
		  (write-line
		   (append '(i consider)
			   (list (issue-subject issue))
			   '(to be a number)
			   (list (issue-units issue))
			   '(priority))))
		(value 's dict))))
   (make-rule
    `(what are the issues for (? s ,in-platform))
    (lambda (dict)
      (let ((issue (value 's dict)))
	(write-line
	 (append '(the real issues for)
		 (list (issue-subject issue))
		 '(are)
		 (issue-prerequisites issue))))) )
   (make-rule
    `(what are the issues in (? s ,in-platform))
    (lambda (dict)
      (let ((issue (value 's dict)))
	(write-line
	 (append '(the real issues for)
		 (list (issue-subject issue))
		 '(are)
		 (issue-prerequisites issue))))) )
   ))

(define (filter test? subjects)
  (cond ((null? subjects) '())
	((test? (car subjects))
	 (cons (car subjects)
	       (filter test? (cdr subjects))))
	(else 
	  (filter test? (cdr subjects)))))

(define (find subject entries)
  (cond ((null? entries) false)
	((eq? subject (entry-subject (car entries)))
	 (car entries))
	(else
	 (find subject (cdr entries)))))

(define (list-union l1 l2)
  (cond ((null? l1) l2)
	((member (car l1) l2)
	 (list-union (cdr l1) l2))
	(else
	 (cons (car l1)
	       (list-union (cdr l1) l2)))))

(define (list-intersection l1 l2)
  (cond ((null? l1) '())
	((member (car l1) l2)
	 (cons (car l1)
	       (list-intersection (cdr l1) l2)))
	(else (list-intersection (cdr l1) l2))))

(define (reduce combiner initial-value list)
  (define (loop list)
    (if (null? list)
	initial-value
	(combiner (car list) (loop (cdr list)))))
  (loop list))

















