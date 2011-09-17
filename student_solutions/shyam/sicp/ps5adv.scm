;;;
;;; ps5.scm
;;;
;;; Implements the Burgundy Freshman Advisor (BFA).
;;;
;;; Modified from the original MIT Press SICP file "adv.scm" by 
;;; Holly Yanco and John Pezaris, October 2000.

;;; Top-level procedure

(define (see-advisor name)
  (write-line (list 'hi name))
  (write-line '(i am your freshman advisor))
  (write-line '(what are your plans for the semester))
  (advisor-driver-loop name)
  '#t)

;;; Driver loop

(define (advisor-driver-loop name)
  (let ((user-response (prompt-for-command-expression "** ")))
    (cond ((equal? user-response '(goodbye))
	   (write-line (list 'goodbye name))
	   (write-line '(have a good semester!)))
	  (else (reply-to user-response)
		(advisor-driver-loop name)))))


;;; Select method for generating the reply

(define (reply-to input)
  (cond ((translate-and-run input subject-knowledge))
	((translate-and-run input conventional-wisdom))
	((with-odds 1 2)
	 (write-line (reflect-input input)))
	(else
	 (write-line (pick-random general-advice)))))


;;; First, let's do the simple, stupid things.

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
  '((you say)
    (why do you say)
    (i am glad to hear that)
    (what do you mean when you say)
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
  '((make sure to take some humanities)
    (Burgundy has a lot of interesting departments)
    (make sure to get time to explore the Boston area)
    (how about a freshman seminar)
    (take a shower everyday)
    (make sure you know where the nearest ATM is)
    (note that 4 right turns in Boston gets you to a new place)
    (you will do well here)))


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

;;; this procedure ignores the match dictionary and just prints the response
(define (simple-response text)
  (lambda (dict) (write-line text)))

(define conventional-wisdom
  (list
   (make-rule
    '((?? x) eecs101 (?? y))
    (simple-response '(eecs101 (SICP) is too much work for freshmen
			     -- wait until next year)))
   (make-rule
    '((?? x) phys101 (?? y))
    (simple-response '(students really enjoy phys101)))
   (make-rule
    '((?? x) seminar (?? y))
    (simple-response '(i hear that snorkeling in Boston Harbor
			 is a really exciting seminar)))

   (make-rule
    '((?? x) to take (?? y) next (?? z))
    (lambda (dict)
      (write-line
       (append '(too bad -- )
	       (value 'y dict)
	       '(is not offered next)
	       (value 'z dict)))))
   (make-rule
    '((?? x) double major in (?? y) and (?? z))
    (lambda (dict)
      (write-line
       (append
	(value 'y dict)
	'(is fascinating and you can make a living doing it if)
	(value 'z dict)
	'(does not work out)))))
   (make-rule
    '((?? x) double major (?? y))
    (simple-response '(doing a double major is a lot of work)))
    ))

;;; More sophisticated responses depend on knowledge drawn from the
;;; Burgundy catalog.

;;; Here is a simple catalog data structure with a few entries:

(define (make-entry subject department summary units satisfies prerequisites)
  (list subject department summary units satisfies prerequisites))

(define (entry-subject entry) (list-ref entry 0))
(define (entry-department entry) (list-ref entry 1))
(define (entry-summary entry) (list-ref entry 2))
(define (entry-units entry) (list-ref entry 3))
(define (entry-satisfies entry) (list-ref entry 4))
(define (entry-prerequisites entry) (list-ref entry 5))


(define catalog
  (list 
   (make-entry 'phys101 'physics   '(classical mechanics) 4 
                                   '(gur physics) '())
   (make-entry 'phys102 'physics   '(electricity and magnetism) 4
                                   '(gur physics) '(phys101 math101))
   (make-entry 'phys103 'physics   '(waves) 4
                                    '(rest) '(phys102 math102))
   (make-entry 'phys304 'physics   '(quantum weirdness) 4
                                   '(rest) '(phys103 math203))
   (make-entry 'math101 'math      '(elementary differential and integral calculus) 4
                                   '(gur calculus) '())
   (make-entry 'math102 'math      '(multivariate calculus) 4
                                   '(gur calculus) '(math101)) 
   (make-entry 'math203 'math      '(differential equations) 4
                                   '(rest) '(math102))
   (make-entry 'math204 'math      '(theory of functions of a complex variable) 4
                                   '() '(math203))
   (make-entry 'math319 'math      '(hard equations) 4
                                   '() '(math204))
   (make-entry 'eecs101 'eecs      '(scheming with yanco and pezaris) 5
                                   '(rest) '(true-grit))
   (make-entry 'eecs202 'eecs      '(circuits) 5
                                   '(rest) '(phys102 math102))
   (make-entry 'chem291 'chemistry '(like crystals dude) 4
                                   '(gur chemistry) '())
   (make-entry 'chem224 'chemistry '(fun with benzene) 4
                                   '(gur chemistry) '(chem111))
   (make-entry 'chem111 'chemistry '(smelly organic crud and goop) 4
                                   '(gur chemistry) '(a-strong-stomach))
   (make-entry 'hist101 'history   '(what has been) 3
                                   '(gur history) '())
   (make-entry 'hist102 'history   '(what will be) 3
                                   '(gur history) '())
   (make-entry 'bio101  'biology   '(squishy things) 3
                                   '(gur biology) '())
   (make-entry 'bio212  'biology   '(diseases and their applications) 4
                                   '(gur biology) '())
   (make-entry 'bio113  'biology   '(drugs and their applications) 4
                                   '(gur biology) '())
   (make-entry 'bio114  'biology   '(you and your brain) 4
                                   '(gur biology) '())
   (make-entry 'geo101  'geology   '(rocks for jocks) 4
                                   '(rest) '())
   (make-entry 'geo104  'geology   '(planets) 4
                                   '() '(math203 phys102)) 
   ))

;;;Using the catalog, we can define some match restrictions

;;;Given a subject, succeeds with the catalog entry for that subject if it
;;;is in the catalog, oterwise fails

(define (in-catalog subject fail succeed)
  (let ((entry (find subject catalog)))
    (if entry
	(succeed entry fail)
	(fail))))


;;;Matches a list of subjects of the form s1 s2 .... and sk

(define (subjects words fail succeed)
  (try-rules
   words
   (list
    (make-rule
     `((? subject ,in-catalog))
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
     `((? subject ,in-catalog))
     (lambda (dict)
       (list (value 'subject dict))) )
    (make-rule
     `((? subject1 ,in-catalog) (?? more-subjects ,subject-seq))
     (lambda (dict)
       (cons (value 'subject1 dict) (value 'more-subjects dict))) )
     )
   fail
   succeed))
	     

;;; Now we can define some actual reponses based upon catalog knowledge

(define subject-knowledge 
  (list
   (make-rule
    `(what is (? s ,in-catalog) about)
    (lambda (dict)
      (let ((entry (value 's dict)))
	(write-line
	 (append (list (entry-subject entry))
		 '(is about)
		 (entry-summary entry))))) )

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
    `(how many units is (? s ,in-catalog))
    (lambda (dict)
      (let ((entry (value 's dict)))
	(write-line
	 (append (list (entry-subject entry))
		 '(is a)
		 (list (entry-units entry))
		 '(unit subject))))) )

   (make-rule
    `(how many units are (?? s ,subjects))
    (lambda (dict)
      (for-each (lambda (entry)
		  (write-line
		   (append (list (entry-subject entry))
			   '(is a)
			   (list (entry-units entry))
			   '(unit subject))))
		(value 's dict))) )

   (make-rule
    `(what are the prerequisites for (?? s ,subjects))
    (lambda (dict)
      (for-each (lambda (entry)
		  (write-line
		   (append '(the prerequisites for)
			   (list (entry-subject entry))
			   '(are)
			   (entry-prerequisites entry))))
		(value 's dict))) )

   (make-rule
    `(can I take (? s ,in-catalog))
    (lambda (dict)
      (let ((entry (value 's dict)))
	(write-line
	 (append '(the prerequisites for)
		 (list (entry-subject entry))
		 '(are)
		 (entry-prerequisites entry))))) )

   ;;************** Exercise 9 ************************
   ;; rule for all-prerequisites
   (make-rule
    `(can i take (? s1 ,in-catalog) if i have not taken (? s2 ,in-catalog))
    (lambda (dict)
      (let ((entry1 (value 's1 dict))
	    (entry2 (value 's2 dict)))
	(write-line
	 (append '(the prerequisites for)
		 (list (entry-subject entry1))
		 (if (member (entry-subject entry2)
			     (all-prerequisites (entry-subject entry1)))
		     '(include)
		     '(does not include))
		 (list (entry-subject entry2)))))))

   ;;***************** Exercise 13 **********************
   (make-rule
    `(i want to take (?? s ,subjects))
    (lambda (dict)
      (write-line
       (check-subject-list
	(map entry-subject (value 's dict))))))

   ;;***************** Ex 14 - 1 *********************
   ;;rule for seeing if a subject is a GUR
   (make-rule
    `(is (? s ,in-catalog) a gur)
    (lambda (dict)
      (let ((entry (value 's dict)))
	(write-line
	 (if (member 'gur (entry-satisfies entry))
	     (append '(yes,) (list (entry-subject entry)) '(is a gur))
	     (append '(no,) (list (entry-subject entry)) '(is not a gur)))))))

   ;;***************** Ex 14 - 2 ********************
   ;;rule for seeing if multiple subjects are GURs
   (make-rule
    `(are (?? s ,subjects) gurs)
    (lambda (dict)
      (for-each (lambda (entry)
		  (write-line
		   (append (list (entry-subject entry))
			   (if (member 'gur (entry-satisfies entry))
			       '(is a gur)
			       '(is not a gur)))))
		(value 's dict))))

   (make-rule
    `(is there a subject about (? s ,in-catalog))
    (lambda (dict)
      
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










