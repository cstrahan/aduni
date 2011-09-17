;;;SICP PS-5
;;;Shyam Visweswaran

;;;=====================================================
;;; Exercise 6
;;;=====================================================

;;extension of store of beginnings and general-advice

;** (what do i do if i run out of money)
;(make sure you know where the nearest atm is)

;** (i am feeling homesick)
;(you will do well here)

;** (what do i do for fun)
;(make sure to get time to explore the boston area)

;**(i am tired)
;(what do you mean when you say you are tired)

;;;=====================================================
;;; Exercise 7
;;;=====================================================

;;conventional-wisdom rules

;** (is eecs101 hard)
;(eecs101 is too much work for freshmen -- wait until next year)

;** (can i take phys101 this semester)
;(students really enjoy phys101)

;** (is there a seminar for freshmen)
;(i hear that snorkeling in boston harbor is a really exciting seminar)

;** (i plan to take math204 next semester)
;(too bad -- math204 is not offered next semester)

;** (i want to double major in history and math)
;(history is fascinating and you can make a living doing it if math does not work out)

;** (i am thinking of doing a double major)
;(doing a double major is a lot of work)

;;subject-knowledge rules

;** (what is hist102 about)
;(hist102 is about what will be)

;** (what are math319 and chem224 about)
;(math319 is about hard equations)
;(chem224 is about fun with benzene)

;** (how many credits is math319)
;(make sure to get time to explore the boston area)

;;typo in problem set - units not credits!

;** (how many credits is geo104)
;(how about a freshman seminar)

;** (how many units is chem291)
;(chem291 is a 4 unit subject)

;**(how many units are math319 and geo104)
;(math319 is a 4 unit subject)
;(geo104 is a 4 unit subject)

;** (what are the prerequisites for math319)
;(the prerequisites for math319 are math204)

;** (can i take eecs202)
;(the prerequisites for eecs202 are phys102 math102)

;;;========================================================
;;;Execise 8
;;;========================================================

;;Get all the prerequisites for a subject recursively
;;The 2nd line gets the prerequisites of a given subject.
;;if tests for prerequisites that do not exist in the catalog
;;and returns a null list to prevent triggering the debugger.

(define (all-prerequisites subject)
  (let ((pre-req-list
	 (if (find subject catalog)
	     (entry-prerequisites (find subject catalog))
	     '())))
    (list-union pre-req-list
		(reduce list-union
			'()
			(map all-prerequisites pre-req-list)))))

;;testing above code

;(all-prerequisites 'geo104)
;Value: (math203 phys102 math102 phys101 math101)

;(all-prerequisites 'hist102)
;Value: #f

;(all-prerequisites 'phys304)
;Value: (phys103 math203 phys102 phys101 math102 math101)

;(all-prerequisites 'chem111)
;Value: (a-strong-stomach)


;;;========================================================
;;; Exercise 9
;;;========================================================






  ;; rule for all-prerequisites
   (make-rule
    `(can i take (? s1 ,in-catalog) if i have not taken (? s2 ,in-catalog))
    (lambda (dict)
      (let ((entry1 (value 's1 dict))
	    (entry2 (value 's2 dict)))
	(write-line
	 (if (member (entry-subject entry2)
		     (all-prerequisites (entry-subject entry1)))
	     '(no, you cannot)
	     '(yes, you can))))))


;;test of above code

;** (can i take geo104 if i have not taken math203)
;(no, you cannot)

;** (can i take math203 if i have not taken geo104)
;(yes, you can)


;;;==========================================================
;;; Exercise 10
;;;==========================================================

;;check-circular-prerequisites

(define (check-circular-prerequisites subj-list)
  (if (list-intersection subj-list
			 (reduce list-union
				 '()
				 (map all-prerequisites subj-list)))
      #t   ;there is a problem
      #f)) ;no problem

;;test of above code

;(check-circular-prerequisites '(math101 geo104))
;Value: #t

;(check-circular-prerequisites '(math101 phys101))
;Value: #f


;;;======================================================================
;;; Exercise 11
;;;======================================================================

;;total-credits proc
;;map the proc of entry-units on to the input list of subjects

(define (total-credits subj-list)
  (reduce +
	  0
	  (map (lambda (x) (entry-units (find x catalog)))
	       subj-list)))

;;test of above code

(total-credits '(math203 phys102 math102))
;Value: 12

(total-credits '(eecs101 math101 phys101))
;Value: 13


;;;==================================================================
;;; Exercise 13
;;;==================================================================

;;check-subject-list proc

(define (check-subject-list subj-list)
   (cond ((> (total-credits subj-list) 18)
	 (write-line
	  (append '(you cannot take more than 18 credits as a freshman))))
	((check-circular-prerequisites subj-list)
	 (write-line
	  (append '(you cannot take subjects and prerequisites concurrently))))
	(else
	 (write-line
	  (append '(you need the following prerequisites - )
		  (reduce list-union
			  '()
			  (map (lambda (x) (entry-prerequisites (find x catalog)))
			       subj-list)))))))


;;test of above code

;(check-subject-list '(bio212 phys103))
;(you need the following prerequisites - phys102 math102)

;(check-subject-list '(phys304 math203 hist102 geo101))
;(you cannot take subjects and prerequisites concurrently)

;(check-subject-list '(phys101 math101 chem291 hist102 bio101 geo101))
;(you cannot take more than 18 credits as a freshman)


;;;===================================================================
;;; Exercise 13
;;;===================================================================

;;new rule for subject-knowledge using check-subject-list
;;uses map with the operator entry-subject to output a list
;;of subjects to check-subject-list

   (make-rule
    `(i want to take (?? s ,subjects))
    (lambda (dict)
      (write-line
       (check-subject-list
	(map entry-subject (value 's dict))))))

;;test of above code

;** (i want to take bio212 and phys103)
;(you need the following prerequisites - phys102 math102)

;** (i want to take phys101 math101 chem291 hist102 bio101 and geo101)
;(you cannot take more than 18 credits as a freshman)

;** (i want to take phys304 math203 hist102 and geo101)
;(you cannot take subjects and prerequisites concurrently)


;;;=====================================================================
;;; Exercise 14
;;;=====================================================================

 ;;rule for seeing if a subject is a GUR
   (make-rule
    `(is (? s ,in-catalog) a gur)
    (lambda (dict)
      (let ((entry (value 's dict)))
	(write-line
	 (if (member 'gur (entry-satisfies entry))
	     (append '(yes,) (list (entry-subject entry)) '(is a gur))
	     (append '(no,) (list (entry-subject entry)) '(is not a gur)))))))

	 
;;test of above rule	 

** (is hist102 a gur)
(yes, hist102 is a gur)

** (is eecs101 a gur)
(no, eecs101 is not a gur)

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


;;test of above rule

;** (are phys101 math204 and bio101 gurs)
;(phys101 is a gur)
;(math204 is not a gur)
;(bio101 is a gur)







