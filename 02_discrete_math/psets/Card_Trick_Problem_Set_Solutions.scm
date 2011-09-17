;;; Discrete Math PS-Card

;;; --------------------------------------------
;;; factorial, combination, permutation procs

(define (fact n)
  (define (helper product count)
    (if (> count n)
	product
	(helper (* count product)
		(+ count 1))))
  (helper 1 1))

(define (comb n r)
  (define (helper n-product n d-product count)
    (cond ((> count r)
	   (/ n-product d-product))
	  (else
	   (helper (* n-product n)
		   (- n 1)
		   (* d-product count)
		   (+ count 1)))))
  (helper 1 n 1 1))

(define (perm n r)
  (define (helper product n count)
    (cond ((> count r)
	   product)
	  (else
	   (helper (* product n)
		   (- n 1)
		   (+ count 1)))))
  (helper 1 n 1))

;;; --------------------------------------------
;;; getting permutations given a s-element list
;;; procs needed are filter, accumulate, remove
;;; flatmap and permutations

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
	((predicate (car sequence))
	 (cons (car sequence)
	       (filter predicate (cdr sequence))))
	(else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
	  sequence))

(define (permutations s)
  (if (null? s)
      (list nil)
      (flatmap (lambda (x)
		 (map (lambda (p) (cons x p))
		      (permutations (remove x s))))
	       s)))

;;; ----------------------------------------------
;;; the card proc - starting point

(define n 0)
(define m 0)
(define hand-lst nil)
(define row-lst nil)
(define code-lst nil)
(define count 0)
(define a 0)
(define b 0)
(define c 0)
(define d 0)
(define e 0)

(define (card choose deck)
  (set! n deck)
  (set! m choose)
  (set! hand-lst nil)
  (set! row-lst nil)
  (set! code-lst nil)
  (set! count 0)
  (cond ((= choose 3) (do3))
	((= choose 4) (do4))
	((= choose 5) (do5))
	(else (newline)
	      (display '(values not acceptable)))))

;;; ------------------------------------------------
;;; choose = 3 or 4 or 5

(define (do3)
  (set! hand-lst (list 1 2 2))
  (deal))

(define (do4)
  (set! hand-lst (list 1 2 3 3))
  (deal))

(define (do5)
  (set! hand-lst (list 1 2 3 4 4))
  (deal))

;;; -------------------------------------------------
;;; getting combinations of m-card hands

(define (deal)
  (cond ((= m 3) (deal3))
	((= m 4) (deal4))
	((= m 5) (deal5))
	(else
	 (newline)
	 (display (list 'broke 'at 'deal)))))

(define (deal3)
  (let ((a (car hand-lst))
	(b (cadr hand-lst))
	(c (caddr hand-lst)))
    (cond ((= a (- n 2))
	   (newline)
	   (display (list 'done 'count count 'calculated (comb n m)))
	   (newline)
	   (show code-lst))
	  ((= b (- n 1))
	   (set! count (+ 1 count))
	   (set! hand-lst (list (+ a 1) (+ a 2) (+ a 3)))
	   (row))
	  ((= c n)
	   (set! count (+ 1 count))
	   (set! hand-lst (list a (+ b 1) (+ b 2)))
	   (row))
	  (else
	   (set! count (+ 1 count))
	   (set! hand-lst (list a b (+ c 1)))
	   (row)))))

(define (deal4)
  (let ((a (car hand-lst))
	(b (cadr hand-lst))
	(c (caddr hand-lst))
	(d (cadddr hand-lst)))
    (cond ((= a (- n 3))
	   (newline)
	   (display (list 'done 'count count 'calculated (comb n m)))
	   (newline)
	   (show code-lst))
	  ((= b (- n 2))
	   (set! count (+ 1 count))
	   (set! hand-lst (list (+ a 1) (+ a 2) (+ a 3) (+ a 4)))
	   (row))
	  ((= c (- n 1))
	   (set! count (+ 1 count))
	   (set! hand-lst (list a (+ b 1) (+ b 2) (+ b 3)))
	   (row))
	  ((= d n)
	   (set! count (+ 1 count))
	   (set! hand-lst (list a b (+ c 1) (+ c 2)))
	   (row))
	  (else
	   (set! count (+ 1 count))
	   (set! hand-lst (list a b c (+ d 1)))
	   (row)))))

(define (deal5)
  (let ((a (car hand-lst))
	(b (cadr hand-lst))
	(c (caddr hand-lst))
	(d (cadddr hand-lst))
	(e (car (cddddr hand-lst))))
    (cond ((= a (- n 4))
	   (newline)
	   (display (list 'done 'count count 'calculated (comb n m)))
	   (newline)
	   (show code-lst))
	  ((= b (- n 3))
	   (set! count (+ 1 count))
	   (set! hand-lst (list (+ a 1) (+ a 2) (+ a 3) (+ a 4) (+ a 5)))
	   (row))
	  ((= c (- n 2))
	   (set! count (+ 1 count))
	   (set! hand-lst (list a (+ b 1) (+ b 2) (+ b 3) (+ b 4)))
	   (row))
	  ((= d (- n 1))
	   (set! count (+ 1 count))
	   (set! hand-lst (list a b (+ c 1) (+ c 2) (+ c 3)))
	   (row))
	  ((= e n)
	   (set! count (+ 1 count))
	   (set! hand-lst (list a b c (+ d 1) (+ d 2)))
	   (row))
	  (else
	   (set! count (+ 1 count))
	   (set! hand-lst (list a b c d (+ e 1)))
	   (row)))))

;;; ---------------------------------------------
;;; make a row of all possible permutations given a hand

(define (row)
  (set! row-lst (permutations (reverse hand-lst)))
  (code))

;;; ----------------------------------------------
;;; test for uniqueness of element and include in
;;; code list

(define (code)
  (define (helper r-lst c-lst)
    (cond ((null? r-lst)
	   (newline)
	   (display (list 'broke 'at 'row count 'rows (comb n m)))
	   (newline)
	   (show code-lst))
	  ((null? c-lst)
	   (newline)
	   (display (list count hand-lst (cdar r-lst) '-> (caar r-lst)))
	   (set! code-lst (cons (car r-lst) code-lst))
	   (deal))
	  ((equal? (cdar r-lst) (cdar c-lst))
	   (helper (cdr r-lst) code-lst))
	  (else
	   (helper r-lst (cdr c-lst)))))
  (helper row-lst code-lst))

;;; --------------------------------------------------
;;; display file

(define (show lst)
  (define (helper lst count)
    (cond ((null? lst))
	  ((= count 5)
	   (newline)
	   (helper lst 0))
	  (else
	   (display (list (cdar lst) '-> (caar lst)))
	   (helper (cdr lst) (+ count 1)))))
  (helper lst 0))

;;; --------------------------------------------------
