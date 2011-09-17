; Probem Set 2 Solutions
;   by Michael Allen

; ==========
; Exercise 1
; ==========

; SUM is a higher order procedure for summing series of numbers
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (+ (term a) result))))
  (iter a 0))

; ==========
; Exercise 2
; ==========

; PART a

; PRODUCT is a higher order procedure for multiplying series of numbers
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (* (term a) result))))
  (iter a 1))

; FACTORIAL computes 1*2*...*n
(define (factorial n)
  (product (lambda (x) x) 1 inc n))

; COMPUTE-PI computes an estimate of pi with k terms
(define (compute-pi k)
  (* 4 (product (lambda (x) (/ (* x (+ x 2)) (square (+ x 1))))
		2
		(lambda (x) (+ x 2))
		k)))

(compute-pi 10000)
;Value: 3.141749705738071

; PART b

; PRODUCT in its recursive form (instead of iterative as above)
(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))

; ==========
; Exercise 3
; ==========

; PART a

; ACCUMULATE is a higher order procedure
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
		(accumulate combiner null-value term (next a) next b))))

; SUM can be written with accumulate
(define (sum term a next b)
  (accumulate + 0 term a next b))

; PRODUCT can as well
(define (product term a next b)
  (accumulate * 1 term a next b))

; PART b

; ACCUMULATE in its iterative form (instead of recursive as above)
(define (accumulate comb init term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (comb (term a) result))))
  (iter a init))

; ==========
; Exercise 4
; ==========

; FILTERED-ACCUMULATE works like ACCUMULATE, but can ignore unwanted terms
(define (filtered-accumulate filter comb init term a next b)
  (cond ((> a b) init)
	((filter a) (comb (term a) 
			  (filtered-accumulate filter comb init term (next a) next b)))
	(else (filtered-accumulate filter comb init term (next a) next b))))

; PART a

; SUM-PRIME-SQUARES can be written with FILTERED-ACCUMULATE
(define (sum-prime-squares a b)
  (filtered-accumulate prime? + 0 square a inc b))

; PART b

; RELATIVELY-PRIME can as well
(define (relatively-prime n)
  (filtered-accumulate (lambda (x) (= (gcd x n) 1)) * 1 (lambda (x) x) 1 inc n))

; ==========
; Exercise 5
; ==========

; REPEATED will apply a function multiple times
(define (repeated p n)
  (cond ((= n 0) (lambda (x) x))
	((= n 1) p)
	(else (lambda (x) (p ((repeated p (- n 1)) x))))))

((repeated square 2) 5)
;Value: 625

((repeated square 5) 2)
;Value: 4294967296

; ==========
; Exercise 6
; ==========

; CONT-FRAC-R recursively computes continued fractions
(define (cont-frac-r n d k)
  (define (cfr-helper i)
    (if (> i k)
	0
	(/ (n i) (+ (d i) (cfr-helper (+ i 1))))))
  (cfr-helper 1))

; CONT-FRAC-I does it iteratively
(define (cont-frac-i n d k)
  (define (cfi-helper i ans)
    (if (= i 0)
	ans
	(cfi-helper (- i 1) (/ (n i) (+ (d i) ans)))))
  (cfi-helper k 0))

(cont-frac-r (lambda (i) 1) (lambda (i) 1) 10)
(cont-frac-i (lambda (i) 1) (lambda (i) 1) 10)
;Value: .6179775280898876

; For k=10, we get a result which is accurrate to 4 decimal places

; ==========
; Exercise 7
; ==========

; ESTIMATE-PI is another procedure for estimating pi
(define (estimate-pi k)
  (/ 4 (+ (brouncker k) 1)))

(define (brouncker k)
  (cont-frac (lambda (i) (square (- (* 2 i) 1)))
	     (lambda (i) 2)
	     k))

; Estimating pi with a recursively computed Continued Fraction
(define cont-frac cont-frac-r)
(estimate-pi 20000)
;Aborting!: maximum recursion depth exceeded

; And with an iterative one
(define cont-frac cont-frac-i)
(estimate-pi 20000)
;Value: 3.1416426510898874

(estimate-pi 2000)
;Value: 3.1420924036835274

; For k=2000, we get a result which is accurrate to 3 decimal places

; ==========
; Exercise 8
; ==========

; ATAN-CF approximates arc-tangents with continued fractions
(define (atan-cf k x)
  (/ x (+ 1 (cont-frac (lambda (i) (square (* i x)))
		       (lambda (i) (+ (* 2 i) 1))
		       k))))

; ==========
; Exercise 9
; ==========

; Some Results
(atan 1)
;Value: .7853981633974483

(atan-cf 1 1)
;Value: .75

(atan 3)
;Value: 1.2490457723982544

(atan 9 3)
;Value: 1.2490457723982544

(atan 10)
;Value: 1.4711276743037347

(atan 100 10)
;Value: 1.4711276743037347

(atan 30)
;Value: 1.5374753309166493

(atan 900 30)
;Value: 1.5374753309166493

(atan 100)
;Value: 1.5607966601082315

(atan 10000 100)
;Value: 1.5607966601082315

; Larger values of x require more terms for accurracy.

; ===========
; Exercise 10
; ===========

; NESTED-ACC computes a series of nested functions
(define (nested-acc op r term k)
  (define (na-helper i)
    (if (> i k)
	r
	((op i) (term i) (na-helper (+ i 1)))))
  (na-helper 1))

; ROOTS uses NESTED-ACC to compute a function
(define (roots k x)
  (nested-acc (lambda (i) (if (even? i) + (lambda (x y) (sqrt y))))
	      0
	      (lambda (i) x)
	      k))

; ===========
; Exercise 11
; ===========

; A Result
(roots 20 1)
;Value: 1.6180165422314876

; For k=20, we get a result which is accurrate to 4 decimal places

; ===========
; Exercise 12
; ===========

; BUILD computes a single term of a continued fraction
(define (build n d b)
  (/ n (+ d b)))

; REPEATED-BUILD computes many
(define (repeated-build k n d b)
  ((repeated (lambda (x) (build n d x)) k) b))

; ===========
; Exercise 13
; ===========

; A Result
(repeated-build 100 1 1 0)
;Value: .6180339887498948

; ===========
; Exercise 14
; ===========

; R computes rational functions
(define (r k)
  (lambda (x)
    (repeated-build k 1 1 x)))

((r 2) 0)
;Value: .5