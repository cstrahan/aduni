;;; SICP PS-2
;;; Shyam Visweswaran

;;;============================================================
;;;Exercise 1 Sum procedure - recursive

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a) next b))))

(define (inc n) (+ n 1))
(define (sum-cubes a b)
  (sum cube a inc b))
(define (cube x)
  (* x x x))

(sum-cubes 1 10)
;Value: 3025

(define (identity x) x)
(define (sum-integers a b)
  (sum identity a inc b))

(sum-integers 1 10)
;Value: 55

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(* 8 (pi-sum 1 1000))
;Value: 3.139592655589783

;;;=============================================================
;;;Exercise 1 Sum procedure - iterative

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (+ (term a) result ))))
  (iter a 0))

;;Testing sum procedure

(sum-cubes 1 10)
;Value: 3025
(sum-integers 1 10)
;Value: 55
(* 8 (pi-sum 1 1000))
;Value: 3.139592655589782

;;;============================================================
;;;Exercise 2 - Product procedure

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
	 (product term (next a) next b))))

;; Factorial procedure
(define (factorial f)
  (product identity 1 inc f))

(factorial 4)
;Value: 24
(factorial 1)
;Value: 1
(factorial 12)
;Value: 479001600

;; Wallis procedure for calculating pi
(define (wallis-pi max)
  (define (inc-2 n) (+ n 2))
  (define (fraction n)
    (/ (* (- n 1) (+ n 1)) (* n n)))
  (product fraction 3 inc-2 max))

(* 4 (wallis-pi 100))
;Value: 3.1573396892175642

;;;=============================================================
;;;Note: skipped Exercise 3 and 4

;;;=============================================================
;;;Exercise 5

;; The first expression computes (square (square 5))
;; The second expression computes (sq (sq (sq (sq (sq 2)))))

(define (repeated p n)
  (cond ((= n 0) (lambda (x) x))
	((= n 1) p)
	(else (lambda (x) (p ((repeated p (- n 1)) x))))))

((repeated square 2) 5)
;Value 625

((repeated square 5) 2)
;Value: 4294967296

;;;==============================================================
;;;Exercise 6
;; recursive procedure

(define (cont-frac-r n d k)
  (define (cont-frac-r-counter i)    ;setting up a counter
  (if (> i k)
      0
      (/ (n i) (+ (d i) (cont-frac-r-counter (+ i 1))))))
  (cont-frac-r-counter 1))

(cont-frac-r (lambda (i) 1)
	     (lambda (i) 1)
	     10)

;Value: .6179775280898876 (for k = 10)
;Value: .6180555555555556 (for k = 11) - accurate to 3 decimal places
;Value: .6180257510729613 (for k = 12) - accurate to 4 decimal places

;; iterative procedure

(define (cont-frac-i n d k)
  (define (cont-frac-i-iter i total)
    (if (= i 0)
	total
	(cont-frac-i-iter (- i 1) (/ (n i) (+ (d i) total)))))
  (cont-frac-i-iter k 0))

;Value: .6179775280898876 (for k = 10)
;Value: .6180555555555556 (for k = 11) - accurate to 3 decimal places
;Value: .6180257510729613 (for k = 12) - accurate to 4 decimal places

;;;===============================================================
;;;Exercise 7

(define (estimate-pi k)
  (/ 4 (+ (brouncker k) 1)))

(define (square x) (* x x))

(define (brouncker k)
  (cont-frac-r (lambda (i) (square (- (* 2 i) 1)))
	       (lambda (i) 2)
	       k))

(estimate-pi 16000)
;Value: 3.1416551496837264 - correct to 3 decimal places
;For k=20000, procedure aborts becasue maximum recursion depth is exceeded
;Using the iterative procedure k=20000 returns the following value
;Value: 3.1416426510898874
;No stack is used for an iterative procedure since it runs in constant space

;;;==============================================================
;;;Exercise 8 - arctan procedure

(define (atan-cf k x)
  (/ x (+ (lambert k x) 1)))
(define (square x) (* x x))

;lambert procedure computes the rest of the fraction except
;the first numerator and denominator

(define (lambert k x)
  (cont-frac-r (lambda (i) (square (* i x)))
	       (lambda (i) (+ (* 2 i) 1))
	       k))

;;;=============================================================
;;;Exercise 9
;;;Procedure to check for the difference in Scheme arctan and our arctan

(define (atan-diff k x)
  (- (atan x) (atan-cf k x)))

(atan-diff 10 1)
;Value: -4.860135294215695e-9
(atan-diff 50 1)
;Value: 0.

(atan-diff 10 3)
;Value: -1.6733303560605428e-3
(atan-diff 100 3)
;Value: 0.

(atan-diff 100 10)
;Value: -4.960133193065985e-9
(atan-diff 10000 10)
;Value: 2.220446049250313e-16

(atan-diff 100 30)
;Value: -3.6260447257741024e-3
(atan-diff 10000 30)
;Value: -2.220446049250313e-16

(atan-diff 10 100)
;Value: -12.135902224693142
(atan-diff 10000 100)
;Value: 2.220446049250313e-16

;;;========================================================
;;;Exercise 10

(define (nested-acc op r term k)
  (define (nested-acc-helper i)
    (if (> i k)
	r
	((op i) (term i) (nested-acc-helper (+ i 1)))))
  (nested-acc-helper 1))

;procedure for the nested square root function
;using nested-acc

(define (nested-sqrt x k)
  (define (helper-op i)
    (lambda (a b) (sqrt (+ a b))))
  (nested-acc helper-op 0 (lambda (i) x) k))

;;;=======================================================
;;;Exercise 11 - testing nested-acc procedure with f(1)

(nested-sqrt 1 10)
;Value: 1.6180165422314876 - accurate to 4 decimal places

;;;=======================================================
;;;Exercise 12

(define (build n d b)
  (/ n (+ d b)))

(define (repeated-build k n d b)
  ((repeated (lambda (x) (/ n (+ d x)) k) b))

;;;=======================================================
;;;Exercise 13

(repeated-build 11 1 1 0)
;Value: .6180555555555556

;;;=======================================================
;;;Exercise 14

(define (r k)
  (lambda (x) (repeated-build k 1 1 x)))

((r 2 ) 0)
;Value: .5