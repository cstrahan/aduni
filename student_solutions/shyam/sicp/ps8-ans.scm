;;; SICP PS-8
;;; Shyam Visweswaran

;;;========================================================
;;; Ex 1 - 3 on paper
;;;========================================================

;; mul-streams evaluated here for future exercises

(define (mul-streams a b)
  (cons-stream
   (* (stream-car a) (stream-car b))
   (mul-streams (stream-cdr a)
		(stream-cdr b))))

;;;========================================================
;;; Exercise 4
;;;========================================================

;; redefine show-series so that the resulting series is
;; displayed horizontally rather than vertically
(define (show-series s nterms)
  (if (= nterms 0)
      'done
      (begin (display "; ")
	     (display (stream-car s))
	     (show-series (stream-cdr s) (- nterms 1)))))

;; infinite stream of ones
(define ones (cons-stream 1 ones))
(show-series ones 10)
; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1

;; infinite stream of integers starting at one
(define integers
  (cons-stream 1 (add-streams ones integers)))

;; infinite stream of non-negative integers starting
;; at zero
(define non-neg-integers
  (cons-stream 0
	       (cons-stream 1
			    (add-streams
			     ones
			     (stream-cdr non-neg-integers)))))

(show-series non-neg-integers 10)
; 0; 1; 2; 3; 4; 5; 6; 7; 8; 9

;; stream of alternating 1 and -1
(define alt-ones (cons-stream 1 (scale-stream -1 alt-ones)))
(show-series alt-ones 10)
; 1; -1; 1; -1; 1; -1; 1; -1; 1; -1

;; stream of zeros using alt-ones
(define zeros (add-streams alt-ones (stream-cdr alt-ones)))
(show-series zeros 10)
; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0

;; coefficient stream of ones
(define series1 (proc->series (lambda (x) 1)))
(show-series series1 10)
; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1
(series-coeff series1 10)
;Value: 1

;; coefficient stream of integers
(define series2 (proc->series (lambda (x) (+ x 1))))
(show-series series2 10)
; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10


;;;=========================================================
;;; Exercise 5
;;;=========================================================

;; multiplication of infinite polynomials

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
	       (add-series (scale-series (stream-car s2) (stream-cdr s1))
			   (mul-series s1 (stream-cdr s2)))))

;; test with sereis1 (1 1 1 1 ...)
(show-series (mul-series series1 series1) 10)
; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10

(define s11111 (coeffs->series '(1 1 1 1 1)))
(define s12321 (coeffs->series '(1 2 3 2 1)))

;; should get (136898631) on multiplying (11111) with (12321)
(show-series (mul-series s11111 s12321) 15)
; 1; 3; 6; 8; 9; 9; 8; 6; 3; 1; 0; 0; 0; 0; 0

;;;========================================================
;;; Exercise 6
;;;========================================================

;; inverting power series

(define (invert-unit-series s)
  (cons-stream 1
	       (negate-series (mul-series (stream-cdr s)
					  (invert-unit-series s)))))

;; now for the test

(show-series (invert-unit-series series1) 10)
; 1; -1; 0; 0; 0; 0; 0; 0; 0; 0
;Value: done

(show-series (invert-unit-series series2) 10)
; 1; -2; 1; 0; 0; 0; 0; 0; 0; 0

;; The proc does not go into infinite loop because the evaluation
;; of each term of invert-unit-series is delayed and allows the
;; the recursive call on the last line to function.

;;;=======================================================
;;; Exercise 7
;;;=======================================================

;; div-series

(define (div-series s1 s2)
  (cond ((= (stream-car s2) 0)
	 (display "Error - denominator series has a zero constant term") s2)
	(else (mul-series s1 (invert-unit-series s2)))))
	       
;; test with zero denominator
(show-series (div-series series1 zeros) 10)
Error - denominator series has a zero constant term; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0

;; series1 / series2 = (1-x)
(show-series (div-series series1 series2) 10)
; 1; -1; 0; 0; 0; 0; 0; 0; 0; 0

;; sereis2 / series1 = 1/(1-x)
(show-series (div-series series2 series1) 10)
; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1

;; divide (1-x)^3 by (1-x)
(define s4 (coeffs->series (list 1 -3 3 -1)))
(define s5 (coeffs->series (list 1 -1)))
(show-series (div-series s4 s5) 10)
; 1; -2; 1; 0; 0; 0; 0; 0; 0; 0


;;;======================================================================
;;; Exercise 8
;;;=====================================================================

;; series integration

(define (integrate-series-tail s)
  (define (helper a b)
    (cons-stream
     (/ (stream-car a) (stream-car b))
     (helper (stream-cdr a)
	     (stream-cdr b))))
  (helper s integers))
   
(show-series (integrate-series-tail series1) 10)
; 1; 1/2; 1/3; 1/4; 1/5; 1/6; 1/7; 1/8; 1/9; 1/10

(show-series (integrate-series-tail series2) 10)
; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1


;;;================================================================
;;; Exercise 9
;;;================================================================

;; series for e^x
(define exp-series
  (cons-stream 1 (integrate-series-tail exp-series)))

;; test e^x		  
(show-series exp-series 10)
; 1; 1; 1/2; 1/6; 1/24; 1/120; 1/720; 1/5040; 1/40320; 1/362880

;; sine and cosine series
(define sine-series
  (cons-stream 0 (negate-series
		  (integrate-series-tail cosine-series))))

(define cosine-series
  (cons-stream 1 (integrate-series-tail sine-series)))

;; test sine and cosine series
(show-series sine-series 10)
; 0; -1; 0; 1/6; 0; -1/120; 0; 1/5040; 0; -1/362880

(show-series cosine-series 10)
; 1; 0; -1/2; 0; 1/24; 0; -1/720; 0; 1/40320; 0

;;;============================================================
;;; Exercise 10
;;;============================================================

;; first define a proc for partial sums of a stream

(define (partial-sums s)
  (cons-stream (stream-car s)
	       (add-streams (partial-sums s)
			    (stream-cdr s))))

;; Now define the proc which generates each term in the series
;; The approx function takes 2 streams - a coefficient series
;; and a exponent series (which here is the non-negative-integer
;; series)

(define (approx var expt-s coeff-s)
  (mul-streams coeff-s
	       (stream-map (lambda (x) (expt var x))
			   expt-s)))

;; Now generate the partial-sums

(define (approximate x s)
  (partial-sums (approx x non-neg-integers s)))

;; ok, time to test
;; using x = 0.5 and coeff-series = (1 1 1 1 1 ...)
;; this should tend to 2
(show-series (approximate .5 series1) 10)
; 1.; 1.5; 1.75; 1.875; 1.9375; 1.96875; 1.984375; 1.9921875; 1.99609375; 1.998046875

;; using x = 0.5 and coeff = (1 2 3 4 5 ...)
(show-series (approximate .5 series2) 10)
; 1.; 2.; 2.75; 3.25; 3.5625; 3.75; 3.859375; 3.921875; 3.95703125; 3.9765625

;; estimating e
(show-series (approximate 1.0 exp-series) 10)
;1.
;2.
;2.5
;2.6666666666666665
;2.708333333333333
;2.7166666666666663
;2.7180555555555554
;2.7182539682539684
;2.71827876984127
;2.7182815255731922

;;;=========================================================










