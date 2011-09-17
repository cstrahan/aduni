; Problem Set 8 Solutions
;   by Michael Allen

; ============
;  Exercise 1
; ============

; S1 is the same as (cons 1 (delay 2)).
(define s1 (cons-stream 1 2))

; S2 is the stream of the powers of two.
(define s2 (cons-stream 1 (add-streams s2 s2)))

; ============
;  Exercise 2
; ============

; PARTIAL-SUMS creates a series whose elements are s0, s0+s1, s0+s1+s2, ...
(define (partial-sums s)
  (cons-stream (stream-car s)
	       (add-streams (stream-cdr s) (partial-sums s))))

; ============
;  Exercise 3
; ============

; X is the stream of the powers of two.
(define x (cons-stream 1 (scale-stream 2 x)))

; MUL-STREAMS does a term-wise multiplication of two streams.
(define (mul-streams a b)
  (cons-stream
   (* (stream-car a) (stream-car b))
   (mul-streams (stream-cdr a)
		(stream-cdr b))))

; Y is a stream of factorials.
(define y (cons-stream 1 (mul-streams y integers)))

; ============
;  Exercise 4
; ============

; ONES: 1, 1, 1, 1, ...
(define ones (cons-stream 1 ones))

; NON-NEG-INTEGERS: 0, 1, 2, 3, ...
(define non-neg-integers (cons-stream 0 (add-streams non-neg-integers ones)))

; ALT-ONES: 1, -1, 1, -1, ...
(define alt-ones (cons-stream 1 (cons-stream -1 alt-ones)))

; ZEROS: 0, 0, 0, 0, ...
(define zeros (add-streams alt-ones (stream-cdr alt-ones)))

; S1: 1 + x + x^2 + ...
(define s1 ones)

; S2: 1 + 2x + 3x^2 + ...
(define s2 (stream-cdr non-neg-integers))

; ============
;  Exercise 5
; ============

; MUL-SERIES multiplies two series together
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
	       (add-series (scale-series (stream-car s1) (stream-cdr s2))
			   (mul-series (stream-cdr s1) s2))))

; Testing...

(show-series (mul-series s1 s1) 10)
; 1 2 3 4 5 6 7 8 9 10

(show-series s2 10)
; 1 2 3 4 5 6 7 8 9 10

(series-coeff (mul-series s2 s2) 10)
; 286

; ============
;  Exercise 6
; ============

; INVERT-UNIT-SERIES computes 1/s for a series whose constant term is 1
(define (invert-unit-series s)
  (cons-stream 1
	       (negate-series (mul-series (stream-cdr s)
					  (invert-unit-series s)))))

; Testing...

(show-series (invert-unit-series s1) 10)
; 1 -1 0 0 0 0 0 0 0 0 

; This procedure does not go into an infinite loop because CONS-STREAM
; delays the evaluation of its second argument.

; ============
;  Exercise 7
; ============

; DIV-SERIES computes s1/s2
(define (div-series s1 s2)
  (if (= 0 (stream-car s2))
      (error "Cannot divide by a series with 0 constant")
      (mul-series s1 
		  (scale-series (/ (stream-car s2))
				(invert-unit-series (scale-series (/ (stream-car s2)) s2))))))

; Testing...

(show-series (div-series s2 s1) 10)
; 1 1 1 1 1 1 1 1 1 1

(show-series (div-series s2 (scale-series 2 s2)) 10)
; 2 0 0 0 0 0 0 0 0 0 

(show-series (div-series s1 non-neg-integers) 10)
;Cannot divide by a series with 0 constant
;Type D to debug error, Q to quit back to REP loop: 

; ============
;  Exercise 8
; ============

; INTEGRATE-SERIES-TAIL integrates s (without the constant)
(define (integrate-series-tail s)
  (define (helper s n)
    (cons-stream (/ (stream-car s) n)
		 (helper (stream-cdr s) (+ n 1))))
  (helper s 1))

; Testing...
(show-series (integrate-series-tail s2) 10)
; 1 1 1 1 1 1 1 1 1 1

; ============
;  Exercise 9
; ============

; EXP-SERIES is the Taylor expansion of e^x
(define exp-series
  (cons-stream 1 (integrate-series-tail exp-series)))

; Testing...
(show-series exp-series 10)
; 1 1 1/2 1/6 1/24 1/120 1/720 1/5040 1/40320 1/362880

; SINE is the Taylor expansion of sin(x)
(define sine
  (cons-stream 0 (integrate-series-tail cosine)))

; COSINE is the Taylor expansion of cos(x)
(define cosine
  (cons-stream 1 (negate-series (integrate-series-tail sine))))

; Testing...
(show-series sine 10)
; 0 1 0 -1/6 0 1/120 0 -1/5040 0 1/362880 

(show-series cosine 10)
; 1 0 -1/2 0 1/24 0 -1/720 0 1/40320 0 

; =============
;  Exercise 10
; =============

; APPROXIMATE computes successive approximations to f(x0)
(define (approximate f x)
  (define (solve f)
    (cons-stream (stream-car f) (scale-series x (solve (stream-cdr f)))))
  (partial-sums (solve f)))

; Testing...

(show-series (approximate exp-series 1) 10)
; 1 2 5/2 8/3 65/24 163/60 1957/720 685/252 109601/40320 98641/36288
; which does indeed approach e

(show-series (approximate exp-series 2) 10)
; 1 3 5 19/3 7 109/15 331/45 155/21 2327/315 20947/2835
; which approaches e^2