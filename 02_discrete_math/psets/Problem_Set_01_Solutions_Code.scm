;;; Discrete Math PS-1
;;; Shyam Visweswaran

;;; Exercise 3
;;; ADU-ball - To express any number >= 60 as a combination
;;; of 11s and 7s. 

(define (adu-ball n)
  (define (helper elevens sevens)
    (let ((total (+ (* 11 elevens) (* 7 sevens))))
      (cond ((< n 60)
	     '(Error: Enter a number greater than 59))
	    ((= total n)
	     (list 'goals: elevens 'misses: sevens))
	    ((> total n)
	     (helper 0 (+ 1 sevens)))
	    (else (helper (+ 1 elevens) sevens)))))
  (helper 0 0))

;; Lets test now
(adu-ball 59)
;Value: (error: enter a number greater than 59)
(adu-ball 60)
;Value: (goals: 1 misses: 7)
(adu-ball 61)
;Value: (goals: 3 misses: 4)
(adu-ball 62)
;Value: (goals: 5 misses: 1)
(adu-ball 63)
;Value: (goals: 0 misses: 9)
(adu-ball 64)
;Value: (goals: 2 misses: 6)
(adu-ball 65)
;Value: (goals: 4 misses: 3)
(adu-ball 66)
;Value: (goals: 6 misses: 0)
(adu-ball 67)
;Value: (goals: 1 misses: 8)
(adu-ball 68)
;Value: (goals: 3 misses: 5)
(adu-ball 69)
;Value: (goals: 5 misses: 2)
(adu-ball 70)
;Value: (goals: 0 misses: 10)
(adu-ball 34987)
;Value: (goals: 3180 misses: 1)
(adu-ball 342099)
;Value: (goals: 31098 misses: 3)


;;; Exercise 8
;;; Here i generate numbers using Euclid's algorithm (e_1=2 e_2=3 e_3=7 ...)
;;; and find that e_5=1807 is composite. Proc prime tests for primality
;;; and proc eucild generates e_1 e_2 e_3 etc.

(define (prime n)
  (define (helper n x)
    (cond ((= n 2) #t)     
	  ((= (remainder n 2) 0) #f)
	  ((= (remainder n x) 0) #f)      
	  ((> x (sqrt n)) #t)
	  (else (helper n (+ x 1)))))
  (helper n 2))

;; e is the euclid number, product is the running product
;; and count keeps track of the nth euclid term

(define (euclid)
 (define (helper count product e)
   (cond ((prime e)
	  (newline)
	  (display (list 'euclid 'number count e 'is 'prime))
	  (helper (+ 1 count) (* product e) (+ (* product e) 1)))
	 (else (newline)
	       (display (list 'euclid 'number count e 'is 'composite)))))
 (helper 1 1 2))

;; Lets run proc euclid
(euclid number 1 2 is prime)
(euclid number 2 3 is prime)
(euclid number 3 7 is prime)
(euclid number 4 43 is prime)
(euclid number 5 1807 is composite)

;;; Now the other way: I will generate primes in ascending order and
;;; apply Euclid's proof to see if i generate a prime or a composite

;; generate primes using streams and sieve from SICP
(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
	   (lambda (x)
	     (not (divisible? x (stream-car stream))))
	   (stream-cdr stream)))))

(define (divisible? x y) (= (remainder x y) 0))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

;; this is a stream of prime numbers
(define prime-numbers (sieve (integers-starting-from 2)))
 
;; now apply Euclid's method
;; p is the prime number generated from the stream above, product
;; is the running product of the primes so far, euclid's number
;; is (+ product 1)

(define (euclid2)
 (define (helper count p product)
   (cond ((prime (+ product 1))
	  (newline)
	  (display (list (+ product 1) 'is 'prime))
	  (helper (+ 1 count) (stream-ref prime-numbers (+ 1 count)) (* product p)))
	 (else (newline)
	       (display (list (+ product 1) 'is 'composite)))))
 (helper 0 (stream-ref prime-numbers 0) 1))

;; Now lets run proc euclid2
(euclid2)
(3 is prime)
(7 is prime)
(31 is prime)
(211 is prime)
(2311 is prime)
(30031 is composite)

;;;============================================================