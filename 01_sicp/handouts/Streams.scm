;;; ADU SICP, October 2000
;;; Code for lecture on streams


;;; stream-ref returns the nth element of a stream (where the first
;;; element of the stream is counted as the 0th)
;;;
;;; NOTE: different from code on p.319 of textbook, which doesn't
;;;       check for stream-null?

(define (stream-ref s n)
  (cond ((stream-null? s) the-empty-stream)
	((= n 0) (stream-car s))
	(else (stream-ref (stream-cdr s) (- n 1)))))


;;; stream-map maps a procedure onto a stream

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
		   (stream-map proc (stream-cdr s)))))


;;; stream-for-each applies a procedure to each element of a 
;;; stream, but does not build the answers back up into a stream

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
	     (stream-for-each proc (stream-cdr s)))))


;;; can use stream-for-each to write a procedure for viewing
;;; streams
;;;
;;; NOTE: do not try this on infinite streams

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))


;;; defining some finite streams

(define a (cons-stream 1 (cons-stream 2 the-empty-stream)))

(define b (cons-stream 3 (cons-stream 4 (cons-stream 5 the-empty-stream))))


;;; a procedure to help us define a finite stream with integers
;;; between a given interval

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low (stream-enumerate-interval (+ low 1) high))))

(define stream-1-to-1000 (stream-enumerate-interval 1 1000))


;;; definitions for cons-stream, stream-car and stream-cdr use
;;; delay and force


;;; memoization: a way to prevent recalculating values every time 
;;; we access a particular element of the stream

(define (memo-proc proc)
  (let ((already-run? false)
	(result false))
    (lambda ()
      (if (not already-run?)
	  (begin (set! result (proc))
		 (set! already-run? true)
		 result)
	  result))))

;;; and now we redefine delay in terms of memo-proc


;;; Infinite streams

;;; a convenient procedure for printing the first n elements of a stream

(define (print-stream s n)
  (cond ((stream-null? s) the-empty-stream)
	((= n 0) 'done)
	(else (display-line (stream-car s))
	      (print-stream (stream-cdr s) (- n 1)))))
    

;;; defining add-streams

(define (add-streams s1 s2)
  (cons-stream (+ (stream-car s1) (stream-car s2))
	       (add-streams (stream-cdr s1)
			    (stream-cdr s2))))


;;; defining scale-stream

(define (scale-stream s factor)
  (cons-stream (* (stream-car s) factor)
	       (scale-stream (stream-cdr s) factor)))

;;; defining a few infinite streams

(define zeros (cons-stream 0 zeros))

(define ones (cons-stream 1 ones))

(define twos (cons-stream 2 twos))

(define odds (cons-stream 1 (add-streams odds twos)))


;;; defining integers, a stream of positive integers, two 
;;; different ways

(define integers
  (cons-stream 1 (add-streams integers ones)))

(define integers
  (add-streams (cons-stream 0 integers)
	       ones))

;;; defining stream of fibonnaci numbers

(define fib
  (cons-stream 1 (cons-stream 1 (add-streams fib
					     (stream-cdr fib)))))

;;; partial-sums takes a stream as an argument and returns the 
;;; stream whose elements are s0, s0+s1, s0+s1+s2, s0+s1+s2+s3, ...
;;; for example, (partial-sum integers) would be 1, 3, 6, 10, 15, ...

(define (partial-sums s)
  (cons-stream (stream-car s)
	       (add-streams (partial-sums s)
			    (stream-cdr s))))



;;; merge combines two ordered streams into one ordered result
;;; stream, eliminating repetitions

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
	((stream-null? s2) s1)
	(else (let ((s1car (stream-car s1))
		     (s2car (stream-car s2)))
		 (cond ((< s1car s2car)
			(cons-stream s1car 
				     (merge (stream-cdr s1) s2)))
		       ((> s1car s2car)
			(cons-stream s2car
				     (merge s1 (stream-cdr s2))))
		       (else
			(cons-stream s1car
				     (merge (stream-cdr s1)
					    (stream-cdr s2)))))))))

;;; A famous problem, first raised by R. Hamming, is to enumerate, 
;;; in ascending order with no repetitions, all positive integers
;;; with no prime factors other than 2, 3, or 5.  One obvious way 
;;; to do this is to simply test each integer in turn to see whether
;;; it has any factors other than 2, 3, and 5.  But this is very 
;;; inefficient, since, as the integers get larger, fewer and fewer
;;; of them fit the requirement.  As an alternative, let us call
;;; the required stream of numbers H and notice the follwoing facts
;;; about it.

;;; h begins with 1
;;; The elements of (scale-stream h 2) are also elements of s.
;;; The same is true for (scale-stream h 3) and (scale-stream h 5)
;;; These are all the elements of h.

(define h
  (cons-stream 1
	       (merge (scale-stream h 2)
		      (merge (scale-stream h 3)
			     (scale-stream h 5)))))
