;;;; HUTLS.SCM
;;; These are utility procedures for Henderson-like graphics systems

;;; Representing points, vectors, segments

(define make-vect cons)
(define xcor-vect car)
(define ycor-vect cdr)

(define zero-vector
  (make-vect 0 0))

(define make-segment cons)
(define segment-start car)
(define segment-end cdr)

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (add-vect v1 (scale-vect -1 v2)))

(define (scale-vect x v)
  (make-vect (* x (xcor-vect v))
             (* x (ycor-vect v))))

;;; repeating an operation

(define (identity x) x)

(define (compose f g)
  (define (f*g x)
    (f (g x)))
  f*g)

(define (repeated f n)
  (cond ((= n 0) identity)
	((= n 1) f)
	(else (compose f (repeated f (- n 1))))))


;;; FOR-EACH is a system procedure.  It is 
;;;  shown here for reference.
;;;(define (for-each proc list)
;;;  (cond ((null? list) "done")
;;;        (else (proc (car list))
;;;              (for-each proc (cdr list)))))
