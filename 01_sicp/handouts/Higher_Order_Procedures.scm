;;; Code for SICP ADU Lecture on higher-order procedures

;;; Add integers between a and b

(define (sum-int a b)
  (if (> a b)
      0
      (+ a (sum-int (+ a 1) b))))


;;; Add squares of integers between a and b

(define (sum-squares a b)
  (if (> a b)
      0
      (+ (square a)
	 (sum-squares (+ a 1) b))))


;;; Add every other number between a and b
;;; a + (a+2) + (a+4) + ... + b
;;; Call it sum-odds, since we'll always call it with a odd

(define (sum-odds a b)
  (if (> a b)
      0
      (+ a (sum-odds (+ a 2) b))))


;;; Add 1/odd^2 between a and b -- assume start odd

(define (another-sum a b)
  (if (> a b)
      0
      (+ (/ 1 (square a))
	 (pi-sum (+ a 2) b))))


;;; Capture the pattern in the above procedures to write sum

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a) next b))))


;;; Use sum to rewrite sum-int

(define (new-sum-int a b)
  (sum (lambda (x) x) 
       a 
       (lambda (x) (+ x 1)) 
       b))


;;; Use sum to rewrite sum-squares

(define (new-sum-squares a b)
  (sum (lambda (x) (square x))
       a
       (lambda (x) (+ x 1))
       b))


;;; Use sum to rewrite sum-odds

(define (new-sum-odds a b)
  (sum (lambda (x) x)
       a 
       (lambda (x) (+ x 2))
       b))


;;; How to compute sum_(i=a to b) i^3+2i?

(define (yet-another-sum a b)
  (sum (lambda (x) (+ (* x x x) (* 2 x)))
       a
       (lambda (x) (+ 1 x))
       b))


;;; A procedure for exponentiation

(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))


;;; Use expt to create procedures to calculate b^2, b^3, b^4

(define (make-expt n)
  (lambda (b) (expt b n)))


;;; Write cube using make-expt

(define cube (make-expt 3))


;;; Write n^8 using make-expt

(define power-8 (make-expt 8))


;;; Let

(let ((a 2)
      (b 3)
      (c 4))
  (+ a b c))

;;; This de-sugars into

((lambda (a b c) (+ a b c)) 2 3 4)

