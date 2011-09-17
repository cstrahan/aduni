;;; Symbols, 11 October 2000

;;; Given the following definitions:

(define a 3)

(define b 4)

(define c (list 5 6))

(define d '(a b))

(define p (list cons +))

(define q '(cons +))

(define r (list 'cons '+))


;;; What is printed when the following expressions are evaluated?

(list 'a b)


'(a b)


(cons b d)


(list 'b c)


(car d)


((car p) 3 4)


((cadr p) 3 4)


((car r) 3 4)


((cadr q) 3 4)



;;; eq? = eqv? equal?

;;; If we've defined the following,

(define a (list 1 2 3))

(define b (list 1 2 3))

(define c a)

;;; What is returned from the following calls?

(eq? a b)


(eq? a c)


(eqv? a b)


(eqv? a c)


(equal? a b)


(equal? a c)


(eq? 2 2)


(eq? 1.4 1.4)


(eqv? 1.4 1.4)


;;; memq, memv, member

(memq 'c '(a b c d e))


(memq 'f '(a b c d e))


(memq 2.3 '(2 4 2.3 5 6))


(memv 2.3 '(2 4 2.3 5 6))


(memq '(a) '(a b (a) c d))


(memv '(a) '(a b (a) c d))


(member '(a) '(a b (a) c d))


(define la (list 'a))


(define lst (list (list 'a) 'b))


(memq la lst)


(memv la lst)


(member la lst)


(define lst2 (list la 'b))


(memq la lst2)


(memv la lst2)


(member la lst2)



;;; This file contains the symbolic differentiation code from Section 2.3.2
;;; of Abelson and Sussman.

(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp)
	 (if (same-variable? exp var) 1 0))
	((sum? exp)
	 (make-sum (deriv (addend exp) var)
		   (deriv (augend exp) var)))
	((product? exp)
	 (make-sum 
	  (make-product (multiplier exp)
			(deriv (multiplicand exp) var))
	  (make-product (deriv (multiplier exp) var)
			(multiplicand exp))))
	(else
	 (error "unknown expression type -- DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2) (list '+ a1 a2))

(define (make-product m1 m2) (list '* m1 m2))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))


;;; Code above does no simplification, so we will redefine 
;;; make-sum and make-product to do simplification

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
	((=number? a2 0) a1)
	((and (number? a1) (number? a2)) (+ a1 a2))
	(else (list '+ a1 a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	((=number? m1 1) m2)
	((=number? m2 1) m1)
	((and (number? m1) (number? m2)) (* m1 m2))
	(else (list '* m1 m2))))

;;; Adding differentiation for exponents

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base x) (cadr x))

(define (exponent x) (caddr x))

(define (make-exponentiation x y)
  (cond ((= y 0) 1)
	((= y 1) x)
	(else (list '** x y))))

;;; Change deriv to include exponentiation

(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp)
	 (if (same-variable? exp var) 1 0))
	((sum? exp)
	 (make-sum (deriv (addend exp) var)
		   (deriv (augend exp) var)))
	((product? exp)
	 (make-sum 
	  (make-product (multiplier exp)
			(deriv (multiplicand exp) var))
	  (make-product (deriv (multiplier exp) var)
			(multiplicand exp))))
	((exponentiation? exp)
	 (make-product (exponent exp)
		       (make-product (make-exponentiation (base exp) 
							  (- (exponent exp) 1))
				     (deriv (base exp) var))))
	(else
	 (error "unknown expression type -- DERIV" exp))))


