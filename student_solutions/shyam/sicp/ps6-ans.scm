;;; SICP PS-6
;;; Shyam Visweswaran

;;;===========================================================
;;; Exercise 1
;;;===========================================================

;;; addition of =number? to THE NUMBER PACKAGE

;;; (RepNum, RepNum) --> Sch-Bool
(define (=number? x y) (= x y))

;;;===========================================================
;;; Exercise 2
;;;===========================================================

;;; addition of equ? to the GENERIC ARITHMRTIC OPERATIONS

;;; (GN, GN) --> Sch-Bool
(define (equ? x y) (apply-generic 'equ? x y))

;;; Install equ? in the generic operations
(put 'equ? '(number number) =number?)

;;; testing equ? on n1 n2 and n3

(define n1 (create-number 1))
;Value: (number . 1)

(define n2 (create-number 2))
;Value: (number . 2)

(define n3 n1)
;Value: (number . 1)

(equ? n1 n2)
;Value: #f

(equ? n1 n3)
;Value: #t

;;;=======================================================
;;; Exercise 3
;;;=======================================================

;;define some rational numbers

(define r5/13 (create-rational (create-number 5) (create-number 13)))
(define r2 (create-rational n2 n1)) ; n1 and n2 defined above

(define rsq (square (add r5/13 r2)))
;Value: (rational (number . 961) number . 169)

;;box-and-pointer diagram of rsq















;;;========================================================
;;; Exercise 4
;;;========================================================

;;; addition of equ-rat? to the RATIONAL ARITHMETIC PACKAGE

;;; (RatNum, RatNum) --> Sch-Bool
;;; use cross-product to test for equality
(define (equ-rat? x y)
  (equ? (mul (numer x) (denom y))
	(mul (denom x) (numer y))))
 
;;; install equ? in the generic operations
(put 'equ? '(rational rational) equ-rat?)

(equ? r5/13 r2)
;Value: #f

;; r3 is 1/2
(define r3 (create-rational
	    (create-number 1)
	    (create-number 2)))

;; r4 is 2/4
(define r4 (create-rational
	    (create-number 2)
	    (create-number 4)))

;; are r3 and r4 equal?
(equ? r3 r4)
;Value: #t

;;;===================================================
;;; Exercise 5
;;;===================================================

;; RepNum --> RepRat
(define (repnum->reprat n)
  (make-rat (create-number n) (create-number 1)))

(repnum->reprat 2)
;Value: ((number . 2) number . 1)

;;;===================================================
;;; Exercise 6
;;;===================================================

;; coercion methods for generic add, sub, mul and div
(define (RRmethod->NRmethod method)
  (lambda (num rat)
    (method
     (repnum->reprat num)
     rat)))

;; pull the required from the table itself so that changes in name of the
;; rational arithmetic operators will not affect us

(put 'add '(number rational) (RRmethod->NRmethod (get 'add '(rational rational))))
(put 'sub '(number rational) (RRmethod->NRmethod (get 'sub '(rational rational))))
(put 'mul '(number rational) (RRmethod->NRmethod (get 'mul '(rational rational))))
(put 'div '(number rational) (RRmethod->NRmethod (get 'div '(rational rational))))

(define (RRmethod->RNmethod method)
  (lambda (rat num)
    (method
     rat
     (repnum->reprat num))))

(put 'add '(rational number) (RRmethod->RNmethod (get 'add '(rational rational))))
(put 'sub '(rational number) (RRmethod->RNmethod (get 'sub '(rational rational))))
(put 'mul '(rational number) (RRmethod->RNmethod (get 'mul '(rational rational))))
(put 'div '(rational number) (RRmethod->RNmethod (get 'div '(rational rational))))

(put 'equ? '(number rational) (RRmethod->NRmethod (get 'equ? '(rational rational))))
(put 'equ? '(rational number) (RRmethod->RNmethod (get 'equ? '(rational rational))))

;; n2 is 2 and r2 is 2/1
(equ? n2 r2)
;Value: #t

;; no problems here since equ-rational? works on unreduced rational numbers
(equ? (sub (add n2 r5/13) r5/13) n2)
;Value: #t

;;;=======================================================================
;;; Exercise 7
;;;=======================================================================

;; since create-polynomial expects a list of GN; convert Sch-Num to GN
;; via map and call create-polynomial

(define (create-numerical-polynomial x Sch-Num-list)
  (create-polynomial x
		     (map create-number Sch-Num-list)))

(define p1 (create-numerical-polynomial 'x (list 1 5 0 -2)))
;Value: (polynomial x (3 (number . 1)) (2 (number . 5)) (0 (number . -2)))

;;;=======================================================================
;;; Exercise 8
;;;=======================================================================

;;map-terms is a map procedure that takes 2 arguments (a procedure and
;;a list) and applies the procedure to every term in the list and returns the
;;results in a new list.

(define (map-terms op lst)
  (map op lst))

(pp (square p1))
;(polynomial x
;            (6 (number . 1))
;            (5 (number . 10))
;            (4 (number . 25))
;            (3 (number . -4))
;            (2 (number . -20))
;            (0 (number . 4)))

(pp (square (square p1)))
;(polynomial x
;            (12 (number . 1))
;            (11 (number . 20))
;            (10 (number . 150))
;            (9 (number . 492))
;            (8 (number . 505))
;            (7 (number . -600))
;            (6 (number . -976))
;            (5 (number . 240))
;            (4 (number . 600))
;            (3 (number . -32))
;            (2 (number . -160))
;            (0 (number . 16)))


;;;===========================================================
;;; Exercise 9
;;;===========================================================

(define p2
  (create-polynomial
   'z
   (list
    p1
    (create-polynomial 'x (list (create-number 3)))
    (create-polynomial 'x (list (create-number 5))))))

;; create 4 rational terms
(define ratpoly1 (create-rational
	     (create-numerical-polynomial 'y '(3))
	     (create-numerical-polynomial 'y '(1 0))))

(define ratpoly2 (create-rational
		  (create-numerical-polynomial 'y '(1 0 1))
		  (create-numerical-polynomial 'y '(1 0))))

(define ratpoly3 (create-rational
		  (create-numerical-polynomial 'y '(1))
		  (create-numerical-polynomial 'y '(1 -1))))

(define ratpoly4 (create-rational
		  (create-numerical-polynomial 'y '(2))
		  (create-numerical-polynomial 'y '(1))))

;;and now create the polynomial p3
(define p3
  (create-polynomial 'x
		     (list ratpoly1
			   (create-number 0)
			   ratpoly2
			   ratpoly3
			   ratpoly4)))


;;;===========================================================
;;; Exercise 10
;;;===========================================================

;;square of p2

(pp (square p2))
;(polynomial
; z
; (4
;  (polynomial x
;              (6 (number . 1))
;              (5 (number . 10))
;              (4 (number . 25))
;              (3 (number . -4))
;              (2 (number . -20))
;              (0 (number . 4))))
; (3 (polynomial x (3 (number . 6)) (2 (number . 30)) (0 (number . -12))))
; (2 (polynomial x (3 (number . 10)) (2 (number . 50)) (0 (number . -11))))
; (1 (polynomial x (0 (number . 30))))
; (0 (polynomial x (0 (number . 25)))))

(pp (square p3))
;(polynomial
; x
; (8 (rational (polynomial y (0 (number . 9))) polynomial y (2 (number . 1))))
; (6 (rational (polynomial y (4 (number . 6)) (2 (number . 6))) polynomial y (4 (number . 1))))
; (5
;  (rational (polynomial y (2 (number . 6)) (1 (number . -6)))
;            polynomial
;            y
;            (4 (number . 1))
;            (3 (number . -2))
;            (2 (number . 1))))
; (4
;  (rational (polynomial y (6 (number . 1)) (4 (number . 2)) (3 (number . 12)) (2 (number . 1)))
;            polynomial
;            y
;            (4 (number . 1))))
; (3
;  (rational (polynomial y (4 (number . 2)) (3 (number . -2)) (2 (number . 2)) (1 (number . -2)))
;            polynomial
;            y
;            (4 (number . 1))
;            (3 (number . -2))
;            (2 (number . 1))))
; (2
;  (rational (polynomial y (5 (number . 4)) (4 (number . -8)) (3 (number . 8)) (2 (number . -7)) (1 (number . 4)))
;            polynomial
;            y
;            (4 (number . 1))
;            (3 (number . -2))
;            (2 (number . 1))))
; (1
;  (rational (polynomial y (1 (number . 4)) (0 (number . -4)))
;            polynomial
;            y
;            (2 (number . 1))
;            (1 (number . -2))
;            (0 (number . 1))))
; (0 (rational (polynomial y (0 (number . 4))) polynomial y (0 (number . 1)))))

;;;======================================================================
;;; Exercise 11
;;;======================================================================

;; RepTerms --> RepTerms
(define (negate-terms tlist)
  (map-terms
   (lambda (term) (negate-term term))
   tlist))

;; Repterm --> RepTerm
(define (negate-term t)
  (make-term
   (order t)
   (negate (coeff t))))

;; RepPoly --> RepPoly
(define (negate-poly p)
  (make-poly (variable p)
	     (negate-terms (term-list p))))

;; RepPoly --> ({polynomial} X RepPoly)
(define (negate-polynomial p)
  (make-polynomial (negate-poly p)))


;;;=================================================
;;; Exercise 12
;;;=================================================

;; (RepPoly, RepPoly) --> RepPoly
(define (sub-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (add-poly p1 (negate-poly p2))
      (error "Polys not in same variable -- SUB-POLY"
	     (list p1 p2))))
 
;; (RepPoly, RepPoly) --> ({polynomial} X RepPoly)
(define (sub-polynomial p1 p2)
  (make-polynomial (sub-poly p1 p2)))

;; RepPoly --> Sch-Bool
(define (equ-poly? p1 p2)
  (if (same-variable? (variable p1) (variable p2))
  (=zero-poly? (sub-poly p1 p2))
  (error "Polys not in same variable -- EQU-POLY?"
	 (list p1 p2))))

;; RepPoly --> Sch-Bool
(define (equ-polynomial? p1 p2)
  (equ-poly? p1 p2))

;;;==================================================
;;; Exercise 13
;;;==================================================

;; install negate, sub and equ? for polynomials

(put 'negate '(polynomial) negate-polynomial)
(put 'sub '(polynomial polynomial) sub-polynomial)
(put 'equ? '(polynomial polynomial) equ-polynomial?)

;; test above methods

(negate p1)
;Value: (polynomial x (3 (number . -1)) (2 (number . -5)) (0 (number . 2)))

(equ? p3 p3)
;Value: #t

(sub p1 p1)
;Value: (polynomial x)

(sub p2 p2)
;Value: (polynomial z)

(sub p3 p3)
;Value: (polynomial x)

;;
(sub p2 p3)
;Polys not in same variable -- SUB-POLY ((z ... ... ...) (x ... ... ... ...))

(sub p1 p3)
;No method -- APPLY-GENERIC (mul (number polynomial))

(sub p3 p1)
;No method -- APPLY-GENERIC (mul (polynomial number))

;;;==========================================================================
;;; Fix for bug in Exercise 13
;;;==========================================================================

;;;----------------------------------------------
;;; number --> polynomial coercion package
;;;----------------------------------------------


;;-1- make a polynomial from a number

;; (var, RepNum) --> RepPoly
(define (repnum->reppoly n var)
  (make-poly var (adjoin-term (make-term 0 (create-number n)) (the-empty-termlist))))

;; test
(repnum->reppoly 2 'x)
;Value: (x (0 (number . 2)))

;;-2- define PPmethod->NPmethod
(define (PPmethod->NPmethod method)
  (lambda (num poly)
    (method
     (repnum->reppoly num (variable poly))
     poly)))

;; test
((PPmethod->NPmethod add-polynomial) 2 (cdr p1))
;Value: (polynomial x (3 (number . 1)) (2 (number . 5)))

;;-3- install generic add, sub, mul
(put 'add '(number polynomial) (PPmethod->NPmethod (get 'add '(polynomial polynomial))))
(put 'sub '(number polynomial) (PPmethod->NPmethod (get 'sub '(polynomial polynomial))))
(put 'mul '(number polynomial) (PPmethod->NPmethod (get 'mul '(polynomial polynomial))))
(put 'equ? '(number polynomial) (PPmethod->NPmethod (get 'equ? '(polynomial polynomial))))

;; test
(add n1 p1)
;Value: (polynomial x (3 (number . 1)) (2 (number . 5)) (0 (number . -1)))
(add n2 p1)
;Value: (polynomial x (3 (number . 1)) (2 (number . 5)))

;;-4- define PPmethod->PNmethod
(define (PPmethod->PNmethod method)
  (lambda (poly num)
    (method
     poly
     (repnum->reppoly num (variable poly)))))

;; test
((PPmethod->PNmethod add-polynomial) (cdr p1) 2)

;Value: (polynomial x (3 (number . 1)) (2 (number . 5)))

;;-5- install generic add, sub, mul
(put 'add '(polynomial number) (PPmethod->PNmethod (get 'add '(polynomial polynomial))))
(put 'sub '(polynomial number) (PPmethod->PNmethod (get 'sub '(polynomial polynomial))))
(put 'mul '(polynomial number) (PPmethod->PNmethod (get 'mul '(polynomial polynomial))))
(put 'equ? '(polynomial number) (PPmethod->PNmethod (get 'equ? '(polynomial polynomial))))

;; test
(pp (add p1 n1))
;(polynomial x (3 (number . 1)) (2 (number . 5)) (0 (number . -1)))

(pp (mul p1 n2))
;(polynomial x (3 (number . 2)) (2 (number . 10)) (0 (number . -4)))

;; test
;(pp (sub p1 p3))
;(polynomial x
;            (4 (rational (polynomial y (0 (number . -3))) polynomial y (1 (number . 1))))
;            (3 (number . 1))
;            (2 (rational (polynomial y (2 (number . -1)) (1 (number . 5)) (0 (number . -1))) polynomial y (1 (number . 1))))
;            (1 (rational (polynomial y (0 (number . -1))) polynomial y (1 (number . 1)) (0 (number . -1))))
;            (0 (rational (polynomial y (0 (number . -4))) polynomial y (0 (number . 1)))))

(define p2xy
  (create-polynomial
   'x
   (list
    (create-numerical-polynomial 'y '(1 5 0 -2))
    (create-polynomial 'y (list (create-number 3)))
    (create-polynomial 'y (list (create-number 5))))))
				

;;;----------------------------------------------------
;;; rational --> polynomial package
;;;----------------------------------------------------

;;-1- make a polynomial from a rational

(define (reprat->reppoly r var)
  (make-poly var (adjoin-term (make-term 0 (make-rational r)) (the-empty-termlist))))

;; test
(reprat->reppoly (cdr r5/13) 'x)
;Value: (x (0 (rational (number . 5) number . 13)))

;;-2- define PPmethod->RPmethod
(define (PPmethod->RPmethod method)
  (lambda (rat poly)
    (method
     (reprat->reppoly rat (variable poly))
     poly)))

;; test
((PPmethod->RPmethod add-polynomial) (cdr r5/13) (cdr p1))
;Value: (polynomial x (3 (number . 1)) (2 (number . 5)) (0 (rational (number . -21) number . 13)))


;;-3- install generic add, sub, mul
(put 'add '(rational polynomial) (PPmethod->RPmethod (get 'add '(polynomial polynomial))))
(put 'sub '(rational polynomial) (PPmethod->RPmethod (get 'sub '(polynomial polynomial))))
(put 'mul '(rational polynomial) (PPmethod->RPmethod (get 'mul '(polynomial polynomial))))
(put 'equ? '(rational polynomial) (PPmethod->RPmethod (get 'equ? '(polynomial polynomial))))

;; test
(pp (add r5/13 p3))
;(polynomial x
;            (4 (rational (polynomial y (0 (number . 3))) polynomial y (1 (number . 1))))
;            (2 (rational (polynomial y (2 (number . 1)) (0 (number . 1))) polynomial y (1 (number . 1))))
;            (1 (rational (polynomial y (0 (number . 1))) polynomial y (1 (number . 1)) (0 (number . -1))))
;            (0 (rational (polynomial y (0 (number . 31))) polynomial y (0 (number . 13)))))

;;-4- define PPmethod->PRmethod
(define (PPmethod->PRmethod method)
  (lambda (poly rat)
    (method
     poly
     (reprat->reppoly rat (variable poly)))))

;; test
((PPmethod->PRmethod add-polynomial) (cdr p1) (cdr r5/13))
 
;;-5- install generic add, sub, mul
(put 'add '(polynomial rational) (PPmethod->PRmethod (get 'add '(polynomial polynomial))))
(put 'sub '(polynomial rational) (PPmethod->PRmethod (get 'sub '(polynomial polynomial))))
(put 'mul '(polynomial rational) (PPmethod->PRmethod (get 'mul '(polynomial polynomial))))
(put 'equ? '(polynomial rational) (PPmethod->PRmethod (get 'equ? '(polynomial polynomial))))

;; test
(pp (add p1 r5/13))
;Value: (polynomial x (3 (number . 1)) (2 (number . 5)) (0 (rational (number . -21) number . 13)))

(pp (sub p2xy p3))
;(polynomial
; x
; (4 (rational (polynomial y (0 (number . -3))) polynomial y (1 (number . 1))))
; (2
;  (polynomial
;   y
;   (3 (number . 1))
;   (2 (number . 5))
;   (0
;    (rational (polynomial y (2 (number . -1)) (1 (number . -2)) (0 (number . -1)))
;              polynomial
;              y
;              (1 (number . 1))))))
; (1
;  (polynomial
;   y
;   (0
;    (rational (polynomial y (1 (number . 3)) (0 (number . -4)))
;              polynomial
;              y
;              (1 (number . 1))
;              (0 (number . -1))))))
; (0 (polynomial y (0 (rational (polynomial y (0 (number . 3))) polynomial y (0 (number . 1)))))))

(pp (mul p1 p3))
;(polynomial
; x
; (7 (rational (polynomial y (0 (number . 3))) polynomial y (1 (number . 1))))
; (6 (rational (polynomial y (0 (number . 15))) polynomial y (1 (number . 1))))
; (5 (rational (polynomial y (2 (number . 1)) (0 (number . 1))) polynomial y (1 (number . 1))))
; (4
;  (rational (polynomial y (4 (number . 5)) (3 (number . -5)) (1 (number . 1)))
;            polynomial
;            y
;            (3 (number . 1))
;            (2 (number . -1))))
; (3 (rational (polynomial y (1 (number . 2)) (0 (number . 3))) polynomial y (1 (number . 1)) (0 (number . -1))))
; (2
;  (rational (polynomial y (2 (number . -2)) (1 (number . 10)) (0 (number . -2))) polynomial y (1 (number . 1))))
; (1 (rational (polynomial y (0 (number . -2))) polynomial y (1 (number . 1)) (0 (number . -1))))
; (0 (rational (polynomial y (0 (number . -4))) polynomial y (0 (number . 1)))))


;;;---------------------------------------------------------------------


