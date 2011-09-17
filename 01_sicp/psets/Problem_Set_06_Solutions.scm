; Problem Set 6 Solutions
;   by Michael Allen

; ============
;  Exercise 1
; ============

; =NUMBER: (RepNum,RepNum) -> SchBool
(define =number =)

; ============
;  Exercise 2
; ============

; Install EQU for (RepNum,RepNum)
(put 'equ '(number number) =number)
(define (equ? x y) (apply-generic 'equ x y))

(equ? (create-number 1) (create-number 1))
;Value: #t

(equ? (create-number 1) (create-number 2))
;Value: #f

; ============
;  Exercise 3
; ============

; Some rational numbers
(define r5/13 (create-rational (create-number 5) (create-number 13)))
(define r2 (create-rational (create-number 2) (create-number 1)))

(define rsq (square (add r5/13 r2)))

; Box and pointer for RSQ
;   |
;   V
; +---+---+  +---+---+  +---+---+
; |   |  --->|   |  --->|   |169|
; +-|-+---+  +-|-+---+  +-|-+---+
;   V          V          V
; rational   +---+---+  number
;            |   |961|
;            +-|-+---+
;              V
;            number

; ============
;  Exercise 4
; ============

; EQU-RAT: (RepRat, RepRat) -> SchBool
(define (equ-rat? x y)
  (equ? (mul (numer x) (denom y))
	(mul (denom x) (numer y))))

(put 'equ '(rational rational) equ-rat?)

(equ? r5/13 r5/13)
;Value: #t

(equ? r5/13 r2)
;Value: #f

; ============
;  Exercise 5
; ============

; REPNUM->REPRAT: RepNum -> RepRat
(define (repnum->reprat n)
  (make-rat (create-number n) (create-number 1)))

(repnum->reprat 2)
;Value: ((number . 2) number . 1)

; ============
;  Exercise 6
; ============

; Install (RepNum,RepRat) operations
(put 'add '(number rational) (RRmethod->NRmethod +rational))
(put 'sub '(number rational) (RRmethod->NRmethod -rational))
(put 'mul '(number rational) (RRmethod->NRmethod *rational))
(put 'div '(number rational) (RRmethod->NRmethod /rational))
(put 'equ '(number rational) (RRmethod->NRmethod equ-rat?))

; RRmethod->RNmethod: ((RepRat,RepRat)->a)->((RepRat,RepNum)->a)
(define (RRmethod->RNmethod method)
  (lambda (num rat)
    ((RRmethod->NRmethod method) rat num)))

; Install (RepRat,RepNum) operations
(put 'add '(rational number) (RRmethod->RNmethod +rational))
(put 'sub '(rational number) (RRmethod->RNmethod -rational))
(put 'mul '(rational number) (RRmethod->RNmethod *rational))
(put 'div '(rational number) (RRmethod->RNmethod /rational))
(put 'equ '(rational number) (RRmethod->RNmethod equ-rat?))

(equ? n2 r2)
;Value: #t

(equ? (sub (add n2 r5/13) r5/13) n2)
;Value: #t

; There is no unexpected answer here, but if your second test returned 
; false, it is because your equ-rat? procedure just compares the
; numerators and denominators and cannot handle unreduced fractions.

; ============
;  Exercise 7
; ============

; CREATE-NUMERICAL-POLYNOMIAL: (Variable,List(SchNum)) -> ({polynomial}xRepPoly)
(define (create-numerical-polynomial x clist)
  (create-polynomial x (map create-number clist)))

; A simple polynomial
(define p1 (create-numerical-polynomial 'x '(1 5 -2)))

(pp p1)
; (polynomial x (2 (number . 1)) (1 (number . 5)) (0 (number . -3)))

; ============
;  Exercise 8
; ============

; MAP-TERMS: (f,RepTerms) -> RepTerms
(define (map-terms func tlist)
  (if (empty-termlist? tlist)
      (the-empty-termlist)
      (adjoin-term (func (first-term tlist))
		   (map-terms func (rest-terms tlist)))))

(pp (square p1))
; (polynomial x
;             (4 (number . 1))
;             (3 (number . 10))
;             (2 (number . 19))
;             (1 (number . -30))
;             (0 (number . 9)))

(pp (square (square p1)))
; (polynomial x
;             (8 (number . 1))
;             (7 (number . 20))
;             (6 (number . 138))
;             (5 (number . 320))
;             (4 (number . -221))
;             (3 (number . -960))
;             (2 (number . 1242))
;             (1 (number . -540))
;             (0 (number . 81)))

; ============
;  Exercise 9
; ============

; Some more polynomials
(define p2
  (create-polynomial
   'z
   (list
    p1
    (create-polynomial 'x (list (create-number 3)))
    (create-polynomial 'x (list (create-number 5))))))

(define p3
  (create-polynomial
   'x
   (list
    (create-rational (create-numerical-polynomial 'y '(3))
		     (create-numerical-polynomial 'y '(1 0)))
    (create-number 0)
    (create-rational (create-numerical-polynomial 'y '(1 0 1))
		     (create-numerical-polynomial 'y '(1 0)))
    (create-rational (create-numerical-polynomial 'y '(1))
		     (create-numerical-polynomial 'y '(1 -1)))
    (create-rational (create-numerical-polynomial 'y '(2))
		     (create-numerical-polynomial 'y '(1))))))

; =============
;  Exercise 10
; =============

; Squaring polynomials
(pp (square p2))

; (polynomial z (4 (polynomial x
;                              (4 (number . 1))
;                              (3 (number . 10))
;                              (2 (number . 19))
;                              (1 (number . -30))
;                              (0 (number . 9))))
;               (3 (polynomial x
;                              (2 (number . 6))
;                              (1 (number . 30))
;                              (0 (number . -18))))
;               (2 (polynomial x 
;                              (2 (number . 10))
;                              (1 (number . 50))
;                              (0 (number . -21))))
;               (1 (polynomial x
;                              (0 (number . 30))))
;               (0 (polynomial x
;                              (0 (number . 25)))))

(pp (square p3))
(pp (square (square p2)))
; These results are too large to include here.

; =============
;  Exercise 11
; =============

; NEGATE-TERMS: RepTerms -> RepTerms
(define (negate-terms tlist)
  (map-terms (lambda (x) (make-term (order x) (negate (coeff x)))) tlist))

; NEGATE-POLY: RepPoly -> RepPoly
(define (negate-poly p)
  (make-poly (variable p) (negate-terms (term-list p))))

; =============
;  Exercise 12
; =============

; SUB-POLY: (RepPoly,RepPoly)->RepPoly
(define (sub-poly p1 p2)
  (add-poly p1 (negate-poly p2)))

; SUB-POLYNOMIAL: (RepPoly,RepPoly) -> ({polynomial}xRepPoly)
(define (sub-polynomial p1 p2)
  (make-polynomial (sub-poly p1 p2)))

; EQU-POLY?: (RepPoly,RepPoly)->SchBool
(define (equ-poly? p1 p2)
  (=zero-poly? (sub-poly p1 p2)))

; EQU-POLYNOMIAL?: (RepPoly,RepPoly)->SchBool
(define (equ-polynomial? p1 p2)
  (equ-poly? p1 p2))

; =============
;  Exercise 13
; =============

; Install (RepPoly,RepPoly) operations
(put 'negate '(polynomial) negate-polynomial)
(put 'sub '(polynomial polynomial) sub-polynomial)
(put 'equ '(polynomial polynomial) equ-polynomial?)

; Stupid polynomial tricks
(equ? p1 p1)
;Value: #t

(equ? (sub (sub p2 (negate p2)) p2) p2)
;Value: #t

(equ? (sub p1 p1) (sub p3 p3))
;Value: #t

(equ? (sub p1 p1) p1)
;Value: #f
