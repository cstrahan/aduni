;;; SICP PS-9
;;; Shyam Visweswaran

;;;======================================================================
;;; Exercise 1
;;;======================================================================

;; nothing to turn in

;;;======================================================================
;;; Exercise 2
;;;======================================================================

;; nothing to turn in

;;;======================================================================
;;; Exercise 3
;;;======================================================================

;;;Procedures to implement adu:or as a special form in our language
;; Will return #f if no clauses or if every clause evaluates to
;; false. If a clause evaluates to true, its value is returned and
;; remaining clauses are not evaluated.

;; Routine to detect adu:or
(define (or? exp) (tagged-list? exp 'adu:or))

;; Routine to select adu:or clauses
(define (or-clauses exp) (cdr exp))

;; Routine to implement adu:or
(define (eval-or exp env)
  (define (iter clauses)   
      (if (null? clauses)
	  #f
	  (if (mc-eval (car clauses) env)
	      (mc-eval (car clauses) env)
	      (iter (cdr clauses)))))
  (iter (or-clauses exp)))

;; Following line to be included in mc-eval
((or? exp) (eval-or exp env))

;; test the adu:or special form

;;; MC-Eval input: (adu:or (adu:< 1 2) (adu:< 2 1))
;;; MC-Eval value: #t

;;; MC-Eval input: (adu:or (adu:> 1 2) (adu:< 2 1) (adu:+ 2 3 4))
;;; MC-Eval value: 9


;;;============================================================
;;; Exercise 4
;;;============================================================

;;;Procedures to implement adu:let
;; Routine to detect expression
(define (let? exp) (tagged-list? exp 'adu:let))

;; Routines for selectors
(define (let-variables exp)
  (map car (cadr exp)))
(define (let-values exp)
  (map cadr (cadr exp)))
(define (let-body exp)
  (cddr exp))

;; Implementation of adu:let
(define (let->combination exp)
  (let ((vars (let-variables exp))
	(vals (let-values exp))
	(body (let-body exp)))
    (cons (make-lambda vars body)
	vals)))

;; Following line to be included in mc-eval
((let? exp) (mc-eval (let->combination exp) env))
 
;; testing adu:let

;;; MC-Eval input: (adu:define (f x)
(adu:let ((a 10) (b 5))
	 (adu:* a b x)))

;;; MC-Eval input: (f 2)
;;; MC-Eval value: 100

;;; MC-Eval input: (adu:define (g x)
(adu:let ((c 10) (d 10))
	 (adu:* c d)
	 x))

;;; MC-Eval input: (f 2)
;;; MC-Eval value: 2


;;;===========================================================
;;; Exercise 5
;;;===========================================================

; 1. this expression should evaluate to 0. Whenever, adu:cond
; is in the operator position, it functions as a regular cond
; since it is parsed by mc-eval normally. So, when the second
; line of the adu:foo is evaluated, adu:cond is parsed as expected
; However, in the first subexpression of the cond clause adu:cond
; appears in the operand position and acts as a variable. In this
; case the variable adu:cond is bound to 2 and the test evaluates
; to true and the consequent 0 is returned.

; 2. this expression should evaluate to 16. when line 3 of
; (adu:define (adu:foo ...) is evalauted, the first edu:else
; is parsed by mc-eval normally. The second adu:else functions
; as a variable and evluates to a squaring procedure in the
; global environment. adu:cond also functions as a variable
; and is bound to 4 in the immediate environment. Hence,
; squaring 4 returns 16

; 3. this expression should evaluate to 25. The first adu:cond
; is parsed normally by mc-eval. The second adu:cond on that line
; is avariable and is bound to 3 in th eglobal environment. since,
; 3 is not equal to 2, the second line is evaluated - the first
; adu:else is parsed normally by mc-eval and the second adu:else
; is looked up in the global environment and the squaring proc
; is returned. Hence 5 is squared and returned.


;;; MC-Eval input: (adu:foo 2 adu:square)
;;; MC-Eval value: 0

;;; MC-Eval input: (adu:foo 4 adu:square)
;;; MC-Eval value: 16

;;; MC-Eval input: (adu:cond ((adu:= adu:cond 2) 0)
(adu:else (adu:else 5)))
;;; MC-Eval value: 25

;;;============================================================
;;; Exercise 6
;;;============================================================

;;;Reserved words
(define reserved-words (list 'adu:and
			     'adu:define
			     'adu:or
			     'adu:if
			     'adu:cond
			     'adu:else))

;;;not-reserved? proc takes a list and tests to see if there
;;;are words common to the reserved-list. If a reserved word
;;;is found it returns an error; else it returns #t

(define (not-reserved? word-lst)
  (cond ((null? word-lst) #t)
	((member (car word-lst) reserved-words)
	 (error "Error: cannot use reserved word")
	(else (not-reserved? (cdr word-lst))))))


;;; to extend an environment by a new frame that associates variables
;;; with values, we make a frame consisting of the list of variables
;;; and the list of values, and we adjoin this to the environment -- 
;;; signal an error if the number of variables does not match the 
;;; number of values
;;; 2nd line added for Ex 6 to test for reserved words

(define (extend-environment vars vals base-env)
  (if (not-reserved? vars)
      (if (= (length vars) (length vals))
	  (cons (make-frame vars vals) base-env)
	  (if (< (length vars) (length vals))
	      (error "Too many arguments supplied" vars vals)
	      (error "Too few arguments supplied" vars vals)))))

;;; to define a variable, we search the first frame for a binding
;;; for the variable, and change the binding if it exists (just as
;;; in set-variable-value!).  If no such binding exists, we adjoin
;;; one to the first frame.
;;; 2nd line added for Ex 6 to test for reserved words

(define (define-variable! var val env)
  (if (not-reserved? (list var))
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
	     (add-binding-to-frame! var val frame))
	    ((eq? var (car vars))
	     (set-car! vals val))
	    (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
	  (frame-values frame)))))

;; test with Ex 5

;;; MC-Eval input:  (adu:define (adu:foo adu:cond adu:else)
(adu:cond ((adu:= adu:cond 2) 0)
	  (adu:else (adu:else adu:cond))))
;;; MC-Eval value: ok

;;; MC-Eval input: (adu:define adu:cond 3)
;;; MC-Eval value: ok

;;; MC-Eval input: (adu:define (adu:else adu:x) (adu:* adu:x adu:x))
;Error: cannot use reserved word

;;another test

;;; MC-Eval input: (adu:define (test adu:if x)
(adu:* adu:if x));;; MC-Eval value: ok

;;; MC-Eval input: (test 2 3)
;Error: cannot use reserved word

;;yet another test

;;; MC-Eval input: (adu:define adu:or 5)
;Error: cannot use reserved word


;;;===========================================================
;;; Exercise 7
;;;===========================================================
;; Procedures to implement adu:list in our language

;; Routine to detect expression
(define (list? exp) (tagged-list? exp 'adu:list))

;; Routine for selector
(define (list-elts exp) (cdr exp))

;; Implementation of adu:let
(define (eval-list exp env)
  (define (iter elts)
    (if (null? elts)
	'()
	(cons (mc-eval (car elts) env) (iter (cdr elts)))))
  (iter (list-elts exp)))

;; Now lets test adu:list

;;; MC-Eval input: (adu:list 1 2 3)
;;; MC-Eval value: (1 2 3)

;;; MC-Eval input: (adu:list (adu:+ 2 3) (adu:* 1 2 3) (adu:list 2 3 4))
;;; MC-Eval value: (5 6 (2 3 4))

;; Implementation of adu:map
(define (map? exp) (tagged-list? exp 'adu:map))
(define (map-elts exp) (cdr exp))
(define (eval-map proc lst env)
  (define (helper p l)
    (if (null? l)
	'()
	(cons (mc-eval (list proc (car l) env))
	      (helper p (cdr l)))))
  (helper proc (map-elts exp)))

;; test

;;; MC-Eval input: (adu:define a (adu:list 1 2 3))
;;; MC-Eval value: ok

;;; MC-Eval input: (adu:define (square x) (adu:* x x))
;;; MC-Eval value: ok

;;; MC-Eval input: (adu:map square a)
;;; MC-Eval value: (1 4 9)

;;;=======================================================