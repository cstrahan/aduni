;;; Problem Set 9 Solutions
;;;  

;;; ==========
;;; Exercise 1
;;; ==========

;;; Nothing to turn in here.

;;; ==========
;;; Exercise 2
;;; ==========

;;; Nothing to turn in here.

;;; ==========
;;; Exercise 3
;;; ==========

;;; Add OR to the evaluator by adding the clasue

	((or? exp)  (eval-or exp env))
 
;;; to mc-eval

;;; and by adding the following:

;;; Procedures to implement adu:or as a special form in our new language

(define (eval-or exp env)
  (define (iter clauses)
    (if (null? clauses)
	#f
	(if (true? (mc-eval (car clauses) env))
	    #t
	    (iter (cdr clauses)))))
  (iter (or-clauses exp)))

(define (or? exp) (tagged-list? exp 'adu:or))

(define (or-clauses exp) (cdr exp))



;;; ==========
;;; Exercise 4
;;; ==========

;;; Recall that a let expression
;;;
;;;
;;;
;;; (let ((<var1> <exp1>)
;;;      (<var2> <exp2>)
;;;      ...
;;;      (<varn> <expn>))
;;;  <body>)
;;;
;;; desugars to
;;;
;;;
;;; ((lambda (<var1> <var2> ... <varn>)
;;;    <body>)
;;;  <exp1> <exp2> ... <expn>)
;;;
;;; We can add the let expression to the metacircular evaluator by converting our
;;; let expression to the desugared form and then calling mc-eval on the  
;;; converted expression. 
;;;
;;; First, let's write the conversion procedures:
;;;

(define (let->combination exp)
    (cons (list 'adu:lambda 
               (let-variables exp)
               (let-body exp))
         (let-values exp)))

(define (let-variables exp)
   (map car (cadr exp)))

(define (let-values exp)
   (map cadr (cadr exp)))

(define (let-body exp)
   (caddr exp))

We now need to change mc-eval by adding the following to the cond statement:


((eq? (car exp) 'adu:let) (mc-eval (let->combination exp) env))


;;; ==========
;;; Exercise 5
;;; ==========

;;; Suppose we define
;;;

(adu:define (adu:foo adu:cond adu:else)
	    (adu:cond ((adu:= adu:cond 2) 0)
		      (adu:else (adu:else adu:cond))))
(adu:define adu:cond 3)
(adu:define (adu:else adu:x) (adu:* adu:x adu:x))
(adu:define (adu:square adu:x) (adu:* adu:x adu:x))

;;; Then

(adu:foo 2 adu:square)
;;; Will return 0.

(adu:foo 4 adu:square)
;;; Will return 16

(adu:cond ((adu:= adu:cond 2) 0)
	  (adu:else (adu:else 5)))
;;; Will return 25


;;; The first adu:cond and the first adu:else in foo never get evaluated.
;;; The first adu:cond trigers the reduction cond->if in the 
;;; meta-circular evaluator. The first adu:else is taken care of
;;; by a special case in expand-clauses, which is called by cond->if. 
;;; The other adu:cond and adu:else
;;; are evaluated normally.


;;; ==========
;;; Exercise 6
;;; ==========
 

;;; We add a reserved words list for special forms
;;; and a check it before we define a new variable.

(define reserved-words (list 'adu:define 'adu:quote 'adu:set! 'adu:if 'adu:and
			     'adu:lambda 'adu:begin 'adu:cond 'adu:or 'adu:let)) 

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
	     (add-binding-to-frame! var val frame))
	    ((eq? var (car vars))
	     (set-car! vals val))
	    (else (scan (cdr vars) (cdr vals)))))
    (if (memq var reserved-words)
	(error "DEFINE-VARIABLE! -- Redefinition of reserved word" var)
	(scan (frame-variables frame)
	      (frame-values frame)))))


;;; We also add a test in make-procedure that checks
;;; for use of reserved-words. We use list-intersection from
;;; problem set five. You could also use map here.

(define (list-intersection l1 l2)
  (cond ((null? l1) '())
	((member (car l1) l2)
	 (cons (car l1)
	       (list-intersection (cdr l1) l2)))
	(else (list-intersection (cdr l1) l2))))


(define (make-procedure parameters body env)
  (let ((a (list-intersection parameters reserved-words)))
    (if a
	(error "MAKE-PROCEDURE -- Redefinition of reserved word(s)" a)
	(list 'procedure parameters body env))))




;;; ==========
;;; Exercise 7
;;; ==========


;;; There are many answers to this problem
;;; Here are some examples from Holly's recitation notes
;;; www.ai.mit.edu/people/holly/6.001/12apr.html
;;;
;;; there are adu:'s missing from everything
;;; 

;;; Holly-Scheme: Changing mc-eval to accept a new language

;;; Today in class, we'll rewrite a few of the Scheme constructs to create a new 
;;; Scheme dialect called Holly-Scheme.  

;;; New combination form

;;; Instead of this in MIT-Scheme we want this in Holly-Scheme
 
 (+ 1 2)
                              (call + 1 2)
 (square 3)
                              (call square 3)


;;; What needs to change?

;;; Add the following to the cond statement in eval on p.366 of the notes: 


((eq? (car exp) 'call)
 (apply
    (eval (cadr exp) env)
    (list-of-values (cddr exp) env)))

;;; Additionally, we should remove the application? test from our 
;;; cond statement in eval. 

;;; New define form

;;;  Instead of this in MIT-Scheme
;;;                               we want this in Holly-Scheme
     (define a 3)
                                 (a := 3)
     (define foo (lambda (x) x))
                              (foo := (lambda (x) x))


;;;  What needs to change?

;;;  Add the following to the cond statement of eval on p.366 of the notes: 


((eq? (cadr exp) ':=)
 (eval-definition 
     (cons 'define (cons (car exp) (cddr exp))) 
     env))

;;; New procedure definition form

;;;  Instead of this in MIT-Scheme
;;;                                we want this in Holly-Scheme

 (lambda (x) x)
                                (procedure (params: x) (body: x))
 (lambda (x y) (+ x y) (* x y))
                                (procedure (params: x y) (body: (+ x y) (* x y)))


;;; What needs to change?

;;; Add the following to the cond statement of eval on p.366 of the notes: 


((eq? (car exp) 'procedure)
 (make-procedure (cdadr exp) (cdaddr exp) env))














