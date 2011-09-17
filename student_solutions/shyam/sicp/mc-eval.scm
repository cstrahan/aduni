;;; mc-eval.scm
;;; Metacircular evaluator from section 4.1 of SICP
;;; mc-eval and mc-apply are the heart of the evaluator

;;; This code supercedes the code handed out in lecture yesterday.
;;; It includes the special form and, which was not included in 
;;; yesterday's notes.  There are also minor modifications to correct
;;; some errors and to add some primitive procedures.

(define (mc-eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
	((list? exp) (eval-list exp env))
	((map? exp) (eval-map exp env))
        ((if? exp) (eval-if exp env))
	((and? exp) (eval-and exp env))
	((or? exp) (eval-or exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp) (lambda-body exp) env))
        ((begin? exp) (eval-sequence (begin-actions exp) env))
        ((cond? exp) (mc-eval (cond->if exp) env))
	((let? exp) (mc-eval (let->combination exp) env))
        ((application? exp)
         (mc-apply (mc-eval (operator exp) env)
		(list-of-values (operands exp) env)))
        (else (error "Unknown expression type -- MC-EVAL"))))


(define (mc-apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
	  (extend-environment (procedure-parameters procedure)
			      arguments
			      (procedure-environment procedure))))
        (else (error "Unknown procedure type -- MC-APPLY" procedure))))

;;; list-of-values is used to produce the list of arguments to which
;;; a procedure is to be applied

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (mc-eval (first-operand exps) env)
	    (list-of-values (rest-operands exps) env))))


;;; eval-if evaluates the predicate part of an if expression in the
;;; given environment

(define (eval-if exp env)
  (if (true? (mc-eval (if-predicate exp) env))
      (mc-eval (if-consequent exp) env)
      (mc-eval (if-alternative exp) env)))


;;; eval-sequence is used by mc-apply to evaluate the sequence of 
;;; expressions in a procedure body and by eval to evaluate the
;;; sequence of expressions in a begin expression

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (mc-eval (first-exp exps) env))
        (else (mc-eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))


;;; eval-assignment handles assignment to variables

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (mc-eval (assignment-value exp) env)
                       env)
  'ok)


;;; eval-definition handles definition of variables

(define (eval-definition exp env)
	 (define-variable! (definition-variable exp)
		(mc-eval (definition-value exp) env)
		env)
	      'ok)


;;; Following are the procedures that define the syntax of our language
;;; We preface the usual scheme commands with "adu:" to allow us to 
;;; differentiate when we are using the metacircular evaluator to 
;;; evaluate expressions (it will only be used for "adu:" expressions)
;;; and when scheme code is being evaluated by the usual scheme 
;;; interpreter

;;; Self-evaluating entities: numbers and strings

(define (self-evaluating? exp)
  (cond ((number? exp) true)
	((string? exp) true)
	(else false)))

;;; Variables

(define (variable? exp) (symbol? exp))

;;; Quotations

(define (quoted? exp) 
  (tagged-list? exp 'adu:quote))

(define (text-of-quotation exp) (cadr exp))

;;; Special forms (in general)

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

;;; Assignment--- set!

(define (assignment? exp) 
  (tagged-list? exp 'adu:set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))

;;; Definitions

(define (definition? exp)
  (tagged-list? exp 'adu:define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)		;formal parameters
		   (cddr exp))))        ;body

;;; lambda expressions

(define (lambda? exp) (tagged-list? exp 'adu:lambda))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

;;; constructor for lambda expressions, used by definition-value above

(define (make-lambda parameters body)
  (cons 'adu:lambda (cons parameters body)))

;;; if conditionals

(define (if? exp) (tagged-list? exp 'adu:if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

;;; constructor for if expressions, to be used for cond->if to 
;;; transform cond expressions into if expressions

(define (make-if predicate consequent alternative)
  (list 'adu:if predicate consequent alternative))

;;; Procedures to implement adu:and as a special form in our new language

(define (eval-and exp env)
  (define (iter clauses)
    (if (null? clauses)
	#t
	(if (false? (mc-eval (car clauses) env))
	    #f
	    (iter (cdr clauses)))))
  (iter (and-clauses exp)))

(define (and? exp) (tagged-list? exp 'adu:and))

(define (and-clauses exp) (cdr exp))

;;; Exercise 3 - Procedures to implement adu:or as a special form in our language
;; Will return #f if no clauses or if every clause evaluates to
;; false. If a clause evaluates to true, its value is returned and
;; remaining clauses are not evaluated.

;; Routine to implement adu:or
(define (eval-or exp env)
  (define (iter clauses)   
      (if (null? clauses)
	  #f
	  (if (mc-eval (car clauses) env)
	      (mc-eval (car clauses) env)
	      (iter (cdr clauses)))))
  (iter (or-clauses exp)))

;; Routine to detect adu:or
(define (or? exp) (tagged-list? exp 'adu:or))

;; Routine to select adu:or clauses
(define (or-clauses exp) (cdr exp))



;;; begin expressions (a.k.a. sequences)

(define (begin? exp) (tagged-list? exp 'adu:begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))

;;; constructor sequence->exp for use by cond->if that transforms
;;; a sequence into a single expression, using begin if necessary

(define (sequence->exp seq)
  (cond ((null? seq) seq)
	((last-exp? seq) (first-exp seq))
	(else (make-begin seq))))

(define (make-begin seq) (cons 'adu:begin seq))

;;; procedure applications

(define (application? exp) (pair? exp))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? args) (null? args))

(define (first-operand args) (car args))

(define (rest-operands args) (cdr args))

;;; cond conditionals: a derived expression

(define (cond? exp) (tagged-list? exp 'adu:cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'adu:else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
	    (rest (cdr clauses)))
	(if (cond-else-clause? first)
	    (if (null? rest)
		(sequence->exp (cond-actions first))
		(error "ELSE clause isn't last -- COND->IF"
		       clauses))
	    (make-if (cond-predicate first)
		     (sequence->exp (cond-actions first))
		     (expand-clauses rest))))))


;;; Exercise 4 - let special form - a derived expression
;; Routine to detect expression
(define (let? exp) (tagged-list? exp 'adu:let))

;; Routines for pulling information from expression
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
  
;;; Data structures for our evaluator

;;; for conditionals, we accept anything that isn't the explicit false
;;; object to be true

(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))

;;; representing procedures

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))

(define (procedure-body p) (caddr p))

(define (procedure-environment p) (cadddr p))

;;; operations on environments

;;; an environment is represented as a list of frames

(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

;;; each frame of an environment is represented as a pair of lists:
;;; a list of the variables bound in that frame and a list of the 
;;; associated values

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))

(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

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


;;; to look up a variable in an environment, we scan the list of 
;;; variables in the first frame.  If we find the desired variable,
;;; we return the corresponding element in the list of values.  If 
;;; we do not find the variable in the current environment, we search
;;; the enclosing environment, and so on.  If we reach the empty 
;;; environment, signal an "unbound variable" error.

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
	     (env-loop (enclosing-environment env)))
	    ((eq? var (car vars))
	     (car vals))
	    (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
	(error "Unbound variable" var)
	(let ((frame (first-frame env)))
	  (scan (frame-variables frame)
		(frame-values frame)))))
  (env-loop env))

;;; to set a variable to a new value in a specified environment,
;;; we scan for the variable, just as in lookup-variable value,
;;; and change the corresponding value when we find it

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
	     (env-loop (enclosing-environment env)))
	    ((eq? var (car vals))
	     (set-car! vals val))
	    (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
	(error "Unbound variable -- SET!" var)
	(let ((frame (first-frame env)))
	  (scan (frame-variables frame)
		(frame-values frame)))))
  (env-loop env))

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


;;; The following functions are used to allow the metacircular
;;; evaluator to be run

(define (setup-environment)
  (let ((initial-env
	 (extend-environment (primitive-procedure-names)
			     (primitive-procedure-objects)
			     the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'adu:car car)
	(list 'adu:cdr cdr)
	(list 'adu:cons cons)
	(list 'adu:null? null?)
	(list 'adu:+ +)
	(list 'adu:- -)
	(list 'adu:* *)
	(list 'adu:/ /)
	(list 'adu:= =)
	(list 'adu:> >)
	(list 'adu:< <)))


(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

(define apply-in-underlying-scheme apply)


(define input-prompt ";;; MC-Eval input: ")

(define output-prompt ";;; MC-Eval value: ")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (mc-eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string))

(define (announce-output string)
  (display string))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure 
		     (procedure-parameters object)
		     (procedure-body object)
		     '<procedure-env>))
      (display object)))

(define the-global-environment (setup-environment))

(define *unparser-list-depth-limit* 7)
(define *unparser-list-breadth-limit* 10)

(define (mc-eval-loop)
  (set! the-global-environment (setup-environment))
  (driver-loop))
  

;;;Reserved words
(define reserved-words (list 'adu:and
			     'adu:define
			     'adu:or
			     'adu:if
			     'adu:cond
			     'adu:else))


(define (not-reserved? word-lst)
  (cond ((null? word-lst) #t)
	((member (car word-lst) reserved-words)
	 (error "Error: cannot use reserved word")
	(else (not-reserved? (cdr word-lst))))))
 

;;; Exercise 7
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

;; Implementation of adu:map
(define (map? exp) (tagged-list? exp 'adu:map))
(define (map-elts exp) cdr exp)
(define (eval-map proc lst env)
  (define (helper p l)
    (if (null? l)
	'()
	(cons (mc-eval (list proc (car l) env))
	      (helper p (cdr l)))))
  (helper proc (map-elts exp)))














